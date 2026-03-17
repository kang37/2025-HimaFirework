# 长期数据CCM因果分析：分析百度指数与安踏股价的长期因果关系
# 数据范围：2025-09-05 至 2025-12-31
library(dplyr)
library(ggplot2)
library(rEDM)
library(tidyquant)
library(tidyr)
library(gridExtra)
library(zoo)
library(openxlsx)
library(lubridate)
library(patchwork)

# Data ----
# 1.1 读取百度指数数据
read_baidu_index <- function(filename, keyword_col_name) {
  df <- read.xlsx(filename) %>%
    mutate(时间 = as_date(时间), `搜索pc+移动` = as.numeric(`搜索pc+移动`)) %>%
    select(date = 时间, !!keyword_col_name := "搜索pc+移动")
  return(df)
}

baidu_caiguoqiang <- read_baidu_index("data_raw/baidu_caiguoqiang.xlsx", "caiguoqiang_cn")
baidu_shizuniao <- read_baidu_index("data_raw/baidu_shizuniao.xlsx", "arctery_cn")
baidu_anta <- read_baidu_index("data_raw/baidu_anta.xlsx", "anta_cn")
baidu_ximalaya <- read_baidu_index("data_raw/baidu_ximalaya.xlsx", "himalaya_cn")

baidu_trend <- baidu_caiguoqiang %>%
  full_join(baidu_shizuniao, by = "date") %>%
  full_join(baidu_anta, by = "date") %>%
  full_join(baidu_ximalaya, by = "date") %>%
  arrange(date)

cat("百度指数数据范围:", as.character(min(baidu_trend$date)), "至",
    as.character(max(baidu_trend$date)), "\n")
cat("数据点数:", nrow(baidu_trend), "\n\n")

# 1.2 读取股价数据
stock_data <- tidyquant::tq_get(
  "2020.HK",
  from = min(baidu_trend$date),
  to = max(baidu_trend$date)
) %>%
  select(date, adj_close = adjusted) %>%
  arrange(date)

cat("股价数据交易日数:", nrow(stock_data), "\n\n")

# 1.3 合并数据并补全周末/节假日股价
anta_data <- baidu_trend %>%
  left_join(stock_data, by = "date") %>%
  arrange(date) %>%
  mutate(adj_close = zoo::na.locf(adj_close, na.rm = FALSE)) %>%
  filter(!is.na(adj_close)) %>%
  mutate(
    stock_change = adj_close - lag(adj_close),
    is_trading_day = date %in% stock_data$date
  ) %>%
  filter(!is.na(stock_change))

# 2. 数据去趋势和标准化
detrend_linear <- function(x) {
  if (all(is.na(x))) return(x)
  valid_idx <- !is.na(x)
  if (sum(valid_idx) < 2) return(x)

  time_idx <- 1:length(x)
  result <- rep(NA_real_, length(x))
  lm_model <- lm(x[valid_idx] ~ time_idx[valid_idx])
  result[valid_idx] <- residuals(lm_model)
  return(result)
}

# 对所有百度指数变量进行去趋势处理
anta_data_processed <- anta_data %>%
  mutate(
    stock_norm = detrend_linear(adj_close),
    anta_norm = detrend_linear(anta_cn),
    arctery_norm = detrend_linear(arctery_cn),
    caiguoqiang_norm = detrend_linear(caiguoqiang_cn),
    himalaya_norm = detrend_linear(himalaya_cn),
    change_norm = detrend_linear(stock_change)
  ) %>%
  filter(!is.na(stock_norm), !is.na(anta_norm), !is.na(change_norm))

cat("处理后数据点数:", nrow(anta_data_processed), "\n\n")

# 3. 确定最优嵌入维度E
embed_data <- data.frame(
  time = 1:nrow(anta_data_processed),
  stock = anta_data_processed$stock_norm,
  anta = anta_data_processed$anta_norm,
  arctery = anta_data_processed$arctery_norm,
  caiguoqiang = anta_data_processed$caiguoqiang_norm,
  change = anta_data_processed$change_norm
)

n_data <- nrow(embed_data)
lib_end <- floor(n_data * 0.7)

cat("计算最优嵌入维度...\n")
E_stock <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 6,
  columns = "stock",
  target = "stock",
  showPlot = FALSE
)

E_anta <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 6,
  columns = "anta",
  target = "anta",
  showPlot = FALSE
)

best_E_stock <- E_stock$E[which.max(E_stock$rho)]
best_E_anta <- E_anta$E[which.max(E_anta$rho)]
best_E <- round(max(c(best_E_stock, best_E_anta)))

cat("最优嵌入维度:\n",
    "  Stock E =", best_E_stock, "\n",
    "  Anta Baidu E =", best_E_anta, "\n",
    "  选择的E =", best_E, "\n\n")

# 4. 多变量多Tp CCM分析
tp_values <- c(0, 1, 2, 3, 5, 7)

# 定义要分析的变量对
variable_pairs <- list(
  list(name = "Anta", col = "anta"),
  list(name = "Arc'teryx", col = "arctery"),
  list(name = "Cai Guoqiang", col = "caiguoqiang")
)

max_lib <- nrow(embed_data) - best_E - max(tp_values)
lib_sizes_str <- sprintf("10 %d 5", max_lib)

cat("库大小范围: 10 至", max_lib, "\n")
cat("开始CCM分析...\n\n")

# 存储所有结果
ccm_results_all <- list()

for (var_info in variable_pairs) {
  var_name <- var_info$name
  var_col <- var_info$col

  cat(sprintf("分析 %s 与股价的因果关系...\n", var_name))

  ccm_data <- data.frame(
    time = 1:nrow(anta_data_processed),
    stock = anta_data_processed$stock_norm,
    baidu = anta_data_processed[[paste0(var_col, "_norm")]]
  )

  for (tp in tp_values) {
    cat(sprintf("  [Tp = %d]\n", tp))

    ccm_result <- CCM(
      dataFrame = ccm_data,
      E = best_E,
      Tp = tp,
      columns = "baidu",
      target = "stock",
      libSizes = lib_sizes_str,
      sample = 100,
      random = TRUE,
      seed = 123 + tp,
      showPlot = FALSE
    ) %>%
      pivot_longer(cols = c("baidu:stock", "stock:baidu")) %>%
      separate_wider_delim(cols = name, delim = ":", names = c("to", "from")) %>%
      rename(lib_size = LibSize, rho = value) %>%
      mutate(
        tp = tp,
        variable = var_name,
        dir = case_when(
          from == "baidu" & to == "stock" ~ paste0("Stock xmap ", var_name),
          from == "stock" & to == "baidu" ~ paste0(var_name, " xmap Stock")
        )
      )

    ccm_results_all[[paste0(var_col, "_Tp", tp)]] <- ccm_result
  }
  cat("\n")
}

# 合并所有结果
ccm_all_data <- bind_rows(ccm_results_all)

# 5. 绘制CCM收敛图（按变量分面）
png(
  paste0("data_proc/ccm_convergence_long_", Sys.Date(), ".png"),
  width = 2400, height = 1800, res = 300
)

ccm_all_data %>%
  ggplot(aes(lib_size, rho, color = dir, linetype = factor(tp))) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
    name = "Tp"
  ) +
  theme_bw(base_size = 12) +
  labs(
    x = "Library size",
    y = "Cross-mapping skill (rho)",
    color = "Direction",
    title = "Long-term CCM Analysis: Baidu Index vs Stock Price",
    subtitle = paste0("Data range: ", min(anta_data_processed$date), " to ",
                      max(anta_data_processed$date))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(size = 11, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 1))

dev.off()
cat("CCM收敛图已保存\n\n")

# 6. 输出收敛性评估
ccm_summary <- ccm_all_data %>%
  group_by(variable, tp, dir) %>%
  summarise(
    max_rho = max(rho, na.rm = TRUE),
    final_rho = last(rho),
    convergence = final_rho - first(rho),
    .groups = "drop"
  ) %>%
  arrange(variable, dir, tp)

cat("CCM收敛性评估结果:\n")
print(ccm_summary, n = 50)

# 保存汇总结果
write.xlsx(ccm_summary, paste0("data_proc/ccm_summary_long_", Sys.Date(), ".xlsx"))
cat("\n汇总结果已保存至 data_proc/ccm_summary_long_", Sys.Date(), ".xlsx\n\n")

# 7. S-map确定因果类型
cat("计算S-map系数以确定因果效应方向...\n")

calculate_smap_coefficient <- function(data, E, target_col, lib_col, tp = 0) {
  smap_data <- data.frame(
    time = 1:nrow(data),
    target = data[[target_col]],
    library = data[[lib_col]]
  )

  smap_result <- SMap(
    dataFrame = smap_data,
    lib = paste("1", floor(nrow(smap_data) * 0.8)),
    pred = paste(1, nrow(smap_data)),
    E = E,
    Tp = tp,
    columns = "library",
    target = "target",
    theta = 2,
    embedded = FALSE
  )

  if ("coefficients" %in% names(smap_result)) {
    coef_matrix <- smap_result$coefficients
  } else if ("smap_coefficients" %in% names(smap_result)) {
    coef_matrix <- smap_result$smap_coefficients
  } else {
    return(list(mean_coefficient = NA, sd_coefficient = NA))
  }

  lib_cols <- grep("library", colnames(coef_matrix), value = TRUE)

  if (length(lib_cols) > 0) {
    lib_coefficients <- as.matrix(coef_matrix[, lib_cols])
    mean_coef <- mean(lib_coefficients, na.rm = TRUE)
    sd_coef <- sd(as.vector(lib_coefficients), na.rm = TRUE)
  } else {
    mean_coef <- NA
    sd_coef <- NA
  }

  return(list(mean_coefficient = mean_coef, sd_coefficient = sd_coef))
}

# 对各变量计算S-map系数
smap_results <- list()
for (var_info in variable_pairs) {
  var_name <- var_info$name
  var_col <- var_info$col

  cat(sprintf("  计算 %s 的S-map系数...\n", var_name))

  for (tp in c(0, 1, 2, 3)) {
    tryCatch({
      smap_result <- calculate_smap_coefficient(
        data = anta_data_processed,
        E = best_E,
        target_col = "stock_norm",
        lib_col = paste0(var_col, "_norm"),
        tp = tp
      )

      smap_results[[paste0(var_col, "_Tp", tp)]] <- data.frame(
        variable = var_name,
        tp = tp,
        smap_mean = smap_result$mean_coefficient,
        smap_sd = smap_result$sd_coefficient,
        effect = ifelse(smap_result$mean_coefficient > 0, "Positive", "Negative")
      )

      cat(sprintf("    Tp=%d: %.4f +/- %.4f (%s)\n",
                  tp, smap_result$mean_coefficient, smap_result$sd_coefficient,
                  ifelse(smap_result$mean_coefficient > 0, "促进", "抑制")))
    }, error = function(e) {
      cat(sprintf("    Tp=%d: 计算失败 - %s\n", tp, e$message))
    })
  }
}

# 汇总S-map结果
smap_summary <- bind_rows(smap_results)
cat("\nS-map因果效应汇总:\n")
print(smap_summary)

# 保存S-map结果
write.xlsx(smap_summary, paste0("data_proc/smap_summary_long_", Sys.Date(), ".xlsx"))
cat("\nS-map结果已保存至 data_proc/smap_summary_long_", Sys.Date(), ".xlsx\n")

cat("\n===== 长期CCM分析完成 =====\n")
