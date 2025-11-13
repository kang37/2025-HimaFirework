# 安踏股价与"安踏"百度指数的CCM因果分析：因果方向和促进/抑制。
library(dplyr)
library(ggplot2)
library(rEDM)
library(tidyquant)
library(tidyr)
library(gridExtra)
library(zoo)
library(openxlsx)

# Data ----
# 1.2 读取股价数据
stock_data <- tidyquant::tq_get(
  "2020.HK", 
  from = min(baidu_trend$date),
  to = max(baidu_trend$date)
) %>% 
  select(date, adj_close = adjusted) %>%
  arrange(date)

# 1.3 合并数据并补全周末/节假日股价

# 先用left_join保留所有百度指数的日期
anta_data <- baidu_trend %>%
  left_join(stock_data, by = "date") %>%
  arrange(date) %>%
  # 向前填充股价（使用前一个交易日的价格）
  mutate(
    # 将NA填充为前一个非NA值。
    adj_close = zoo::na.locf(adj_close, na.rm = FALSE)
  ) %>%
  # 移除开始的NA（如果第一天没有股价数据）
  filter(!is.na(adj_close)) %>%
  mutate(
    # 计算股价变化量（相对于上一个日期，不管是否交易日）
    stock_change = adj_close - lag(adj_close),
    # 标记是否为交易日
    is_trading_day = date %in% stock_data$date
  ) %>%
  filter(!is.na(stock_change))

# 2. 数据去趋势和标准化
# 线性去趋势函数
detrend_linear <- function(x) {
  if (all(is.na(x))) return(x)
  valid_idx <- !is.na(x)
  if (sum(valid_idx) < 2) return(x)
  
  time_idx <- 1:length(x)
  result <- rep(NA_real_, length(x))
  
  # 只对非NA值进行线性去趋势
  lm_model <- lm(x[valid_idx] ~ time_idx[valid_idx])
  result[valid_idx] <- residuals(lm_model)
  
  return(result)
}

anta_data_processed <- anta_data %>%
  mutate(
    stock_norm = detrend_linear(adj_close),
    # 要检测哪个变量就输入其变量名。
    baidu_norm = detrend_linear(anta_cn), 
    change_norm = detrend_linear(stock_change)
  ) %>%
  # 移除任何包含NA的行。
  filter(!is.na(stock_norm), !is.na(baidu_norm), !is.na(change_norm))

# 3. 确定最优嵌入维度E
embed_data <- data.frame(
  time = 1:nrow(anta_data_processed),
  stock = anta_data_processed$stock_norm,
  baidu = anta_data_processed$baidu_norm,  
  change = anta_data_processed$change_norm
)

n_data <- nrow(embed_data)
lib_end <- floor(n_data * 0.7)

# 计算最优嵌入维度。
E_stock <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "stock",
  target = "stock",
  showPlot = FALSE
)

E_baidu <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "baidu",  # 修改为百度指数
  target = "baidu",
  showPlot = FALSE
)

E_change <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "change",
  target = "change",
  showPlot = FALSE
)

best_E_stock <- E_stock$E[which.max(E_stock$rho)]
best_E_baidu <- E_baidu$E[which.max(E_baidu$rho)]  # 修改变量名
best_E_change <- E_change$E[which.max(E_change$rho)]
best_E <- round(max(c(best_E_stock, best_E_baidu, best_E_change)))

cat(
  "最优嵌入维度：\n", 
  "  Stock E =", best_E_stock, "\n", 
  "  Baidu E =", best_E_baidu, "\n", 
  "  选择的E =", best_E, "\n"
)

# 4. 多Tp CCM分析
tp_values <- c(0, 1, 2, 3)

# 准备数据（修改为使用百度指数）
ccm_data_level <- data.frame(
  time = 1:nrow(anta_data_processed),
  stock = anta_data_processed$stock_norm,
  baidu = anta_data_processed$baidu_norm  # 修改为百度指数
)

ccm_data_change <- data.frame(
  time = 1:nrow(anta_data_processed),
  change = anta_data_processed$change_norm,
  baidu = anta_data_processed$baidu_norm  # 修改为百度指数
)

max_lib <- nrow(ccm_data_level) - best_E - max(tp_values)
lib_sizes_str <- sprintf("10 %d 3", max_lib)

cat("库大小范围：10 到", max_lib, "\n")

# 运行CCM分析
ccm_results_all <- list()
for (tp in tp_values) {
  cat("  [Tp =", tp, "]\n")
  
  # 股价水平
  ccm_level <- CCM(
    dataFrame = ccm_data_level,
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
    mutate(tp = tp)
  
  # 股价变化量。
  ccm_change <- CCM(
    dataFrame = ccm_data_change,
    E = best_E,
    Tp = tp,
    columns = "baidu",
    target = "change",
    libSizes = lib_sizes_str,
    sample = 100,
    random = TRUE,
    seed = 456 + tp
  ) %>% 
    pivot_longer(cols = c("baidu:change", "change:baidu")) %>%  # 修改列名
    separate_wider_delim(cols = name, delim = ":", names = c("to", "from")) %>% 
    rename(lib_size = LibSize, rho = value) %>% 
    mutate(tp = tp)
  
  ccm_results_all[[paste0("Tp", tp, "_level")]] <- ccm_level
  ccm_results_all[[paste0("Tp", tp, "_change")]] <- ccm_change
  
  cat("\t完成\n")
}

# 合并所有结果。
ccm_all_data <- bind_rows(ccm_results_all) %>% 
  mutate(
    dir = case_when(
      from == "baidu" & to == "stock" ~ "Stock xmap Baidu", 
      from == "baidu" & to == "change" ~ "Stock change xmap Baidu", 
      from == "stock" & to == "baidu" ~ "Baidu xmap Stock", 
      from == "change" & to == "baidu" ~ "Baidu xmap Stock change"
    ), 
    type = case_when(
      from == "stock" | to == "stock" ~ "Stock",
      from == "change" | to == "change" ~ "Stock change"
    )
  )

# 绘制CCM收敛图
png(
  paste0("data_proc/ccm_convergence_baidu_", Sys.Date(), ".png"), 
  width = 2000, height = 1000, res = 300
)
ccm_all_data %>% 
  # 仅保留股价本身因果分析的结果。
  filter(type == "Stock") %>% 
  ggplot() + 
  geom_line(aes(lib_size, rho, col = dir), linewidth = 1.2) + 
  facet_wrap(
    .~ tp, labeller = labeller(tp = function(x) paste("Tp =", x)), 
    nrow = 2
  ) + 
  scale_color_manual(
    breaks = c(
      "Baidu xmap Stock", "Stock xmap Baidu", 
      "Baidu xmap Stock change", "Stock change xmap Baidu"
    ),
    values = c("lightblue3", "darkred", "darkgreen", "orange")
  ) +
  theme_bw(base_size = 14) + 
  labs(
    x = "Library size", 
    y = "Cross-mapping skill (ρ)",
    col = "Direction"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")
  )
dev.off()

# 输出收敛性评估
ccm_summary <- ccm_all_data %>%
  filter(type == "Stock") %>%
  group_by(tp, dir) %>%
  summarise(
    max_rho = max(rho, na.rm = TRUE),
    final_rho = last(rho),
    convergence = final_rho - first(rho),
    .groups = "drop"
  ) %>% 
  arrange(dir, tp)
print(ccm_summary)

# 5. S-map确定因果类型。
# 函数：S-map计算交互强度系数
calculate_smap_coefficient <- function(data, E, target_col, lib_col, tp = 0) {
  # 准备数据
  smap_data <- data.frame(
    time = 1:nrow(data),
    target = data[[target_col]],
    library = data[[lib_col]]
  )
  
  # 运行S-map
  smap_result <- SMap(
    dataFrame = smap_data,
    lib = paste("1", floor(nrow(smap_data) * 0.8)),
    pred = paste(1, nrow(smap_data)),
    E = E,
    Tp = tp,
    columns = "library",
    target = "target",
    theta = 2,  # 非线性参数
    embedded = FALSE
  )
  
  # 提取系数
  # 同时检查两种可能的命名
  if("coefficients" %in% names(smap_result)) {
    coef_matrix <- smap_result$coefficients
  } else if("smap_coefficients" %in% names(smap_result)) {
    coef_matrix <- smap_result$smap_coefficients
  } else {
    warning("无法找到S-map系数")
    return(list(mean_coefficient = NA, sd_coefficient = NA))
  }
  
  # 提取library变量的系数（排除C0截距）
  # 找到包含"library"的列
  lib_cols <- grep("library", colnames(coef_matrix), value = TRUE)
  
  if(length(lib_cols) > 0) {
    # 提取所有library相关的系数
    lib_coefficients <- as.matrix(coef_matrix[, lib_cols])
    
    # 计算平均系数和标准差
    mean_coef <- mean(lib_coefficients, na.rm = TRUE)
    sd_coef <- sd(as.vector(lib_coefficients), na.rm = TRUE)
  } else {
    mean_coef <- NA
    sd_coef <- NA
  }
  
  return(list(
    mean_coefficient = mean_coef,
    sd_coefficient = sd_coef
  ))
}

# 计算百度指数对股价的影响性质：百度指数增加导致股价增加还是减少？
smap_results <- list()
for(tp in tp_values) {
  cat(sprintf("  计算 Tp=%d 的S-map系数...\n", tp))
  
  # 股价水平：百度指数对stock的影响
  tryCatch({
    smap_stock <- calculate_smap_coefficient(
      data = anta_data_processed,
      E = best_E,
      target_col = "stock_norm",
      lib_col = "baidu_norm",  # 修改为百度指数
      tp = tp
    )
    
    smap_results[[paste0("Tp", tp, "_stock")]] <- data.frame(
      tp = tp,
      type = "Stock",
      smap_mean = smap_stock$mean_coefficient,
      smap_sd = smap_stock$sd_coefficient
    )
    
    cat(sprintf("    Tp=%d: 平均系数=%.4f ± %.4f\n", 
                tp, smap_stock$mean_coefficient, smap_stock$sd_coefficient))
  }, error = function(e) {
    cat(sprintf("    警告: Tp=%d Stock 的S-map计算失败: %s\n", tp, e$message))
    smap_results[[paste0("Tp", tp, "_stock")]] <<- data.frame(
      tp = tp,
      type = "Stock",
      smap_mean = NA,
      smap_sd = NA
    )
  })
}

# 汇总S-map结果
bind_rows(smap_results)

