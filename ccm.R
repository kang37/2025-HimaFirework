# 安踏股价与"安踏"百度指数的CCM因果分析：因果方向和促进/抑制。
library(dplyr)
library(ggplot2)
library(rEDM)
library(tidyquant)
library(tidyr)
library(gridExtra)
library(zoo)
library(openxlsx)
library(lubridate)

# Data Initialization ----
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

baidu_trend <- baiguoqiang <- baidu_caiguoqiang %>%
  full_join(baidu_shizuniao, by = "date") %>%
  full_join(baidu_anta, by = "date") %>%
  full_join(baidu_ximalaya, by = "date") %>%
  arrange(date)

# 1.2 读取股价数据
stock_data <- tidyquant::tq_get(
  "2020.HK",
  from = min(baidu_trend$date),
  to = max(baidu_trend$date)
) %>%
  select(date, adj_close = adjusted) %>%
  arrange(date)

# 1.3 合并数据并补全周末/节假日股价
anta_data_all <- baidu_trend %>%
  left_join(stock_data, by = "date") %>%
  arrange(date) %>%
  mutate(
    adj_close = zoo::na.locf(adj_close, na.rm = FALSE)
  ) %>%
  filter(!is.na(adj_close)) %>%
  mutate(
    stock_change = adj_close - lag(adj_close),
    is_trading_day = date %in% stock_data$date
  ) %>%
  filter(!is.na(stock_change))

# 线性去趋势函数
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

# S-map计算交互强度系数函数 (更新为即时导数法)
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
  coef_matrix <- if("coefficients" %in% names(smap_result)) smap_result$coefficients else smap_result$smap_coefficients
  lib_cols <- grep("library", colnames(coef_matrix), value = TRUE)
  
  if(length(lib_cols) > 0) {
    # 只提取第一列（当前时刻 t 的导数）
    lib_coefficients <- coef_matrix[, lib_cols[1]]
    mean_coef <- mean(lib_coefficients, na.rm = TRUE)
    sd_coef <- sd(lib_coefficients, na.rm = TRUE)
  } else {
    mean_coef <- NA
    sd_coef <- NA
  }
  return(list(mean_coefficient = mean_coef, sd_coefficient = sd_coef))
}

# 定义时间窗口
time_windows <- list(
  short_term = list(end_date = "2025-10-18", label = "Short-term (Sept-Oct)"),
  long_term = list(end_date = max(anta_data_all$date), label = "Long-term (Sept-Dec)")
)

# 循环开始 ----
for(win_name in names(time_windows)) {
  cat(sprintf("\n\n>>> 正在处理 %s 分析 <<<\n", time_windows[[win_name]]$label))
  
  # 筛选当前窗口数据
  current_end <- as.Date(time_windows[[win_name]]$end_date)
  anta_data_processed <- anta_data_all %>%
    filter(date <= current_end) %>%
    mutate(
      stock_norm = detrend_linear(adj_close),
      baidu_norm = detrend_linear(anta_cn), 
      change_norm = detrend_linear(stock_change)
    ) %>%
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
  
  E_stock <- EmbedDimension(dataFrame = embed_data, lib = paste("1", lib_end), pred = paste(lib_end + 1, n_data), maxE = 4, columns = "stock", target = "stock", showPlot = FALSE)
  E_baidu <- EmbedDimension(dataFrame = embed_data, lib = paste("1", lib_end), pred = paste(lib_end + 1, n_data), maxE = 4, columns = "baidu", target = "baidu", showPlot = FALSE)
  E_change <- EmbedDimension(dataFrame = embed_data, lib = paste("1", lib_end), pred = paste(lib_end + 1, n_data), maxE = 4, columns = "change", target = "change", showPlot = FALSE)
  
  best_E_stock <- E_stock$E[which.max(E_stock$rho)]
  best_E_baidu <- E_baidu$E[which.max(E_baidu$rho)]
  best_E <- round(max(c(best_E_stock, best_E_baidu)))

  # 4. 多Tp CCM分析
  tp_values <- c(0, 1, 2, 3)
  ccm_data_level <- data.frame(time = 1:nrow(anta_data_processed), stock = anta_data_processed$stock_norm, baidu = anta_data_processed$baidu_norm)
  ccm_data_change <- data.frame(time = 1:nrow(anta_data_processed), change = anta_data_processed$change_norm, baidu = anta_data_processed$baidu_norm)
  max_lib <- nrow(ccm_data_level) - best_E - max(tp_values)
  lib_sizes_str <- sprintf("10 %d 3", max_lib)

  ccm_results_all <- list()
  for (tp in tp_values) {
    cat("  CCM [Tp =", tp, "]\n")
    ccm_level <- CCM(dataFrame = ccm_data_level, E = best_E, Tp = tp, columns = "baidu", target = "stock", libSizes = lib_sizes_str, sample = 100, random = TRUE, seed = 123 + tp, showPlot = FALSE) %>% 
      pivot_longer(cols = c("baidu:stock", "stock:baidu")) %>% separate_wider_delim(cols = name, delim = ":", names = c("to", "from")) %>% rename(lib_size = LibSize, rho = value) %>% mutate(tp = tp)
    ccm_change <- CCM(dataFrame = ccm_data_change, E = best_E, Tp = tp, columns = "baidu", target = "change", libSizes = lib_sizes_str, sample = 100, random = TRUE, seed = 456 + tp) %>% 
      pivot_longer(cols = c("baidu:change", "change:baidu")) %>% separate_wider_delim(cols = name, delim = ":", names = c("to", "from")) %>% rename(lib_size = LibSize, rho = value) %>% mutate(tp = tp)
    ccm_results_all[[paste0("Tp", tp, "_level")]] <- ccm_level
    ccm_results_all[[paste0("Tp", tp, "_change")]] <- ccm_change
  }

  ccm_all_data <- bind_rows(ccm_results_all) %>% 
    mutate(
      dir = case_when(
        from == "baidu" & to == "stock" ~ "Stock xmap Baidu", 
        from == "baidu" & to == "change" ~ "Stock change xmap Baidu", 
        from == "stock" & to == "baidu" ~ "Baidu xmap Stock", 
        from == "change" & to == "baidu" ~ "Baidu xmap Stock change"
      ), 
      type = case_when(from == "stock" | to == "stock" ~ "Stock", from == "change" | to == "change" ~ "Stock change")
    )

  png(paste0("data_proc/ccm_convergence_", win_name, "_", Sys.Date(), ".png"), width = 2000, height = 1000, res = 300)
  p_ccm <- ccm_all_data %>% filter(type == "Stock") %>% 
    ggplot() + geom_line(aes(lib_size, rho, col = dir), linewidth = 1.2) + 
    facet_wrap(.~ tp, labeller = labeller(tp = function(x) paste("Tp =", x)), nrow = 2) + 
    scale_color_manual(breaks = c("Baidu xmap Stock", "Stock xmap Baidu", "Baidu xmap Stock change", "Stock change xmap Baidu"), values = c("lightblue3", "darkred", "darkgreen", "orange")) +
    theme_bw(base_size = 14) + labs(x = "Library size", y = "Cross-mapping skill (ρ)", col = "Direction") + theme(legend.position = "right")
  print(p_ccm)
  dev.off()

  # 5. S-map确定因果类型
  smap_results <- list()
  smap_daily_list <- list()
  for(tp in tp_values) {
    cat("  S-map [Tp =", tp, "]\n")
    tryCatch({
      smap_stock <- calculate_smap_coefficient(data = anta_data_processed, E = best_E, target_col = "stock_norm", lib_col = "baidu_norm", tp = tp)
      smap_results[[paste0("Tp", tp, "_stock")]] <- data.frame(tp = tp, type = "Stock", smap_mean = smap_stock$mean_coefficient, smap_sd = smap_stock$sd_coefficient)
      
      # 重新运行S-map获取每日导数
      smap_data_daily <- data.frame(time = 1:nrow(anta_data_processed), target = anta_data_processed$stock_norm, library = anta_data_processed$baidu_norm)
      smap_res <- SMap(dataFrame = smap_data_daily, lib = paste("1", floor(nrow(smap_data_daily) * 0.8)), pred = paste(1, nrow(smap_data_daily)), E = best_E, Tp = tp, columns = "library", target = "target", theta = 2, embedded = FALSE)
      coef_mtx <- if("coefficients" %in% names(smap_res)) smap_res$coefficients else smap_res$smap_coefficients
      lib_col_names <- grep("library", colnames(coef_mtx), value = TRUE)
      if(length(lib_col_names) > 0) {
        daily_deriv <- coef_mtx[, lib_col_names[1]]
        n_rows <- length(daily_deriv)
        smap_daily_list[[paste0("Tp", tp)]] <- data.frame(date = anta_data_processed$date[1:n_rows], derivative = daily_deriv, tp = tp)
      }
    }, error = function(e) cat(sprintf("    警告: Tp=%d S-map失败: %s\n", tp, e$message)))
  }

  smap_summary_final <- bind_rows(smap_results)
  write.xlsx(smap_summary_final, paste0("data_proc/smap_summary_", win_name, "_", Sys.Date(), ".xlsx"))
  
  smap_daily_df <- bind_rows(smap_daily_list)
  time_fig_x_date <- c("2025-9-19", "2025-9-21", "2025-9-22", "2025-9-24", "2025-9-26", "2025-9-28", "2025-9-29", "2025-10-04")
  
  png(paste0("data_proc/smap_daily_derivatives_", win_name, "_", Sys.Date(), ".png"), width = 2400, height = 1600, res = 300)
  p_smap <- ggplot(smap_daily_df, aes(x = date, y = derivative)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.6) +
    geom_line(color = "gray70", alpha = 0.4) +
    geom_point(aes(color = derivative > 0), size = 1.2) +
    geom_smooth(method = "loess", span = 0.3, color = "black", linewidth = 0.6, se = FALSE) +
    facet_wrap(~ tp, scales = "free_y", ncol = 2, labeller = labeller(tp = function(x) paste("Tp =", x))) +
    scale_color_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"), name = "Effect Direction", labels = c("Inhibition (Negative)", "Promotion (Positive)")) +
    scale_x_date(breaks = as.Date(time_fig_x_date), labels = gsub("2025-", "", time_fig_x_date)) +
    coord_cartesian(ylim = c(NA, 0.005)) +
    labs(title = paste("Daily Interaction Strength:", time_windows[[win_name]]$label), subtitle = "Dynamic effect of Baidu Index on Anta Stock Price Level", x = "Date", y = "Local Derivative (∂Stock / ∂Baidu)") +
    theme_bw() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), strip.background = element_rect(fill = "gray95"), strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
  print(p_smap)
  dev.off()
  
  write.xlsx(smap_daily_df, paste0("data_proc/smap_daily_details_", win_name, "_", Sys.Date(), ".xlsx"))
}
cat("\n分析完成。结果已保存至 data_proc 文件夹。\n")
