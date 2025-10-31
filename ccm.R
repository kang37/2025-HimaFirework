# 安踏股价与“始祖鸟”谷歌趋势的CCM因果分析。
# 包含因果方向、性质（正负相关）、强度分析。

library(dplyr)
library(ggplot2)
library(rEDM)
library(tidyquant)
library(tidyr)
library(gridExtra)
library(zoo)

# Data ----
# 1.1 读取谷歌趋势数据：2025-09-05至2025-10-18。
google_trends <- read.csv("data_raw/arctery_cn_long.csv", skip = 2) %>%
  rename_with(~ c("date", "google_trend")) %>% 
  mutate(
    date = as.Date(date),
    google_trend = as.numeric(google_trend)
  ) %>%
  filter(!is.na(google_trend))

# 1.2 读取股价数据
stock_data <- tidyquant::tq_get(
  "2020.HK", 
  from = min(google_trends$date),
  to = max(google_trends$date)
) %>% 
  select(date, adj_close = adjusted) %>%
  arrange(date)

# 1.3 合并数据并补全周末/节假日股价

# 先用left_join保留所有谷歌趋势的日期
anta_data <- google_trends %>%
  left_join(stock_data, by = "date") %>%
  arrange(date) %>% 
  # 向前填充股价（使用前一个交易日的价格）
  mutate(
    # 使用tidyr::fill向前填充NA值
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

# ============================================================================
# 2. 数据去趋势和标准化
# ============================================================================
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
    # 去趋势
    stock_norm = detrend_linear(adj_close),
    trend_norm = detrend_linear(google_trend),
    change_norm = detrend_linear(stock_change)
  ) %>%
  # 移除任何包含NA的行
  filter(!is.na(stock_norm), !is.na(trend_norm), !is.na(change_norm))

# ============================================================================
# 3. 确定最优嵌入维度E
# ============================================================================
embed_data <- data.frame(
  time = 1:nrow(anta_data_processed),
  stock = anta_data_processed$stock_norm,
  trend = anta_data_processed$trend_norm,
  change = anta_data_processed$change_norm
)

n_data <- nrow(embed_data)
lib_end <- floor(n_data * 0.7)

E_stock <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "stock",
  target = "stock",
  showPlot = TRUE
)

E_trend <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "trend",
  target = "trend",
  showPlot = TRUE
)

E_change <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "change",
  target = "change",
  showPlot = TRUE
)

best_E_stock <- E_stock$E[which.max(E_stock$rho)]
best_E_trend <- E_trend$E[which.max(E_trend$rho)]
best_E_change <- E_change$E[which.max(E_change$rho)]
best_E <- round(max(c(best_E_stock, best_E_trend, best_E_change)))

# ============================================================================
# 4. 多Tp CCM分析
# ============================================================================
tp_values <- c(0, 1, 2, 3)

# 准备数据
ccm_data_level <- data.frame(
  time = 1:nrow(anta_data_processed),
  stock = anta_data_processed$stock_norm,
  trend = anta_data_processed$trend_norm
)

ccm_data_change <- data.frame(
  time = 1:nrow(anta_data_processed),
  change = anta_data_processed$change_norm,
  trend = anta_data_processed$trend_norm
)

max_lib <- nrow(ccm_data_level) - best_E - max(tp_values)
lib_sizes_str <- sprintf("10 %d 3", max_lib)

# 运行CCM分析
ccm_results_all <- list()
for (tp in tp_values) {
  cat("  [Tp =", tp, "]\n")
  
  # 股价水平
  ccm_level <- CCM(
    dataFrame = ccm_data_level,
    E = best_E,
    Tp = tp,
    columns = "trend",
    target = "stock",
    libSizes = lib_sizes_str,
    sample = 100,
    random = TRUE,
    seed = 123 + tp, 
    showPlot = T
  ) %>% 
    pivot_longer(cols = c("trend:stock", "stock:trend")) %>% 
    separate_wider_delim(cols = name, delim = ":", names = c("to", "from")) %>% 
    rename(lib_size = LibSize, rho = value) %>% 
    mutate(tp = tp)
  
  # 股价变化量
  ccm_change <- CCM(
    dataFrame = ccm_data_change,
    E = best_E,
    Tp = tp,
    columns = "trend",
    target = "change",
    libSizes = lib_sizes_str,
    sample = 100,
    random = TRUE,
    seed = 456 + tp
  ) %>% 
    pivot_longer(cols = c("trend:change", "change:trend")) %>% 
    separate_wider_delim(cols = name, delim = ":", names = c("to", "from")) %>% 
    rename(lib_size = LibSize, rho = value) %>% 
    mutate(tp = tp)
  
  ccm_results_all[[paste0("Tp", tp, "_level")]] <- ccm_level
  
  ccm_results_all[[paste0("Tp", tp, "_change")]] <- ccm_change
  
  cat("\t完成\n")
}
ccm_all_data <- bind_rows(ccm_results_all) %>% 
  mutate(
    dir = case_when(
      from == "trend" & to == "stock" ~ "Stock xmap Trend", 
      from == "trend" & to == "change" ~ "Stock change xmap Trend", 
      from == "stock" & to == "trend" ~ "Trend xmap Stock", 
      from == "change" & to == "trend" ~ "Trend xmap Stock change"
    ), 
    type = case_when(
      from == "stock" | to == "stock" ~ "Stock",
      from == "change" | to == "change" ~ "Stock change"
    )
  )

ccm_all_data %>% 
  ggplot() + 
  geom_line(aes(lib_size, rho, col = dir)) + 
  facet_grid(type ~ tp) + 
  scale_color_manual(
    breaks = c(
      "Trend xmap Stock", "Stock xmap Trend", 
      "Trend xmap Stock change", "Stock change xmap Trend"
    ),
    values = c("lightblue3", "darkred", "darkgreen", "orange")
  ) +
  theme_bw() + 
  labs(x = "Library size", col = "Direction")

# ============================================================================
# 5. 确定因果性质
# ============================================================================
# S-mapping函数：计算交互强度系数
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

# 计算趋势对股价的影响性质：A增导致B增加还是减少？
smap_results <- list()
for(tp in tp_values) {
  cat(sprintf("  计算 Tp=%d 的S-map系数...\n", tp))
  
  # 股价水平：trend对stock的影响
  tryCatch({
    smap_stock <- calculate_smap_coefficient(
      data = anta_data_processed,
      E = best_E,
      target_col = "stock_norm",
      lib_col = "trend_norm",
      tp = tp
    )
    
    smap_results[[paste0("Tp", tp, "_stock")]] <- data.frame(
      tp = tp,
      type = "Stock",
      smap_mean = smap_stock$mean_coefficient,
      smap_sd = smap_stock$sd_coefficient
    )
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
bind_rows(smap_results)
