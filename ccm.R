# ============================================================================
# 安踏股价与谷歌趋势的完整CCM因果分析
# 包含因果方向、性质（正负相关）、强度分析
# ============================================================================

library(dplyr)
library(ggplot2)
library(rEDM)
library(tidyquant)
library(tidyr)
library(gridExtra)
library(zoo)  # 用于na.locf()函数

cat("\n", rep("=", 80), "\n", sep = "")
cat("   安踏调整股价 vs 谷歌趋势 - 完整CCM因果分析\n")
cat("   包含：因果方向 + 因果性质 + 因果强度\n")
cat(rep("=", 80), "\n\n", sep = "")

# ============================================================================
# 1. 数据准备
# ============================================================================

cat("=== 步骤1: 数据读取 ===\n")

# 1.1 读取谷歌趋势数据
google_trends <- read.csv("data_raw/anta_cn_long.csv", skip = 2) %>%
  rename(
    date = Day,
    google_trend = 安踏...Worldwide.
  ) %>%
  mutate(
    date = as.Date(date),
    google_trend = as.numeric(google_trend)
  ) %>%
  filter(!is.na(google_trend))

cat("✓ 谷歌趋势数据：", nrow(google_trends), "个数据点\n")
cat("  日期范围：", as.character(min(google_trends$date)), "至", 
    as.character(max(google_trends$date)), "\n")

# 1.2 读取真实股价数据
cat("\n正在获取安踏真实股价数据（2020.HK）...\n")

stock_data_raw <- tidyquant::tq_get(
  "2020.HK", 
  from = min(google_trends$date),
  to = max(google_trends$date)
)

stock_data <- stock_data_raw %>%
  select(date, adj_close = adjusted) %>%
  arrange(date)

cat("✓ 股价数据：", nrow(stock_data), "个数据点\n")
cat("  日期范围：", as.character(min(stock_data$date)), "至", 
    as.character(max(stock_data$date)), "\n")
cat("  价格范围：", round(min(stock_data$adj_close), 2), "至", 
    round(max(stock_data$adj_close), 2), "HKD\n")

# 1.3 合并数据并补全周末/节假日股价
cat("\n正在处理周末和节假日数据...\n")

# 先用left_join保留所有谷歌趋势的日期
anta_data_raw <- google_trends %>%
  left_join(stock_data, by = "date") %>%
  arrange(date)

# 向前填充股价（使用前一个交易日的价格）
anta_data <- anta_data_raw %>%
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

cat("✓ 数据补全完成\n")
cat("  总数据点数:", nrow(anta_data), "\n")
cat("  其中交易日:", sum(anta_data$is_trading_day), "\n")
cat("  非交易日:", sum(!anta_data$is_trading_day), "\n")
cat("  非交易日占比:", round(100 * sum(!anta_data$is_trading_day) / nrow(anta_data), 1), "%\n")

cat("\n✓ 合并后数据：", nrow(anta_data), "个数据点\n")
cat("  最终日期范围：", as.character(min(anta_data$date)), "至", 
    as.character(max(anta_data$date)), "\n")
cat("  股价变化量范围：", round(min(anta_data$stock_change), 2), "至", 
    round(max(anta_data$stock_change), 2), "\n")
cat("  股价变化量NA数量：", sum(is.na(anta_data$stock_change)), "\n\n")

# ============================================================================
# 2. 数据去趋势和标准化
# ============================================================================

cat("=== 步骤2: 数据预处理 ===\n")

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
    adj_close_detrended = detrend_linear(adj_close),
    google_trend_detrended = detrend_linear(google_trend),
    stock_change_detrended = detrend_linear(stock_change),
    
    # 标准化（只对非NA值）
    stock_norm = scale(adj_close_detrended)[,1],
    trend_norm = scale(google_trend_detrended)[,1],
    change_norm = scale(stock_change_detrended)[,1]
  ) %>%
  # 移除任何包含NA的行
  filter(!is.na(stock_norm), !is.na(trend_norm), !is.na(change_norm))

cat("✓ 去趋势和标准化完成\n")
cat("  处理后数据点数:", nrow(anta_data_processed), "\n")
cat("\n统计摘要：\n")
cat("  股价水平   - 均值:", round(mean(anta_data_processed$stock_norm), 3), 
    "标准差:", round(sd(anta_data_processed$stock_norm), 3), "\n")
cat("  谷歌趋势   - 均值:", round(mean(anta_data_processed$trend_norm), 3), 
    "标准差:", round(sd(anta_data_processed$trend_norm), 3), "\n")
cat("  股价变化量 - 均值:", round(mean(anta_data_processed$change_norm), 3), 
    "标准差:", round(sd(anta_data_processed$change_norm), 3), "\n\n")

# ============================================================================
# 3. 确定最优嵌入维度E
# ============================================================================

cat("=== 步骤3: 确定最优嵌入维度E ===\n")

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
  showPlot = FALSE
)

E_trend <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "trend",
  target = "trend",
  showPlot = FALSE
)

E_change <- EmbedDimension(
  dataFrame = embed_data,
  lib = paste("1", lib_end),
  pred = paste(lib_end + 1, n_data),
  maxE = 4,
  columns = "change",
  target = "change",
  showPlot = T
)

best_E_stock <- E_stock$E[which.max(E_stock$rho)]
best_E_trend <- E_trend$E[which.max(E_trend$rho)]
best_E_change <- E_change$E[which.max(E_change$rho)]
best_E <- round(mean(c(best_E_stock, best_E_trend, best_E_change)))

cat("  股价水平最优E:", best_E_stock, "\n")
cat("  谷歌趋势最优E:", best_E_trend, "\n")
cat("  股价变化最优E:", best_E_change, "\n")
cat("  使用平均E:", best_E, "\n\n")

# ============================================================================
# 4. 多Tp CCM分析
# ============================================================================

cat("=== 步骤4: 多滞后期CCM分析 ===\n")

tp_values <- c(0, 1, 2, 3)

cat("测试的滞后期Tp:", paste(tp_values, collapse = ", "), "\n\n")

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
    seed = 123 + tp
  )
  
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
  )
  
  ccm_results_all[[paste0("Tp", tp, "_level")]] <- ccm_level %>%
    mutate(Tp = tp, Type = "Stock Level", Target = "stock")
  
  ccm_results_all[[paste0("Tp", tp, "_change")]] <- ccm_change %>%
    mutate(Tp = tp, Type = "Stock Change", Target = "change")
  
  cat("    ✓ 完成\n")
}

cat("\n✓ 所有Tp分析完成\n\n")

# ============================================================================
# 5. 计算相关性（确定因果性质）
# ============================================================================

cat("=== 步骤5: 计算相关性（因果性质分析）===\n")

correlation_results <- list()

for (tp in tp_values) {
  # 股价水平：计算t时刻趋势与t+Tp时刻股价的相关性
  if (tp == 0) {
    cor_level <- cor.test(
      anta_data_processed$trend_norm,
      anta_data_processed$stock_norm
    )
    trend_vals <- anta_data_processed$trend_norm
    stock_vals <- anta_data_processed$stock_norm
  } else {
    valid_idx <- 1:(nrow(anta_data_processed) - tp)
    cor_level <- cor.test(
      anta_data_processed$trend_norm[valid_idx],
      anta_data_processed$stock_norm[valid_idx + tp]
    )
    trend_vals <- anta_data_processed$trend_norm[valid_idx]
    stock_vals <- anta_data_processed$stock_norm[valid_idx + tp]
  }
  
  # 股价变化量
  if (tp == 0) {
    cor_change <- cor.test(
      anta_data_processed$trend_norm,
      anta_data_processed$change_norm
    )
    trend_vals_ch <- anta_data_processed$trend_norm
    change_vals <- anta_data_processed$change_norm
  } else {
    valid_idx <- 1:(nrow(anta_data_processed) - tp)
    cor_change <- cor.test(
      anta_data_processed$trend_norm[valid_idx],
      anta_data_processed$change_norm[valid_idx + tp]
    )
    trend_vals_ch <- anta_data_processed$trend_norm[valid_idx]
    change_vals <- anta_data_processed$change_norm[valid_idx + tp]
  }
  
  correlation_results[[paste0("Tp", tp, "_level")]] <- list(
    Tp = tp,
    Type = "Stock Level",
    r = cor_level$estimate,
    p = cor_level$p.value,
    trend = trend_vals,
    target = stock_vals
  )
  
  correlation_results[[paste0("Tp", tp, "_change")]] <- list(
    Tp = tp,
    Type = "Stock Change",
    r = cor_change$estimate,
    p = cor_change$p.value,
    trend = trend_vals_ch,
    target = change_vals
  )
}

cat("✓ 相关性分析完成\n\n")

# ============================================================================
# 6. 整理结果
# ============================================================================

cat("=== 步骤6: 整理结果 ===\n")

# 合并CCM结果
ccm_all_data <- bind_rows(ccm_results_all)

# 提取最终CCM结果
ccm_summary <- ccm_all_data %>%
  group_by(Tp, Type, Target) %>%
  slice_max(LibSize, n = 1) %>%
  ungroup() %>% 
  pivot_longer(cols = contains("trend"),
               names_to = "direction",
               values_to = "rho") %>% 
  filter(!is.na(rho)) %>%
  separate_wider_delim(cols = direction, names = c("To", "From"), delim = ":", cols_remove = F) %>% 
  select(Tp, Type, From, To, rho)

# 为每种类型提取正确的rho列
ccm_summary <- ccm_summary %>%
  rowwise() %>%
  mutate(
    ccm_rho = {
      # 找到包含冒号的列（trend:stock 或 trend:change）
      rho_cols <- names(.) %>% .[grepl(":", .) & grepl("trend", ., ignore.case = TRUE)]
      if (length(rho_cols) > 0) {
        as.numeric(.[[rho_cols[1]]])
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup()

# 合并相关性结果
correlation_summary <- bind_rows(lapply(correlation_results, function(x) {
  data.frame(
    Tp = x$Tp,
    Type = x$Type,
    correlation_r = as.numeric(x$r),
    correlation_p = x$p
  )
}))

# 合并CCM和相关性
final_summary <- ccm_summary %>%
  left_join(correlation_summary, by = c("Tp", "Type")) %>%
  mutate(
    # 因果方向
    causality_direction = "Trend → Target",
    
    # 因果性质
    causality_nature = case_when(
      correlation_r > 0 ~ "正向 (Trend↑ → Target↑)",
      correlation_r < 0 ~ "负向 (Trend↑ → Target↓)",
      TRUE ~ "无相关"
    ),
    
    # 因果强度
    causality_strength = case_when(
      rho >= 0.5 ~ "强",
      rho >= 0.3 ~ "中等",
      rho >= 0.2 ~ "弱",
      TRUE ~ "极弱"
    ),
    
    # 是否显著
    ccm_significant = rho >= 0.3,
    cor_significant = correlation_p < 0.05,
    overall_significant = ccm_significant & cor_significant
  )

cat("✓ 结果整理完成\n\n")

ggplot(ccm_all_data %>% filter(!is.na(`stock:trend`))) + 
  geom_line(aes(LibSize, `stock:trend`), col = "red") + 
  geom_point(aes(LibSize, `stock:trend`)) +
  geom_line(aes(LibSize, `trend:stock`), col = "blue") + 
  geom_point(aes(LibSize, `trend:stock`)) + 
  facet_wrap(.~ Tp)

ggplot(ccm_all_data %>% filter(!is.na(`change:trend`))) + 
  geom_line(aes(LibSize, `change:trend`), col = "red") + 
  geom_point(aes(LibSize, `change:trend`)) +
  geom_line(aes(LibSize, `trend:change`), col = "blue") + 
  geom_point(aes(LibSize, `trend:change`)) + 
  facet_wrap(.~ Tp)

(
  ggplot(final_summary) + 
    geom_tile(aes(x = factor(Tp), y = Type, fill = causality_nature), color = "white") + 
    facet_grid(. ~ causality_direction) + 
    theme(legend.position = "bottom")
) | (
  ggplot(final_summary) + 
    geom_tile(aes(x = factor(Tp), y = Type, fill = ccm_significant), color = "white") + 
    facet_grid(. ~ causality_direction) + 
    theme(legend.position = "bottom")
) | (
  ggplot(final_summary) + 
    geom_tile(aes(x = factor(Tp), y = Type, fill = cor_significant), color = "white") + 
    facet_grid(. ~ causality_direction) + 
    theme(legend.position = "bottom")
)

