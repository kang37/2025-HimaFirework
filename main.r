# Package ----
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels) 
library(dplyr)
library(tidyr)
# library(jiebaR)
library(stringr)
library(openxlsx)
library(lubridate)
library(readr)
library(purrr)
library(tibble)
library(showtext)
library(plotly)
library(rlang)
library(LSX)
library(patchwork)
library(quantmod)
library(Hmisc)
library(reshape2)
library(TTR)

showtext_auto()

# Basic data ----
# 函数：推算日期。从一个data.frame的一列中提取日期。这列中有两种类型数据，一类类似于“2025/09/28 11:35:15”，另一类类似于“1小时前 转赞人数超过100”。对于前者，直接提取日期并转化成日期类型，对于后者，我的数据下载时间是2025-09-29 10:55:00，根据这个推算。
# 定义推算相对日期的函数
calc_relative_date <- function(time_str, download_time) {
  # 提取下载时间。
  download_time <- ymd_hms(download_time, tz = "Asia/Tokyo")
  
  # --- 新增逻辑 1: 处理 "昨天" ---
  if (str_detect(time_str, "^昨天")) {
    # "昨天" 意味着下载日期的前一天
    result_datetime <- download_time - days(1)
    return(as_date(result_datetime))
  }
  
  # --- 现有逻辑: 处理 "X小时/天/分钟前" ---
  # 提取数字（时间量）
  value <- as.numeric(str_extract(time_str, "\\d+"))
  
  # 判断时间单位
  unit <- case_when(
    str_detect(time_str, "小时前") ~ "hours",
    str_detect(time_str, "天前") ~ "days",
    str_detect(time_str, "分钟前") ~ "minutes",
    TRUE ~ "unknown"
  )
  if (unit == "unknown" | is.na(value)) {
    return(as.Date(NA)) # 无法解析则返回 NA
  }
  
  # 使用 lubridate::duration() 进行推算
  result_datetime <- download_time - duration(value, units = unit)
  result_datetime <- with_tz(result_datetime, tzone = "Asia/Shanghai")
  
  return(as_date(result_datetime))
}


data <- lapply(
  paste0("data_raw/weibo_himalaya_firework_", 1:3, ".xlsx"), read.xlsx
) %>% 
  bind_rows() %>% 
  tibble() %>%
  rename_with(~tolower(.x)) %>%
  mutate(
    post_date = case_when(
      # 条件 A: 标准日期时间格式 (包含4位年份)
      str_detect(post_time, "(\\d{4}[-/]\\d{2}[-/]\\d{2})") ~
        with_tz(ymd_hms(post_time), tzone = "Asia/Shanghai") %>% 
        as_date(),
      # 条件 B: 月-日格式 (如 "9-26")
      # 匹配开头是数字-数字，且不含年份的格式
      str_detect(post_time, "^\\d{1,2}-\\d{1,2}") ~ 
        # 拼接日期。
        paste0("2025-", str_extract(post_time, "^\\d{1,2}-\\d{1,2}")) %>% 
        ymd(),
      
      # 条件 C: 相对时间格式 ("小时前", "昨天", "天前")
      TRUE ~ map2_vec(post_time, current_time, ~calc_relative_date(.x, .y))
    ),
    .after = "post_time"
  ) %>% 
  filter(
    post_date >= as_date("2025-09-19") & post_date <= as_date("2025-10-04")
  ) %>% 
  select(
    username, post_date, content, forward_count, 
    review_count, like_count, review_count, forward_count
  ) %>% 
  distinct()

# 数据量
# Bug: 是否可靠？不清楚Octoparse的抓取方式。

# 安踏股价。
anta_stock <- 
  tidyquant::tq_get("2020.HK", from = "2025-09-15", to = "2025-10-04") %>%
  mutate(ma5 = SMA(close, n = 5)) %>% 
  filter(date >= "2025-09-19")

# 谷歌趋势：“蔡国强”、“始祖鸟”、“安踏”、“喜马拉雅”的谷歌趋势。
goo_trend <- lapply(
  c("caiguoqiang_cn", "arctery_cn", "anta_cn", "himalaya_cn"), 
  function(x) {
    read.csv(paste0("data_raw/", x, ".csv")) %>% 
      slice(-1) %>% 
      rownames_to_column(var = "date") %>% 
      rename_with(~ c("date", x))
  }
) %>% 
  reduce(left_join, by = "date") %>% 
  mutate(date = as_date(date)) %>% 
  mutate(across(-date, ~ as.numeric(gsub("[^0-9.]", "", .x))))

# 舆论变量数量汇总：微博文本数，安踏股价，谷歌趋势。
# Bug: 百度指数暂时无法获得。
all_dt_num_smry <- 
  # 微博文本数。
  data %>% 
  # Bug: 将点赞数据转化成数字。
  mutate(
    forward_count = case_when(
      str_detect(forward_count, "转发") ~ 0,  # 含“赞”时设为0
      TRUE ~ str_replace_all(forward_count, "万", "e4") %>% parse_number()
    ), 
    review_count = case_when(
      str_detect(review_count, "评论") ~ 0,  # 含“赞”时设为0
      TRUE ~ str_replace_all(review_count, "万", "e4") %>% parse_number()
    ), 
    like_count = case_when(
      str_detect(like_count, "赞") ~ 0,  # 含“赞”时设为0
      TRUE ~ str_replace_all(like_count, "万", "e4") %>% parse_number()
    )
  ) %>% 
  group_by(post_date) %>% 
  dplyr::summarise(
    weibo_num = n(), 
    weibo_num_forward = sum(forward_count), 
    weibo_num_review = sum(review_count), 
    weibo_num_like = sum(like_count), 
    .groups = "drop"
  ) %>% 
  # 谷歌指数。
  full_join(goo_trend, by = c("post_date" = "date")) %>% 
  # 安踏股价。
  full_join(
    anta_stock %>% select(date, adj_stock = adjusted, ma5), 
    by = c("post_date" = "date")
  )

# 各数据趋势。
(
  ggplot(all_dt_num_smry, aes(post_date)) +
    # 1. 将颜色映射到 aes() 内部，使用描述性字符串作为图例键
    geom_line(aes(y = weibo_num, color = "Weibo Posts")) +
    geom_line(aes(y = weibo_num_like / 1000, color = "Likes / 1000")) + 
    geom_line(aes(y = weibo_num_review / 100, color = "Reviews / 80")) +
    geom_line(aes(y = weibo_num_forward /50 , color = "Forwards / 50")) + 
    # 使用 scale_color_manual 来控制图例的外观
    scale_color_manual(
      breaks = c("Weibo Posts", "Likes / 1000","Reviews / 80", "Forwards / 50"), 
      values = c("#8B0000", "#FF4500", "#FFD700", "#FF69B4")
    ) + 
    labs(x = "Date", y = "Weibo index", color = NULL, linetype = NULL) +
    scale_x_date(
      limits = c(as.Date("2025-09-19"), NA),
      breaks = as.Date(c("2025-09-19", "2025-09-22", "2025-09-29")),
      labels = c("9-19", "9-22", "9-29")
    ) + 
    theme_bw() +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.05, "lines"),
      legend.spacing.y = unit(0.05, "lines"),
      legend.background = element_rect(fill = "white", color = "grey")
    )
) / (
  all_dt_num_smry %>%
    select(post_date, caiguoqiang_cn, arctery_cn, anta_cn, himalaya_cn) %>%
    pivot_longer(
      -post_date, names_to  = "keyword", values_to = "value"
    ) %>%
    mutate(
      keyword = recode(
        keyword, 
        caiguoqiang_cn = "Cai Guoqiang", arctery_cn = "Arc'teryx", 
        anta_cn = "Anta", himalaya_cn = "Himalaya"
      ),
      keyword = factor(keyword, levels = c("Cai Guoqiang", "Arc'teryx", "Anta", "Himalaya"))
    ) %>% 
    ggplot(aes(post_date, value, color = keyword)) + 
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c(
      "Cai Guoqiang" = "#1f77b4", 
      "Arc'teryx" = "#2ca02c", 
      "Anta" = "#17becf",
      "Himalaya" = "purple"
    )) +
    labs(x = "Date", y = "Google trend\nindex", color = NULL, linetype = NULL) +
    theme_bw() +
    scale_x_date(
      limits = c(as.Date("2025-09-19"), NA),
      breaks = as.Date(c("2025-09-19", "2025-09-22", "2025-09-29")),
      labels = c("9-19", "9-22", "9-29")
    ) + 
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.05, "lines"),
      legend.spacing.y = unit(0.05, "lines"),
      legend.background = element_rect(fill = "white", color = "grey")
    )
) / (
  # 绘制K线图，包含5日均线。
  ggplot(anta_stock, aes(x = date)) +
    geom_segment(
      aes(y = low, yend = high, xend = date, color = close > open)
    ) +
    geom_rect(
      aes(
        xmin = date - 0.3, xmax = date + 0.3,
        ymin = pmin(open, close), ymax = pmax(open, close),
        fill = close > open
      ),
      color = NA, 
      linewidth = 0.1
    ) +
    scale_fill_manual(
      values = c("TRUE" = "#E74C3C", "FALSE" = "green"), guide = "none"
    ) + 
    scale_color_manual(
      values = c("TRUE" = "#E74C3C", "FALSE" = "green"), guide = "none"
    ) + 
    geom_line(aes(y = ma5), color = "orange") + 
    labs(x = "Date", y = "Stock price (HKD)") +
    scale_x_date(
      breaks = as.Date(c("2025-09-19", "2025-09-22", "2025-09-29")),
      labels = c("9-19", "9-22", "9-29")
    ) + 
    theme_bw() 
)
# 多Y轴版本。
library(highcharter)

highchart() %>% 
  hc_add_series(
    data = all_dt_num_smry, type = "line", name = "Weibo posts",
    hcaes(x = post_date, y = weibo_num), yAxis = 0, color = "#FFD700"  # 金黄
  ) %>% 
  hc_add_series(
    data = all_dt_num_smry, type = "line", name = "Weibo forwards",
    hcaes(x = post_date, y = weibo_num_forward), yAxis = 1, color = "orange"
  ) %>% 
  hc_add_series(
    data = all_dt_num_smry, type = "line", name = "Weibo reviews",
    hcaes(x = post_date, y = weibo_num_review), yAxis = 2, color = "pink"
  ) %>%
  hc_add_series(
    data = all_dt_num_smry, type = "line", name = "Weibo likes",
    hcaes(x = post_date, y = weibo_num_like), yAxis = 3, color = "red"
  ) %>% 
  hc_xAxis(
    type = "datetime",
    breaks = as.numeric(as.POSIXct(c("2025-10-02", "2025-10-03")))*1000,  # 指定两天
    labels = list(format = "{value:%m-%d}")  # 显示为 “10-02” “10-03”
  ) %>% 
  hc_yAxis_multiples(
    list(lineWidth = 1, lineColor='yellow', title=list(text="Weibo posts")),
    list(lineWidth = 1, lineColor="orange", title=list(text="Weibo forwards"), opposite=TRUE),
    list(lineWidth = 1, lineColor="pink", title=list(text="Weibo reviews"), opposite=TRUE),
    list(lineWidth = 1, lineColor="red", title=list(text="Weibo likes"), opposite=TRUE)
  ) %>%
  hc_plotOptions(
    series = list(marker = list(enabled = FALSE))
  )

# 相关性。
# 函数：输入数据框，获得每两列之间的相关系数。
get_cor <- function(df_x) {
  # 去掉日期列，只保留数值列
  num_df <- df_x %>%
    select(-post_date)
  
  # 计算相关系数和p值。
  rc <- rcorr(as.matrix(num_df), , type="spearman")
  cor_mat <- rc$r
  p_mat   <- rc$P
  
  # 把矩阵转成长表
  cor_long <- melt(cor_mat, na.rm = TRUE)
  p_long   <- melt(p_mat, na.rm = TRUE)
  merged   <- left_join(cor_long, p_long, by = c("Var1", "Var2"))
  names(merged) <- c("Var1", "Var2", "cor", "p")
  
  # 显著性标记。
  merged <- merged %>%
    mutate(
      sig = case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        p < 0.1 ~ ".", 
        TRUE      ~ ""
      )
    )
  
  # 绘图。
  ggplot(merged, aes(Var1, Var2, fill = cor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(round(cor, 2), sig)), size = 3) +
    scale_fill_gradient2(
      low = "#6baed6", mid = "white", high = "red", midpoint = 0, limits = c(-1,1)
    ) +
    theme_minimal() +
    labs(fill = "r") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
get_cor(all_dt_num_smry)

# 将你的评论文本列 (comment_text) 转换为 quanteda 语料库
weibo_corpus <- corpus(
  data, text_field = "content", docid_field = "doc_id"
) 

# 中文停止词。
ch_stop <- c(
  stopwords("zh", source = "misc"), 
  # 根据分析结果增加停止词。
  "更", "一个"
)

# Tag temporal change ----
# 标签热度随着时间变化。
# 找出总频率最高的前 20 个标签
dfmat_tag <- weibo_corpus %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = ch_stop) %>% 
  # 提取标签。
  dfm(.) %>% 
  dfm_select(., pattern = "#*")
# 各日期各标签的频度。
df_date_freq <- 
  # 筛选 DFM，只保留这 20 个标签
  dfm_select(dfmat_tag, pattern = top_tag) %>% 
  # 2.1 按 'post_date' 变量对 DFM 进行汇总 (即按日期求和)
  dfm_group(., groups = post_date) %>%
  convert(to = "data.frame") %>%
  # 重命名日期列 (dfm_group 会将日期移到 docname 列)
  rename(date = doc_id) %>%
  # 转换为长格式：将所有特征列合并为两列 (Tag 和 Frequency)
  pivot_longer(
    cols = !date, 
    names_to = "Tag",
    values_to = "Frequency"
  ) %>%
  # 确保 Date 是日期类型
  mutate(date = as_date(date)) %>% 
  arrange(-Frequency)

# 绘制高频标签随时间变化的图表
# ggplotly(
#   df_date_freq %>%
#     # 仅保留出现次数大于 0 的数据点
#     filter(Frequency > 0) %>%
#     ggplot(aes(x = date, y = Frequency, fill = Tag)) +
#     geom_col(position = "fill") +
#     labs(
#       title = "高频标签在各个日期的出现频率",
#       x = "日期",
#       y = "出现频率 (计数)",
#       color = "高频标签"
#     ) +
#     theme_minimal() +
#     scale_x_date(date_breaks = "1 day", date_labels = "%m-%d") + # 优化日期轴显示
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋转 x 轴标签
# )

# 组合词。
multi_word_phrases <- list(
  c("始祖", "鸟"), c("蔡", "国", "强"), c("硬", "气"), c("升", "龙" ), 
  c("炸", "山"), c("锐", "评"), c("杂", "物"), c("圣", "山"), 
  c("微", "博"), c("中", "日")
)

toks_ch_split_tag <- weibo_corpus %>% 
  tokens(remove_punct = TRUE, split_tags = TRUE) %>% 
  tokens_remove(pattern = ch_stop) %>% 
  tokens_compound(pattern = multi_word_phrases, concatenator = "")
# Construct a dfm
dfmat_ch <- dfm(toks_ch_split_tag)

# Freq word ----
# Get most frequent features
topfeatures(dfmat_ch, 30) %>% 
  as.data.frame() %>% 
  rename_with(~ "freq") %>% 
  rownames_to_column(var = "term") %>% 
  mutate(term = factor(term, levels = rev(term))) %>% 
  ggplot() + 
  geom_col(aes(term, freq)) + 
  coord_flip() + 
  theme_bw()

# 词云
# Plot a word cloud
set.seed(100)
textplot_wordcloud(
  dfmat_ch, min_count = 50, random_order = FALSE,
  rotation = .25, max_words = 100,
  min_size = 1, max_size = 5,
  font = if (Sys.info()['sysname'] == "Darwin") "SimHei" else NULL,
  color = RColorBrewer::brewer.pal(8, "Dark2")
)
# 英文词云：需要手动翻译。
# 输出高频词并手动翻译：
write.xlsx(
  data.frame(topfeatures(dfmat_ch, 200)) %>% rownames_to_column(var = "term"), 
  "data_proc/high_freq_term.xlsx"
)
# 输出英文词云。
library(wordcloud)
df_freq <- read.xlsx("data_proc/high_freq_term.xlsx")
png("test.png", width = 2000, height = 2000, res = 300)
set.seed(123)
wordcloud(
  words = df_freq$term_en,
  freq = df_freq$freq,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.3,
  scale = c(3, 0.5),              # 放大整体文字比例
  colors = brewer.pal(8, "Dark2") # 更清新的配色
)
dev.off()

# Collocations ----
# bigrams cross the whole dataset
tstat_col_ch <- textstat_collocations(toks_ch_split_tag, size = 2, min_count = 20)
knitr::kable(head(tstat_col_ch, 10))
tstat_col_ch <- textstat_collocations(toks_ch_split_tag, size = 3, min_count = 20)
knitr::kable(head(tstat_col_ch, 10))

# Topic ----
# 主题模型
# 大约花1-2分钟
# library(stm)
# my_lda_fit20 <- stm(dfmat_ch, K = 10, verbose = FALSE)
# plot(my_lda_fit20)  

# Coding ----
# 1. 定义中文关键词列表
coding_keywords <- list(
  # --- 1. 生态危害维度 (原有) ---
  "eco_animal" = c("动物", "雪豹", "鼠兔", "牲畜", "栖息地", "物种", "生灵", "惊扰", "迁徙", "受惊"),
  "eco_plant" = c("植被", "植物", "草", "地衣", "苔藓", "真菌", "草毡层", "修复", "绿化"),
  "eco_ecosystem" = c("生态", "环境", "脆弱", "高原", "冰川", "山脉", "自然", "破坏", "不可逆", "创伤"), 
  
  # --- 2. 污染信息维度 (新增) ---
  "pollution_light" = c("光污染", "强光", "光害"),
  "pollution_noise" = c("噪声", "噪音", "声响", "爆破声", "惊扰", "安静"),
  "pollution_waste" = c("垃圾", "残渣", "遗留物", "清理", "杂物", "废物"),
  "pollution_air" = c("空气污染", "烟雾", "大气", "粉尘", "PM2.5", "颗粒物"),
  "pollution_water" = c("水污染", "水体", "融水", "雪水", "河流"),
  
  # --- 3. 品牌/商业维度 ---
  "brand_company" = c("始祖鸟", "安踏", "品牌", "广告"),
  "brand_cai" = c("人设", "虚伪", "傲慢"),
  
  # --- 4. 诉求/行动维度 (新增) ---
  "act_accountability" = c("调查", "处罚", "追责", "立法", "合规", "审查", "立案", "判刑"),
  "act_remedy" = c("道歉", "赔偿", "修复", "补救", "清理", "评估"),
  "act_boycott" = c("抵制", "不买", "下架", "退货", "转黑", "卸载")
)

# 定义一个辅助函数，用于检查文本是否包含任何关键词
# 辅助函数（与你之前定义的函数相同）
detect_keywords <- function(text, keywords) {
  # 确保文本转换为小写以匹配，提高健壮性（如果数据未预先处理）
  text <- tolower(text) 
  pattern <- paste(keywords, collapse = "|")
  # str_detect 返回 TRUE/FALSE，转换为整数 1/0
  as.integer(str_detect(text, pattern))
}


# 批量计算所有新列的值
# map_dfc (.dfc 返回一个数据框，列名沿用 list 的 names)
coding_results_df <- map_dfc(coding_keywords, function(keywords) {
  # 对数据框的 content 列应用 detect_keywords 函数
  # 这里的 data 替换为你的数据框变量名
  detect_keywords(data$content, keywords) 
})

# 将结果列绑定到原始数据
data_coding <- data %>%
  bind_cols(coding_results_df)

# 检查结果
# 你现在应该能看到 animal, plant, ecosystem, pollution_light, brand_image 等所有新增列
data_coding %>% 
  select(matches("^(eco|call|pollution)_|brand")) %>% 
  colSums()

# Word cloud by topic ----
# 3. 核心函数：词云生成器
#' 生成中文评论词云的函数
#'
#' @param subset_data 经过筛选后的数据框子集，必须包含 'content' 列。
#' @param plot_title 词云图的标题。
#' @param min_freq 词语在 DFM 中的最低出现频率 (用于过滤噪声)。
#' @param max_words 词云中显示的最大词语数量。
#'
generate_wordcloud_cn <- function(coding_col, min_freq = 5, max_words = 100) {
  subset_data <- data_coding %>% filter(get(coding_col) == 1)
  # 1. 创建 quanteda 语料库
  weibo_corpus <- corpus(subset_data, text_field = "content")
  
  # 2. 分词、清洗和复合
  toks_cleaned <- weibo_corpus %>%
    # 分词时移除标点符号、数字和符号，并拆分标签（split_tags = TRUE）
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, split_tags = TRUE) %>%
    # 移除官方和自定义的停用词
    tokens_remove(pattern = c(ch_stop, c(
      "始祖鸟", "蔡国强", "喜马拉雅", "烟花秀", "升龙", "活动", 
      "这个", "真是", "就是", "不要", "大师", "没有", "什么", "知道", 
      "这种", "可以", "怎么", "已经", "感觉", "评论", "转发", "点赞",
      "微博", "一个", "我们", "大家", "这种", "行为", "烟花"
    ))) %>%
    # 移除长度小于 2 的词
    tokens_select(min_nchar = 2) %>%
    # 组合多词短语
    tokens_compound(pattern = multi_word_phrases, concatenator = "")
  
  # 3. 构建 DFM
  dfmat_ch <- dfm(toks_cleaned)
  
  # 调整字体以适应 macOS 环境（您的代码中已有此判断）
  cloud_font <- if (Sys.info()['sysname'] == "Darwin") "SimHei" else NULL
  
  textplot_wordcloud(
    dfmat_ch, 
    min_count = min_freq, 
    max_words = max_words,
    random_order = FALSE,
    rotation = .25, 
    min_size = 1, 
    max_size = 5,
    font = cloud_font,
    color = RColorBrewer::brewer.pal(8, "Dark2")
  )
  title(main = coding_col, cex.main = 1.2)
  
  return(dfmat_ch)
}

lapply(
  names(coding_keywords), generate_wordcloud_cn
)

# 提及各项比例的时间变化。
# Bug: 只选取大于50条文本的日期分析。
get_coding_day_prop <- function(coding_col_name) {
  data_coding %>% 
    filter(
      post_date >= as_date("2025-09-24") & post_date <= as_date("2025-09-30")
    ) %>% 
    group_by(post_date) %>% 
    summarise(
      coding_n = sum(get(coding_col_name) == 1), 
      day_n = n(), 
      prop = coding_n / day_n, 
      .groups = "drop"
    ) %>% 
    mutate(coding_col = coding_col_name, .before = 1)
}
coding_smry <- lapply(
  names(coding_keywords), 
  get_coding_day_prop
) %>% 
  bind_rows() %>% 
  mutate(cat = str_extract(coding_col, "^[^_]+"))
ggplotly(
  ggplot(coding_smry) + 
    geom_area(
      aes(post_date, prop, fill = coding_col), position = "dodge", 
      alpha = 0.5
    ) + 
    facet_wrap(.~ cat)
)
# Bug: 27日之后有什么新的主题出现吗？

# Textplot ----
#' 函数：生成 LSS 词语极性图表
#'
#' @param data 包含文本内容的数据框 (文本必须在 'content' 列)。
#' @param pos_seeds 向量，代表正极的种子词 (e.g., 动物相关词)。
#' @param neg_seeds 向量，代表负极的种子词 (e.g., 植物相关词)。
#' @param pos_label 字符串，正极轴线标签 (e.g., "动物")。
#' @param neg_label 字符串，负极轴线标签 (e.g., "植物")。
#' @param n_top_features 整数，在图表中高亮显示的词语数量 (默认 50)。
#' @return 返回 LSS 模型对象 (lss_model) 并绘制词语极性图。
#'
generate_lss_polarity_plot <- function(data, pos_seeds, neg_seeds, 
                                       pos_label, neg_label, 
                                       n_top_features = 50) {
  
  # ----------------------------------------------------------------------------
  # 2. 定义分词和预处理参数 (硬编码特定于此事件的参数)
  # ----------------------------------------------------------------------------
  
  # 2.1. 停用词和通用过滤词
  ch_stop <- stopwords("zh", source = "misc")
  custom_event_stopwords <- c(
    "始祖鸟", "蔡国强", "喜马拉雅", "烟花秀", "升龙", "活动", 
    "这个", "真是", "就是", "不要", "大师", "没有", "什么", "知道", 
    "这种", "可以", "怎么", "已经", "感觉", "评论", "转发", "点赞",
    "微博", "一个", "我们", "大家", "这种", "行为"
  )
  
  # ----------------------------------------------------------------------------
  # 3. 数据准备和 DFM 构建
  # ----------------------------------------------------------------------------
  
  weibo_corpus <- corpus(data, text_field = "content")
  
  # 3.1. 分词、清洗和复合
  toks_lss <- weibo_corpus %>%
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, split_tags = TRUE) %>%
    tokens_remove(pattern = c(ch_stop, custom_event_stopwords)) %>%
    tokens_select(min_nchar = 2) %>%
    tokens_compound(pattern = multi_word_phrases, concatenator = "")
  
  # 3.2. 构建 DFM
  df_matrix <- dfm(toks_lss) %>%
    # 保留文档频率至少为 2 的词语
    dfm_trim(min_docfreq = 2) 
  
  # ----------------------------------------------------------------------------
  # 4. LSS 模型输入准备
  # ----------------------------------------------------------------------------
  
  # 4.1. 定义种子词向量
  seed_word <- c(
    rep(1, length(pos_seeds)), 
    rep(-1, length(neg_seeds))
  ) %>%
    setNames(c(pos_seeds, neg_seeds))
  
  # 4.2. 定义上下文词汇 (使用 DFM 中出现频率最高的前 5000 个词)
  lss_terms <- names(topfeatures(df_matrix, n = 5000)) 
  
  # ----------------------------------------------------------------------------
  # 5. 运行 LSS 模型
  # ----------------------------------------------------------------------------
  
  lss_model <- textmodel_lss(
    df_matrix, 
    seeds = seed_word, 
    terms = lss_terms, 
    k = 300, # 潜在语义空间维度
    include_data = FALSE 
  )
  
  # ----------------------------------------------------------------------------
  # 6. 可视化和返回
  # ----------------------------------------------------------------------------
  
  # 筛选出用于高亮显示的词汇
  top_features_to_highlight <- names(topfeatures(df_matrix, n_top_features))
  
  # 绘制 LSS 词语极性图
  textplot_terms(
    lss_model, 
    highlighted = top_features_to_highlight, 
    title = paste0("词语极性分析：", pos_label, " vs. ", neg_label),
    subtitle = paste0("轴线极性：「", pos_label, " (+1)」到「", neg_label, " (-1)」的 LSS 轴线")
  )
  
  # 返回模型对象，以便后续检查词语得分
  return(lss_model)
}

# 2. 调用函数
# 假设您的数据框已加载为 data_coding
map2(
  list(
    coding_keywords$eco_animal, 
    coding_keywords$call_remedy, 
    c("动物", "鼠兔")
  ), 
  list(
    coding_keywords$eco_animal, 
    coding_keywords$call_boycott, 
    c("植物", "真菌")
  ), 
  function(x, y) {
    generate_lss_polarity_plot(
      data = data_coding, 
      pos_seeds = x, 
      neg_seeds = y, 
      pos_label = "", 
      neg_label = "",
      n_top_features = 50 # 高亮显示前 60 个词语
    ) %>% 
      textplot_terms(
        ., 
        highlighted = c(x, y), 
        title = "词语极性：动物相关 vs. 植物相关"
      )
  }
)
