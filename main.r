# Preparation ----
pacman::p_load(
  quanteda, quanteda.textstats, quanteda.textplots, quanteda.textmodels, 
  dplyr, ggplot2, tidyr, stringr, openxlsx, lubridate, readr, purrr, tibble, 
  showtext, rlang, LSX, patchwork, quantmod, Hmisc, reshape2, TTR, ggsankey
)
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
  distinct() %>% 
  # 将点赞、评论、转发等转化成数字。
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
  )

# 安踏股价。
anta_stock <- 
  tidyquant::tq_get("2020.HK", from = "2025-09-15", to = "2025-10-04") %>%
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
# Bug: 微博数据量是否可靠？不清楚Octoparse的抓取方式。
all_dt_num_smry <- 
  # 微博文本数。
  data %>% 
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
    anta_stock %>% select(date, adj_stock = adjusted), 
    by = c("post_date" = "date")
  ) %>% 
  # 股价环比变化量和变化率。
  arrange(post_date) %>%
  mutate(
    # 取出所有非 NA 股价对应日期的向量
    valid_stock = ifelse(!is.na(adj_stock), adj_stock, NA_real_),
    prev_stock = zoo::na.locf(adj_stock, na.rm = FALSE), # 向前填充上一个非 NA 值
    prev_stock = lag(prev_stock),
    stock_change = adj_stock - prev_stock
  ) %>%
  select(-valid_stock, -prev_stock)

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
    labs(x = NULL, y = "Weibo index", color = NULL, linetype = NULL) +
    scale_x_date(
      limits = c(as.Date("2025-09-18"), as.Date("2025-10-05")),
      breaks = as.Date(c("2025-09-19", "2025-09-21", "2025-09-26", "2025-10-04")),
      labels = c("9-19", "9-21", "09-26", "10-04")
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
    labs(x = NULL, y = "Google trend\nindex", color = NULL, linetype = NULL) +
    theme_bw() +
    scale_x_date(
      limits = c(as.Date("2025-09-18"), as.Date("2025-10-05")),
      breaks = as.Date(c("2025-09-19", "2025-09-21", "2025-09-26", "2025-10-04")),
      labels = c("9-19", "9-21", "09-26", "10-04")
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
    labs(x = "Date", y = "Stock price (HKD)") +
    scale_x_date(
      limits = c(as.Date("2025-09-18"), as.Date("2025-10-05")),
      breaks = as.Date(c("2025-09-19", "2025-09-21", "2025-09-26", "2025-10-04")),
      labels = c("9-19", "9-21", "09-26", "10-04")
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

# 将你的评论文本列 (comment_text) 转换为quanteda语料库。
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
# 1) 你的关键词表保持不变
coding_keywords <- list(
  animal = c("动物","雪豹","鼠兔","牲畜","栖息地","物种","生灵","惊扰","迁徙","受惊"),
  plant  = c("植被","植物","草","地衣","苔藓","真菌","草毡层","修复","绿化"),
  ecosystem = c("生态", "脆弱","高原","冰川","山脉","自然", "不可逆"),
  light = c("光污染","强光","光害"),
  noise = c("噪声","噪音","声响","爆破声","惊扰","安静"),
  waste = c("垃圾","残渣","遗留物","清理","杂物","废物"),
  air   = c("空气污染","烟雾","大气","粉尘","PM2.5","颗粒物"),
  water = c("水污染","水体","融水","雪水","河流"),
  company = c("始祖鸟","安踏","品牌","广告"),
  cai     = c("人设","虚伪","傲慢"),
  accountability = c("调查","处罚","追责","立法","合规","审查","立案","判刑"),
  remedy        = c("道歉","赔偿","修复","补救","清理","评估"),
  boycott       = c("抵制","不买","下架","退货","转黑","卸载")
)

# 2) 子类 → 大类 的映射
category_map <- c(
  animal="生态危害", plant="生态危害", ecosystem="生态危害",
  light="污染信息", noise="污染信息", waste="污染信息", air="污染信息", water="污染信息",
  company="品牌/商业", cai="品牌/商业",
  accountability="诉求/行动", remedy="诉求/行动", boycott="诉求/行动"
)

# 3) 关键词检测函数（更稳健：转小写对中文无影响；转义正则特殊字符）
detect_keywords <- function(text, keywords){
  kw <- str_replace_all(keywords, "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\\\1")
  pattern <- str_c(kw, collapse="|")
  as.integer(str_detect(text, pattern))
}

# 4) 生成各子类 0/1 列
coding_results_df <- map_dfc(coding_keywords, ~ detect_keywords(data$content, .x))

data_coding <- data %>% 
  bind_cols(coding_results_df)

# 编码主题分类。
coding_cat <- rbind(
  data.frame(cat = "environment", cat_sub = c("animal", "plant", "ecosystem")), 
  data.frame(
    cat = "pollution", cat_sub = c("light", "noise", "waste", "air", "water")
  ), 
  data.frame(cat = "brand", cat_sub = c("company", "cai")),
  data.frame(cat = "act", cat_sub = c("accountability", "remedy", "boycott"))
)

# 汇总编码结果。
coding_smry <- data_coding %>% 
  pivot_longer(
    cols = coding_cat$cat_sub, names_to = "cat_sub", values_to = "mention"
  ) %>% 
  left_join(coding_cat, by = "cat_sub") %>% 
  # 计算各天总提及数量。
  group_by(post_date) %>% 
  mutate(day_tot_mention = sum(mention)) %>% 
  ungroup() %>% 
  # 计算每天各主题提及数量。
  group_by(post_date, cat) %>% 
  mutate(day_cat_mention = sum(mention)) %>% 
  ungroup() %>% 
  # 计算各天子各主题的提及数量，并计算它们在一天内各主题的比例。
  group_by(post_date, cat, cat_sub, day_tot_mention, day_cat_mention) %>% 
  summarise(day_catsub_mention = sum(mention), .groups = "drop") %>% 
  mutate(mention_prop = day_catsub_mention / day_cat_mention) %>% 
  filter(mention_prop != 0)

# 作图。
ggplot() + 
  geom_col(aes(post_date, mention_prop, fill = cat_sub)) + 
  facet_grid(.~ cat)
  
  

# 3️⃣ 加权比例函数：跳过发帖数 < 50 的日期
get_coding_day_prop_weighted <- function(coding_col_name) {
  data_coding %>%
    group_by(post_date) %>%
    summarise(
      day_n           = n(),
      weighted_coding = sum(weight_score * .data[[coding_col_name]], na.rm = TRUE),
      total_weight    = sum(weight_score, na.rm = TRUE),
      prop_raw        = weighted_coding / total_weight,
      .groups = "drop"
    ) %>%
    mutate(
      prop = ifelse(day_n >= 20, prop_raw, NA_real_),  # 小于50条则设为 NA
      coding_col = coding_col_name,
      .before = 1
    ) %>%
    select(-prop_raw)  # 可选，去掉中间列
}

# 4️⃣ 批量计算所有主题
coding_smry_weighted <- map_df(names(coding_keywords), get_coding_day_prop_weighted) %>%
  mutate(cat = recode(coding_col, !!!category_map))

# 5️⃣ 可视化
ggplot(coding_smry_weighted) +
  geom_area(aes(post_date, prop, fill = coding_col), position = "dodge", alpha = 0.5) +
  facet_wrap(~cat) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "日期",
    y = "加权占比（考虑传播量，仅≥50条日样本）",
    fill = "主题"
  ) +
  lims(x = c(as_date("2025-09-19"), as_date("2025-10-04"))) + 
  theme_minimal(base_size = 12)

