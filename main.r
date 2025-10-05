# Package ----
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(dplyr) 
library(stringr)
library(lubridate)
library(purrr)
library(showtext)
library(plotly)
showtext_auto()

# Basic data ----
# 函数：推算日期。从一个data.frame的一列中提取日期。这列中有两种类型数据，一类类似于“2025/09/28 11:35:15”，另一类类似于“1小时前 转赞人数超过100”。对于前者，直接提取日期并转化成日期类型，对于后者，我的数据下载时间是2025-09-29 10:55:00，根据这个推算。
# 定义推算相对日期的函数
calc_relative_date <- function(
    time_str, download_time = ymd_hm("2025-09-28 21:34", tz = "Asia/Shanghai")) {
  
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
  
  return(as_date(result_datetime))
}

# 定义下载时间（包含在函数默认值中，但此处也单独定义，便于逻辑判断）
DOWNLOAD_DATE <- as_date(ymd_hm("2025-09-29 10:55", tz = "Asia/Shanghai"))

data <- openxlsx::read.xlsx("data_raw/weibo_himalaya_firework.xlsx") %>%
  tibble() %>%
  rename_with(~tolower(.x)) %>%
  mutate(
    # Bug: 需要确认时区。
    download_time = ymd_hms(current_time, ymd_hms(tz = "Asia/Shanghai")), 
    post_date = case_when(
      # 条件 A: 标准日期时间格式 (包含4位年份)
      str_detect(post_time, "(\\d{4}[-/]\\d{2}[-/]\\d{2})") ~
        as_date(ymd_hms(post_time, truncated = 1)),
      
      # 条件 B: 月-日格式 (如 "9-26")
      # 匹配开头是数字-数字，且不含年份的格式
      str_detect(post_time, "^\\d{1,2}-\\d{1,2}") ~ {
        # 提取当前下载年份
        current_year <- year(download_time)
        # 将 "9-26" 和 "2025-" 拼接成 "2025-9-26"
        date_str <- paste0(
          current_year, "-", str_extract(post_time, "^\\d{1,2}-\\d{1,2}")
        )
        # 使用 lubridate::ymd() 转换
        ymd(date_str)
      },
      
      # 条件 C: 相对时间格式 ("小时前", "昨天", "天前")
      TRUE ~ map_vec(post_time, ~calc_relative_date(.x))
    ),
    .after = "post_time"
  ) %>% 
  filter(post_date >= as_date("2025-09-19")) %>% 
  select(-current_time, -download_time) %>% 
  distinct()

# 数据量
# Bug: 是否可靠？不清楚Octoparse的抓取方式。
data %>% 
  group_by(post_date) %>% 
  summarise(n = n()) %>% 
  ggplot() + 
  geom_col(aes(post_date, n))

# 将你的评论文本列 (comment_text) 转换为 quanteda 语料库
weibo_corpus <- corpus(data, text_field = "content", 
                       docid_field = "doc_id") 

# Chinese stopwords
ch_stop <- stopwords("zh", source = "misc")

# tokenize
toks_ch_with_tag <- weibo_corpus %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = ch_stop)

# 提取标签。
dfmat_tag <- dfm(toks_ch_with_tag) %>% 
  dfm_select(., pattern = "#*")
names(topfeatures(dfmat_tag, 20))

# Tag temporal change ----
# 标签热度随着时间变化。
# 1.1 找出总频率最高的前 20 个标签
top_20_tags <- topfeatures(dfmat_tag, 15) %>%
  names() # 提取词语名称（作为字符向量）

# 1.2 筛选 DFM，只保留这 20 个标签
dfmat_top_tags <- dfm_select(dfmat_tag, pattern = top_20_tags)

df_date_freq <- 
  # 2.1 按 'post_date' 变量对 DFM 进行汇总 (即按日期求和)
  dfm_group(dfmat_top_tags, groups = post_date) %>%
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
  mutate(date = as_date(date)) 

# 3.2 绘制高频标签随时间变化的图表
ggplotly(
  df_date_freq %>%
    # 仅保留出现次数大于 0 的数据点
    filter(Frequency > 0) %>%
    ggplot(aes(x = date, y = Frequency, fill = Tag)) +
    geom_col(position = "fill") +
    labs(
      title = "高频标签在各个日期的出现频率",
      x = "日期",
      y = "出现频率 (计数)",
      color = "高频标签"
    ) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 day", date_labels = "%m-%d") + # 优化日期轴显示
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 旋转 x 轴标签
)

# 组合词。
multi_word_phrases <- list(c("始祖", "鸟"), c("蔡", "国", "强"), c("硬", "气"), c("升", "龙" ), c("炸", "山"), c("锐", "评"), c("杂", "物"), c("圣", "山"))

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

# to set the font correctly for macOS
library(quanteda.textplots)
textplot_wordcloud(
  dfmat_ch, min_count = 50, random_order = FALSE,
  rotation = .25, max_words = 100,
  min_size = 0.5, max_size = 2.8,
  font = if (Sys.info()['sysname'] == "Darwin") "SimHei" else NULL,
  color = RColorBrewer::brewer.pal(8, "Dark2")
)

# Collocations ----
# bigrams cross the whole dataset
library("quanteda.textstats")

tstat_col_ch <- textstat_collocations(toks_ch_split_tag, size = 2, min_count = 20)
knitr::kable(head(tstat_col_ch, 10))
tstat_col_ch <- textstat_collocations(toks_ch_split_tag, size = 3, min_count = 20)
knitr::kable(head(tstat_col_ch, 10))

# Topic ----
# 主题模型
# 大约花1-2分钟
library(stm)
my_lda_fit20 <- stm(dfmat_ch, K = 10, verbose = FALSE)
plot(my_lda_fit20)  

