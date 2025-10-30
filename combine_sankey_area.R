# 手动指定节点和箭头颜色。
cat_colors <- c(
  # 主题颜色。
  "environment" = "#1b9e77",
  "pollution" = "#d73027",
  "brand" = "#756bb1",
  "act" = "#08519c",
  
  # ==== eco 子类 ==== (保持原有配色，区分度较好)
  "animal" = "#33a02c",
  "plant" = "#66c2a5",
  "ecosystem" = "#b2df8a",
  
  # ==== pollution 子类 ==== (优化：增加明暗和色相对比)
  "light" = "#fdae61",     # 亮橙色
  "noise" = "pink",     # 深红色
  "waste" = "#a50026",     # 暗红色
  "air" = "#fee08b",       # 浅黄色
  "water" = "#8c510a",     # 蓝色（对比色）
  
  # ==== brand 子类 ==== (保持原有配色，区分度较好)
  "company" = "#9e9ac8",
  "cai" = "#6a51a3",
  
  # ==== act 子类 ==== (优化：增加明暗对比)
  "accountability" = "#08519c",  # 深蓝
  "remedy" = "#3182bd",          # 中蓝
  "boycott" = "#9ecae1"          # 浅蓝
)

# Sankey ----
# 1) 计算从 cat → coding_col 的权重
flows <- coding_smry %>%
 group_by(cat, cat_sub) %>%
 summarise(value = sum(day_catsub_mention, na.rm = TRUE), .groups = "drop")

# 2) 转换为 ggsankey 长格式
df_long <- ggsankey::make_long(flows, cat, cat_sub, value = value)

# 3) 建立映射：每个 coding_col 对应的 cat
map_tbl <- flows %>% distinct(cat_sub, cat)

# 桑基图节点因子排序。
coding_smry %>% 
 group_by(cat) %>% 
 summarise(cat_mention = sum(day_cat_mention), .groups = "drop") %>% 
 arrange(cat_mention) %>% 
 pull(cat)
coding_fac_lvl <- coding_smry %>% 
 # 计算主题比例。
 group_by(cat) %>% 
 mutate(cat_mention = sum(day_cat_mention)) %>% 
 ungroup() %>% 
 # 计算子主题比例。
 group_by(cat, cat_sub, cat_mention) %>% 
 summarise(catsub_mention = sum(day_catsub_mention), .groups = "drop") %>% 
 # 排序。
 arrange(cat_mention, cat, catsub_mention)

df_long <- df_long %>%
 mutate(
 # 左边节点是cat
 cat_left = ifelse(x == "cat", node, NA_character_)
 ) %>%
 left_join(map_tbl, by = c("node" = "cat_sub")) %>%
 mutate(
 cat_fill = dplyr::coalesce(cat_left, cat)
 ) %>%
 select(-cat_left, -cat) %>% 
 # 设置节点排序。
 mutate(
 node = factor(
 node, levels = c(unique(coding_fac_lvl$cat), coding_fac_lvl$cat_sub)
 ),
 next_node = factor(
 next_node, levels = c(unique(coding_fac_lvl$cat), coding_fac_lvl$cat_sub)
 )
 ) 

# 5) 绘图
# 修改标签为首字母大写
df_long_capitalized <- df_long %>%
  mutate(
    # 将 node 和 next_node 的标签改为首字母大写
    node = tools::toTitleCase(as.character(node)),
    next_node = tools::toTitleCase(as.character(next_node)),
    # 重新设置为因子，保持原有的排序
    node = factor(node, levels = tools::toTitleCase(levels(df_long$node))),
    next_node = factor(next_node, levels = tools::toTitleCase(levels(df_long$next_node)))
  )

# 同时需要更新颜色映射表中的键名为首字母大写
cat_colors_capitalized <- setNames(
  cat_colors,
  tools::toTitleCase(names(cat_colors))
)

# 绘制桑基图
plt_sankey <- ggplot(
  df_long_capitalized,
  aes(x = x, next_x = next_x, node = node, next_node = next_node,
      value = value, fill = node)
) +
  geom_sankey(flow.alpha = 0.7, node.color = "grey30", width = 0.3, node.size = 0.1) + 
  geom_sankey_label(aes(label = node), size = 6, col = NA, fill = "white", alpha = 0.7, text.color = "black") +
  scale_fill_manual(values = cat_colors_capitalized, name = NULL) +
  labs(x = NULL, y = NULL) +
  ggsankey::theme_sankey(base_size = 20) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, hjust = 0.5),
    # 去掉底部的 x 轴标签（"cat" 和 "cat_sub"）
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
ggsave("data_proc/sankey_plot.png", plot = plt_sankey, width = 5, height = 5, dpi = 300)

# Area plot ----
# 创建首字母大写版本的颜色映射
cat_colors_capitalized <- setNames(
  cat_colors,
  tools::toTitleCase(names(cat_colors))
)

# 获取类别列表（按照之前的排序）
categories <- rev(unique(coding_fac_lvl$cat))

# 创建单个面积图的函数
plot_single_area <- function(data_subset, category_name, valid_dates, all_dates, show_x_axis = TRUE) {
  x_var <- sym("post_date")
  y_var <- sym("mention_prop")
  fill_var <- sym("cat_sub")
  
  # 填充缺失日期为0（仅在有效日期范围内）
  # 1. 使用传入的有效日期序列（day_tot_mention >= 50的日期）
  date_range <- valid_dates
  
  # 2. 获取所有子类别
  all_cat_sub <- unique(data_subset$cat_sub)
  
  # 3. 创建完整的日期-子类别组合（仅限有效日期）
  complete_data <- expand.grid(
    post_date = date_range,
    cat_sub = all_cat_sub,
    stringsAsFactors = FALSE
  )
  
  # 4. 与原始数据合并，缺失值填充为0
  data_subset_complete <- complete_data %>%
    left_join(data_subset, by = c("post_date", "cat_sub")) %>%
    mutate(
      mention_prop = ifelse(is.na(mention_prop), 0, mention_prop),
      cat = ifelse(is.na(cat), first(na.omit(data_subset$cat)), cat)
    )
  
  # 5. 计算每个子类别的总和，用于排序（从大到小）
  cat_sub_order <- data_subset_complete %>%
    group_by(cat_sub) %>%
    summarise(total_mention = sum(mention_prop, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_mention)) %>%  # 从大到小排序
    pull(cat_sub)
  
  # 6. 将 cat_sub 转换为因子，并设置排序，同时首字母大写
  data_subset_complete <- data_subset_complete %>%
    mutate(
      cat_sub = tools::toTitleCase(as.character(cat_sub)),  # 首字母大写
      cat_sub = factor(cat_sub, levels = tools::toTitleCase(cat_sub_order))
    )
  
  # 7. 识别被删除的日期（总帖子数不足的日期）
  excluded_dates <- setdiff(all_dates, valid_dates)
  
  # 8. 如果有被删除的日期，创建矩形数据框
  if (length(excluded_dates) > 0) {
    rect_data <- data.frame(
      xmin = excluded_dates,
      xmax = excluded_dates + 1,  # 一天的宽度
      ymin = -Inf,
      ymax = Inf
    )
  }
  
  # 绘图
  p <- ggplot(
    data_subset_complete, aes(x = !!x_var, y = !!y_var, group = !!fill_var)
  ) +
    # 添加灰色半透明矩形标记删除的日期
    {if (length(excluded_dates) > 0) {
      geom_rect(data = rect_data, 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "grey50", alpha = 0.3, inherit.aes = FALSE)
    }} +
    # 非堆叠面积图和线条
    geom_line(aes(color = !!fill_var), size = 1.5) +
    geom_point(aes(color = !!fill_var), size = 2) +  # 添加点的大小
    # 设置颜色（使用首字母大写版本）
    scale_fill_manual(values = cat_colors_capitalized) +
    scale_color_manual(values = cat_colors_capitalized) +
    labs(x = NULL, y = "Mention proportion") +  # 删除 title
    # X轴范围设置为完整日期范围，显示每一天
    scale_x_date(
      limits = c(min(all_dates), max(all_dates)),
      breaks = all_dates,  # 显示每一天
      date_labels = "%m-%d",  # 日期格式：月-日
      expand = c(0, 0)  # 不留边距
    ) +
    theme_bw(base_size = 14) +  # 增加基础字体大小
    theme(
      # 图例设置：放在右边，垂直排列，无标题
      legend.position = "right",
      legend.direction = "vertical",  # 垂直排列
      legend.title = element_blank(),
      legend.margin = margin(l = 5),
      legend.key.size = unit(0.8, "cm"),  # 图例方块大小
      legend.text = element_text(size = 25),  # 增大图例文字
      axis.title = element_text(size = 25),  # 增大轴标题
      axis.text = element_text(size = 25),   # 增大轴文字
      plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")
    ) +
    # 使用 guide_legend 进一步控制图例
    guides(
      fill = guide_legend(reverse = FALSE),  # 不反转，保持从大到小
      color = guide_legend(reverse = FALSE)
    )
  
  # 控制 X 轴显示
  if (!show_x_axis) {
    p <- p + theme(
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  } else {
    p <- p + labs(x = "Date") +  # 简化标签
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16))  # 减小字号以适应更多日期
  }
  
  return(p)
}

# Bug: 筛选数据：只保留 day_tot_mention >= 50 的日期
filtered_data <- coding_smry %>% filter(day_tot_mention >= 30)

# 获取所有符合条件的有效日期（已经过滤了 day_tot_mention < 50 的日期）
valid_dates <- filtered_data %>%
  distinct(post_date) %>%
  arrange(post_date) %>%
  pull(post_date)

# 获取完整日期范围（2025-09-19 到 2025-10-04）
all_dates <- seq(as_date("2025-09-19"), as_date("2025-10-04"), by = "day")

# 作图 - 增大图片尺寸
for (i in seq_along(categories)) {
  cat_name <- categories[i]
  cat_data <- filtered_data %>% filter(cat == cat_name)
  is_last <- (i == length(categories))
  
  png(filename = paste0("data_proc/area_plot_", cat_name, ".png"),
      width = 8, height = 4, units = "in", res = 300)  
  plot_single_area(cat_data, cat_name, 
                   valid_dates = valid_dates, 
                   all_dates = all_dates,
                   show_x_axis = is_last) %>% print()
  dev.off()
}

# 各主题提及数占总提及数的百分比。
coding_smry %>% 
  group_by(cat) %>% 
  summarise(cat_mention = sum(day_catsub_mention), .groups = "drop") %>% 
  mutate(cat_to_tot = sprintf(
    "%.2f", round(cat_mention / sum(cat_mention) * 100, digits = 2)
  ))
# 各子主题提及占相应主题，以及占总提及数的百分比。
coding_smry %>% 
  group_by(cat, cat_sub) %>% 
  summarise(cat_sub_mention = sum(day_catsub_mention), .groups = "drop") %>% 
  # 各子主题提及占相应主题的比例。
  group_by(cat) %>% 
  mutate(
    catsub_to_cat = cat_sub_mention / sum(cat_sub_mention), 
    catsub_to_cat = round(catsub_to_cat * 100, digits = 2)
  ) %>%
  ungroup() %>% 
  # 各子主题占总提及数的百分比。
  mutate(
    catsub_to_tot = round(cat_sub_mention / sum(cat_sub_mention) * 100, digits = 2)
  ) %>% 
  write.xlsx("data_proc/coding_subcategory_summary.xlsx")

