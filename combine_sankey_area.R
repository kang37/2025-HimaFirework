# 手动指定节点和箭头颜色。
cat_colors <- c(
  # 主题颜色。
  "environment" = "#1b9e77",
  "pollution" = "#d73027",
  "brand" = "#756bb1",
  "act" = "#08519c",
  
  # ==== eco 子类 ====
  "animal" = "#33a02c",
  "plant" = "#66c2a5",
  "ecosystem" = "#b2df8a",
  
  # ==== pollution 子类 ====
  "light" = "#fc8d59",
  "noise" = "#ef6548",
  "waste" = "#d7301f",
  "air" = "#f46d43",
  "water" = "#fee090",
  
  # ==== brand 子类 ====
  "company" = "#9e9ac8",
  "cai" = "#6a51a3",
  
  # ==== act 子类 ====
  "accountability" = "#3182bd",
  "remedy" = "#6baed6",
  "boycott" = "#9ecae1"
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
library(dplyr)
library(ggplot2)
library(patchwork) # 用于把多张图拼在一起显示




library(dplyr)
library(patchwork)
library(grid)

# 假设 df_data, cat_colors 和其他依赖项已定义
# 为了函数独立性，我们在函数内部定义 cat_colors，或将其作为参数传入



plot_area_by_cat <- function(df, spacing_cm_vector, color_map = cat_colors) {
  # 1. 识别唯一的类别并检查间距向量长度
  categories <- rev(unique(coding_fac_lvl$cat))
  N <- length(categories)
  
  if (length(spacing_cm_vector) != N - 1) {
    stop(paste0("间距向量的长度必须是类别数量减一 (N-1)。期望长度: ", N - 1, 
                ", 实际长度: ", length(spacing_cm_vector)))
  }
  
  # 2. 核心绘图函数（单个子图）
  plot_single_cat <- function(data_subset, category_name, is_last_plot) {
    # 使用 rlang::sym() 来处理列名字符串
    x_var <- sym("post_date")
    y_var <- sym( "mention_prop")
    fill_var <- sym("cat_sub")
    
    p <- ggplot(
      data_subset, aes(x = !!x_var, y = !!y_var, group = !!fill_var)
    ) +
      # 非堆叠面积图和线条
      geom_area(aes(fill = !!fill_var), position = "identity", alpha = 0.4) +
      geom_line(aes(color = !!fill_var), size = 0.8) +
      # 设置颜色
      scale_fill_manual(values = color_map) +
      scale_color_manual(values = color_map) +
      labs(x = NULL, y = "Mention\nproportion", title = category_name) +
      lims(x = c(as_date("2025-09-19"), as_date("2025-10-04"))) + 
      theme_bw() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0),
        # 移除图表边距，以确保间距控制准确
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm") 
      )
    
    # 如果不是最后一个图，移除 X 轴的刻度和标签，以紧凑排列
    if (!is_last_plot) {
      p <- p + theme(axis.title.x = element_blank(), 
                     axis.text.x = element_blank())
    } else {
      # 最后一个图保留完整的 X 轴
      p <- p + labs(x = "日期 (post_date)") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(p)
  }
  
  # 3. 循环创建所有子图和间隙
  plots <- list()
  for (i in 1:N) {
    cat_name <- categories[i]
    data_subset <- df %>% filter(.data[[ "cat"]] == cat_name)
    
    # 绘制当前子图
    is_last <- (i == N)
    p <- plot_single_cat(data_subset, cat_name, is_last)
    plots[[length(plots) + 1]] <- p
    
    # 插入间隙 (除了最后一个图之外)
    if (i < N) {
      gap_cm <- spacing_cm_vector[i]
      # 创建空图，并用 grid::unit() 定义其高度
      gap_plot <- plot_spacer() + 
        theme_void() + 
        # 使用 plot_layout 时的 "cm" 单位
        plot_layout(heights = unit(gap_cm, "cm"))
      plots[[length(plots) + 1]] <- gap_plot
    }
  }
  
  # 4. 使用 patchwork 组合
  # 使用 reduce 函数将所有图表和间隙用 / 运算符堆叠起来
  final_plot <- Reduce(`+`, plots) + 
    plot_layout(ncol = 1, heights = sapply(plots, function(p) {
      # 对于实际的图表，使用 "null" 单位使其均匀分配高度
      if (inherits(p, "ggplot")) {
        return(unit(1, "null")) 
      } else {
        # 对于间隙 (plot_spacer)，高度已在创建时定义为 "cm"
        return(p$layout$heights)
      }
    }))
  
  return(final_plot)
}


sankey_plt <- ggplot(
  df_long,
  aes(x = x, next_x = next_x, node = node, next_node = next_node,
      value = value, fill = node)
) +
  geom_sankey(flow.alpha = 0.7, node.color = "grey30", width = 0.5) + 
  geom_sankey_label(aes(label = node), size = 3, col = "black", fill = "white", alpha = 0.7, text.color = "black") +
  scale_fill_manual(values = cat_colors, name = NULL) +
  labs(x = NULL, y = "sum(coding_n)") +
  ggsankey::theme_sankey(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
area_plt <- plot_area_by_cat(coding_smry %>% filter(day_tot_mention >= 50), c(0, 0, 0)) 
(sankey_plt | area_plt) + 
  plot_layout(
    widths = c(4, 1),
    # 关键步骤：统一收集 guides (legends)
    # 这将确保左右两个图之间不会有额外的空间来放置它们各自的 legend
    guides = "collect"
  ) & theme(
    # 减少图表外部的边距
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm") 
  )

# 各个子主题占主题的百分比。
coding_smry %>% 
  group_by(cat, cat_sub) %>% 
  summarise(cat_sub_mention = sum(day_catsub_mention), .groups = "drop") %>% 
  group_by(cat) %>% 
  mutate(
    catsub_to_cat = cat_sub_mention / sum(cat_sub_mention), 
    catsub_to_cat = round(catsub_to_cat * 100, digits = 2)
  ) %>% 
  select(cat, cat_sub, catsub_to_cat) %>% 
  View()

