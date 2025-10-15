# Sankey ----
library(dplyr)
library(ggplot2)
library(ggsankey)

# 1) 计算从 cat → coding_col 的权重
flows <- coding_smry %>%
  group_by(cat, coding_col) %>%
  summarise(value = sum(coding_n, na.rm = TRUE), .groups = "drop")

# 2) 转换为 ggsankey 长格式
df_long <- ggsankey::make_long(flows, cat, coding_col, value = value)

# 3) 建立映射：每个 coding_col 对应的 cat
map_tbl <- flows %>% distinct(coding_col, cat)

df_long <- df_long %>%
  mutate(
    # 左边节点是cat
    cat_left = ifelse(x == "cat", node, NA_character_)
  ) %>%
  left_join(map_tbl, by = c("node" = "coding_col")) %>%
  mutate(
    cat_fill = dplyr::coalesce(cat_left, cat)
  ) %>%
  select(-cat_left, -cat)

# 4) 手动指定颜色（包括右侧各节点）
node_colors <- c(
  # ==== 左侧 cat ====
  "eco"    = "#1b9e77",
  "pollution" = "#d73027",
  "brand"   = "#756bb1",
  "act"    = "#08519c",
  
  # ==== eco 子类 ====
  "eco_animal"  = "#33a02c",
  "eco_plant"   = "#66c2a5",
  "eco_ecosystem" = "#b2df8a",
  
  # ==== pollution 子类 ====
  "pollution_light" = "#fc8d59",
  "pollution_noise" = "#ef6548",
  "pollution_waste" = "#d7301f",
  "pollution_air" = "#f46d43",
  "pollution_water" = "#fee090",
  
  # ==== brand 子类 ====
  "brand_company" = "#9e9ac8",
  "brand_cai"  = "#6a51a3",
  
  # ==== act 子类 ====
  "act_accountability" = "#3182bd",
  "act_remedy"    = "#6baed6",
  "act_boycott"    = "#9ecae1"
)

# 5) 绘图
ggplot(
  df_long,
  aes(x = x, next_x = next_x, node = node, next_node = next_node,
      value = value, fill = node)
) +
  geom_sankey(flow.alpha = 0.7, node.color = "grey30", width = 0.5) + # 👈 调整这里
  geom_sankey_label(aes(label = node), size = 3, col = NA, fill = NA, text.color = "black") +
  scale_fill_manual(values = node_colors, name = NULL) +
  labs(x = NULL, y = "sum(coding_n)") +
  ggsankey::theme_sankey(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


# Area plot ----
library(dplyr)
library(ggplot2)
library(patchwork) # 用于把多张图拼在一起显示

# ----------------------------------------------------
# 1. 手动指定颜色向量 (使用您提供的颜色)
# ----------------------------------------------------
my_colors <- c(
  # ---- eco（绿色系）----
  "eco_animal"       = "#1b9e77",  # 深绿
  "eco_plant"        = "#66c2a5",  # 中绿
  "eco_ecosystem"    = "#a6dba0",  # 浅绿
  
  # ---- pollution（红橙系）----
  "pollution_light"  = "#d73027",  # 红
  "pollution_noise"  = "#fc8d59",  # 橙红
  "pollution_waste"  = "#fee090",  # 浅橙
  "pollution_air"    = "#f46d43",  # 橙
  "pollution_water"  = "#d7301f",  # 深橙红
  
  # ---- brand（紫系）----
  "brand_company"    = "#9e9ac8",  # 深紫
  "brand_cai"        = "#756bb1",     # 浅紫
  
  # ---- act（蓝系）----
  "act_accountability" = "#08519c",  # 深蓝
  "act_remedy"         = "#3182bd",  # 中蓝
  "act_boycott"        = "#6baed6"   # 浅蓝
)

# ----------------------------------------------------
# 2. 修正绘图函数：移除自动配色逻辑，直接引用 my_colors
# ----------------------------------------------------
# 单类别绘图函数：小值在最上层 + 图内图例 + 手动颜色
plot_one_cat <- function(cat_name, dat = coding_smry, all_colors = my_colors) {
  df_cat <- dat %>% filter(cat == cat_name)
  
  # 计算绘制顺序：大的先画（底层），小的后画（上层）
  order_tbl <- df_cat %>%
    group_by(coding_col) %>%
    summarise(mean_prop = mean(prop, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_prop)) %>%
    mutate(draw_order = row_number())
  
  df_cat <- df_cat %>%
    left_join(order_tbl, by = "coding_col") %>%
    arrange(draw_order, post_date)
  
  # 筛选出当前类别需要的颜色
  # 确保只有当前 df_cat 中存在的 coding_col 对应的颜色被选中
  current_cols_names <- unique(df_cat$coding_col)
  cols_for_plot <- all_colors[current_cols_names]
  
  # 画图（identity 叠加，透明度0.7）
  p <- ggplot(df_cat, aes(post_date, prop, fill = coding_col, group = coding_col)) +
    geom_area(position = "identity", alpha = 0.7, color = NA) +
    
    # 关键修改：直接使用筛选后的手动颜色
    scale_fill_manual(values = cols_for_plot) + 
    
    labs(
      title = cat_name,
      x = "Date", y = "Proportion", fill = NULL
    ) +
    theme_bw() +
    theme(
      legend.background = element_rect(fill = "white"),
      legend.text       = element_text(size = 8)
    )
  
  return(p)
}

# ----------------------------------------------------
# 3. 执行绘图并组合
# ----------------------------------------------------
# 假设 coding_smry 数据框已存在，并且包含 cat, coding_col, post_date, prop 等列
cats <- unique(coding_smry$cat)
plots <- lapply(c("pollution", "eco", "brand", "act"), plot_one_cat)

# 展示：使用 patchwork 组合 4 个子图
# plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
# 或使用更简洁的写法 (需要确保 plots 列表长度为 4)
wrap_plots(plots, ncol = 1)

# Combine ----
# 假设 coding_smry 数据已加载
# 假设 flows, df_long, map_tbl, node_colors 均已通过您的原代码计算和定义

sankey_plot <- ggplot(
  df_long,
  aes(x = x, next_x = next_x, node = node, next_node = next_node,
      value = value, fill = node)
) +
  geom_sankey(flow.alpha = 0.7, node.color = "grey30", width = 0.5) +
  # 调整标签大小和位置
  geom_sankey_label(aes(label = node), size = 3, col = NA, fill = NA, text.color = "black") +
  scale_fill_manual(values = node_colors, name = NULL) +
  labs(x = NULL, y = "sum(coding_n)") +
  ggsankey::theme_sankey(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # 移除或减小 x 轴文字，让图更紧凑
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# 假设 plot_df 数据框（包含 prop 和 post_date）已存在
# 假设 my_colors 向量已定义

area_plots_wrapped <- wrap_plots(plots, ncol = 1)

# 组合图表
sankey_plot | area_plots_wrapped

# 条形图加面积图版本。
(
  coding_smry %>% 
    group_by(cat) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    mutate(cat = factor(cat, levels = c("pollution", "eco", "brand", "act"))) %>% 
    ggplot() + 
    geom_col(aes(cat, n, fill = cat)) + 
    scale_fill_manual(
      breaks = c("pollution", "eco", "brand", "act"), 
      values = c("#d73027", "#1b9e77", "#756bb1", "#08519c")
    ) + 
    theme_bw() + 
    labs(x = NULL, y = "Number of posts") + 
    theme(legend.position = "none")
) | wrap_plots(plots, ncol = 2)
