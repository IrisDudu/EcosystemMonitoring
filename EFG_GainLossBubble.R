library(tidyverse)
library(ggrepel)
library(scales)
library(forcats)
library(ggbreak)

norm_code <- function(x) gsub("_", ".", gsub(" ", "", x))

color_map <- tribble(
  ~code_norm, ~fill_col,
  "T1.1","#228B22","T1.2","#8FBC8F","T1.3","#2E8B57","T1.4","#3CB371",
  "T2.1","#556B2F","T2.2","#6B8E23","T2.3","#8F9779","T2.4","#A9A9A9",
  "T2.5","#BDB76B","T2.6","#A0522D",
  "T3.1","#BC8F8F","T3.2","#DAA520","T3.3","#DEB887","T3.4","#8B4513",
  "T4.1","#CD853F","T4.2","#FFA07A","T4.3","#FFD700","T4.4","#FFE4B5","T4.5","#EEE8AA",
  "T5.1","#C1A07E","T5.2","#D2B48C","T5.3","#E3C5A3","T5.4","#F4D6B5","T5.5","#E4C9AD",
  "T6.1","#D9D9D9","T6.2","#D9EAD3","T6.3","#A9CCE3","T6.4","#C6D9F0","T6.5","#98CF8F",
  "T7.1","#D7A6D6","T7.2","#E4B7E3","T7.3","#DA70D6","T7.4","#8B008B","T7.5","#D8BFD8",
  "TF1.1","#76C2A1","TF1.2","#8CCCCC","TF1.3","#A2DED2","TF1.4","#B8E8E4",
  "TF1.5","#CEF2F6","TF1.6","#D4F6EC","TF1.7","#E0F7F2",
  "Freshwater","#4682B4",
  "Marine-Freshwater-Terrestrial","#FFD966",
  "Marine-Terrestrial","#E2EFDA"
)

df_col <- df %>%
  mutate(code_norm = norm_code(efg_band)) %>%
  left_join(color_map, by = "code_norm") %>%
  mutate(fill_col = if_else(is.na(fill_col), "#A0A0A0", fill_col))  # 回退灰色

df_plot <- df_col %>%
  filter(!is.na(change_ratio_pct), !is.na(area_2024_km2))

stopifnot(nrow(df_plot) > 0)

xabs <- max(abs(df_plot$change_ratio_pct), na.rm = TRUE)
xmax <- if (!is.finite(xabs) || xabs == 0) 1 else ceiling(xabs)
xlim_safe <- c(-xmax, xmax)

set.seed(123)
y_levels <- df_plot %>% arrange(change_ratio_pct) %>% pull(efg_band) %>% unique()
x_min <- floor(min(df_plot$change_ratio_pct, na.rm = TRUE))
x_max <- ceiling(max(df_plot$change_ratio_pct, na.rm = TRUE))
step  <- 10

p <- ggplot(
  df_plot,
  aes(x = change_ratio_pct,
      y = factor(efg_band, levels = y_levels))
) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_point(aes(size = area_2024_km2, fill = fill_col),
             shape = 21, color = "black", alpha = 0.45, stroke = 0.5) +
  scale_size_area(
    max_size = 20,
    breaks = pretty(df_plot$area_2024_km2, n = 4),
    labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si("")),
    name   = "2024 area (km²)"
  ) +
  scale_fill_identity(guide = "none") +
  scale_y_discrete(expand = expansion(add = 0.8)) +
  scale_x_continuous(breaks = seq(floor(x_min/step)*step,
                                  ceiling(x_max/step)*step,
                                  by = step)) +
  labs(x = "Area change ratio (%), 1982–2024", y = "Ecosystem functional group") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
  )

out_path <- "./Ecosystem/results/EFG"
if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

ggsave(
  filename = file.path(out_path, "efg_bubble_chart_NEW.png"),
  plot     = p,
  width    = 20, height = 10, dpi = 300, bg = "white"
)
