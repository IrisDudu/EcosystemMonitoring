library(tidyverse)
library(scales)

csv_path <- "./Ecosystem/results/Quality/efg_EQI_area_norm_top_bottom.csv"
dat <- readr::read_csv(csv_path, show_col_types = FALSE)

if (!"efg" %in% names(dat) && "efg_band" %in% names(dat)) {
  dat <- dat %>% rename(efg = efg_band)
}

need_cols <- c("efg",
               "decrease_pct","increase_pct",
               "decrease_pct_top","decrease_pct_bottom",
               "increase_pct_top","increase_pct_bottom")
stopifnot(all(need_cols %in% names(dat)))

dat <- dat %>%
  mutate(
    across(all_of(need_cols[need_cols != "efg"]), as.numeric)
  )

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
  "Marine-Terrestrial","#E2EFDA",
  "ALL","#8A7F7A"
)

dat <- dat %>%
  mutate(code_norm = norm_code(efg)) %>%
  left_join(color_map, by = "code_norm") %>%
  mutate(fill_col = if_else(is.na(fill_col), "#A0A0A0", fill_col)) %>%
  
efg_levels_in_input_order <- dat$efg %>% unique()

long <- bind_rows(
  dat %>%
    transmute(
      efg,
      direction = "Decrease",
      value = decrease_pct,
      lower = decrease_pct_top, 
      upper = decrease_pct_bottom,
      fill_col
    ),
  dat %>%
    transmute(
      efg,
      direction = "Increase",
      value =  increase_pct,
      lower =  increase_pct_bottom,
      upper =  increase_pct_top,
      fill_col
    )
) %>%
  mutate(
    efg = factor(efg, levels = efg_levels_in_input_order)
  )

rng <- range(c(long$lower, long$upper), na.rm = TRUE)

max_abs <- max(abs(rng), na.rm = TRUE)
if (!is.finite(max_abs) || max_abs == 0) {
  rng <- c(-1, 1)
} else if (max_abs < 2) {
  pad <- max(1, ceiling(max_abs))
  rng <- c(-pad, pad)
}

xbks <- scales::breaks_extended(n = 7)(rng)
if (!any(abs(xbks) < 1e-9)) xbks <- sort(c(xbks, 0))
xlims <- range(xbks, na.rm = TRUE)

x_lower <- -60
x_upper <- max(xlims[2], 60)
xbks_fixed <- sort(unique(c(seq(-60, x_upper, by = 20), xbks)))
label_pct_fixed <- function(x) paste0(formatC(x, format = "f", digits = 0), "%")

p <- ggplot(long, aes(y = efg)) +
  geom_col(aes(x = value, fill = fill_col, alpha = 0.3),
           width = 0.8, color = NA) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = efg, color = fill_col),
                 height = 0.3, linewidth = 0.8) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.4) +
  scale_fill_identity(guide = "none") +
  scale_color_identity(guide = "none") +
  scale_x_continuous(
    breaks = xbks_fixed,
    labels = label_pct_fixed,
    limits = c(x_lower, x_upper)
  )+
  labs(
    x = "Change (%):  left = decrease, right = increase",
    y = NULL,
    title = "EFG quality change (significant) with uncertainty"
  ) +
  theme_minimal(base_family = "Arial", base_size = 25) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(color = "black")
  )

print(p)

ggsave("./Ecosystem/results/Quality/efg_quality_bars.png",
       p, width = 10, height = 15, dpi = 300, bg = "white")
