library(tidyverse)

csv_path <- "./Ecosystem/results/Quality/efg_EQI_area.csv"

# ========= 1) 配色 =========
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
  "Freshwater","#4682B4","Marine-Freshwater-Terrestrial","#FFD966",
  "Marine-Terrestrial","#E2EFDA","ALL","#8A7F7A"
)

name_to_col <- c(
  "T1"="#538234","T2"="#A9D08F","T3"="#E6A700","T4"="#FFD54F","T5"="#FF6600",
  "T6"="#BDD7EF","T7"="#FF99FF","TF1"="#4DB6AC",
  "MT"="#C6E0B4","MFT"="#FFD966","Freshwater"="#4682B4"
)

dat <- readr::read_csv(csv_path, show_col_types = FALSE)

stopifnot(all(c("efg","decrease_area_km2") %in% names(dat)))
dat <- dat %>%
  mutate(
    efg = as.character(efg),
    deg_area = as.numeric(decrease_area_km2)
  ) %>%
  filter(is.finite(deg_area) & deg_area > 0)

is_subtype <- str_detect(dat$efg, "^[A-Za-z]+[0-9]+\\.[0-9]+$")
efg_sub <- dat %>% filter(is_subtype)

efg_sum <- efg_sub %>%
  group_by(efg) %>%
  summarise(area = sum(deg_area), .groups = "drop") %>%
  mutate(share = area / sum(area)) %>%
  arrange(desc(area))

biome_from_efg <- function(x) sub("^([A-Za-z]+[0-9]+)\\..*$", "\\1", x)
biome_sum <- efg_sum %>%
  mutate(biome = biome_from_efg(efg)) %>%
  group_by(biome) %>%
  summarise(area = sum(area), .groups = "drop") %>%
  mutate(share = area / sum(area)) %>%
  arrange(desc(area))

efg_cols <- color_map %>%
  filter(code_norm %in% efg_sum$efg) %>%
  deframe()
missing_efg <- setdiff(efg_sum$efg, names(efg_cols))
if (length(missing_efg) > 0) {
  efg_cols <- c(efg_cols, setNames(rep("#A0A0A0", length(missing_efg)), missing_efg))
}

biome_cols <- name_to_col[intersect(names(name_to_col), biome_sum$biome)]
missing_biome <- setdiff(biome_sum$biome, names(biome_cols))
if (length(missing_biome) > 0) {
  biome_cols <- c(biome_cols, setNames(rep("#B0B0B0", length(missing_biome)), missing_biome))
}

p <- ggplot() +
  geom_bar(
    data = efg_sum,
    aes(x = 2, y = share, fill = efg),
    stat = "identity", width = 0.6, color = "white", linewidth = 0.2
  ) +
  geom_bar(
    data = biome_sum,
    aes(x = 1, y = share, fill = biome),
    stat = "identity", width = 0.6, color = "white", linewidth = 0.2
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(biome_cols, efg_cols),
                    breaks = c(names(biome_cols), names(efg_cols))) +
  xlim(0.4, 2.6) +
  theme_void(base_family = "Arial") +
  theme(legend.position = "none",
        plot.margin = margin(6, 6, 6, 6))

print(p)

ggsave("./Ecosystem/results/Quality/degradPie.png",
       p, width = 8, height = 8, dpi = 300, bg = "white")
