library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(mblm)

name <- "warm_temp"
csv_path <- sprintf("./Ecosystem/results/Ecosystem/LC_Area_ByYear_%s.csv", name)
df <- read_csv(csv_path, show_col_types = FALSE)
year_cols <- grep("^\\d{4}$", names(df), value = TRUE)

long <- df |>
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "area") |>
  mutate(year = as.integer(year))

label_map <- c(
  water = "Water",
  forest = "Forest",
  grass = "Grassland",
  wetland = "Wetland",
  crop = "Cropland",
  shrub = "Shrubland",
  ImperviousSurface = "ImperviousSurface",
  bareland = "Bareland",
  SnowIce = "Snow/ice",
  tundra = "Tundra"
)

long <- long |>
  mutate(Class = label_map[lc_band])

slopes <- long |>
  group_by(Class) |>
  summarise(
    slope = coef(mblm(area ~ year, data = cur_data()))[2],
    .groups = "drop"
  )

slopes <- slopes |>
  mutate(abs_slope = abs(slope)) |>
  arrange(desc(abs_slope)) |>
  slice_head(n = 6) |>                # 只保留绝对值最大的前6个
  mutate(Class = factor(Class, levels = Class))
print(slopes)

sld_cols <- c(
  "Water"="#419bdf","Forest"="#397d49","Grassland"="#88b053","Wetland"="#7a87c6",
  "Cropland"="#e49635","Shrubland"="#dfc35a","ImperviousSurface"="#c4281b",
  "Bareland"="#a59b8f","Snow/ice"="#a8ebff","Tundra"="#D1EB7A"
)

p <- ggplot(slopes, aes(x = Class, y = slope, fill = Class)) +
  geom_col() +
  scale_fill_manual(values = sld_cols) +
  scale_y_continuous(
    labels = function(x) x/1e4,
    breaks = scales::pretty_breaks(n = 8)
  ) +
  coord_cartesian(ylim = c(-8e4, 8e4)) + 
  labs(
    x = NULL,
    y = expression("Theil–Sen slope ("*10^4*" km"^2*"/year)"),
    title = sprintf("%s",name)
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x  = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.title.y  = element_blank(),
    plot.title   = element_text(size = 30, face = "bold"), 
    axis.text.y  = element_text(size = 28)
  )

print(p)

ggsave(sprintf("./Ecosystem/results/Ecosystem/theilsen_slopes_%s.png",name), p, width = 3, height = 5, dpi = 300, bg = "#F8F4EC")




