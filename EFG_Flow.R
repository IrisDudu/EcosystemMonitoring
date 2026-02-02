library(tidyverse)
library(ggalluvial)
library(scales)
library(gtools)

csv_path <- "./Ecosystem/results/EFG/EFG_flows_1982_to_2024_T2.csv"
flows_raw <- read_csv(csv_path,
                      col_types = cols(
                        from = col_character(),
                        to   = col_character(),
                        area_km2 = col_double()
                      ))

flows <- flows_raw %>%
  filter(
    !str_detect(from, rm_pat),
    !str_detect(to,   rm_pat)
  ) %>%
  filter(!is.na(area_km2) & area_km2 > 0)

norm_code <- function(x) gsub("_", ".", gsub(" ", "", x))

sld_colors <- c(
  "T1.1"="#228B22","T1.2"="#8FBC8F","T1.3"="#2E8B57","T1.4"="#3CB371",
  "T2.1"="#556B2F","T2.2"="#6B8E23","T2.3"="#8F9779","T2.4"="#A9A9A9",
  "T2.5"="#BDB76B","T2.6"="#A0522D",
  "T3.1"="#BC8F8F","T3.2"="#DAA520","T3.3"="#DEB887","T3.4"="#8B4513",
  "T4.1"="#CD853F","T4.2"="#FFA07A","T4.3"="#FFD700","T4.4"="#FFE4B5","T4.5"="#EEE8AA",
  "T5.1"="#C1A07E","T5.2"="#D2B48C","T5.3"="#E3C5A3","T5.4"="#F4D6B5","T5.5"="#E4C9AD",
  "T6.1"="#D9D9D9","T6.2"="#D9EAD3","T6.3"="#A9CCE3","T6.4"="#C6D9F0","T6.5"="#98CF8F",
  "T7.1"="#D7A6D6","T7.2"="#E4B7E3","T7.3"="#DA70D6","T7.4"="#8B008B","T7.5"="#D8BFD8",
  "TF1.1"="#76C2A1","TF1.2"="#8CCCCC","TF1.3"="#A2DED2","TF1.4"="#B8E8E4",
  "TF1.5"="#CEF2F6","TF1.6"="#D4F6EC","TF1.7"="#E0F7F2"
)

flows_long <- flows %>%
  mutate(alluvium_id = row_number(),
         from_norm = norm_code(from),
         to_norm   = norm_code(to)) %>%
  select(alluvium_id, area_km2, from, to, from_norm, to_norm) %>%
  pivot_longer(cols = c(from, to),
               names_to = "year", values_to = "efg_label") %>%
  mutate(
    year = recode(year, from = "1982", to = "2024"),
    efg_norm = norm_code(efg_label)
  )

all_labels <- flows_long |> distinct(efg_label) |> pull(efg_label)
order_alpha_num <- all_labels[gtools::mixedorder(all_labels)]
flows_long <- flows_long %>%
  mutate(
    year = factor(year, levels = c("1982", "2024")),
    efg_label = factor(efg_label, levels = order_alpha_num)
  )

pal <- sld_colors[unique(norm_code(levels(flows_long$efg_label)))]
names(pal) <- unique(norm_code(levels(flows_long$efg_label)))

p <- ggplot(flows_long,
            aes(x = year, stratum = efg_label, alluvium = alluvium_id,
                y = area_km2, fill = efg_norm)) +
  geom_flow(alpha = 0.55, color = 'white', linewidth = 0) + 
  geom_stratum(color = NA, linewidth = 0, width = 0.15) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = NULL, y = "Area (km²)", title = "EFG transitions: 1982 → 2024") +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

ggsave("./Ecosystem/results/EFG/EFG_sankey_vertical_T2.png",
       p, width = 10, height = 4, dpi = 300, bg = "white")
