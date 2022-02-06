#### 12 FIGURES FOR PAPER ######################################################

source("R/01_startup.R")
qload("output/LL.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
library(patchwork)


# Figure 1. Percentage of financialized ownership -------------------------

prop_fin <-
  LL_sf_centroid |> 
  filter(number_rental_units > 0) |> 
  mutate(fin = publicly_traded + direct_involvement_FM + financial_partners) |>
  mutate(fin = if_else(fin > 0, TRUE, FALSE)) |> 
  st_intersection(CT) |> 
  st_drop_geometry() |> 
  group_by(GeoUID, fin) |> 
  summarize(rental_units = sum(number_rental_units)) |> 
  summarize(p_financialized = rental_units[fin] / sum(rental_units),
            total = sum(rental_units), .groups = "drop")

fin_ownership <- 
  CT |> 
  left_join(prop_fin, by = "GeoUID") |> 
  mutate(p_financialized = coalesce(p_financialized, 0),
         total = coalesce(total, 0)) |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = p_financialized), color = "transparent") +
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3) +
  scale_fill_stepsn(name= "Financialized rental units", 
                    colors = col_palette[c(4, 1, 2, 9)],
                    breaks = c(0.15, 0.30, 0.45, 0.60),
                    #values = c(0.2, 0.4, 0.6),
                    na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    labels = scales::percent) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7))

fo_zoom <- 
  fin_ownership +
  scale_fill_stepsn(name= "Financialized rental units", 
                    colors = col_palette[c(4, 1, 2, 9)],
                    breaks = c(0.15, 0.30, 0.45, 0.60),
                    #values = c(0.2, 0.4, 0.6),
                    na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    labels = scales::percent, guide = NULL) +
  geom_sf(data = streets_downtown, size = 0.3, colour = "white") +
  coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

inset_fin_ownership <- 
  fin_ownership + 
  inset_element(fo_zoom, left = 0, bottom = 0.45, right = 0.55, top = 0.92,
                ignore_tag = TRUE)

hist <- 
  prop_fin |> 
  mutate(fill = case_when(
    p_financialized > 0.6 ~ "4",
    p_financialized > 0.45 ~ "3",
    p_financialized > 0.3 ~ "2",
    p_financialized > 0.15 ~ "1",
    TRUE ~ "0"
  )) |> 
  ggplot(aes(p_financialized, fill = fill)) +
  geom_histogram(bins = 28) +
  scale_x_continuous(name = NULL, #"Financialized rental units",
                     labels = scales::percent) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = c("#F3B45F", "#EE7A35", "#DA6D61", "#BC6591", 
                               "#A53B6A"), guide = NULL) +
  theme_minimal()

layout <- "
AABB
AABB
AABB
AABB
AABB
CCCC"

fig_1 <- inset_fin_ownership + hist + guide_area() + 
  theme(legend.position = "bottom") + 
  plot_layout(design = layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figure_1.pdf", plot = fig_1, width = 8, height = 5, units = "in", 
       useDingbats = FALSE)



