#### 13 FIGURES FOR PAPER ######################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
qload("output/cluster.qsm", nthreads = availableCores())
qload("output/models/extra.qsm", nthreads = availableCores())

library(patchwork)
library(ggpubr)
fig_alpha <- 0.8


# Figure 1. Percentage of financialized ownership -------------------------

fig_1_full <- 
  data_CT |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = p_financialized, colour = p_financialized), lwd = 0.3) +
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3) +
  scale_fill_stepsn(name= "Financialized rental units", 
                    colors = alpha(col_palette[c(4, 1, 2, 9)], fig_alpha),
                    breaks = c(0.15, 0.30, 0.45, 0.60),
                    na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    labels = scales::percent) +
  scale_colour_stepsn(name= "Financialized rental units", 
                      colors = col_palette[c(4, 1, 2, 9)],
                      breaks = c(0.15, 0.30, 0.45, 0.60),
                      na.value = "grey80",
                      limits = c(0, 0.75), oob = scales::squish, 
                      labels = scales::percent,
                      guide = NULL) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom",
        legend.text = element_text(size = 7))

fig_1_inset <- 
  fig_1_full +
  # geom_sf(data = streets_downtown, size = 0.4, colour = "grey93") +
  coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

fig_1_map <- 
  fig_1_full + 
  inset_element(fig_1_inset, left = 0.02, bottom = 0.47, right = 0.53, 
                top = 0.97, ignore_tag = TRUE)

fig_1_hist <-
  data_CT |> 
  mutate(fill = case_when(p_financialized > 0.6 ~ "4",
                          p_financialized > 0.45 ~ "3",
                          p_financialized > 0.3 ~ "2",
                          p_financialized > 0.15 ~ "1",
                          TRUE ~ "0")) |> 
  ggplot(aes(p_financialized, fill = fill, color=fill)) +
  geom_histogram(bins = 30, alpha=fig_alpha) +
  scale_x_continuous(name = NULL, #"Financialized rental units",
                     labels = scales::percent) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = c("#F3B45F", "#EE7A35", "#DA6D61", "#BC6591", 
                               "#A53B6A"), guide = NULL) +
  scale_color_manual(values = c("#F3B45F", "#EE7A35", "#DA6D61", "#BC6591", 
                               "#A53B6A"), guide = NULL) +
  geom_hline(yintercept = 0, color = "grey86") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))
  

fig_1_layout <- "
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
CCCCC
"

fig_1 <- fig_1_map + fig_1_hist + guide_area() + 
  theme(legend.position = "bottom", text = element_text(family = "Futura")) + 
  plot_layout(design = fig_1_layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figures/figure_1.png", plot = fig_1, width = 6.5, height = 3.8, 
       units = "in")


# Figure 2. Bivariate regressions -----------------------------------------

# Housing stress
p1_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_thirty_renter, p_financialized) |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_financialized)

p1 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_thirty_renter, p_financialized, 
             size = parent_renter, alpha = parent_renter)) +
  geom_point(color = col_palette[1]) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p1_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Renters in housing stress", 
                     label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(0.3, 1.5), guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# Median rent
p2_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(median_rent, p_financialized) |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_financialized)

p2 <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  filter(median_rent > 500, median_rent < 1800) |> 
  ggplot(aes(median_rent, p_financialized, 
             size = parent_renter, alpha = parent_renter)) +
  geom_point(color = col_palette[2]) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p2_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 500, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Median rent", label = scales::dollar,
                     limits = c(500, NA)) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(0.3, 1.5), guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# One-year mobility
p3_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_mobility_one_year, p_financialized) |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_financialized)

p3 <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_mobility_one_year, p_financialized, size = parent_renter, 
             alpha = parent_renter)) +
  geom_point(color = col_palette[3]) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p3_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Households having moved in the past year",
                     label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(0.3, 1.5), guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# Visible minorities
p4_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_vm, p_financialized) |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_financialized)

p4 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_vm, p_financialized, alpha = dwellings, size = dwellings)) +
  geom_point(color = col_palette[5]) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p4_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Visible minorities", label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(0.3, 1.5), guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))


# Five+ stories
p5_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_five_more_storeys, p_financialized) |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_financialized)

p5 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_five_more_storeys, 
             p_financialized, alpha = parent_renter, size = parent_renter)) +
  geom_point(color = col_palette[7]) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p5_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Households in 5+ storey buildings", 
                     label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(0.3, 1.5), guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# 18-24 year olds
p6_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_18_24, p_financialized) |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_financialized)

p6 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_18_24,
             p_financialized, alpha = parent_renter, size = parent_renter)) +
  geom_point(color = col_palette[9]) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p6_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Population aged 18-24", label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(0.3, 1.5), guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

fig_2 <- p1 + p2 + p3 + p4 + p5 + p6

ggsave("output/figures/figure_2.png", plot = fig_2, width = 6.5, 
       height = 4.5, units = "in")


# Figure 3. Credible intervals --------------------------------------------

pos <- position_nudge(y = case_when(
  combined$model == "bym" ~ 0, 
  combined$model == "binomial" ~ 0.1,
  combined$model == "linear" ~ 0.2))

fig_3 <-
  combined |> 
  ggplot(aes(x = m, y = parameter, colour = model)) + 
  geom_vline(xintercept = 0.0, colour = "red", alpha = 1, size = 0.3, lty = 2) + 
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size = 2) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos) +
  geom_point(position = pos, size = 1, alpha = 10) +
  scale_colour_manual(breaks = c("linear", "binomial", "bym"),
                      labels = c("linear", "binomial", "binomial-bym2"), 
                      values = col_palette[c(3, 2, 1)], name = "Model") + 
  xlab("Estimate") + 
  scale_y_discrete(name = NULL, limits = rev) + 
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white"))

ggsave("output/figures/figure_3.png", plot = fig_3, width = 6.5, height = 4, 
       units = "in")


# Figure 4. Cluster map ---------------------------------------------------

fig_4_poly <-
  data_CT |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey', alpha = 1) +
  geom_sf(aes(fill = cluster), colour = "transparent") +
  scale_fill_manual(name = NULL, values = alpha(col_palette[c(1, 3, 4, 2, 5)]), 
                    fig_alpha) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  upgo::gg_bbox(data_CT) +
  theme_void() +
  theme(legend.position = "bottom")

fig_4_points <- 
  data_building |> 
  left_join(
    data_CT_clusters |> 
      st_drop_geometry() |> 
      select(GeoUID, cluster), by = "GeoUID") |> 
  filter(!is.na(cluster)) |> 
  group_by(cluster) |> 
  summarize() |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(data = uef, fill = "grey85", colour = "transparent") +
  geom_sf(aes(fill = cluster, colour = cluster), lwd = 0.05) +
  scale_fill_manual(name = NULL, 
                    values = alpha(col_palette[c(1, 3, 4, 2, 5)], fig_alpha),
                    guide = NULL) +
  scale_colour_manual(name = NULL, 
                      values = alpha(col_palette[c(1, 3, 4, 2, 5)], fig_alpha),
                      guide = NULL) +
  # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  upgo::gg_bbox(data_CT) +
  theme_void() +
  theme(legend.position = "bottom")

fig_4 <- fig_4_poly + fig_4_points + guide_area() + 
  plot_layout(design = fig_1_layout, guides = "collect") +
  theme(legend.position = "bottom") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figure_4.pdf", plot = fig_4, width = 8, height = 5, units = "in", 
       useDingbats = FALSE)



# Figure 5. Cluster analysis table ----------------------------------------

# First part
cluster_averages |> 
  select(cluster, p_financialized, median_rent, asking_rent, p_thirty_renter,
         med_hh_income, average_value_dwellings, p_renter, p_condo, p_built_after_2005) |> 
  mutate(across(starts_with("p_"), scales::percent, 0.1),
         across(c(median_rent, asking_rent, med_hh_income, 
                  average_value_dwellings), scales::dollar, 1)) |> 
  set_names(c("Cluster", "Financialized rental units", "Median rent",
              "Average asking rent", "Renters in housing stress", 
              "After-tax median HH income", "Average dwelling value",
              "Renter households", "Condo households", "Rental units built after 2005")) |> 
  gt::gt()

# Second part
  cluster_averages |> 
    select(cluster, p_five_storeys, p_mobility_one_year, p_mobility_five_years,
           p_vm, p_immigrants, d_downtown, p_18_24, p_65_plus) |> 
  mutate(across(starts_with("p_"), scales::percent, 0.1),
         d_downtown = round(as.numeric(d_downtown/1000), 1)) |> 
  set_names(c("Cluster", "Households in 5+ storey buildings",
              "Households having moved in the past year",
              "Households having moved in the past 5 years",
              "Visible minorities", "Immigrants", "Distance from downtown (km)",
              "Population aged 18-24", "Population aged 65+")) |> 
  gt::gt()

