#### 13 FIGURES FOR PAPER ######################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
qload("output/cluster.qsm", nthreads = availableCores())
qload("output/models/extra.qsm", nthreads = availableCores())

library(patchwork)
library(ggpubr)
library(ineq)
fig_alpha <- 0.8


# Figure 1. Percentage of financialized ownership -------------------------

gini <- round(ineq(data_CT$n_fin, type = "Gini"), 2)

fig_1_full <- 
  data_CT |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(colour = p_fin, fill = after_scale(alpha(colour, fig_alpha))), 
          lwd = 0.3) +
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3) +
  scale_colour_stepsn(name = "Financialized rental units", 
                      colors = col_palette[c(4, 1, 2, 9)],
                      breaks = c(0.15, 0.30, 0.45, 0.60),
                      na.value = "grey80",
                      limits = c(0, 0.75), oob = scales::squish, 
                      labels = scales::percent) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom",
        legend.text = element_text(size = 7))

fig_1_inset <- 
  fig_1_full +
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
  ggplot(aes(p_fin, colour = after_stat(x), 
             fill = after_scale(alpha(colour, fig_alpha)))) +
  geom_histogram(bins = 30, alpha = fig_alpha) +
  scale_x_continuous(name = NULL, labels = scales::percent) +
  scale_y_continuous(name = NULL) +
  scale_colour_stepsn(name = "Financialized rental units", 
                      colors = col_palette[c(4, 1, 2, 9)],
                      breaks = c(0.15, 0.30, 0.45, 0.60),
                      na.value = "grey80",
                      limits = c(0, 0.75), oob = scales::squish, 
                      labels = scales::percent) +
  annotate("text", x = 0.7, y = 182, label = paste0("Gini coefficient: ", gini), 
           family = "Futura", size = 2.75) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "none")
  

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
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura")) + 
  plot_layout(design = fig_1_layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figures/figure_1.png", plot = fig_1, width = 6.5, height = 3.8, 
       units = "in")


# Figure 2. Bivariate regressions -----------------------------------------

size_range <- c(0.2, 2)

# Housing stress
p1_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_stress, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p1 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_stress, p_fin, 
             size = parent_renter, alpha = parent_renter)) +
  geom_point(color = col_palette[1], stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p1_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Renters in housing stress", 
                     label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# Median rent
p2_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(median_rent, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p2 <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  filter(median_rent > 500, median_rent < 1800) |> 
  #mutate(median_rent = log(median_rent)) |> 
  ggplot(aes(median_rent, p_fin, 
             size = parent_renter, alpha = parent_renter)) +
  geom_point(color = col_palette[2], stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p2_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 500, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Median rent", label = scales::dollar,
                     limits = c(500, NA)) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# One-year mobility
p3_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_mobility_one_year, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p3 <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_mobility_one_year, p_fin, size = parent_renter, 
             alpha = parent_renter)) +
  geom_point(color = col_palette[3], stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p3_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Households having moved in the past year",
                     label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# Visible minorities
p4_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_vm, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p4 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_vm, p_fin, alpha = dwellings, size = dwellings)) +
  geom_point(color = col_palette[5], stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p4_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Visible minorities", label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))


# Five+ stories
p5_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_five_more_storeys, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p5 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_five_more_storeys, 
             p_fin, alpha = parent_renter, size = parent_renter)) +
  geom_point(color = col_palette[7], stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p5_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Households in 5+ storey buildings", 
                     label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# 18-24 year olds
p6_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_18_24, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p6 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_18_24,
             p_fin, alpha = parent_renter, size = parent_renter)) +
  geom_point(color = col_palette[9], stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p6_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Population aged 18-24", label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

# Percentage built after 2005
p7_cor <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(p_built_after_2005, p_fin) |> 
  na.omit() |> 
  cor() |> 
  as_tibble() |> 
  slice(1) |> 
  pull(p_fin)

p7 <-
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  ggplot(aes(p_built_after_2005,
             p_fin, alpha = parent_renter, size = parent_renter)) +
  geom_point(color = "#3a8c00", stroke = 0) +
  geom_line(stat = "smooth", method = "lm", color = "black", alpha = p7_cor) +
  stat_cor(aes(label = ..r.label..), label.x = 0, label.y = 0.875,
           family = "Futura", size = 2.5) +
  scale_x_continuous(name = "Units built after 2005", label = scales::percent) +
  scale_y_continuous("Financialized rental units", label = scales::percent, 
                     limits = c(0, 1)) +
  scale_size_continuous(range = size_range, guide = "none") +
  scale_alpha_continuous(guide = "none") +
  theme_minimal() + 
  theme(text = element_text(family = "Futura", size = 6.5))

fig_2 <- p1 + p2 + p3 + p4 + p5 + p6 + p7

ggsave("output/figures/figure_2.png", plot = fig_2, width = 6.5, 
       height = 7.5, units = "in")


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
  geom_point(position = pos, size = 1, alpha = 10) +
  scale_colour_manual(breaks = c("linear", "binomial", "bym"),
                      labels = c("linear", "binomial", "binomial-bym2"), 
                      values = col_palette[c(3, 2, 1)], name = "Model") + 
  xlab("Estimate") + 
  scale_y_discrete(name = NULL, limits = rev) + 
  theme_minimal() +
  theme(legend.position = "right", text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figures/figure_3.png", plot = fig_3, width = 6.5, height = 4, 
       units = "in")


# Figure 4. Cluster map ---------------------------------------------------

fig_4_poly <-
  data_CT |> 
  filter(!is.na(cluster)) |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(colour = cluster, fill = after_scale(alpha(colour, fig_alpha))), 
          lwd = 0.3) +
  scale_colour_manual(name = NULL, values = col_palette[c(4, 3, 1, 2, 5)]) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  upgo::gg_bbox(data_CT) +
  theme_void() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"))

fig_4_points <- 
  data_building |> 
  filter(!is.na(cluster)) |> 
  group_by(cluster) |> 
  summarize() |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(data = uef, fill = "grey85", colour = "transparent") +
  geom_sf(aes(colour = cluster, fill = after_scale(alpha(colour, fig_alpha))), 
          lwd = 0.05) +
  scale_colour_manual(name = NULL, values = col_palette[c(4, 3, 1, 2, 5)],
                      guide = NULL) +
  upgo::gg_bbox(data_CT) +
  theme_void() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"))

fig_4_layout <- "
AABB
AABB
AABB
AABB
AABB
AABB
AABB
AABB
CCCC
"

fig_4 <- fig_4_poly + fig_4_points + guide_area() + 
  plot_layout(design = fig_4_layout, guides = "collect") +
  plot_annotation(tag_levels = "A") +
  theme(legend.position = "bottom", text = element_text(family = "Futura"))

ggsave("output/figures/figure_4.png", plot = fig_4, width = 8, height = 5, 
       units = "in")


# Table 1. Cluster analysis table -----------------------------------------

cluster_averages |> 
  select(cluster, p_fin, median_rent, asking_rent, p_stress,
         med_hh_income, avg_value, p_renter, p_condo, 
         p_built_after_2005, p_five_storeys, p_mobility_one_year, 
         p_mobility_five_years, p_vm, p_immigrants, d_downtown, p_18_24, 
         p_65_plus) |> 
  mutate(across(starts_with("p_"), scales::percent, 0.1),
         across(c(median_rent, asking_rent), scales::dollar, 10),
         across(c(med_hh_income, avg_value), scales::dollar, 1000),
         d_downtown = scales::comma(as.numeric(d_downtown), 0.1, 
                                    scale = 1 / 1000, suffix = " km")) |> 
  set_names(c("Cluster", "Financialized rental units", "Median rent",
              "Average asking rent", "Renters in housing stress", 
              "After-tax median HH income", "Average dwelling value",
              "Renter households", "Condo households", 
              "Rental units built after 2005", 
              "Households in 5+ storey buildings",
              "Households having moved in the past year",
              "Households having moved in the past 5 years",
              "Visible minorities", "Immigrants", "Distance from downtown (km)",
              "Population aged 18-24", "Population aged 65+")) |> 
  t() |> 
  as_tibble(rownames = "Variable") |> 
  (\(x) set_names(x, x[1,]))() |> 
  slice(-1) |> 
  rename(Variable = Cluster) |> 
  gt::gt()
