#### 12 FIGURES FOR PAPER ######################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
k_result <- qread("output/k_result.qs")
library(patchwork)


# Figure 1. Percentage of financialized ownership -------------------------

fig_1_full <- 
  data_CT |> 
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

fig_1_inset <- 
  fig_1_full +
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

fig_1_map <- 
  fig_1_full + 
  inset_element(fig_1_inset, left = 0, bottom = 0.45, right = 0.55, top = 0.92,
                ignore_tag = TRUE)

fig_1_hist <-
  data_CT |> 
  mutate(fill = case_when(
    p_financialized > 0.6 ~ "4",
    p_financialized > 0.45 ~ "3",
    p_financialized > 0.3 ~ "2",
    p_financialized > 0.15 ~ "1",
    TRUE ~ "0"
  )) |> 
  ggplot(aes(p_financialized, fill = fill)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(name = NULL, #"Financialized rental units",
                     labels = scales::percent) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = c("#F3B45F", "#EE7A35", "#DA6D61", "#BC6591", 
                               "#A53B6A"), guide = NULL) +
  theme_minimal()

fig_1_layout <- "
AABB
AABB
AABB
AABB
AABB
CCCC"

fig_1 <- fig_1_map + fig_1_hist + guide_area() + 
  theme(legend.position = "bottom") + 
  plot_layout(design = fig_1_layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figure_1.pdf", plot = fig_1, width = 8, height = 5, units = "in", 
       useDingbats = FALSE)


# Figure 2. Cluster map ---------------------------------------------------

fig_2_poly <-
  data_CT |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = cluster), colour = "transparent") +
  scale_fill_manual(name = NULL, values = col_palette[c(1, 3, 4, 2, 5)]) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  upgo::gg_bbox(data_CT) +
  theme_void() +
  theme(legend.position = "bottom")

fig_2_points <- 
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
  scale_fill_manual(name = NULL, values = col_palette[c(1, 3, 4, 2, 5)],
                    guide = NULL) +
  scale_colour_manual(name = NULL, values = col_palette[c(1, 3, 4, 2, 5)],
                      guide = NULL) +
  # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  upgo::gg_bbox(data_CT) +
  theme_void() +
  theme(legend.position = "bottom")

fig_2 <- fig_2_poly + fig_2_points + guide_area() + 
  plot_layout(design = fig_1_layout, guides = "collect") +
  theme(legend.position = "bottom") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figure_2.pdf", plot = fig_2, width = 8, height = 5, units = "in", 
       useDingbats = FALSE)



# Figure 5. Cluster analysis table ----------------------------------------

# Compute weighted means instead of straight means -------------------

CT_parent_vectors <- 
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "CT",
    vectors = c("v_CA16_4897", "v_CA16_4840", "v_CA16_4836", "v_CA16_4870", 
                "v_CA16_3954", "v_CA16_3405", "v_CA16_6692", "v_CA16_6719",
                "v_CA16_4890", "v_CA16_408", "v_CA16_2396", "v_CA16_1"),
    geo_format = "sf") |> 
  st_transform(32618) |> 
  select(-c(Type, Households, `Adjusted Population (previous Census)`:CSD_UID, 
            PR_UID:`Area (sq km)`)) |> 
  set_names(c("GeoUID", "dwellings", "parent_renter", "parent_repairs", 
              "parent_owner", "parent_condo", "parent_tenure", "parent_vm", 
              "parent_immigrants", "parent_mobility_one_year", 
              "parent_mobility_five_years", "parent_dwellings", 
              "parent_hh_income", "parent_age", "geometry")) |> 
  select(-dwellings) |> 
  as_tibble() |> 
  st_as_sf(agr = "constant")

data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  na.omit() |> 
  mutate(cluster = k_result$cluster) |> 
  mutate(cluster = case_when(
    cluster == 1 ~ "Suburban non-financialized",
    cluster == 2 ~ "Immigrant periphery non-financialized",
    cluster == 3 ~ "Precarious and student financialized",
    cluster == 4 ~ "Gentrifying non-financialized",
    cluster == 5 ~ "Affluent financialized")) |> 
  mutate(cluster = factor(cluster, levels = c(
    "Precarious and student financialized", "Affluent financialized",
    "Suburban non-financialized", "Gentrifying non-financialized",
    "Immigrant periphery non-financialized"))) |> 
  group_by(cluster) |> 
  summarize(p_financialized = weighted.mean(p_financialized, parent_renter, 
                                            na.rm = TRUE),
            p_thirty_renter = weighted.mean(p_thirty_renter, parent_renter, 
                                            na.rm = TRUE),
            median_rent = weighted.mean(median_rent, parent_renter, 
                                        na.rm = TRUE),
            p_condo = weighted.mean(p_condo, parent_condo, na.rm = TRUE),
            p_renter = weighted.mean(p_renter, parent_tenure, na.rm = TRUE),
            p_repairs = weighted.mean(p_repairs, parent_repairs, na.rm = TRUE),
            p_vm = weighted.mean(p_vm, parent_vm, na.rm = TRUE),
            p_immigrants = weighted.mean(p_immigrants, parent_immigrants, 
                                         na.rm = TRUE),
            p_mobility_one_year = weighted.mean(
              p_mobility_one_year, parent_mobility_one_year, na.rm = TRUE),
            p_mobility_five_years = weighted.mean(
              p_mobility_five_years, parent_mobility_five_years, na.rm = TRUE),
            d_downtown = mean(distance_dt, na.rm = TRUE),
            asking_rent = weighted.mean(asking_rent, parent_renter, 
                                        na.rm = TRUE),
            change_renter_dwellings = weighted.mean(
              change_renter_dwellings, parent_renter, na.rm = TRUE),
            average_value_dwellings = weighted.mean(
              average_value_dwellings, parent_owner, na.rm = TRUE),
            p_five_storeys = weighted.mean(p_five_more_storeys, 
                                           parent_dwellings, na.rm = TRUE),
            med_hh_income = weighted.mean(med_hh_income, parent_hh_income, 
                                          na.rm = TRUE),
            p_18_24 = weighted.mean(p_18_24, parent_age, na.rm = TRUE)) |> 
  st_drop_geometry() |> 
  mutate(across(starts_with("p_"), scales::percent, 0.1),
         median_rent = scales::dollar(median_rent, 1))

kmeans_CT %>% 
  left_join(., st_drop_geometry(CT_parent_vectors), by = "GeoUID") %>% 
  na.omit() %>% 
  mutate(group = 1) %>% 
  group_by(group) %>% 
  summarize(p_financialized = weighted.mean(p_financialized, parent_renter, na.rm=TRUE),
            p_thirty_renter = weighted.mean(p_thirty_renter, parent_renter, na.rm=TRUE),
            median_rent = weighted.mean(median_rent, parent_renter, na.rm=TRUE),
            p_condo = weighted.mean(p_condo, parent_condo, na.rm=TRUE),
            p_renter = weighted.mean(p_renter, parent_tenure, na.rm=TRUE),
            p_repairs = weighted.mean(p_repairs, parent_repairs, na.rm=TRUE),
            p_vm = weighted.mean(p_vm, parent_vm, na.rm=TRUE),
            p_immigrants = weighted.mean(p_immigrants, parent_immigrants, na.rm=TRUE),
            p_mobility_one_year = weighted.mean(p_mobility_one_year, parent_mobility_one_year, na.rm=TRUE),
            p_mobility_five_years = weighted.mean(p_mobility_five_years, parent_mobility_five_years, na.rm=TRUE),
            d_downtown = mean(distance_dt, na.rm=TRUE),
            asking_rent = weighted.mean(asking_rent, parent_renter, na.rm=TRUE),
            change_renter_dwellings = weighted.mean(change_renter_dwellings, parent_renter, na.rm=TRUE),
            average_value_dwellings = weighted.mean(average_value_dwellings, parent_owner, na.rm = TRUE),
            p_five_storeys = weighted.mean(p_five_more_storeys, parent_dwellings, na.rm = TRUE),
            med_hh_income = weighted.mean(med_hh_income, parent_hh_income, na.rm = TRUE),
            p_18_24 = weighted.mean(p_18_24, parent_age, na.rm = TRUE)) %>% 
  View()

write_csv(CT, "data/CT.csv")


