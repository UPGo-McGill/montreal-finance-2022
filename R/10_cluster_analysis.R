#### 10 CLUSTER ANALYSIS #######################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())

library(cluster)
library(factoextra)
# library(gridExtra)
# library(ggpubr)

set.seed(2022)


# Prepare dataset_kmeans --------------------------------------------------

data_kmeans <- 
  data_CT |> 
  st_drop_geometry() |> 
  select(p_thirty_renter, median_rent, average_value_dwellings, p_condo,
         p_renter, p_repairs, p_vm, p_immigrants, p_mobility_one_year,
         p_mobility_five_years, p_five_more_storeys, med_hh_income, p_18_24, 
         p_65_plus, distance_dt, p_built_after_2005, asking_rent) |> 
  na.omit() |> 
  scale() |> 
  as_tibble()


# Determine number of clusters --------------------------------------------

fviz_nbclust(data_kmeans, kmeans, method = "silhouette")
data_kmeans |>
  clusGap(FUN = kmeans, nstart = 25, K.max = 10, B = 50) |> 
  fviz_gap_stat()


# Compute k means with 5 clusters -----------------------------------------

k_result <- kmeans(data_kmeans, centers = 5, nstart = 25)
fviz_cluster(k_result, data = data_kmeans)


# Add to data -------------------------------------------------------------

data_CT <- 
  data_CT |> 
  na.omit() |> 
  mutate(cluster = k_result$cluster,
         cluster = case_when(
           cluster == 1 ~ "Suburban non-financialized",
           cluster == 2 ~ "Precarious and student financialized",
           cluster == 3 ~ "Immigrant periphery non-financialized",
           cluster == 4 ~ "Affluent financialized",
           cluster == 5 ~ "Gentrifying non-financialized"), 
         cluster = factor(cluster, levels = c(
           "Precarious and student financialized", "Affluent financialized",
           "Suburban non-financialized", "Gentrifying non-financialized",
           "Immigrant periphery non-financialized")), .before = geometry)

data_building <- 
  data_building |> 
  left_join(data_CT |> 
              st_drop_geometry() |> 
              select(GeoUID, cluster), by = "GeoUID")


# Save updated data -------------------------------------------------------

qsavem(data_CT, data_building, data_kmeans, file = "output/data.qsm",
       nthreads = availableCores())


# Get cluster averages ----------------------------------------------------

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

CT_parent_vectors <- 
  data_building |> 
  st_drop_geometry() |>
  group_by(GeoUID) |>
  summarise(parent_cons_year = sum(number_rental_units)) |>
  left_join(CT_parent_vectors, ., by = "GeoUID") |>
  relocate(parent_cons_year, .after = parent_age) |>
  st_as_sf()

sum_func <- function(data) {
  
  summarize(
    data,
    p_financialized = weighted.mean(p_financialized, parent_renter, 
                                    na.rm = TRUE),
    p_thirty_renter = weighted.mean(p_thirty_renter, parent_renter, 
                                    na.rm = TRUE),
    median_rent = weighted.mean(median_rent, parent_renter, na.rm = TRUE),
    p_condo = weighted.mean(p_condo, parent_condo, na.rm = TRUE),
    p_renter = weighted.mean(p_renter, parent_tenure, na.rm = TRUE),
    p_repairs = weighted.mean(p_repairs, parent_repairs, na.rm = TRUE),
    p_vm = weighted.mean(p_vm, parent_vm, na.rm = TRUE),
    p_immigrants = weighted.mean(p_immigrants, parent_immigrants, na.rm = TRUE),
    p_mobility_one_year = weighted.mean(p_mobility_one_year, 
                                        parent_mobility_one_year, na.rm = TRUE),
    p_mobility_five_years = weighted.mean(
      p_mobility_five_years, parent_mobility_five_years, na.rm = TRUE),
    d_downtown = mean(distance_dt, na.rm = TRUE),
    asking_rent = weighted.mean(asking_rent, parent_renter, na.rm = TRUE),
    #change_renter_dwellings = weighted.mean(change_renter_dwellings, 
    #                                        parent_renter, na.rm = TRUE),
    average_value_dwellings = weighted.mean(average_value_dwellings, 
                                            parent_owner, na.rm = TRUE),
    p_five_storeys = weighted.mean(p_five_more_storeys, parent_dwellings, 
                                   na.rm = TRUE),
    med_hh_income = weighted.mean(med_hh_income, parent_hh_income, 
                                  na.rm = TRUE),
    p_18_24 = weighted.mean(p_18_24, parent_age, na.rm = TRUE),
    p_65_plus = weighted.mean(p_65_plus, parent_age, na.rm = TRUE),
    p_built_after_2005 = weighted.mean(p_built_after_2005, parent_cons_year, na.rm = TRUE)) |> 
    st_drop_geometry()
  
}

cluster_averages <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  na.omit() |> 
  group_by(cluster) |> 
  sum_func()

city_wide <- 
  data_CT |> 
  left_join(st_drop_geometry(CT_parent_vectors), by = "GeoUID") |> 
  na.omit() |> 
  group_by(cluster = "Montreal average") |> 
  sum_func()

cluster_averages <- bind_rows(city_wide, cluster_averages)


# Save cluster results ----------------------------------------------------

qsavem(cluster_averages, CT_parent_vectors, k_result, 
       file = "output/cluster.qsm", nthreads = availableCores())




# Multilinear regression analysis ----------------------------------------------

# Make a correlation matrix
library(Hmisc)
library(corrplot)

autocor <- na.omit(kmeans_CT %>% select(-GeoUID, -dwellings, -p_renter, -p_financialized)) %>% 
  set_names(c("P. renters' housing stress", "Median rent", "Avg. value dwellings",
            "P. condos dwellings", "P. major repairs",
            "P. visible minorities", "P. immigrants", "P. one year mobility",
            "P. five years mobility", "P. dwellings in five+ storeys", "Median household income",
            "P. pop 18-24",
            "Change in renter dwellings", "Distance from downtown", "Asking rents"))

res <- cor(autocor)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

reg_kmeans_CT <- na.omit(kmeans_CT %>% select(-GeoUID, -dwellings, -p_renter)) %>% 
  set_names(c("P. renters' housing stress", "Median rent", "Avg. value dwellings",
              "P. condos dwellings", "P. major repairs",
              "P. visible minorities", "P. immigrants", "P. one year mobility",
              "P. five years mobility", "P. dwellings in five+ storeys", 
              "Median household income",
              "P. pop 18-24",
              "Change in renter dwellings", "Distance from downtown", 
              "Asking rents", "P. financialized rental ownership"))

reg1 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_condo + p_immigrants + p_vm +
           p_mobility_one_year + distance_dt + change_renter_dwellings + 
             asking_rent + average_value_dwellings, data=reg_kmeans_CT)
reg_kmeans_CT$reg1pred <- predict(reg1)
reg_kmeans_CT$reg1res <- rstandard(reg1)
summary(reg1)

reg2 <- lm(p_financialized ~ p_condo + p_immigrants + p_mobility_one_year + 
             distance_dt + change_renter_dwellings + asking_rent, 
           data=reg_kmeans_CT)
reg_kmeans_CT$reg2pred <- predict(reg2)
reg_kmeans_CT$reg2res <- rstandard(reg2)
summary(reg2)

reg3 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + 
             p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg3pred <- predict(reg3)
reg_kmeans_CT$reg3res <- rstandard(reg3)
summary(reg3)

reg4 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_immigrants, 
           data=reg_kmeans_CT)
reg_kmeans_CT$reg4pred <- predict(reg4)
reg_kmeans_CT$reg4res <- rstandard(reg4)
summary(reg4)

reg5 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_immigrants +
             p_mobility_one_year + asking_rent, data=reg_kmeans_CT)
reg_kmeans_CT$reg5pred <- predict(reg5)
reg_kmeans_CT$reg5res <- rstandard(reg5)
summary(reg5)

reg6 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + 
             p_immigrants + average_value_dwellings, data=reg_kmeans_CT)
reg_kmeans_CT$reg6pred <- predict(reg6)
reg_kmeans_CT$reg6res <- rstandard(reg6)
summary(reg6)

reg7 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + 
             p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg7pred <- predict(reg7)
reg_kmeans_CT$reg7res <- rstandard(reg7)
summary(reg7)

reg8 <- lm(`Percent. of financialized landlords` ~ 
             `Percent. renters in housing stress` + `Median rent` + 
             `Percent. one year mobility` + 
             `Percent. immigrants` + 
             `Percent. dwellings in five+ storey buildings`, data=reg_kmeans_CT)
reg_kmeans_CT$reg8pred <- predict(reg8)
reg_kmeans_CT$reg8res <- rstandard(reg8)
summary(reg8)

reg9 <- lm(`Percent. of financialized landlords` ~ 
             `Percent. renters in housing stress` + `Median rent` + 
             `Percent. one year mobility` + 
             `Percent. immigrants` + 
             `Percent. dwellings in five+ storey buildings` + 
             `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg9pred <- predict(reg9)
reg_kmeans_CT$reg9res <- rstandard(reg9)
summary(reg9)

reg10 <- lm(`Percent. of financialized landlords` ~ 
              `Percent. renters in housing stress` + `Median rent` + 
              `Percent. one year mobility` + 
             `Percent. immigrants` + 
              `Percent. dwellings in five+ storey buildings` + 
              `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg10pred <- predict(reg10)
reg_kmeans_CT$reg10res <- rstandard(reg10)
summary(reg10)

reg11 <- lm(`Percent. of financialized landlords` ~ 
              `Percent. renters in housing stress` + `Median rent` + 
              `Five years mobility` + 
              `Percent. immigrants` + 
              `Percent. dwellings in five+ storey buildings` + 
              `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg11pred <- predict(reg11)
reg_kmeans_CT$reg11res <- rstandard(reg11)
summary(reg11)

reg12 <- lm(`P. financialized rental ownership` ~ 
              `P. renters' housing stress` + `Median rent` + 
              `P. one year mobility` + 
              `P. visible minorities` + `P. dwellings in five+ storeys` + 
              `P. pop 18-24`, data=reg_kmeans_CT)
reg_kmeans_CT$reg12pred <- predict(reg12)
reg_kmeans_CT$reg12res <- rstandard(reg12)
summary(reg12)


stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, 
          type="html", out="regFZ.html")
stargazer(reg8, reg9, reg10, reg11, reg12, type="html", out="regFZ2.1.html")
stargazer(reg9, type="html", out="regFZ9.html")
stargazer(reg10, type="html", out="regFZ10.html")
stargazer(reg12, type="html", out="regFZ12.html")

# Residuals plot --------------------------------------------------------

residuals <- 
  reg_kmeans_CT %>% 
  cbind(., na.omit(CT) %>% select(GeoUID)) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill=reg12res), color=NA)+
  scale_fill_gradient2(name="Residuals",
                       low = col_palette[3],
                       high = col_palette[1],
                       mid = "white", 
                       midpoint = 0,
                       limits=c(-2.5, 2.5),
                       oob=scales::squish)+
  gg_bbox(boroughs) +
  theme_void()

ggsave("output/figures/residuals.pdf", plot = residuals, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

  