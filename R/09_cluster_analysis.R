#### 09 CLUSTER ANALYSIS #######################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())

library(cluster)
library(factoextra)

set.seed(2022)


# Prepare dataset_kmeans --------------------------------------------------

data_kmeans <- 
  data_CT |> 
  st_drop_geometry() |> 
  select(GeoUID, p_thirty_renter, median_rent, average_value_dwellings, p_condo,
         p_renter, p_repairs, p_vm, p_immigrants, p_mobility_one_year,
         p_mobility_five_years, p_five_more_storeys, med_hh_income, p_18_24, 
         p_65_plus, distance_dt, p_built_after_2005, asking_rent) |> 
  na.omit()

ID_to_keep <- data_kmeans$GeoUID

data_kmeans <- 
  data_kmeans |> 
  select(-GeoUID) |> 
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
  filter(GeoUID %in% ID_to_keep) |> 
  mutate(cluster = k_result$cluster,
         cluster = case_when(
           cluster == 1 ~ "Suburban non-financialized",
           cluster == 4 ~ "Precarious and student financialized",
           cluster == 2 ~ "Immigrant periphery non-financialized",
           cluster == 5 ~ "Affluent financialized",
           cluster == 3 ~ "Gentrifying non-financialized"), 
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

rm(ID_to_keep)


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

rm(city_wide, sum_func)
