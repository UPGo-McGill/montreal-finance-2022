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
  select(GeoUID, p_stress, median_rent, avg_value, p_condo,
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


# Compute k means with 5 clusters -----------------------------------------

k_result <- kmeans(data_kmeans, centers = 5, nstart = 25)
fviz_cluster(k_result, data = data_kmeans)


# Add to data -------------------------------------------------------------

cluster_with_id <- tibble(GeoUID = ID_to_keep, cluster = k_result$cluster)

data_CT <- 
  data_CT |> 
  left_join(cluster_with_id) |> 
  mutate(cluster = case_when(
           cluster == 1 ~ "3. Non-financialized suburbs",
           cluster == 4 ~ "1. Financialized precarious and student",
           cluster == 2 ~ "5. Non-financialized immigrant periphery",
           cluster == 5 ~ "2. Financialized affluent",
           cluster == 3 ~ "4. Non-financialized gentrified"), 
         cluster = factor(cluster, levels = c(
           "1. Financialized precarious and student", 
           "2. Financialized affluent",
           "3. Non-financialized suburbs", 
           "4. Non-financialized gentrified",
           "5. Non-financialized immigrant periphery"))) |> 
  relocate(cluster, .before = geometry)

data_building <- 
  data_building |> 
  left_join(data_CT |> 
              st_drop_geometry() |> 
              select(GeoUID, cluster), by = "GeoUID")


# Save updated data -------------------------------------------------------

qsavem(data_CT, data_building, file = "output/data.qsm",
       nthreads = availableCores())

rm(cluster_with_id, data_kmeans, ID_to_keep)


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

sum_func <- function(data) {
  
  data |> 
    summarize(
      p_fin = weighted.mean(p_fin, n_rentals, na.rm = TRUE),
      p_stress = weighted.mean(p_stress, parent_renter, 
                                      na.rm = TRUE),
      median_rent = weighted.mean(median_rent, parent_renter, na.rm = TRUE),
      p_condo = weighted.mean(p_condo, parent_condo, na.rm = TRUE),
      p_renter = weighted.mean(p_renter, parent_tenure, na.rm = TRUE),
      p_repairs = weighted.mean(p_repairs, parent_repairs, na.rm = TRUE),
      p_vm = weighted.mean(p_vm, parent_vm, na.rm = TRUE),
      p_immigrants = weighted.mean(p_immigrants, parent_immigrants, 
                                   na.rm = TRUE),
      p_mobility_one_year = weighted.mean(p_mobility_one_year, 
                                          parent_mobility_one_year, 
                                          na.rm = TRUE),
      p_mobility_five_years = weighted.mean(
        p_mobility_five_years, parent_mobility_five_years, na.rm = TRUE),
      d_downtown = mean(distance_dt, na.rm = TRUE),
      asking_rent = weighted.mean(asking_rent, parent_renter, na.rm = TRUE),
      avg_value = weighted.mean(avg_value, 
                                              parent_owner, na.rm = TRUE),
      p_five_storeys = weighted.mean(p_five_more_storeys, parent_dwellings, 
                                     na.rm = TRUE),
      med_hh_income = weighted.mean(med_hh_income, parent_hh_income, 
                                    na.rm = TRUE),
      p_18_24 = weighted.mean(p_18_24, parent_age, na.rm = TRUE),
      p_65_plus = weighted.mean(p_65_plus, parent_age, na.rm = TRUE),
      p_built_after_2005 = weighted.mean(p_built_after_2005, n_rentals, 
                                         na.rm = TRUE)) |> 
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

qsavem(cluster_averages, CT_parent_vectors, data_kmeans, k_result, 
       file = "output/cluster.qsm", nthreads = availableCores())

rm(city_wide, sum_func)
