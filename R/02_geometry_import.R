#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(cancensus)

# This script requires `montreal_boroughs_2019.shp` to be present in
# `data/shapefiles`.


# Quebec province ---------------------------------------------------------

province <- 
  get_census("CA16", regions = list(PR = "24"), geo_format = "sf") |> 
  st_transform(32618) |> 
  select(geometry)


# Montreal DAs ------------------------------------------------------------

DA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
                "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872",
                "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
                "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
                "v_CA16_6719", "v_CA16_4896"),
    geo_format = "sf") |> 
  st_transform(32618) |> 
  select(-c(`Shape Area`:Households, CSD_UID:`Area (sq km)`)) |> 
  set_names(c("dwellings", "GeoUID", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", 
              "major_repairs", "median_rent", "average_value_dwellings", "vm", 
              "parent_vm", "immigrants", "parent_immigrants", 
              "mobility_one_year", "parent_mobility_one_year", 
              "mobility_five_years", "parent_mobility_five_years", 
              "geometry")) |> 
  mutate(p_condo = condo / parent_condo,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_vm = vm/parent_vm,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_thirty_renter = p_thirty_renter / 100) |> 
  select(GeoUID, dwellings, renter, p_thirty_renter, median_rent, 
         average_value_dwellings, p_condo, p_renter, p_repairs, p_vm, 
         p_immigrants, p_mobility_one_year, p_mobility_five_years) |> 
  as_tibble() |> 
  st_as_sf(agr = "constant")


# Montreal CTs ------------------------------------------------------------

CT <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "CT",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838",
                "v_CA16_4897", "v_CA16_4899", "v_CA16_4870", "v_CA16_4872",
                "v_CA16_4900", "v_CA16_3957", "v_CA16_3954", "v_CA16_3411", 
                "v_CA16_3405", "v_CA16_6698", "v_CA16_6692", "v_CA16_6725", 
                "v_CA16_6719", "v_CA16_4896", "v_CA16_410", "v_CA16_408", 
                "v_CA16_2397", "v_CA16_76", "v_CA16_79", "v_CA16_82", 
                "v_CA16_1"),
    geo_format = "sf") |> 
  st_transform(32618) |> 
  select(-c(Type, Households, `Adjusted Population (previous Census)`:CSD_UID, 
            PR_UID:`Area (sq km)`)) |> 
  set_names(c("GeoUID", "dwellings", "parent_condo", "condo", "parent_tenure", 
              "renter", "parent_thirty", "p_thirty_renter", "parent_repairs", 
              "major_repairs", "median_rent", "average_value_dwellings", "vm",
              "parent_vm", "immigrants", "parent_immigrants", 
              "mobility_one_year", "parent_mobility_one_year", 
              "mobility_five_years", "parent_mobility_five_years", 
              "five_storeys", "parent_storeys", "med_hh_income", "age_18", 
              "age_19", "age_20_24", "age_total", "geometry")) |> 
  mutate(p_condo = condo / parent_condo,
         p_renter = renter / parent_tenure, 
         p_repairs = major_repairs / parent_repairs,
         p_vm = vm/parent_vm,
         p_immigrants = immigrants/parent_immigrants,
         p_mobility_one_year = mobility_one_year/parent_mobility_one_year,
         p_mobility_five_years = mobility_five_years/parent_mobility_five_years,
         p_five_more_storeys = five_storeys/parent_storeys,
         p_18_24 = (age_18+age_19+age_20_24)/age_total,
         p_thirty_renter = p_thirty_renter / 100) |> 
  select(GeoUID, dwellings, renter, p_thirty_renter, median_rent, 
         average_value_dwellings, p_condo, p_renter, p_repairs, p_vm, 
         p_immigrants, p_mobility_one_year, p_mobility_five_years, 
         p_five_more_storeys, med_hh_income, p_18_24) |> 
  as_tibble() |> 
  st_as_sf(agr = "constant") |> 
  mutate(renter = if_else(is.na(renter) & dwellings == 0, 0, renter))

CT_06 <- 
  cancensus::get_census(
    dataset = "CA06", regions = list(CSD = c("2466023")), level = "CT",
    vectors = c("v_CA06_101", "v_CA06_103"),
    geo_format = "sf") |> 
  st_transform(32618) |> 
  select(-c(`Shape Area`, Type, Households, `Quality Flags`, CMA_UID:CSD_UID, 
            PR_UID:`Area (sq km)`)) |> 
  set_names(c("GeoUID", "dwellings", "parent_tenure", "renter", "geometry")) |> 
  mutate(p_renter = renter / parent_tenure) |> 
  select(GeoUID, dwellings, renter, p_renter) |> 
  as_tibble() |> 
  st_as_sf(agr = "constant") |> 
  mutate(renter = if_else(is.na(renter) & dwellings == 0, 0, renter))

CT <- 
  CT_06 |> 
  select(renter) |>  
  rename(renter_06 = renter) |> 
  st_interpolate_aw(CT, extensive = TRUE) |> 
  st_drop_geometry() |> 
  select(renter_06) |> 
  cbind(CT) |> 
  as_tibble() |> 
  st_as_sf() |> 
  mutate(change_renter_dwellings = renter - renter_06, .before = geometry) |> 
  select(-renter, -renter_06) 

# The change in renter dwellings gave non-justified NAs; this code fixes that.
CT <-
  CT |> 
  mutate(renter_16 = p_renter * dwellings) |> 
  right_join(CT_06 |> select(GeoUID, renter) |> st_drop_geometry(), 
             by = "GeoUID") |> 
  mutate(change_renter_dwellings = if_else(
    is.na(change_renter_dwellings) & !is.na(renter_16), renter_16 - renter, 
    change_renter_dwellings)) |> 
  select(-renter, -renter_16)


# Add distance to downtown as a variable -----------------------------------

downtown <-
  c(-73.571599, 45.504156) |> 
  st_point() |> 
  st_sfc(crs = 4326) |> 
  st_transform(32618)

CT <- 
  CT |> 
  mutate(distance_dt = st_distance(st_centroid(geometry), downtown)) 


# Montreal boroughs -------------------------------------------------------

boroughs_raw <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") |> 
  filter(TYPE == "Arrondissement") |> 
  select(borough = NOM) |> 
  st_set_agr("constant") |> 
  st_transform(32618)

boroughs <- 
  boroughs_raw |> 
  st_intersection(province)

boroughs <- 
  DA |> 
  select(dwellings) |> 
  st_interpolate_aw(boroughs, extensive = TRUE) |>  
  st_drop_geometry() |> 
  select(dwellings) |> 
  cbind(boroughs) |>  
  as_tibble() |> 
  st_as_sf() |> 
  arrange(borough) |> 
  relocate(borough)


# Save output and clean up ------------------------------------------------

qsavem(boroughs, CT, CT_06, province, file = "output/geometry.qsm",
       nthreads = availableCores())
rm(boroughs_raw, DA, downtown)
