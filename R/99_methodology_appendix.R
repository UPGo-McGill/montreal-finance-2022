#### ROUGH MATERIAL FOR METHODS APPENDIX #######################################

CT_raw <-
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


CT_raw |> 
  pull(renter) |> 
  sum(na.rm = TRUE)

CT |> 
  pull(renter) |> 
  sum()


prop_fin_raw <-
  LL_sf_centroid |> 
  filter(number_rental_units > 0) |> 
  mutate(fin = publicly_traded + direct_involvement_FM + financial_partners) |>
  mutate(fin = if_else(fin > 0, TRUE, FALSE)) |> 
  st_intersection(CT_raw) |> 
  st_drop_geometry()

prop_fin_raw |> 
  group_by(GeoUID) |> 
  summarize(census_rentals = mean(renter, na.rm = TRUE),
            data_rentals = sum(number_rental_units, na.rm = TRUE)) |> 
  ggplot(aes(census_rentals, data_rentals)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
