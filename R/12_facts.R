#### 12 FACTS FOR PAPER ########################################################

source("R/01_startup.R")
landlord <- qread("output/landlord.qs", nthreads = availableCores())
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())


# Number of financialized buildings and units -----------------------------

data_building |> 
  st_drop_geometry() |> 
  mutate(financialized = coalesce(financialized, 0)) |> 
  summarize(
    buildings = n(),
    f_buildings = sum(financialized > 0),
    units = sum(number_rental_units, na.rm = TRUE),
    f_units = sum((financialized > 0) * number_rental_units, na.rm = TRUE)) |> 
  mutate(building_pct = f_buildings / buildings,
         unit_pct = f_units / units) |> 
  mutate(across(c(buildings, f_buildings, units, f_units), scales::comma),
         across(c(building_pct, unit_pct), scales::percent, 0.1))



# Spatial distribution ----------------------------------------------------

data_CT |> 
  select(total, n_fin) |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  st_intersection(st_set_agr(boroughs, "constant")) |> 
  st_drop_geometry() |> 
  group_by(borough) |> 
  summarize(
    total = sum(total, na.rm = TRUE),
    fin = sum(n_fin, na.rm = TRUE),
    fin_pct = fin / total) |> 
  arrange(fin_pct) |> 
  mutate(fin_pct = scales::percent(fin_pct, 0.1))

data_CT |> 
  pull(p_fin) |> 
  summary() |> 
  as.numeric() |> 
  scales::percent(0.1)


# Appendix facts ----------------------------------------------------------

data_CT |> 
  st_drop_geometry() |> 
  select(renter, n_rentals) |> 
  na.omit() |> 
  cor()
