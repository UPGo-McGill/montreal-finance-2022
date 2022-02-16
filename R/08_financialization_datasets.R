#### 08 FINANCIALIZATION DATASETS ##############################################

source("R/01_startup.R")
qload("output/LL.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())


# Join LL data to CTs -----------------------------------------------------

LL_CT <- 
  LL_sf |> 
  filter(number_rental_units > 0) |> 
  st_intersection(CT) |> 
  mutate(across(c(publicly_traded, direct_involvement_FM, financial_partners),
                coalesce, FALSE)) |> 
  mutate(fin = as.logical(publicly_traded + direct_involvement_FM + 
                            financial_partners)) |> 
  st_set_geometry("geometry")

LL_CT_summary <- 
  LL_CT |> 
  st_drop_geometry() |> 
  group_by(GeoUID, fin) |> 
  summarize(rental_units = sum(number_rental_units)) |> 
  summarize(n_financialized = sum(rental_units[fin]),
            total = sum(rental_units),
            p_financialized = n_financialized / total,
            .groups = "drop")


# Assemble final datasets -------------------------------------------------

data_CT <- 
  CT |> 
  left_join(LL_CT_summary, by = "GeoUID") |> 
  mutate(p_financialized = coalesce(p_financialized, 0),
         total = coalesce(total, 0)) |> 
  relocate(geometry, .after = p_financialized)

LL_CT_to_join <- 
  data_CT |> 
  st_drop_geometry() |> 
  select(GeoUID, n_financialized:p_financialized)

data_building <- 
  LL_CT |> 
  left_join(LL_CT_to_join, by = "GeoUID") |> 
  relocate(geometry, .after = last_col())

data_CT <- 
  data_building |> 
  st_drop_geometry() |>
  group_by(GeoUID) |>
  summarise(n_after_2005 = sum(annee_construction >= 2005, na.rm = TRUE),
            n_buildings = n(),
            p_built_after_2005 = n_after_2005 / n_buildings,
            .groups = "drop") |>
  select(-n_after_2005, -n_buildings) |>
  left_join(data_CT, by = "GeoUID") |>
  relocate(p_built_after_2005, .after = p_financialized) |>
  st_as_sf()

  
# Save output -------------------------------------------------------------

qsavem(data_building, data_CT, file = "output/data.qsm", 
       nthreads = availableCores())

rm(LL_CT, LL_CT_summary, LL_CT_to_join)
  