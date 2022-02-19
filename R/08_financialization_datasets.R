#### 08 FINANCIALIZATION DATASETS ##############################################

source("R/01_startup.R")
landlord <- qread("output/landlord.qs", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())


# Join LL data to CTs -----------------------------------------------------

LL_CT <- 
  landlord |> 
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
  summarize(n_fin = sum(rental_units[fin]),
            total = sum(rental_units),
            p_fin = n_fin / total,
            .groups = "drop")


# Assemble final datasets -------------------------------------------------

data_CT <- 
  CT |> 
  left_join(LL_CT_summary, by = "GeoUID") |> 
  mutate(p_fin = coalesce(p_fin, 0),
         total = coalesce(total, 0)) |> 
  relocate(geometry, .after = p_fin)

LL_CT_to_join <- 
  data_CT |> 
  st_drop_geometry() |> 
  select(GeoUID, n_fin:p_fin)

data_building <- 
  LL_CT |> 
  left_join(LL_CT_to_join, by = "GeoUID") |> 
  relocate(geometry, .after = last_col())

data_CT <- 
  data_building |> 
  st_drop_geometry() |>
  group_by(GeoUID) |>
  summarise(n_after_2005 = sum(number_rental_units[annee_construction >= 2005], 
                               na.rm = TRUE),
            n_rentals = sum(number_rental_units),
            p_built_after_2005 = n_after_2005 / n_rentals,
            .groups = "drop") |>
  select(-n_after_2005) |>
  left_join(data_CT, by = "GeoUID") |>
  relocate(p_built_after_2005, .after = p_fin) |>
  relocate(n_rentals, .after = dwellings) |> 
  st_as_sf()

  
# Save output -------------------------------------------------------------

qsavem(data_building, data_CT, file = "output/data.qsm", 
       nthreads = availableCores())

rm(LL_CT, LL_CT_summary, LL_CT_to_join)
  