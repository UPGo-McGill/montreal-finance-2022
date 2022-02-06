#### 07 LANDLORD DATA PROCESSING ###############################################

source("R/01_startup.R")
qload("output/LL.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
req_parsed <- qread("output/req_parsed.qs", nthreads = availableCores())

# This script requires `ltr_processed.qs` (the result of scraping Craigslist and
# Kijiji rental listings) to be present in `data`.


# Use REQ scrape to process landlords -------------------------------------

req_parsed <- 
  req_parsed |> 
  mutate(person = if_else(str_detect(new_name, "[:upper:]\\,\\s"), TRUE, 
                          person),
         new_name = if_else(name == new_name, "Loop, same shareholder as owner", 
                            new_name)) |> 
  select(-full_text)

non_financialized <- 
  req_parsed |> 
  filter(firm_type %in% c("Personne morale sans but lucratif", 
                          "Syndicat de copropriété", "Coopérative")) |> 
  pull(name)

obnl <- 
  LL_analyzed |>
  filter(is.na(landlord_name), number_rental_units > 0, 
         nom_1 %in% non_financialized) |> 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed |> 
  mutate(
    landlord_name = if_else(numero_matricule %in% obnl, nom_1, landlord_name),
    type = if_else(numero_matricule %in% obnl, "Non-profit", type),
    company_type = if_else(numero_matricule %in% obnl, 
                           "Non-profit property management", company_type),
    publicly_traded = if_else(numero_matricule %in% obnl, FALSE, 
                              publicly_traded),
    direct_involvement_FM = if_else(numero_matricule %in% obnl, FALSE, 
                                    direct_involvement_FM),
    financial_partners = if_else(numero_matricule %in% obnl, FALSE, 
                                 financial_partners))

no_corporate_info_avail <- 
  req_parsed |> 
  filter(is.na(firm_type), !str_detect(name, "[:upper:]\\,\\s[:upper:]")) |>
  pull(name)

rest <- 
  LL_analyzed |> 
  filter(is.na(landlord_name), number_rental_units > 0) |> 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed |> 
  mutate(
    landlord_name = if_else(numero_matricule %in% rest, adresse_postale, 
                            landlord_name),
    type = if_else(numero_matricule %in% rest, "Private", type),
    company_type = if_else(numero_matricule %in% rest, "Property management", 
                           company_type),
    publicly_traded = if_else(numero_matricule %in% rest, FALSE, 
                              publicly_traded),
    direct_involvement_FM = if_else(numero_matricule %in% rest, FALSE, 
                                    direct_involvement_FM),
    financial_partners = if_else(numero_matricule %in% rest, FALSE, 
                                 financial_partners))

individuals_landlords <- 
  LL_analyzed |> 
  filter(numero_matricule %in% individuals)

non_fz_PM_landlords <- 
  LL_analyzed |>
  filter(numero_matricule %in% rest)

total_landlords <- 
  LL_analyzed |> 
  filter(!is.na(number_rental_units)) |> 
  filter(number_rental_units > 0) |> 
  distinct(landlord_name)


# Download UEF for geometry -------------------------------------------------------------

uef <-
  read_sf("data/shapefiles/uniteevaluationfonciere.shp") |> 
  st_transform(32618) |> 
  as_tibble() |> 
  distinct(ID_UEV, .keep_all = TRUE) |> 
  st_as_sf() |>
  select(NOM_RUE, CIVIQUE_DE, CIVIQUE_FI, NOMBRE_LOG, ANNEE_CONS, 
         MATRICULE8) |> 
  rename(numero_matricule = MATRICULE8) |> 
  mutate(centroid = st_centroid(geometry))

LL_sf <- 
  LL_analyzed |> 
  inner_join(select(uef, numero_matricule, centroid, geometry), 
             by = "numero_matricule") |> 
  st_as_sf()


# 2020 asking rents -------------------------------------------------------------

ltr <- 
  qread("data/ltr_processed.qs", nthreads = availableCores()) |> 
  arrange(desc(scraped)) |> 
  distinct(id, .keep_all = TRUE)

asking_rents <- 
  ltr |> 
  filter(price > 425, price < 8000) |> 
  select(-GeoUID, -property_ID) |> 
  st_intersection(CT)

CT <- 
  asking_rents |> 
  st_drop_geometry() |> 
  filter(!furnished) |> 
  filter(short_long == "long") |> 
  group_by(GeoUID) |> 
  summarize(asking_rent = mean(price, na.rm = TRUE)) |> 
  right_join(CT, by = "GeoUID") |> 
  relocate(distance_dt, asking_rent, .before = geometry) |> 
  st_as_sf()


# Save output -------------------------------------------------------------

qsavem(LL_2020, LL_analyzed, LL_sf, file = "output/LL.qsm",
       nthreads = availableCores())
qsavem(boroughs, CT, CT_06, province, streets_downtown, 
       file = "output/geometry.qsm", nthreads = availableCores())

rm(asking_rents, individuals_landlords, ltr, non_fz_PM_landlords,
   req_names, req_parsed, total_landlords, uef, individuals,
   no_corporate_info_avail, non_financialized, obnl, rest)
