#### 07 LANDLORD DATA PROCESSING ###############################################



# Remainder of the units are classified automatically using a scrape of the REQ
# The document is called full_parsed

full_parsed_raw <- qread("output/full_parsed.qs", nthreads = availableCores())

full_parsed <- 
  full_parsed_raw %>% 
  mutate(person = ifelse(str_detect(new_name, "[:upper:]\\,\\s"), TRUE, person)) %>% 
  mutate(new_name = ifelse(name == new_name, "Loop, same shareholder as owner", new_name)) %>% 
  select(-full_text)

non_financialized <- c("Personne morale sans but lucratif", "Syndicat de copropriété", "Coopérative")

non_financialized <- 
  full_parsed %>% 
  filter(firm_type %in% non_financialized) %>% 
  pull(name)

obnl <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%  
  filter(number_rental_units >0) %>% 
  filter(nom1 %in% non_financialized) %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% obnl, nom1, landlord_name),
         type = ifelse(numero_matricule %in% obnl, "Nonprofit", type),
         company_type = ifelse(numero_matricule %in% obnl, "Non-profit property management", company_type),
         publicly_traded = ifelse(numero_matricule %in% obnl, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% obnl, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% obnl, FALSE, financial_partners)
  )

LL_analyzed %>% 
  filter(is.na(landlord_name)) %>% 
  View()

no_corporate_info_avail <- 
  full_parsed %>% 
  filter(is.na(firm_type)) %>% 
  filter(!str_detect(name, "[:upper:]\\,\\s[:upper:]")) %>% 
  pull(name)

rest <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%  
  filter(number_rental_units > 0) %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% rest, adresse_postale, landlord_name),
         type = ifelse(numero_matricule %in% rest, "Private", type),
         company_type = ifelse(numero_matricule %in% rest, "Property management", company_type),
         publicly_traded = ifelse(numero_matricule %in% rest, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% rest, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% rest, FALSE, financial_partners)
  )

individuals_landlords <- 
  LL_analyzed %>% 
  filter(numero_matricule %in% individuals)

non_fz_PM_landlords <- 
  LL_analyzed %>% 
  filter(numero_matricule %in% rest)

sum(non_fz_PM_landlords$number_rental_units)/sum(LL_analyzed$number_rental_units, na.rm = TRUE)
sum(individuals_landlords$number_rental_units)/sum(LL_analyzed$number_rental_units, na.rm = TRUE)
sum(LL_analyzed$nombre_logements, na.rm = TRUE)
sum(LL_analyzed$number_rental_units, na.rm = TRUE)

top500 <- LL_analyzed %>% filter(landlord_rank <= 500)
rest500plus <- LL_analyzed %>% filter(landlord_rank >500)
sum(top500$number_rental_units, na.rm = TRUE)/
  sum(LL_analyzed$number_rental_units, na.rm = TRUE)
sum(rest500plus$number_rental_units, na.rm = TRUE)

total_landlords <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units >0) %>% 
  distinct(landlord_name)


# Download UEF for geometry -------------------------------------------------------------

uef_raw <-
  read_sf("data/uniteevaluationfonciere/uniteevaluationfonciere.shp") %>%
  st_transform(32618) %>%
  #filter(!is.na(NOMBRE_LOG)) %>%
  as_tibble() %>%
  distinct(ID_UEV, .keep_all = TRUE) %>%
  st_as_sf()

uef <- 
  uef_raw %>% 
  select(NOM_RUE, CIVIQUE_DE, CIVIQUE_FI, NOMBRE_LOG, ANNEE_CONS, MATRICULE8) %>% 
  rename(numero_matricule=MATRICULE8)

uef_centroid <- 
  uef %>% 
  st_centroid()

LL_sf <- 
  LL_analyzed %>% 
  inner_join(., uef %>% select(numero_matricule), by = "numero_matricule") %>% 
  st_as_sf() %>% 
  st_transform(32618)

LL_sf_centroid <- 
  LL_analyzed %>% 
  inner_join(., uef_centroid %>% select(numero_matricule), by = "numero_matricule") %>% 
  st_as_sf() %>% 
  st_transform(32618)


# Download permit dataset -------------------------------------------------------------

# permits <-
#   read_sf("data/permis-construction/permis-construction.shp") %>%
#   st_transform(32618) %>%
#   as_tibble() %>%
#   st_as_sf()


# 2020 asking rents -------------------------------------------------------------

ltr <- qread("output/ltr_processed.qs")

ltr_unique <- 
  ltr %>% 
  #st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = TRUE)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000)

asking_rents_CT <- 
  asking_rents %>%
  select(-GeoUID, -property_ID) %>% 
  st_intersection(., CT)

CT <- 
  asking_rents_CT %>% 
  st_drop_geometry() %>% 
  filter(furnished==FALSE) %>% 
  filter(short_long=="long") %>% 
  group_by(GeoUID) %>% 
  summarize(asking_rent = mean(price, na.rm=TRUE)) %>% 
  left_join(CT, ., by = "GeoUID")


# Save output -------------------------------------------------------------

save(LL_2020_test4, LL_sf, LL_sf_centroid, LL_2020, uef, LL_analyzed,
     file = "output/LL.Rdata")
