#### 03 LANDLORD DATA IMPORT ################################################################

library(readxl)
library(qs)

# Landlord dataset -------------------------------------------------------------
# Import 2019 data

# LL_analyzed <- 
#   LL_analyzed %>% 
#   mutate(landlord_rank = as.numeric(landlord_rank))

LL_raw_2019 <-
  read_csv("data/landlords_2019.csv")

LL_2019 <-  LL_raw_2019 %>% 
  select(-c(Aire_detages, Bâtiment_imposable:`Bâtiment_non_imposable_(compensable)`, 
            Immeuble_imposable:`Immeuble_non_imposable_(remboursable)`, Mesure_frontale, 
            Nom100:Nom109, Nom110:Nom19, Nom20:Nom29, Nom30:Nom39, Nom40:Nom49, Nom50:Nom59, Nom60:Nom69,
            Nom70:Nom79, Nom80:Nom89, Nom90:Nom99, Note:Numero_dunite_de_voisinage, 
            Statut_aux_fins_dimposition_scolaire100:Statut_aux_fins_dimposition_scolaire109,
            Statut_aux_fins_dimposition_scolaire110:Statut_aux_fins_dimposition_scolaire19,
            Statut_aux_fins_dimposition_scolaire20:Statut_aux_fins_dimposition_scolaire29,
            Statut_aux_fins_dimposition_scolaire30:Statut_aux_fins_dimposition_scolaire39,
            Statut_aux_fins_dimposition_scolaire40:Statut_aux_fins_dimposition_scolaire49,
            Statut_aux_fins_dimposition_scolaire50:Statut_aux_fins_dimposition_scolaire59, 
            Statut_aux_fins_dimposition_scolaire60:Statut_aux_fins_dimposition_scolaire69,
            Statut_aux_fins_dimposition_scolaire70:Statut_aux_fins_dimposition_scolaire79,
            Statut_aux_fins_dimposition_scolaire80:Statut_aux_fins_dimposition_scolaire89,
            Statut_aux_fins_dimposition_scolaire90:Statut_aux_fins_dimposition_scolaire99,
            Terrain_imposable:`Terrain_non_imposable_(compensable)`)) %>% 
  set_names(c("numero_matricule", "adresse", "adresse_postale", "annee_construction", "borough", 
              "categorie_classe_immeuble", "conditions_particulieres", "date_ref_marche_ant",
              "date_ref_marche_courant", "date_inscription", "date_rapport", "genre_construction",
              "info_en_date", "lien_physique", "nom1", "nom10", "nom11", "nom2", "nom3", "nom4", "nom5",
              "nom6", "nom7", "nom8", "nom9", "nombre_chambres_locatives", "nombre_locaux_non_residentiels",
              "nombre_logements", "nombre_etages", "statut_owner_1", "statut_owner_10", "statut_owner_11",
              "statut_owner_2", "statut_owner_3", "statut_owner_4", "statut_owner_5", "statut_owner_6", "statut_owner_7",
              "statut_owner_8", "statut_owner_9", "superficie", "utilisation_predo", "valeur_immeuble_ant", "valeur_immeuble_courant",
              "valeur_batiment_courant", "valeur_terrain_courant", "valeur_imposable", "valeur_non_imposable"))


# Import 2020 data

LL_raw_2020 <-
  read_csv("data/merged_Nov2020.csv")

LL_2020 <-  LL_raw_2020 %>% 
  select(-c(Aire_detages, Bâtiment_imposable:`Bâtiment_non_imposable_(compensable)`, 
            Immeuble_imposable:`Immeuble_non_imposable_(remboursable)`, Mesure_frontale, 
            Nom100:Nom109, Nom110:Nom19, Nom20:Nom29, Nom30:Nom39, Nom40:Nom49, Nom50:Nom59, Nom60:Nom69,
            Nom70:Nom79, Nom80:Nom89, Nom90:Nom99, Note:Numero_dunite_de_voisinage, 
            Statut_aux_fins_dimposition_scolaire100:Statut_aux_fins_dimposition_scolaire109,
            Statut_aux_fins_dimposition_scolaire110:Statut_aux_fins_dimposition_scolaire19,
            Statut_aux_fins_dimposition_scolaire20:Statut_aux_fins_dimposition_scolaire29,
            Statut_aux_fins_dimposition_scolaire30:Statut_aux_fins_dimposition_scolaire39,
            Statut_aux_fins_dimposition_scolaire40:Statut_aux_fins_dimposition_scolaire49,
            Statut_aux_fins_dimposition_scolaire50:Statut_aux_fins_dimposition_scolaire59, 
            Statut_aux_fins_dimposition_scolaire60:Statut_aux_fins_dimposition_scolaire69,
            Statut_aux_fins_dimposition_scolaire70:Statut_aux_fins_dimposition_scolaire79,
            Statut_aux_fins_dimposition_scolaire80:Statut_aux_fins_dimposition_scolaire89,
            Statut_aux_fins_dimposition_scolaire90:Statut_aux_fins_dimposition_scolaire99,
            Terrain_imposable:`Terrain_non_imposable_(compensable)`)) %>% 
  set_names(c("numero_matricule", "adresse", "adresse_postale", "annee_construction", "borough", 
              "categorie_classe_immeuble", "conditions_particulieres", "date_ref_marche_ant",
              "date_ref_marche_courant", "date_inscription", "date_rapport", "genre_construction",
              "info_en_date", "lien_physique", "nom1", "nom10", "nom11", "nom2", "nom3", "nom4", "nom5",
              "nom6", "nom7", "nom8", "nom9", "nombre_chambres_locatives", "nombre_locaux_non_residentiels",
              "nombre_logements", "nombre_etages", "statut_owner_1", "statut_owner_10", "statut_owner_11",
              "statut_owner_2", "statut_owner_3", "statut_owner_4", "statut_owner_5", "statut_owner_6", "statut_owner_7",
              "statut_owner_8", "statut_owner_9", "superficie", "utilisation_predo", "valeur_immeuble_ant", "valeur_immeuble_courant",
              "valeur_batiment_courant", "valeur_terrain_courant", "valeur_imposable", "valeur_non_imposable")) %>% 
  select(-c(date_rapport, date_ref_marche_ant, date_ref_marche_courant, genre_construction, info_en_date, statut_owner_8, statut_owner_9)) %>% 
  mutate(annee_construction=str_replace_all(annee_construction, c(" (estimée)" = "")),
         annee_construction = year(as.Date(as.character(annee_construction), format= "%Y")),
         date_inscription = as.Date(date_inscription, format = "%d-%m-%Y")
         )


# Format 2020 postal addresses ------------------------------------------------------------

PA <- str_split_fixed(LL_2020$adresse_postale, ",", n=3) %>% 
  as.data.frame() %>% 
  as_tibble()

PA1 <- str_split_fixed(PA$V1, "\\s", n=Inf) %>% 
  as.data.frame() %>% 
  as_tibble()

PA2 <- PA1 %>% 
  unite("adress", V2:V11, sep = " ",  remove = TRUE, na.rm = TRUE)

# Rebuild address
# Civid number
#civic_number <- 
#  PA1$V1 %>% 
#  as.data.frame() %>% 
#  as_tibble()

# Postal code
#pc_pa <- PA$V3 %>% 
#as_data_frame() %>% 
#  as_tibble() %>% 
#  rename(postal_code = value)

# City + Province
#ville <- str_trim(PA$V2, side=c("left")) %>% 
#  as_data_frame() %>% 
#  as_tibble() %>% 
#  rename(ville = value)

# Street name
street_name <- 
  PA2 %>% 
  mutate(street_name = str_trim(str_to_lower(str_remove(PA2$adress, "AV |STREET |CRES |BOUL |TSSE |PL | STREET|CR |ETAGE | ETAGE| rd|ch |av. ")), side="right")) %>% 
  mutate(street_name = str_trim(gsub('[[:digit:]]+', '', street_name))) %>% 
  select(-V1, -adress)

# Street prefix
#street_prefix <- 
#  PA2 %>% 
#  mutate(street_prefix = str_trim(str_to_lower(str_extract(PA2$adress, "AV |STREET |CRES |BOUL |TSSE |PL | STREET|CR |ETAGE | ETAGE")), side="right")) %>% 
#  select(-V1, -adress)

# Suite number
#suite_number <- 
#  str_extract(street_name$street_name, "\\s+[:digit:]+\\s") %>% 
#  as_data_frame() %>% 
#  as_tibble() %>% 
#  rename(suite_number = value)

LL_2020_test1 <- 
  LL_2020 %>% 
  mutate(adresse=str_to_lower(adresse)) %>% 
  mutate(adresse = stringi::stri_trans_general(adresse, "Latin-ASCII")) %>% 
#  cbind(., civic_number) %>% 
#  cbind(., street_prefix) %>%
#  cbind(., suite_number) %>%
#  cbind(., ville) %>%
#  cbind(., pc_pa) %>% 
  cbind(., street_name) 

LL_2020_test1 <- 
  LL_2020_test1 %>% 
  as_tibble()

# If condition for owner/renter -------------------------------------------------------------

LL_2020_test2 <- 
  LL_2020_test1 %>% 
  mutate(owner = str_detect(LL_2020_test1$adresse, LL_2020_test1$street_name)) %>% 
  mutate(owner = ifelse(is.na(owner), FALSE, owner)) %>% 
  select(adresse, street_name,owner, everything()) %>% 
  mutate(number_rental_units = ifelse(owner == TRUE, nombre_logements-1, nombre_logements))


# If condition for owner/renter -------------------------------------------------------------

LL_2020_test3 <- 
  LL_2020_test2 %>% 
  group_by(adresse_postale) %>% 
  summarize(nombre_unite_totales=sum(nombre_logements, na.rm = TRUE),
            nombre_loca = sum(number_rental_units, na.rm = TRUE),
            nombre_chambres=sum(nombre_chambres_locatives, na.rm = TRUE)) %>% 
  arrange(desc(nombre_loca)) %>% 
  mutate(landlord_rank = as.character(1:363603)) %>% 
  mutate(landlord_rank = as.numeric(landlord_rank))

LL_2020_test4 <- 
  left_join(LL_2020_test2, LL_2020_test3, by = "adresse_postale") %>% 
  mutate(landlord_name = landlord_rank,
         landlord_type = landlord_rank) %>% 
  select(adresse, adresse_postale, landlord_rank, landlord_name, landlord_type, number_rental_units, nombre_loca, everything())

LL_2020_test5 <- 
  LL_2020_test4 %>% 
  select(adresse, numero_matricule, adresse_postale, landlord_rank, owner, annee_construction, borough, date_inscription,
         nom1, nom2, statut_owner_1, statut_owner_2, nombre_chambres_locatives, nombre_logements, number_rental_units)
  
# postal_adresses_owners <- 
#   LL_2020_test4 %>% 
#   group_by(adresse_postale) %>% 
#   summarize(number_rental_units=sum(number_rental_units, na.rm=TRUE)) %>% 
#   arrange(desc(number_rental_units)) %>% 
#   filter(number_rental_units>=1)
# 
# write_csv(postal_adresses_owners, "data/postal_adresses_owners.csv")

# import address and owners once they have been categorized in Excel

landlord_names <- 
  readxl::read_xlsx("data/postal_adresses_owners.xlsx")

# landlords <- 
#   LL_2020_test4 %>% 
#   group_by(landlord_name) %>% 
#   summarize(number_rental_units=sum(number_rental_units, na.rm=TRUE)) %>% 
#   arrange(desc(number_rental_units)) %>% 
#   filter(number_rental_units>=1)
#
# write_csv(landlords, "data/landlords2.csv")

# import landlord analysis once it has been conducted in Excel

landlord_analysis <- 
  readxl::read_xlsx("data/landlords2.xlsx")

# combine landlord information with postal addresses

LL_analyzed <- 
  landlord_analysis %>% 
  full_join(., landlord_names, by = "landlord_name") %>% 
  mutate(financialized = publicly_traded+direct_involvement_FM+financial_partners) %>% 
  select(-number_rental_units) %>% 
  full_join(LL_2020_test5, ., by = "adresse_postale") %>% 
  distinct(numero_matricule, .keep_all=TRUE)


# Classify the rest of the landlords ----------------------------------------------------

# Individual(s)

individuals <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%
  filter(number_rental_units >0) %>% 
  filter(statut_owner_1 == "Personne physique") %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% individuals, nom1, landlord_name),
         type = ifelse(numero_matricule %in% individuals, "Private", type),
         company_type = ifelse(numero_matricule %in% individuals, "Property management", company_type),
         publicly_traded = ifelse(numero_matricule %in% individuals, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% individuals, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% individuals, FALSE, financial_partners)
         )

cooperatives <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%  
  filter(number_rental_units >0) %>% 
  filter(statut_owner_1 == "Personne morale") %>%
  mutate(type = ifelse(str_detect(nom1, "COOP|CO-OP|C0OP|CO OP"), "Cooperative", type)) %>% 
  filter(type=="Cooperative") %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% cooperatives, nom1, landlord_name),
         type = ifelse(numero_matricule %in% cooperatives, "Cooperative", type),
         company_type = ifelse(numero_matricule %in% cooperatives, "Non-profit property management", company_type),
         publicly_traded = ifelse(numero_matricule %in% cooperatives, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% cooperatives, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% cooperatives, FALSE, financial_partners)
         )

owner_occupier <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%  
  filter(owner == TRUE) %>% 
  filter(nombre_logements == 1) %>% 
  filter(statut_owner_1 == "Personne physique") %>%
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% owner_occupier, nom1, landlord_name),
         type = ifelse(numero_matricule %in% owner_occupier, "Owner-Occupier", type),
         company_type = ifelse(numero_matricule %in% owner_occupier, "Owner-Occupier", company_type),
         publicly_traded = ifelse(numero_matricule %in% owner_occupier, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% owner_occupier, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% owner_occupier, FALSE, financial_partners)
         )

non_housing <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>% 
  filter(is.na(number_rental_units)) %>% 
  filter(is.na(nombre_logements)) %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% non_housing, nom1, landlord_name),
         type = ifelse(numero_matricule %in% non_housing, "Non-housing", type),
         company_type = ifelse(numero_matricule %in% non_housing, "Other - non-housing", company_type),
         publicly_traded = ifelse(numero_matricule %in% non_housing, NA, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% non_housing, NA, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% non_housing, NA, financial_partners)
  )

communautaire <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%  
  filter(number_rental_units >0) %>% 
  filter(statut_owner_1 == "Personne morale") %>%
  mutate(type = ifelse(str_detect(nom1, "COMMUNAUTAIRE|COMMUNAITAIRE|MAISON DALAUZE|AIDE REHABILITATION|CHARITY|CHEVALIERS|COMMUNAUTES|FONDATION|MAIS0NS TRANSITIONNELLES|OASIS POINTE ST-CHARLES|MAISON L ACCOLADE|POPULAIRE|SOEURS|SANCTUAIRE|RESEAU HABITATION FEMMES DE MONTREAL|JEUNESSE|JEUNES FEMMES|TRANSITOIRE|MISSION OLD BREWERY|L'ESCALIER|BROTHERHOOD|SOCIETE D'HABITATION DU QUEBEC|SALVATION|SALUT|GREENBERG-MIRIAM|HANDICAPES"), "Communautaire", type)) %>% 
  filter(type=="Communautaire") %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% communautaire, nom1, landlord_name),
         type = ifelse(numero_matricule %in% communautaire, "Nonprofit", type),
         company_type = ifelse(numero_matricule %in% communautaire, "Non-profit property management", company_type),
         publicly_traded = ifelse(numero_matricule %in% communautaire, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% communautaire, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% communautaire, FALSE, financial_partners)
  )

religious_and_gvnmtl <- 
  LL_analyzed %>% 
  filter(is.na(landlord_name)) %>%  
  filter(number_rental_units >0) %>% 
  filter(statut_owner_1 == "Personne morale") %>%
  mutate(type = ifelse(str_detect(nom1, "TEMPLE|HOSPITAL|CIUSS|HOSPITALIER|CHARITY|HYDRO-QUEBEC|IDELOGICAL STUDIES|MISSIONNAIRES|CATHEDRAL|CONSERVATION DE LA NATURE|SERVICES SCOLAIRE|MAISON DE JEUNES|RÉSEAU DE TRANSPORT MÉTROPOLITAIN|EGLISE|ORATOIRE|REPUBLIC|CATHOLIQUE|CATHOLIC|RUSSIE|APOTRES|GOVERNMENT|REPUBLIQUE|INSTITUTION|GOUVERNEMENT|CONGREGATION|ESPAGNOL|ANGLICAN|DORVAL"), "Institutional", type)) %>% 
  filter(type=="Institutional") %>% 
  pull(numero_matricule)

LL_analyzed <- 
  LL_analyzed %>% 
  mutate(landlord_name = ifelse(numero_matricule %in% religious_and_gvnmtl, nom1, landlord_name),
         type = ifelse(numero_matricule %in% religious_and_gvnmtl, "Institutional", type),
         company_type = ifelse(numero_matricule %in% religious_and_gvnmtl, "Non-profit property management", company_type),
         publicly_traded = ifelse(numero_matricule %in% religious_and_gvnmtl, FALSE, publicly_traded),
         direct_involvement_FM = ifelse(numero_matricule %in% religious_and_gvnmtl, FALSE, direct_involvement_FM),
         financial_partners = ifelse(numero_matricule %in% religious_and_gvnmtl, FALSE, financial_partners)
  )

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




