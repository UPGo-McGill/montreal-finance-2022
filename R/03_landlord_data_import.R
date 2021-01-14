#### 03 LANDLORD DATA IMPORT ################################################################

# Landlord dataset -------------------------------------------------------------
# Import 2019 data

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
  select(-c(date_rapport, genre_construction, info_en_date, statut_owner_8, statut_owner_9))


# Exploration ----------------------------------------------------

#LL_2019 %>% 
#  group_by(nom1) %>% 
#  summarize(n=n(), logements=sum(nombre_logements)) %>% 
#  View()

LL_2020_test2 %>% 
  group_by(nom1) %>% 
  summarize(n=n(), logements=sum(nombre_logements)) %>% 
  group_by(adresse_postale) %>% 
  summarize(n1=sum(n), logements1=sum(logements)) %>% 
  View()

#LL_2020 %>% 
#  group_by(adresse_postale) %>% 
#  summarize(n=n(), logements=sum(nombre_logements)) %>% 
#  View()


# Set global variables ----------------------------------------------------

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
  mutate(street_name = str_trim(str_to_lower(str_remove(PA2$adress, "AV |STREET |CRES |BOUL |TSSE |PL | STREET|CR |ETAGE | ETAGE")), side="right")) %>% 
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
  mutate(number_rental_units = ifelse(owner == TRUE, nombre_logements-1, nombre_logements))


# If condition for owner/renter -------------------------------------------------------------

LL_2020_test3 <- 
  LL_2020_test2 %>% 
  nest(-adresse_postale)

LL_2020_test4 <- 
  LL_2020_test3 %>% 
  mutate(property_group_ID = 1:363603) %>% 
  unnest()

# The following analysis code looks at the companies that get combined with a group_by that combines
# entries that have both the same name AND postal address. The second group_by combines all the
# names that were not grouped together because they did not have the same postal address for both.
# A careful analysis of these entries that operated with more than two postal addresses and owned, 
# when all the subsidiares combined, more than 10 units were analyzed. Most were people with the 
# same name (i.e. Andre Tremblay), but the ones with company names will be analyzed for a more 
# accurate breakdown of the number of units owned by landlords.

LL_test_2020_4 %>% 
  group_by(nom1, property_group_ID) %>% 
  summarize(n=n(), logements=sum(nombre_logements)) %>% 
  group_by(nom1) %>% 
  summarize(n=n(), logements1=sum(logements)) %>% 
  filter(n>1) %>% 
  filter(logements1>10) %>% 
  View()

# Modify names with slight string differences -------------------------------------------

# Combining the dataset by postal address gives us a great sense of the companies that use
# multiple names or company subsidiaries to operate in Montreal. Now, it is time to both correct
# the companies that are owned by the same stakeholders but operated under different postal addresses. 
# Next, we will give a clear name of the biggest landlords in Montreal (for the ones whose used name
# is not of recognessence to anyone, but are well known under another name). The list will be long, but 
# essential for an exhaustive analysis. Such modifications are based on investigative work from my 
# end, with links provided when pertinent.

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

LL_2020sf <- 
  LL_2020_test4 %>% 
  select(-valeur_batiment_courant, -valeur_terrain_courant) %>% 
  inner_join(., uef, by = "numero_matricule")


# Save output -------------------------------------------------------------

save(LL_2020, LL_2020_test4, LL_2020sf, uef_raw, 
     file = "output/LL.Rdata")








