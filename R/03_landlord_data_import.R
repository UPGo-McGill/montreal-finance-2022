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

#LL_2020 %>% 
#  group_by(nom1) %>% 
#  summarize(n=n(), logements=sum(nombre_logements)) %>% 
#  View()

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
civic_number <- 
  PA1$V1 %>% 
  as_data_frame() %>% 
  as_tibble()

# Postal code
pc_pa <- PA$V3 %>% 
  as_data_frame() %>% 
  as_tibble() %>% 
  rename(postal_code = value)

# City + Province
ville <- str_trim(PA$V2, side=c("left")) %>% 
  as_data_frame() %>% 
  as_tibble() %>% 
  rename(ville = value)

# Street name
street_name <- 
  PA2 %>% 
  mutate(street_name = str_trim(str_to_lower(str_remove(PA2$adress, "AV |STREET |CRES |BOUL |TSSE |PL | STREET|CR |ETAGE | ETAGE")), side="right")) %>% 
  select(-V1, -adress)

# Street prefix
street_prefix <- 
  PA2 %>% 
  mutate(street_prefix = str_trim(str_to_lower(str_extract(PA2$adress, "AV |STREET |CRES |BOUL |TSSE |PL | STREET|CR |ETAGE | ETAGE")), side="right")) %>% 
  select(-V1, -adress)

# Suite number
suite_number <- 
  str_extract(street_name$street_name, "\\s+[:digit:]+\\s") %>% 
  as_data_frame() %>% 
  as_tibble() %>% 
  rename(suite_number = value)

LL_2020_test1 <- 
  LL_2020 %>% 
  mutate(adresse=str_to_lower(adresse)) %>% 
  cbind(., civic_number) %>% 
  cbind(., street_name) %>% 
  cbind(., street_prefix) %>%
  cbind(., suite_number) %>%
  cbind(., ville) %>%
  cbind(., pc_pa) 

LL_2020_test1 <- 
  LL_2020_test1 %>% 
  as_tibble()

# If condition for owner/renter -------------------------------------------------------------

LL_2020_test2 <- 
  LL_2020_test1 %>% 
  mutate(owner = str_detect(LL_2020_test1$adresse, LL_2020_test1$street_name)) %>% 
  mutate(number_rental_units = ifelse(owner == TRUE, nombre_logements-1, nombre_logements))


# Download UEF for geometry -------------------------------------------------------------

uef <-
  read_sf("data/uniteevaluationfonciere/uniteevaluationfonciere.shp") %>%
  st_transform(32618) %>%
  filter(!is.na(NOMBRE_LOG)) %>%
  as_tibble() %>%
  distinct(ID_UEV, .keep_all = TRUE) %>%
  st_as_sf()



# Save output -------------------------------------------------------------

#save(TKTK, 
#     file = "output/geometry.Rdata")