### Montreal financialization and landlord analysis

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(strr)
library(sf)


# Set global variables ----------------------------------------------------

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

LL_2019 %>% 
  group_by(nom1) %>% 
  summarize(n=n(), logements=sum(nombre_logements)) %>% 
  View()

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
              "valeur_batiment_courant", "valeur_terrain_courant", "valeur_imposable", "valeur_non_imposable"))

LL_2020 %>% 
  group_by(nom1) %>% 
  summarize(n=n(), logements=sum(nombre_logements)) %>% 
  View()

LL_2020 %>% 
  group_by(adresse_postale) %>% 
  summarize(n=n(), logements=sum(nombre_logements)) %>% 
  View()


# Montreal DAs ------------------------------------------------------------

DA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = "2466023"), level = "DA",
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, Dwellings) %>% 
  set_names(c("GeoUID", "dwellings", "geometry")) %>% 
  st_set_agr("constant")


# Montreal boroughs -------------------------------------------------------

boroughs_raw <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") %>% 
  filter(TYPE == "Arrondissement") %>% 
  select(borough = NOM) %>% 
  st_set_agr("constant") %>% 
  st_transform(32618) 

boroughs <- 
  boroughs_raw %>% 
  st_intersection(province)

boroughs <- 
  DA %>% 
  select(dwellings) %>% 
  st_interpolate_aw(boroughs, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(dwellings) %>% 
  cbind(boroughs, .) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  arrange(borough)


LL %>% 
  group_by(nom1) %>% 
  summarize(n=n(), logements=sum(nombre_logements, na.rm = TRUE), bureaux=sum(nombre_locaux_non_residentiels, na.rm=TRUE)) %>% 
  View()



