#### 03 LANDLORD DATA IMPORT PART 1 ############################################

source("R/01_startup.R")

# This script requires `merged_Nov2020.csv` to be present in `data`.


# Import 2020 landlord data -----------------------------------------------

LL_2020 <- 
  read_csv("data/merged_Nov2020.csv") |> 
  select(numero_matricule = Numero_de_matricule,
         adresse = Adresse, 
         adresse_postale = Adresse_postale, 
         annee_construction = Annee_de_construction, 
         borough = Arrondissement, 
         categorie_classe_immeuble = 
           Categorie_et_classe_dimmeuble_a_des_fins_dapplication_des_taux_varies_de_taxation, 
         conditions_particulieres = Conditions_particulires_dinscription, 
         date_inscription = Date_dinscription_au_rle, 
         lien_physique = Lien_physique,
         nom_1 = Nom, nom_10 = Nom10, nom_11 = Nom11, nom_2 = Nom2, 
         nom_3 = Nom3, nom_4 = Nom4, nom_5 = Nom5, nom_6 = Nom6, nom_7 = Nom7,
         nom_8 = Nom8, nom_9 = Nom9, 
         nombre_chambres_locatives = Nombre_de_chambres_locatives, 
         nombre_locaux_non_residentiels = Nombre_de_locaux_non_residentiels,
         nombre_logements = Nombre_de_logements, 
         nombre_etages = Nombre_detages, 
         statut_owner_1 = Statut_aux_fins_dimposition_scolaire, 
         statut_owner_10 = Statut_aux_fins_dimposition_scolaire10, 
         statut_owner_11 = Statut_aux_fins_dimposition_scolaire11,
         statut_owner_2 = Statut_aux_fins_dimposition_scolaire2, 
         statut_owner_3 = Statut_aux_fins_dimposition_scolaire3, 
         statut_owner_4 = Statut_aux_fins_dimposition_scolaire4, 
         statut_owner_5 = Statut_aux_fins_dimposition_scolaire5, 
         statut_owner_6 = Statut_aux_fins_dimposition_scolaire6, 
         statut_owner_7 = Statut_aux_fins_dimposition_scolaire7,
         superficie = Superficie, 
         utilisation_predo = Utilisation_predominante, 
         valeur_immeuble_ant = Valeur_de_limmeuble_au_role_anterieur, 
         valeur_immeuble_courant = Valeur_de_limmeuble_courant,
         valeur_batiment_courant = Valeur_du_batiment_courant, 
         valeur_terrain_courant = Valeur_du_terrain_courant, 
         valeur_imposable = Valeur_imposable_de_limmeuble, 
         valeur_non_imposable = Valeur_non_imposable_de_limmeuble) |> 
  mutate(annee_construction = str_replace_all(annee_construction, 
                                              c(" (estimée)" = "")),
         annee_construction = year(as.Date(as.character(annee_construction), 
                                           format= "%Y")),
         date_inscription = as.Date(date_inscription, format = "%d-%m-%Y"))


# Format 2020 postal addresses --------------------------------------------

street_name <- 
  LL_2020$adresse_postale |> 
  str_split_fixed(",", n = 3) |> 
  as.data.frame() |> 
  as_tibble() |> 
  pull(V1) |> 
  str_split_fixed("\\s", n = Inf) |> 
  as.data.frame() |> 
  as_tibble() |>
  unite("address", V2:V11, sep = " ",  remove = TRUE, na.rm = TRUE) |>
  mutate(street_name = str_trim(str_to_lower(str_remove(
    address, 
    "AV |STREET |CRES |BOUL |TSSE |PL | STREET|CR |ETAGE | ETAGE| rd|ch |av. ")
    ), side = "right")) |> 
  mutate(street_name = str_trim(gsub('[[:digit:]]+', '', street_name))) |>
  select(-V1, -address)

LL_2020_postal <- 
  LL_2020 |>
  mutate(adresse = str_to_lower(adresse)) |> 
  mutate(adresse = stringi::stri_trans_general(adresse, "Latin-ASCII")) |> 
  cbind(street_name) |> 
  as_tibble()


# If condition for owner/renter -------------------------------------------

LL_2020_postal <-
  LL_2020_postal |> 
  mutate(owner = str_detect(adresse, street_name)) |> 
  mutate(owner = coalesce(owner, FALSE)) |> 
  select(adresse, street_name, owner, everything()) |>
  mutate(number_rental_units = nombre_logements - owner)


# Get landlord ranks ------------------------------------------------------

LL_2020_ranked <-
  LL_2020_postal |> 
  group_by(adresse_postale) |> 
  summarize(nombre_unite_totales = sum(nombre_logements, na.rm = TRUE),
            nombre_loca = sum(number_rental_units, na.rm = TRUE),
            nombre_chambres = sum(nombre_chambres_locatives, na.rm = TRUE)) |> 
  arrange(desc(nombre_loca)) |> 
  mutate(landlord_rank = seq_along(adresse_postale))

LL_2020_postal <- 
  LL_2020_postal |> 
  left_join(LL_2020_ranked, by = "adresse_postale") |> 
  mutate(landlord_name = landlord_rank, landlord_type = landlord_rank) |>  
  select(adresse, adresse_postale, landlord_rank, landlord_name, landlord_type, 
         number_rental_units, nombre_loca, everything())


# Export postal address owners for manual analysis ------------------------

postal_addresses_owners <-
  LL_2020_postal |> 
  group_by(adresse_postale) |> 
  summarize(number_rental_units = sum(number_rental_units, na.rm = TRUE)) |> 
  arrange(desc(number_rental_units)) |> 
  filter(number_rental_units >= 1)

write_csv(postal_addresses_owners, "data/postal_addresses_owners.csv")


# Save data for landlord analysis -----------------------------------------

qsavem(LL_2020, LL_2020_postal, file = "output/LL.qsm", 
       nthreads = availableCores())

rm(LL_2020_ranked, postal_addresses_owners, street_name)
