#### 05 LANDLORD DATA IMPORT PART 3 ############################################

source("R/01_startup.R")
qload("output/LL.qsm", nthreads = availableCores())

# This script requires `landlords.xlsx` (the result of manual classification) 
# to be present in `data`.


# Import landlord analysis once it has been conducted in Excel ------------

landlord_analysis <- readxl::read_xlsx("data/landlords.xlsx")


# Combine landlord information with postal addresses ----------------------

LL_2020_postal <- 
  LL_2020_postal |>  
  select(adresse, numero_matricule, adresse_postale, landlord_rank, owner, 
         annee_construction, borough, date_inscription, nom_1, nom_2, 
         statut_owner_1, statut_owner_2, nombre_chambres_locatives, 
         nombre_logements, number_rental_units)

LL_analyzed <- 
  landlord_analysis |> 
  full_join(landlord_names, by = "landlord_name") |> 
  mutate(financialized = publicly_traded + direct_involvement_FM + 
           financial_partners) |> 
  select(-number_rental_units)

LL_analyzed <- 
  LL_2020_postal |> 
  full_join(LL_analyzed, by = "adresse_postale") |> 
  distinct(numero_matricule, .keep_all = TRUE)


# Get categories to classify remaining landlords --------------------------

# Individuals
individuals <- 
  LL_analyzed |> 
  filter(is.na(landlord_name), 
         number_rental_units > 0,
         statut_owner_1 == "Personne physique") |> 
  pull(numero_matricule)

# Cooperatives
cooperatives <-
  LL_analyzed |>  
  filter(is.na(landlord_name),
         number_rental_units > 0,
         statut_owner_1 == "Personne morale",
         str_detect(nom_1, "COOP|CO-OP|C0OP|CO OP")) |> 
  pull(numero_matricule)

# Owner-occupied housing
owner_occupier <- 
  LL_analyzed |> 
  filter(is.na(landlord_name), owner, nombre_logements == 1,
         statut_owner_1 == "Personne physique") |> 
  pull(numero_matricule)

# Non-housing
non_housing <- 
  LL_analyzed |> 
  filter(is.na(landlord_name), is.na(number_rental_units), 
         is.na(nombre_logements)) |> 
  pull(numero_matricule)

# Communautaire
communautaire <- 
  LL_analyzed |> 
  filter(is.na(landlord_name), number_rental_units > 0, 
         statut_owner_1 == "Personne morale", str_detect(nom_1, paste0(
           "COMMUNAUTAIRE|COMMUNAITAIRE|MAISON DALAUZE|AIDE REHABILITATION|",
           "CHARITY|CHEVALIERS|COMMUNAUTES|FONDATION|MAIS0NS TRANSITIONNELLES|",
           "OASIS POINTE ST-CHARLES|MAISON L ACCOLADE|POPULAIRE|SOEURS|",
           "SANCTUAIRE|RESEAU HABITATION FEMMES DE MONTREAL|JEUNESSE|JEUNES ",
           "FEMMES|TRANSITOIRE|MISSION OLD BREWERY|L'ESCALIER|BROTHERHOOD|",
           "SOCIETE D'HABITATION DU QUEBEC|SALVATION|SALUT|GREENBERG-MIRIAM|",
           "HANDICAPES"))) |> 
  pull(numero_matricule)

# Religious and government
religious_and_gvnmtl <- 
  LL_analyzed |> 
  filter(is.na(landlord_name), number_rental_units > 0,
         statut_owner_1 == "Personne morale", str_detect(nom_1, paste0(
           "TEMPLE|HOSPITAL|CIUSS|HOSPITALIER|CHARITY|HYDRO-QUEBEC|",
           "IDELOGICAL STUDIES|MISSIONNAIRES|CATHEDRAL|CONSERVATION DE LA ",
           "NATURE|SERVICES SCOLAIRE|MAISON DE JEUNES|RÉSEAU DE TRANSPORT ",
           "MÉTROPOLITAIN|EGLISE|ORATOIRE|REPUBLIC|CATHOLIQUE|CATHOLIC|",
           "RUSSIE|APOTRES|GOVERNMENT|REPUBLIQUE|INSTITUTION|GOUVERNEMENT|",
           "CONGREGATION|ESPAGNOL|ANGLICAN|DORVAL"))) |>
  pull(numero_matricule)


# Apply categories --------------------------------------------------------

LL_analyzed <-
  LL_analyzed |> 
  mutate(
    
    landlord_name = case_when(
      numero_matricule %in% c(individuals, cooperatives, owner_occupier, 
                              non_housing, communautaire, 
                              religious_and_gvnmtl) ~ nom_1,
      TRUE ~ landlord_name),
    
    type = case_when(
      numero_matricule %in% individuals ~ "Private",
      numero_matricule %in% cooperatives ~ "Cooperative",
      numero_matricule %in% owner_occupier ~ "Owner-occupier",
      numero_matricule %in% non_housing ~ "Non-housing",
      numero_matricule %in% communautaire ~ "Non-profit",
      numero_matricule %in% religious_and_gvnmtl ~ "Institutional",
      TRUE ~ type),
    
    company_type = case_when(
      numero_matricule %in% individuals ~ "Property management",
      numero_matricule %in% c(cooperatives, communautaire, 
                              religious_and_gvnmtl) ~ 
        "Non-profit property management",
      numero_matricule %in% owner_occupier ~ "Owner-occupier",
      numero_matricule %in% non_housing ~ "Other - non-housing",
      TRUE ~ company_type),
    
    publicly_traded = case_when(
      numero_matricule %in% c(individuals, cooperatives, owner_occupier,
                              communautaire, religious_and_gvnmtl) ~ FALSE,
      numero_matricule %in% non_housing ~ NA,
      TRUE ~ publicly_traded),
    
    direct_involvement_FM = case_when(
      numero_matricule %in% c(individuals, cooperatives, owner_occupier,
                              communautaire, religious_and_gvnmtl) ~ FALSE,
      numero_matricule %in% non_housing ~ NA,
      TRUE ~ direct_involvement_FM),
    
    financial_partners = case_when(
      numero_matricule %in% c(individuals, cooperatives, owner_occupier,
                              communautaire, religious_and_gvnmtl) ~ FALSE,
      numero_matricule %in% non_housing ~ NA,
      TRUE ~ financial_partners)
    
    )
  

# Create table for REQ scraping -------------------------------------------

req_names <- 
  LL_analyzed |> 
  filter(statut_owner_1 == "Personne morale", number_rental_units >= 1) |> 
  select(name = nom_1) |> 
  distinct()


# Save output -------------------------------------------------------------

qsavem(LL_2020, LL_analyzed, file = "output/LL.qsm", 
       nthreads = availableCores())
qsave(req_names, file = "output/req_names.qs", nthreads = availableCores())

rm(landlord_analysis, landlord_names, LL_2020_postal, communautaire,
   cooperatives, individuals, non_housing, owner_occupier, religious_and_gvnmtl)
