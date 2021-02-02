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
  select(-c(date_rapport, genre_construction, info_en_date, statut_owner_8, statut_owner_9)) %>% 
  mutate(annee_construction = as.numeric(sub(" .*", "", annee_construction)),
         date_inscription = as.Date(date_inscription, format="%d-%m-%Y"),
         date_ref_marche_ant = as.Date(date_ref_marche_ant, format="%d-%m-%Y"),
         date_ref_marche_courant = as.Date(date_ref_marche_courant, format="%d-%m-%Y"))


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
  group_by(adresse_postale) %>% 
  summarize(nombre_unite_totales=sum(nombre_logements, na.rm = TRUE),
            nombre_loca = sum(number_rental_units, na.rm = TRUE),
            nombre_chambres=sum(nombre_chambres_locatives, na.rm = TRUE)) %>% 
  arrange(desc(nombre_loca)) %>% 
  mutate(landlord_rank = as.character(1:363603)) 

LL_2020_test4 <- 
  left_join(LL_2020_test2, LL_2020_test3, by = "adresse_postale") %>% 
  mutate(landlord_name = landlord_rank) %>% 
  select(adresse, adresse_postale, landlord_rank, landlord_name, everything())
  

# The following analysis code looks at the companies that get combined with a group_by that combines
# entries that have both the same name AND postal address. The second group_by combines all the
# names that were not grouped together because they did not have the same postal address for both.
# A careful analysis of these entries that operated with more than two postal addresses and owned, 
# when all the subsidiaries combined, more than 10 units were analyzed. Most were people with the 
# same name (i.e. Andre Tremblay), but the ones with company names will be analyzed for a more 
# accurate breakdown of the number of units owned by landlords.

# Modify names with slight string differences -------------------------------------------

# Combining the dataset by postal address gives us a great sense of the companies that use
# multiple names or company subsidiaries to operate in Montreal. Now, it is time to both correct
# the companies that are owned by the same stakeholders but operated under different postal addresses. 
# Next, we will give a clear name of the biggest landlords in Montreal (for the ones whose used name
# is not of recognessence to anyone, but are well known under another name). The list will be long, but 
# essential for an exhaustive analysis. Such modifications are based on investigative work from my 
# end, with links provided when pertinent.

LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "1"] <- "Office municipal d'habitation de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "2"] <- "Office municipal d'habitation de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "3"] <- "CAPREIT"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "4"] <- "Akelius"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "5"] <- "Boardwalk REIT"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "6"] <- "Societe d'habitation et de developpement de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "7"] <- "Selection retraite"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "8"] <- "InterRent REIT"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "9"] <- "Turret Management Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "10"] <- "Hapopex"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "11"] <- "Northview"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "12"] <- "Fondation Luc Maurice" # Groupe Maurice
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "13"] <- "Les Residences Soleil"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "14"] <- "Oxford Properties Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "15"] <- "Federal Real Estate"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "16"] <- "Cromwell Management"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "17"] <- "Immoappart"  #this is a real estate MGMT company managing other ppl's units
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "18"] <- "Groupe Laberge"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "19"] <- "Cogir Real Estate GP"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "20"] <- "RAAMCO International Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "21"] <- "Placements LLC Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "22"] <- "Empire Building Management Inc" #Joe Teller
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "23"] <- "IG Wealth Management/MINTO Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "24"] <- "IMMO 1ere"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "25"] <- "Societe d'habitation populaire de l'Est de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "26"] <- "TKTK"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "27"] <- "Cooperative d'habitation Pointe-St-Charles"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "28"] <- "Immeubles Agostino"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "29"] <- "Batir son quartier"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "30"] <- "Groupe Canvar Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "31"] <- "Aquilini Investment Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "32"] <- "Gestion Immobiliere Langlois"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "33"] <- "Joe Caprera Inc/Saxxcrop"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "34"] <- "Gestion des Trois Pignons Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "35"] <- "Andre Nault"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "36"] <- "Hazelview Properties (Timbercreek)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "37"] <- "Placements Sergakis"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "38"] <- "Immeubles Howard Szalavetz Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "39"] <- "Greenwin"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "40"] <- "Gestion Immopolis"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "41"] <- "Groupe Maxera" # Migliara Family
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "42"] <- "Fairway Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "43"] <- "The Dorchester Corporation"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "44"] <- "Caldwell Residences"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "45"] <- "Luger Group/York Holdings"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "46"] <- "Federal Construction Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "47"] <- "Jean Durocher (subs)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "48"] <- "Gestion Lameer Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "49"] <- "Rester Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "50"] <- "Gestion XX Immobiliere"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "51"] <- "Mecyva Realties Inc (Max Latifi)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "52"] <- "Federation des OSBL d'habitation de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "53"] <- "Westmount Estates Management"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "54"] <- "Creccal Investments Ltd"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "55"] <- "Residences au fil de l'eau" # Migliara Family
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "56"] <- "Les immeubles Charles Choucair"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "57"] <- "Fairway Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "58"] <- "Residence Cite-Rive"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "59"] <- "Office municipal d'habitation de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "60"] <- "Cogeim (Acmon)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "61"] <- "IG Wealth Management/MINTO Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "62"] <- "Hazelview Properties (Timbercreek)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "63"] <- "Daniel Germain"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "64"] <- "Groupe Accueil"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "65"] <- "Robert Levac"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "66"] <- "CIF Properties"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "67"] <- "Rakotta"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "68"] <- "Capital Augusta"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "69"] <- "Hotels Gouverneur"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "70"] <- "Cooperative d'habitation Village Cloverdale"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "71"] <- "Recan/Developpement Plazacan Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "72"] <- "M Carre"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "73"] <- "Immoappart" # Real estate management company managing other ppl's units
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "74"] <- "Jopeca Management/Morris family/Matna Investments"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "75"] <- "GWL Realty Advisors"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "76"] <- "Stevens Coulombe"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "77"] <- "Groupe Leduc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "78"] <- "Tours Gouin"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "79"] <- "Hazelview Properties (Timbercreek)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "80"] <- "Interloge"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "81"] <- "Immeubles Howard Szalavetz Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "82"] <- "Hillpark Capital"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "83"] <- "Cominar"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "84"] <- "Discepola Family"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "85"] <- "Les Immeubles Blouin SENC"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "86"] <- "Tandel Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "87"] <- "Placements Parthenais-Messier Inc" # Baruch Posner, Jay Hunt
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "88"] <- "Alfid Group" # property MGMT company
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "89"] <- "Prets Via" # Claude Parent Cimon Gail
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "90"] <- "Groupe Denux"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "91"] <- "Residence Portofino Inc" # Famille Venturelli, Famille Paolo Pina
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "92"] <- "Cromwell Management" #https://www.journaldemontreal.com/2019/10/12/une-vue-du-fleuve-a-89-millions
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "93"] <- "Paul Lagace et Diane Gaudreau"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "94"] <- "ELAD Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "95"] <- "Realstar Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "96"] <- "Alfid Group" # Property MGMT company
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "97"] <- "Otera Capital" # Units owned by Alfonso Graceffa, Thomas Marcantonio and others // CDPQ
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "98"] <- "Placements Sommet"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "99"] <- "Cogefimo"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "100"] <- "Immeubles Regentor/Gestion Immoparc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "101"] <- "Sabatino Caprera" #Joe Caprera Inc/Saxxcrop??
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "102"] <- "Michael Abraham Oliel"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "103"] <- "Cooperative d'habitation Village Cloverdale"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "104"] <- "Entreprises Sacks Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "105"] <- "Israel Freundlich" #https://www.lapresse.ca/actualites/grand-montreal/201806/20/01-5186619-un-proprietaire-de-taudis-arnaque-des-migrants.php
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "106"] <- "Lacopar Inc" #https://montreal.ctvnews.ca/ndg-tenants-feeling-the-chill-1.746913
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "107"] <- "Viglione Properties"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "108"] <- "Chartwell"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "109"] <- "Melatti"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "110"] <- "Group Properties Azzouz Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "111"] <- "TRAMS Property Management"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "112"] <- "Richard Majewski" #Trylon Apartments
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "113"] <- "Lieberman Family" #Groupe Lamour
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "114"] <- "Ahmad Chaar"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "115"] <- "Salim Fattal" #https://www.lapresse.ca/actualites/grand-montreal/201204/19/01-4516908-logements-insalubres-une-sale-affaire-a-montreal.php
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "116"] <- "Vincenzo Barrasso" #https://www.thestar.com/news/gta/2019/02/08/former-worst-landlord-ends-up-in-citys-good-books-with-program-to-revamp-underused-amenities-spaces.html
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "117"] <- "Lesniak Family"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "118"] <- "Gestion Immobiliere Pare et Charron"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "119"] <- "Trianon Properties"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "120"] <- "Proprietes Bella Vita Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "121"] <- "Casco Apartments/Gustav Levinschi Foundation"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "122"] <- "Groupe Karmel Inc" #THE GUIRAGOSSIAN FAMILY TRUST
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "123"] <- "Marlin Spring"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "124"] <- "Developpements Nelligan Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "125"] <- "Groupe Petra/HPDG Associes Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "126"] <- "Jean-Yves Roy"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "127"] <- "Soeurs de la charite Sainte-Marie"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "128"] <- "Cite des Retraites NDF Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "129"] <- "Societe d'habitation du Quebec"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "130"] <- "Peter Skierka/Joseph Bensimon" #https://globalnews.ca/news/4080056/duff-court-tenants-in-lachine-angry-over-apartment-conditions/
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "131"] <- "SCHL/CMHC"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "132"] <- "Habitations communautaires de Cote-des-Neiges"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "133"] <- "Zimmerman Properties" #John Zimmerman
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "134"] <- "The Royal Charter of McGill University"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "135"] <- "SCHL/CMHC"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "136"] <- "Aquilini Investment Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "137"] <- "Les habitations du centre-ville"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "138"] <- "La corporation Headway"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "139"] <- "Layher Family"  #Margarete et Elisabeth 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "140"] <- "Immeubles Grondin"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "141"] <- "Plan A"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "142"] <- "Lazar Equities" #Mark Lazar
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "143"] <- "Investissement Immobilier CCSM Ltee"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "144"] <- "Residences les Jardins Millen Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "145"] <- "Sec Square Phillips" # Jean-Rene Fournelle Inc
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "146"] <- "Royal European Investments" #Moshe Englander and Ben Zion Weiss
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "147"] <- "Axwell Management"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "148"] <- "Forum Realties"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "149"] <- "Atwater Properties"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "150"] <- "Rossdeutscher Family" # Lionel, Charles https://www.thesuburban.com/news/city_news/city-positioned-to-sue-delinquent-cdn-landlords/article_37a617c7-db52-5581-b990-eecb95a128dd.html
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "151"] <- "Jean-Noel Goupil et Francine Nadeau" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "152"] <- "Office municipal d'habitation de Montreal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "153"] <- "Stephane Sheppard and Jessica Arriaga"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "154"] <- "Appartements BL Inc" # Famille Lessard
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "155"] <- "FECHIMM" # Coop
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "156"] <- "Hazelview Properties (Timbercreek)" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "157"] <- "Nelson Family"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "158"] <- "Azim Lalani" #Owns a hotel REIT too
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "159"] <- "Ecole de Technologie Superieure"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "160"] <- "Hazout Group Inc" #HP et Associes
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "161"] <- "James Bond"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "162"] <- "Frank Maluorni"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "163"] <- "Mondev"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "164"] <- "Gad Bitton and Michael Serruya" #BITTON FAMILY TRUST
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "165"] <- "Residences les Deux Aires Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "166"] <- "Fondation Luc Maurice" #Groupe Maurice
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "167"] <- "Societe d'habitation du Quebec"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "168"] <- "Habibur Rahman" #https://www.cbc.ca/news/canada/montreal/fire-rooming-house-ndg-cdn-1.4642635
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "169"] <- "Gestion Immobiliere Progim"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "170"] <- "Gelber Family" #Gelber Family Foundation
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "171"] <- "John Herzog" #Impenco Lte
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "172"] <- "Selection retraite" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "173"] <- "Francois Brais" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "174"] <- "Alberta Investment Management Corporation"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "175"] <- "Devimco"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "176"] <- "Real Capital Group" #Multicapital, Anthony Garone
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "177"] <- "Mercadante Family" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "178"] <- "David and Boris Daych" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "179"] <- "Fondation d'amenagement Saint Patrick" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "180"] <- "Jonathan Mutch" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "181"] <- "Emile Ghattas" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "182"] <- "Joseph Bultz" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "183"] <- "Immeubles Howard Szalavetz Inc" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "184"] <- "Cooperative de solidarite fusion verte" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "185"] <- "Les soeurs sainte-marcelline" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "186"] <- "Northview" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "187"] <- "Creccal Investments Ltd" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "188"] <- "Habitations la Traversee"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "189"] <- "Placements Francois Nault Ltee"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "190"] <- "Norman Gordon"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "191"] <- "Cora Cohen"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "192"] <- "Turek Family"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "193"] <- "BentallGreenOak"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "194"] <- "Hubert Thouin"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "195"] <- "Lambert Boileau" #Nathalie Laporte
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "196"] <- "Iber Immobilier"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "197"] <- "Boileau Family"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "198"] <- "Residences les retrouvailles"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "199"] <- "Gestion immobiliere Altima Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "200"] <- "Les developpements Immobilis Inc" #Andre Parenteau
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "201"] <- "Cooperative d'habitation terrasse soleil"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "202"] <- "Chartwell" #Welltower
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "203"] <- "Aldo Coviello Junior" #Aldo construction
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "204"] <- "Louise Martel and Mikhail Perun" #Groupe immobilier pertel Inc
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "205"] <- "Gestion Paul Motor"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "206"] <- "Douglas Cohen"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "207"] <- "Habitations communautaires Mainbourg"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "208"] <- "Placements Louis-Philippe Michaud"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "209"] <- "Cote Saint-Luc Building Corporation" #Gerald Issenman and Judy Sigler
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "210"] <- "Immobilier Yuliv Inc" #Ali and Yousef Farasat
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "211"] <- "Uqam"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "212"] <- "Entreprises Jafec Inc" #Guy Lavoie
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "213"] <- "Trustcan Real Estate Management"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "214"] <- "Tidan Inc"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "215"] <- "Cogir Real Estate GP"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "216"] <- "Mackroc Corporation" #Mahfuzur Ullah and Rahi Kobra
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "217"] <- "Empire Building Management Inc" #Joe Teller
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "218"] <- "RFA"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "219"] <- "Longpre Family" #Pierre, Lucie, Diane
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "220"] <- "Immeubles Centraux"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "221"] <- "Frank Scarpelli"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "222"] <- "Port Royal Apartments Inc" #Mitzi and Mel Dobrin
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "223"] <- "Marlin Spring"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "224"] <- "Frederic Aubry"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "225"] <- "Chartwell"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "226"] <- "Normand Fauteux"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "227"] <- "Gestion immobiliere Norrach" # Charron Family
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "228"] <- "Jean-Pierre Lefebvre"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "229"] <- "Gaetan Brouillard" #and al
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "230"] <- "Realstar Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "231"] <- "Antonio Lucifero"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "232"] <- "Alberta Investment Management Corporation"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "233"] <- "Igor Ezril"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "234"] <- "Ian Binstock"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "235"] <- "Habitations communautaires Loggia"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "236"] <- "Joe Caprera Inc/Saxxcrop"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "237"] <- "HVS" #Selim El Zyr, Sami Wadih Sidawi, Naser Mohamed Alnawais in the UAE https://www.hvs.com/news/8583/hvs-completes-acquisition-of-residence-inn-downtown-montreal-on-behalf-of-private-client
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "238"] <- "John Antoniou"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "239"] <- "Foyer Hongrois" #NFP org
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "240"] <- "Allan Fainman" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "241"] <- "Groupe Denux" #Marie Denux
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "242"] <- "Capital Augusta" #Eric Audet
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "243"] <- "Edward Hayes" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "244"] <- "Gestion Immobiliere Progim" #SOCIÉTÉ IMMOBILIÈRE L.G.J. POULIN S.E.N.C., Gaston Dagenais, Richard Monette 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "245"] <- "Nikpour Ismail Zadeh" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "246"] <- "Jean-Mercier Leduc" #Not related to Groupe Leduc
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "247"] <- "Carmine Latella" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "248"] <- "Scalia Family" #Salvatore et Pasquale 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "249"] <- "Daniel Desjardins" #And associates 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "250"] <- "Michael Fischler"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "251"] <- "Daniel Dirzu"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "252"] <- "Raymond Girard"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "253"] <- "Wolfgang Rathgeber, Sandra Sherman and al"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "254"] <- "JPMorgan Chase"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "255"] <- "Carlos Quintal"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "256"] <- "Moise Chokron"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "257"] <- "Societe d'habitation Loge Henri"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "258"] <- "Northview"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "259"] <- "Leon and Michael Hirsch"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "260"] <- "Societe d'habitation du Quebec"

LL_2020_test4 %>% 
  filter(landlord_rank == 260) %>% 
  View()

LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "334"] <- "Hazelview Properties (Timbercreek)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "359"] <- "Hazelview Properties (Timbercreek)"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "577"] <- "Richard Majewski" #Trylon Apartments
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "589"] <- "Hazelview Properties (Timbercreek)" 
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "778"] <- "Richard Majewski" #Trylon Apartments
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "3816"] <- "Krishan Suri and Kira Suri"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "108579"] <- "IG Wealth Management/MINTO Group"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "224795"] <- "Chartwell"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "224796"] <- "Chartwell"
LL_2020_test4$landlord_name[LL_2020_test4$landlord_name == "255404"] <- "Suncor Energy"





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








