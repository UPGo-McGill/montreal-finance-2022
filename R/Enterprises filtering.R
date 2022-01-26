library(qs)

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

potential_financialized <- 
  full_parsed %>% 
  filter(!firm_type %in% non_financialized) %>% 
  filter(person == FALSE) %>% 
  #filter(str_detect(new_name, "[:upper:]\\,\\s[:upper]")) %>% 
  #filter(str_detect(new_name, "[:upper:]\\,\\s")) %>% 
  filter(!str_detect(str_to_lower(new_name), "family trust|fiducie")) 

potential_financialized_2 <- full_parsed

potential_financialized_2 <- 
  potential_financialized_2 %>% 
  rename(new_name1 = new_name,
         new_name = name)

joined_parsed <- left_join(potential_financialized, potential_financialized_2, by = "new_name") 

potential_financialized_3 <- full_parsed

potential_financialized_3 <- 
  potential_financialized_3 %>% 
  filter(person == FALSE) %>% 
  rename(new_name2 = new_name,
         new_name1 = name)

joined_parsed <- left_join(joined_parsed, potential_financialized_3, by = "new_name1") 

potential_financialized_4 <- full_parsed

potential_financialized_4 <- 
  potential_financialized_4 %>% 
  filter(person == FALSE) %>% 
  rename(new_name3 = new_name,
         new_name2 = name)

joined_parsed <- left_join(joined_parsed, potential_financialized_4, by = "new_name2") 

potential_financialized_5 <- full_parsed

potential_financialized_5 <- 
  potential_financialized_5 %>% 
  filter(person == FALSE) %>% 
  rename(new_name4 = new_name,
         new_name3 = name)

joined_parsed <- left_join(joined_parsed, potential_financialized_5, by = "new_name3") 

potential_financialized_6 <- full_parsed

potential_financialized_6 <- 
  potential_financialized_6 %>% 
  filter(person == FALSE) %>% 
  rename(new_name5 = new_name,
         new_name4 = name)

joined_parsed <- left_join(joined_parsed, potential_financialized_6, by = "new_name4") 

potential_financialized_7 <- full_parsed

potential_financialized_7 <- 
  potential_financialized_7 %>% 
  filter(person == FALSE) %>% 
  rename(new_name6 = new_name,
         new_name5 = name)

joined_parsed <- left_join(joined_parsed, potential_financialized_7, by = "new_name5") 

joined_parsed_clean <- 
  joined_parsed %>% 
  set_names(c("name", "firm_type", "activity", "person",
              "name2", "firm_type2", "activity2", "person2", 
              "name3", "firm_type3", "activity3", "person3", 
              "name4", "firm_type4", "activity4", "person4",
              "name5", "firm_type5", "activity5", "person5", 
              "name6", "firm_type6", "activity6", "person6",
              "name7", "firm_type7", "activity7", "person7", "final_name"))



joined_parsed %>% 
  filter(!is.na(new_name7))

joined_parsed_clean %>% 
  filter(person == FALSE) %>% 
  #filter(person2 == FALSE) %>% 
  count(name) %>% 
  View()





View(unique(full_parsed$firm_type))
