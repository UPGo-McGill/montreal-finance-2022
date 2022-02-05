#### 058 CMHC RENTS IMPORT #####################################################

source("R/01_startup.R")
library(readxl)


# Average rents 2019 ------------------------------------------------------

rents_2019 <- 
  read_excel("data/average_rents/2019_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2019") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2019 <- 
  rents_2019 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2018 <- 
  read_excel("data/average_rents/2018_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2018") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2018 <- 
  rents_2018 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2017 <- 
  read_excel("data/average_rents/2017_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2017") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2017 <- 
  rents_2017 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2016 <- 
  read_excel("data/average_rents/2016_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2016") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2016 <- 
  rents_2016 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2015 <- 
  read_excel("data/average_rents/2015_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2015") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2015 <- 
  rents_2015 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2014 <- 
  read_excel("data/average_rents/2014_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2014") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2014 <- 
  rents_2014 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2013 <- 
  read_excel("data/average_rents/2013_rents.xlsx") %>% 
  select(-Province, -Centre, -"...6") %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2013") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2013 <- 
  rents_2013 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2012 <- 
  read_excel("data/average_rents/2012_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2012") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2012 <- 
  rents_2012 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2011 <- 
  read_excel("data/average_rents/2011_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2011") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2011 <- 
  rents_2011 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2010 <- 
  read_excel("data/average_rents/2010_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("GeoUID", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(prefix = "462", year="2010") %>% 
  unite(prefix, GeoUID, col=GeoUID, sep = "", remove=TRUE) %>% 
  as_tibble()

rents_total_2010 <- 
  rents_2010 %>% 
  filter(dwelling_type == "Total") %>% 
  select(GeoUID, total, year)

rents_2009 <- 
  read_excel("data/average_rents/2009_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("region_name", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(year="2009") %>% 
  as_tibble()

rents_total_2009 <- 
  rents_2009 %>% 
  filter(dwelling_type == "Total") %>% 
  select(region_name, total, year)

rents_2008 <- 
  read_excel("data/average_rents/2008_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("region_name", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(year="2008") %>% 
  as_tibble()

rents_total_2008 <- 
  rents_2008 %>% 
  filter(dwelling_type == "Total") %>% 
  select(region_name, total, year)

rents_2007 <- 
  read_excel("data/average_rents/2007_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("region_name", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(year="2007") %>% 
  as_tibble()

rents_total_2007 <- 
  rents_2007 %>% 
  filter(dwelling_type == "Total") %>% 
  select(region_name, total, year)

rents_2006 <- 
  read_excel("data/average_rents/2006_rents.xlsx") %>% 
  select(-Province, -Centre) %>% 
  set_names(c("region_name", "dwelling_type", "bachelor", "one_bedroom", "two_bedrooms", "three_bedrooms", "total")) %>% 
  mutate(bachelor = ifelse(bachelor == "**", NA, bachelor),
         bachelor = ifelse(bachelor == "--", NA, bachelor),
         one_bedroom = ifelse(one_bedroom == "**", NA, one_bedroom),
         one_bedroom = ifelse(one_bedroom == "--", NA, one_bedroom),
         two_bedrooms = ifelse(two_bedrooms == "**", NA, two_bedrooms),
         two_bedrooms = ifelse(two_bedrooms == "--", NA, two_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "**", NA, three_bedrooms),
         three_bedrooms = ifelse(three_bedrooms == "--", NA, three_bedrooms),
         total = ifelse(total == "**", NA, total),
         total = ifelse(total == "--", NA, total)) %>% 
  mutate(bachelor = as.numeric(str_replace(bachelor, pattern = "\\$", replacement = "")),
         one_bedroom = as.numeric(str_replace(one_bedroom, pattern = "\\$", replacement = "")),
         two_bedrooms = as.numeric(str_replace(two_bedrooms, pattern = "\\$", replacement = "")),
         three_bedrooms = as.numeric(str_replace(three_bedrooms, pattern = "\\$", replacement = "")),
         total = as.numeric(str_replace(total, pattern = "\\$", replacement = ""))) %>% 
  mutate(dwelling_type = ifelse(dwelling_type == "Row / En\r\nbande", "Row Housing", dwelling_type),
         dwelling_type = ifelse(dwelling_type == "Apt & Other /\r\nApp. &\r\nautres", "Appartment and other", dwelling_type)) %>%
  mutate(year="2006") %>% 
  as_tibble()

rents_total_2006 <- 
  rents_2006 %>% 
  filter(dwelling_type == "Total") %>% 
  select(region_name, total, year)

rents_decade <-  
  rbind(rents_total_2019, rents_total_2018, rents_total_2017, rents_total_2016,
        rents_total_2015, rents_total_2014, rents_total_2013, rents_total_2012,
        rents_total_2011, rents_total_2010)

# Doing this for every year

rents_total_2019 <- 
  rents_total_2019 %>% 
  rename(rent_2019 = total) %>% 
  select(-year)

rents_total_2018 <- 
  rents_total_2018 %>% 
  rename(rent_2018 = total) %>% 
  select(-year)

rents_total_2017 <- 
  rents_total_2017 %>% 
  rename(rent_2017 = total) %>% 
  select(-year)

rents_total_2016 <- 
  rents_total_2016 %>% 
  rename(rent_2016 = total) %>% 
  select(-year)

rents_total_2015 <- 
  rents_total_2015 %>% 
  rename(rent_2015 = total) %>% 
  select(-year)

rents_total_2014 <- 
  rents_total_2014 %>% 
  rename(rent_2014 = total) %>% 
  select(-year)

rents_total_2013 <- 
  rents_total_2013 %>% 
  rename(rent_2013 = total) %>% 
  select(-year)

rents_total_2012 <- 
  rents_total_2012 %>% 
  rename(rent_2012 = total) %>% 
  select(-year)

rents_total_2011 <- 
  rents_total_2011 %>% 
  rename(rent_2011 = total) %>% 
  select(-year)

rents_total_2010 <- 
  rents_total_2010 %>% 
  rename(rent_2010 = total) %>% 
  select(-year)

rents_CT <- 
  reduce(list(rents_total_2019, rents_total_2018, rents_total_2017,
              rents_total_2016, rents_total_2015, rents_total_2014,
              rents_total_2013, rents_total_2012, rents_total_2011,
              rents_total_2010), full_join, by = "GeoUID")

rents_CT <- 
  rents_CT %>% 
  mutate(p_change_2015_2019 = (rent_2019-rent_2015)/rent_2015,
         p_change_2010_2019 = (rent_2019-rent_2010)/rent_2010) 

qsavem(rents_CT, rents_decade, file = "output/rents_cmhc.qsm",
       nthreads = availableCores())
