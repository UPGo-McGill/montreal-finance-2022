#### 04 BASIC DESCRIPTIVE ANALYSIS ########################################

source("R/01_startup.R")
load("output/LL.Rdata")

# GRAPH 1 - Number of properties built by year of construction ---------------------------------

LL_2020 %>% 
  filter(!is.na(nombre_logements)) %>% 
  filter(annee_construction >= 1800) %>% 
  group_by(annee_construction) %>%
  summarize(`Number of residential properties built`= n()) %>% 
  ggplot()+
  geom_point(aes(annee_construction, `Number of residential properties built`), colour=col_palette[1]) +
  xlab("Year of construction")+ 
  theme_minimal()


# GRAPH 2 - Number of logements by property ---------------------------------

LL_2020 %>% 
  filter(!is.na(nombre_logements)) %>% 
  group_by(nombre_logements) %>% 
  summarize(n=n()) %>% 
  filter(nombre_logements<1000, n<5000) %>% 
  ggplot()+
  geom_point(aes(nombre_logements, n), colour=col_palette[1])+
  xlab("Number of residential units")+ 
  ylab("Number of properties")+ 
  theme_minimal()

# Not a very conclusive graph...


# GRAPH 3 - Year of sale and Number of units in the property ------------------

LL_2020_test4 %>% 
  filter(!is.na(number_rental_units), number_rental_units >0) %>% 
  mutate(year_of_acquisition = floor_date(date_inscription, "year")) %>%
  group_by(date_inscription) %>% 
  summarize(n=sum(number_rental_units)) %>% 
  filter(date_inscription >= "01-01-1980", date_inscription <= "01-01-2021") %>% 
  ggplot()+
  geom_point(aes(date_inscription, n), colour=col_palette[1])+
  xlab("Date of last acquisition")+ 
  ylab("Number of rental units acquired")+ 
  theme_minimal()







