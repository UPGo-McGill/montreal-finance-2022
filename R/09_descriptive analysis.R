#### 04 BASIC DESCRIPTIVE ANALYSIS ########################################

source("R/01_startup.R")
qload("output/LL.qsm", nthreads = availableCores())


# Number of properties built by year of construction ----------------------

LL_2020 |> 
  filter(!is.na(nombre_logements), annee_construction >= 1920) |> 
  group_by(annee_construction) |> 
  summarize(`Number of residential units built`=sum(nombre_logements)) |> 
  ggplot(aes(x = annee_construction, y = `Number of residential units built`)) +
  geom_line(color = col_palette[1]) +
  geom_smooth(se = FALSE, color = col_palette[2]) +
  xlab("Year of construction") + 
  scale_y_continuous(name = "Number of units built", label = scales::comma) +
  theme_minimal()


# Ownership concentration in the city -------------------------------------

rental_concentration <- 
  LL_analyzed |> 
  filter(!is.na(number_rental_units), number_rental_units > 0) |> 
  group_by(landlord_name) |> 
  summarize(n = sum(number_rental_units))

rental_concentration_500 <- 
  LL_analyzed |> 
  filter(!is.na(number_rental_units), number_rental_units > 0) |> 
  group_by(landlord_name) |> 
  summarize(n = sum(number_rental_units)) |> 
  arrange(desc(n)) |> 
  slice(1:500)

rental_concentration |> 
  count(n, name = "number_landlords") |> 
  ggplot() +
  geom_line(aes(x = n, y = number_landlords)) +
  xlim(c(0, 100))

# Average units owned
mean(rental_concentration$n) 
mean(rental_concentration_500$n) 

# Top 10%
rental_concentration |> 
  summarize(top_10 = sum(n[n > quantile(n, .9)]) / sum(n))

# Top 5%
rental_concentration |> 
  summarize(top_5 = sum(n[n > quantile(n, .95)]) / sum(n))

# Top 1%
rental_concentration |> 
  summarize(top_1 = sum(n[n > quantile(n, .99)]) / sum(n))

landlord_repartition <- 
  rental_concentration %>%
  rename(rev=n) %>% 
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 
                                        'Top 20%'))) %>% 
  ggplot() +
  geom_bar(aes(percentile, value, fill = percentile), stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = col_palette[1:4]) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(face = "plain"),
        legend.title = element_text(face = "bold",
                                    size = 10),
        legend.text = element_text(size = 10),
        legend.position = "none")


# Ownership by type of landlord -------------------

sum(LL_analyzed$number_rental_units, na.rm=TRUE)
sum((LL_analyzed %>% filter(landlord_rank <= 1000))$number_rental_units, na.rm = TRUE)
sum((LL_analyzed %>% filter(landlord_rank > 500))$number_rental_units, na.rm = TRUE)

top_500_landlords <- 
  LL_analyzed %>%
  filter(landlord_rank <= 500) %>% 
  group_by(landlord_name) %>% 
  summarize(sum_rentals=sum(number_rental_units, na.rm=TRUE))

mean(top_500_landlords$sum_rentals, na.rm = TRUE)

LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  group_by(landlord_name, type) %>% 
  summarize(number_landlords = n(),
            n=sum(number_rental_units))%>% 
  mutate(type = ifelse(type == "Private" & n > 5, "Private, more than\nfive units", type)) %>%
  mutate(type = ifelse(type == "Private" & n <= 5, "Private, five or\nless units", type)) %>% 
  group_by(type) %>% 
  summarize(landlords=sum(n()),
            total_rentals=sum(n),
            all=sum(total_rentals/609411*100))

rental_type <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  mutate(type = ifelse(is.na(type), "Private", type)) %>% 
  group_by(landlord_name, type) %>% 
  summarize(n=sum(number_rental_units)) %>% 
  mutate(type = ifelse(type == "Private" & n > 5, "Private, more than\nfive units", type)) %>%
  mutate(type = ifelse(type == "Private" & n <= 5, "Private, five or\nfewer units", type)) %>% 
  group_by(type) %>% 
  summarize(total_rentals=sum(n),
            total_rentals=sum(n),
            all=sum(total_rentals/609411)) %>% 
  mutate(all = scales::percent(all, suffix = " %", decimal.mark = ",")) %>% 
  ggplot()+
  geom_col(aes(x=type, y=total_rentals), fill=col_palette[c(1:6)])+
  geom_text(aes(x=type, y=total_rentals+8000, label=all)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_discrete(name=NULL) +
  theme_minimal()

ggsave("output/figures/rental_type.pdf", plot = rental_type, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


# Landlord ownership table -------------------

top_landlords <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  group_by(landlord_name) %>% 
  summarize(rental_units = sum(number_rental_units)) %>% 
  arrange(desc(rental_units)) %>% 
  slice(1:50) %>%
  inner_join(., LL_analyzed %>% select(landlord_name, company_type, location_HO), by="landlord_name") %>% 
  distinct(landlord_name, .keep_all=TRUE)

write_csv(top_landlords, "data/top_landlords.csv")


# Top50 landlords and geographic repartition -------------------

# top50 <- 
#   LL_analyzed %>% 
#   group_by(landlord_name) %>% 
#   summarize(rentals=sum(number_rental_units, na.rm = TRUE)) %>% 
#   arrange(desc(rentals)) %>% 
#   slice(1:50) %>% 
#   pull(landlord_name)
# 
# top50_map <- 
#   LL_sf_centroid %>% 
#   filter(landlord_name %in% top50) %>% 
#   mutate(type = ifelse(company_type=="REIT", "REIT", type)) %>% 
#   ggplot()+
#   geom_sf(data = boroughs, fill=NA, color="grey80")+
#   geom_sf(aes(color=type, size=number_rental_units))+
#   facet_wrap(~type)+
#   scale_colour_manual(name = "Landlord type", values = col_palette[c(1, 6, 3, 4, 2, 5)])+
#   scale_size_continuous(name = "Number of rental units", limits = c(1, 3200), 
#                         breaks = c(25, 50, 100, 300))+
#   theme_void()
# 
# ggsave("output/figures/top50_map.pdf", plot = top50_map, width = 8, 
#        height = 5, units = "in", useDingbats = FALSE)


# Percentage of financialized ownership -------------------

proportion_financialized <- 
  LL_sf_centroid %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  mutate(financialized = publicly_traded+direct_involvement_FM+financial_partners) %>% 
  mutate(financialized = ifelse(financialized > 0, TRUE, FALSE)) %>% 
  st_intersection(., CT) %>% 
  st_drop_geometry() %>% 
  group_by(GeoUID, financialized) %>% 
  summarize(rental_units=sum(number_rental_units))

percentage_financialized <- 
  proportion_financialized %>% 
  group_by(GeoUID) %>% 
  summarise(total=sum(rental_units, na.rm=TRUE)) %>% 
  full_join(., proportion_financialized, by="GeoUID") %>% 
  mutate(p_financialized=rental_units/total) %>% 
  filter(financialized == TRUE) %>% 
  select(GeoUID, p_financialized)

financialized_ownership <- 
  CT %>% 
  left_join(., percentage_financialized, by="GeoUID") %>% 
  mutate(p_financialized=ifelse(is.na(p_financialized), 0, p_financialized)) %>% 
  ggplot()+
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill=p_financialized), color=NA)+
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3)+
  scale_fill_stepsn(name="Percentage of\nfinancialized\nrental\nownership", 
                    colors = col_palette[c(4, 1, 2, 9)],
                    breaks = c(0.1, 0.20, 0.40, 0.60),
                    #values = c(0.2, 0.4, 0.6),
                    na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    labels = scales::percent)+
  gg_bbox(boroughs) +
  theme_void()

fo_zoom <- 
  financialized_ownership +
  geom_sf(data = streets_downtown, size = 0.3, colour = "white") +
  coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

inset_financialized_ownership <- 
  financialized_ownership + 
  inset_element(fo_zoom, left = 0, bottom = 0.5, right = 0.6, top = 1)


ggsave("output/figures/inset_financialized_ownership.pdf", plot = inset_financialized_ownership, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


# Acquisitions made by current financialized landlords in Montreal by ------
# number of rental units and type of financialized landlord ----------------

total_acquisitions <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  mutate(date_inscription = year(floor_date(date_inscription, "year"))) %>% 
  filter(date_inscription != annee_construction) %>% 
  group_by(date_inscription) %>% 
  summarise(total_rental_units=sum(number_rental_units)) 

acquisitions <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  mutate(financialized = publicly_traded+direct_involvement_FM+financial_partners) %>% 
  mutate(date_inscription = year(floor_date(date_inscription, "year"))) %>% 
  filter(date_inscription != annee_construction) %>% 
  filter(financialized > 0) %>% 
  group_by(date_inscription) %>% 
  summarise(number_rental_units=sum(number_rental_units)) %>% 
  mutate(all_reit = "All financialized landlords")

acquisitions_REIT <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  mutate(financialized = publicly_traded+direct_involvement_FM+financial_partners) %>% 
  mutate(date_inscription = year(floor_date(date_inscription, "year"))) %>% 
  filter(date_inscription != annee_construction) %>% 
  filter(financialized > 0) %>%
  filter(company_type=="REIT") %>% 
  group_by(date_inscription) %>% 
  summarise(number_rental_units=sum(number_rental_units))%>% 
  mutate(all_reit = "REITs only")

acquisitions_graph <- 
  rbind(acquisitions, acquisitions_REIT)

acquisitions_gpglot <- 
  acquisitions_graph %>% 
  full_join(., total_acquisitions, by="date_inscription") %>% 
  mutate(percent_acquisition = number_rental_units/total_rental_units) %>% 
  filter(date_inscription > 1986) %>% 
  View()
  ggplot()+
  geom_line(aes(date_inscription, percent_acquisition, color=all_reit, size=all_reit))+
  #geom_smooth(aes(date_inscription, percent_acquisition, color=all_reit), se=FALSE)+
  scale_y_continuous(name = "Percentage of rental units acquired", label = scales::percent) +
  scale_x_continuous(name = NULL)+
  scale_colour_manual(name = NULL, values = col_palette[c(1, 2)]) +
  scale_size_manual(name = NULL, values = c("All financialized landlords" = 1.25, 
                               "REITs only" = 0.75)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())


ggsave("output/figures/acquisitions_gpglot.pdf", plot = acquisitions_gpglot, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


# Construction of rental stock by year and financialized level ------

new_c_fz <- 
  LL_analyzed %>% 
  filter(!is.na(number_rental_units)) %>% 
  filter(number_rental_units > 0) %>% 
  filter(annee_construction >= 1990) %>% 
  mutate(financialized = publicly_traded+direct_involvement_FM+financial_partners) %>% 
  #mutate(date_inscription = year(floor_date(date_inscription, "year"))) %>%
  #mutate(new_construction = ifelse(annee_construction == date_inscription, TRUE, FALSE)) %>% 
  #mutate(new_construction = ifelse(annee_construction > date_inscription, TRUE, new_construction)) %>% 
  #filter(new_construction == TRUE) %>% 
  mutate(financialized = ifelse(financialized > 0, "Financialized", "Non-financialized")) %>% 
  group_by(annee_construction, financialized) %>% 
  summarize(number_rental_units=sum(number_rental_units)) %>% 
  filter(financialized == "Financialized") %>% 
  ggplot()+
  geom_line(aes(annee_construction, number_rental_units), color=col_palette[9], size=1) +
  scale_y_continuous(name = "Number of rental units built by financialized landlords", label = scales::comma) +
  scale_x_continuous(name = NULL)+
  scale_colour_manual(name = NULL, values = col_palette[c(9, 3)]) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())


ggsave("output/figures/new_c_fz.pdf", plot = new_c_fz, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

