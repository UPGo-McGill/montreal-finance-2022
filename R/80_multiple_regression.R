#### 10 MULTIPLE REGRESSION 


# Multilinear regression analysis ----------------------------------------------

# Make a correlation matrix
library(Hmisc)
library(corrplot)

autocor <- na.omit(kmeans_CT %>% select(-GeoUID, -dwellings, -p_renter, -p_fin)) %>% 
  set_names(c("P. renters' housing stress", "Median rent", "Avg. value dwellings",
              "P. condos dwellings", "P. major repairs",
              "P. visible minorities", "P. immigrants", "P. one year mobility",
              "P. five years mobility", "P. dwellings in five+ storeys", "Median household income",
              "P. pop 18-24",
              "Change in renter dwellings", "Distance from downtown", "Asking rents"))

res <- cor(autocor)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

reg_kmeans_CT <- na.omit(kmeans_CT %>% select(-GeoUID, -dwellings, -p_renter)) %>% 
  set_names(c("P. renters' housing stress", "Median rent", "Avg. value dwellings",
              "P. condos dwellings", "P. major repairs",
              "P. visible minorities", "P. immigrants", "P. one year mobility",
              "P. five years mobility", "P. dwellings in five+ storeys", 
              "Median household income",
              "P. pop 18-24",
              "Change in renter dwellings", "Distance from downtown", 
              "Asking rents", "P. financialized rental ownership"))

reg1 <- lm(p_fin ~ p_stress + median_rent + p_condo + p_immigrants + p_vm +
             p_mobility_one_year + distance_dt + change_renter_dwellings + 
             asking_rent + avg_value, data=reg_kmeans_CT)
reg_kmeans_CT$reg1pred <- predict(reg1)
reg_kmeans_CT$reg1res <- rstandard(reg1)
summary(reg1)

reg2 <- lm(p_fin ~ p_condo + p_immigrants + p_mobility_one_year + 
             distance_dt + change_renter_dwellings + asking_rent, 
           data=reg_kmeans_CT)
reg_kmeans_CT$reg2pred <- predict(reg2)
reg_kmeans_CT$reg2res <- rstandard(reg2)
summary(reg2)

reg3 <- lm(p_fin ~ p_stress + median_rent + p_mobility_one_year + 
             p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg3pred <- predict(reg3)
reg_kmeans_CT$reg3res <- rstandard(reg3)
summary(reg3)

reg4 <- lm(p_fin ~ p_stress + median_rent + p_immigrants, 
           data=reg_kmeans_CT)
reg_kmeans_CT$reg4pred <- predict(reg4)
reg_kmeans_CT$reg4res <- rstandard(reg4)
summary(reg4)

reg5 <- lm(p_fin ~ p_stress + median_rent + p_immigrants +
             p_mobility_one_year + asking_rent, data=reg_kmeans_CT)
reg_kmeans_CT$reg5pred <- predict(reg5)
reg_kmeans_CT$reg5res <- rstandard(reg5)
summary(reg5)

reg6 <- lm(p_fin ~ p_stress + median_rent + p_mobility_one_year + 
             p_immigrants + avg_value, data=reg_kmeans_CT)
reg_kmeans_CT$reg6pred <- predict(reg6)
reg_kmeans_CT$reg6res <- rstandard(reg6)
summary(reg6)

reg7 <- lm(p_fin ~ p_stress + median_rent + p_mobility_one_year + 
             p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg7pred <- predict(reg7)
reg_kmeans_CT$reg7res <- rstandard(reg7)
summary(reg7)

reg8 <- lm(`Percent. of financialized landlords` ~ 
             `Percent. renters in housing stress` + `Median rent` + 
             `Percent. one year mobility` + 
             `Percent. immigrants` + 
             `Percent. dwellings in five+ storey buildings`, data=reg_kmeans_CT)
reg_kmeans_CT$reg8pred <- predict(reg8)
reg_kmeans_CT$reg8res <- rstandard(reg8)
summary(reg8)

reg9 <- lm(`Percent. of financialized landlords` ~ 
             `Percent. renters in housing stress` + `Median rent` + 
             `Percent. one year mobility` + 
             `Percent. immigrants` + 
             `Percent. dwellings in five+ storey buildings` + 
             `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg9pred <- predict(reg9)
reg_kmeans_CT$reg9res <- rstandard(reg9)
summary(reg9)

reg10 <- lm(`Percent. of financialized landlords` ~ 
              `Percent. renters in housing stress` + `Median rent` + 
              `Percent. one year mobility` + 
              `Percent. immigrants` + 
              `Percent. dwellings in five+ storey buildings` + 
              `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg10pred <- predict(reg10)
reg_kmeans_CT$reg10res <- rstandard(reg10)
summary(reg10)

reg11 <- lm(`Percent. of financialized landlords` ~ 
              `Percent. renters in housing stress` + `Median rent` + 
              `Five years mobility` + 
              `Percent. immigrants` + 
              `Percent. dwellings in five+ storey buildings` + 
              `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg11pred <- predict(reg11)
reg_kmeans_CT$reg11res <- rstandard(reg11)
summary(reg11)

reg12 <- lm(`P. financialized rental ownership` ~ 
              `P. renters' housing stress` + `Median rent` + 
              `P. one year mobility` + 
              `P. visible minorities` + `P. dwellings in five+ storeys` + 
              `P. pop 18-24`, data=reg_kmeans_CT)
reg_kmeans_CT$reg12pred <- predict(reg12)
reg_kmeans_CT$reg12res <- rstandard(reg12)
summary(reg12)


stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, 
          type="html", out="regFZ.html")
stargazer(reg8, reg9, reg10, reg11, reg12, type="html", out="regFZ2.1.html")
stargazer(reg9, type="html", out="regFZ9.html")
stargazer(reg10, type="html", out="regFZ10.html")
stargazer(reg12, type="html", out="regFZ12.html")

# Residuals plot --------------------------------------------------------

residuals <- 
  reg_kmeans_CT %>% 
  cbind(., na.omit(CT) %>% select(GeoUID)) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill=reg12res), color=NA)+
  scale_fill_gradient2(name="Residuals",
                       low = col_palette[3],
                       high = col_palette[1],
                       mid = "white", 
                       midpoint = 0,
                       limits=c(-2.5, 2.5),
                       oob=scales::squish)+
  gg_bbox(boroughs) +
  theme_void()

ggsave("output/figures/residuals.pdf", plot = residuals, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

