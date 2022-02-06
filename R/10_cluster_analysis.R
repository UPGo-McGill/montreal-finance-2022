#### 10 CLUSTER ANALYSIS #######################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())

library(cluster)
library(factoextra)
# library(gridExtra)
# library(ggpubr)

set.seed(2022)


# Prepare dataset_kmeans --------------------------------------------------

data_kmeans <- 
  data_CT |> 
  st_drop_geometry() |> 
  select(p_thirty_renter, median_rent, average_value_dwellings, p_condo,
         p_renter, p_repairs, p_vm, p_immigrants, p_mobility_one_year,
         p_mobility_five_years, p_five_more_storeys, med_hh_income, p_18_24,
         change_renter_dwellings, distance_dt, asking_rent) |> 
  na.omit() |> 
  scale() |> 
  as_tibble()


# Determine number of clusters --------------------------------------------

fviz_nbclust(data_kmeans, kmeans, method = "silhouette")
data_kmeans |>
  clusGap(FUN = kmeans, nstart = 25, K.max = 10, B = 50) |> 
  fviz_gap_stat()


# Compute k means with 5 clusters -----------------------------------------

k_result <- kmeans(data_kmeans, centers = 5, nstart = 25)
qsave(k_result, file = "output/k_result.qs")
fviz_cluster(k_result, data = data_kmeans)


# Add to data -------------------------------------------------------------

data_CT <- 
  data_CT |> 
  na.omit() |> 
  mutate(cluster = k_result$cluster,
         cluster = case_when(
           cluster == 1 ~ "Suburban non-financialized",
           cluster == 2 ~ "Immigrant periphery non-financialized",
           cluster == 3 ~ "Precarious and student financialized",
           cluster == 4 ~ "Gentrifying non-financialized",
           cluster == 5 ~ "Affluent financialized"), 
         cluster = factor(cluster, levels = c(
           "Precarious and student financialized", "Affluent financialized",
           "Suburban non-financialized", "Gentrifying non-financialized",
           "Immigrant periphery non-financialized")), .before = geometry)

data_building <- 
  data_building |> 
  left_join(data_CT |> 
              st_drop_geometry() |> 
              select(GeoUID, cluster), by = "GeoUID")



# Regression analyses -----------------------------------------------

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute((r)^2~"="~r2,
                   list(
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# Housing stress and financialized landlords

p1 <- 
  kmeans_CT %>% 
  left_join(., st_drop_geometry(CT_parent_vectors), by = "GeoUID") %>% 
  ggplot(aes(x=p_thirty_renter, y=p_financialized, size=parent_renter, alpha=parent_renter))+
  geom_point(color=col_palette[1])+
  geom_smooth(method="lm", se=FALSE, color="black")+
  scale_x_continuous(name = "Percentage of renters in housing stress",
                     label = scales::percent)+
  scale_y_continuous("Percentage of financialized\nrental ownership",
                     label = scales::percent, 
                     limits = c(-0.05, 1))+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide = "none")+
  theme_minimal()

p1 <- 
  p1 + stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  label.x = 0.01, label.y = 0.875
)


# Median rent and financialized landlords

p2 <- 
  kmeans_CT %>% 
  left_join(., st_drop_geometry(CT_parent_vectors), by = "GeoUID") %>% 
  ggplot(aes(x=median_rent, y=p_financialized, size=parent_renter, alpha=parent_renter))+
  geom_point(color=col_palette[2])+
  geom_smooth(method="lm", se=FALSE, color="black")+
  scale_x_continuous(name = "Median rent",
                     label = scales::dollar)+
  scale_y_continuous("Percentage of financialized\nrental ownership",
                     label = scales::percent, 
                     limits = c(-0.05, 1))+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide = "none")+
  theme_minimal()

p2 <- p2 + stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  label.x = 125, label.y = 0.875
)


# Average value of dwellings
# 
# p3 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=average_value_dwellings, y=p_financialized, alpha=dwellings, size=dwellings))+
#   geom_point(color=col_palette[6])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Average value of dwellings",
#                      label = scales::dollar)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p3 <- p3 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0, label.y = 0.875
# )
# # r squared is 0.055 == not included

# Change in renter dwellings and financialized landlords

# p4 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=change_renter_dwellings, y=p_financialized))+
#   geom_point(color=col_palette[9])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Change in the number of renter dwellings",
#                      label = scales::comma)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p4 <- p4 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )
# # r squared is 0.0085 == not included

# Mobiliy one year and financialized landlords 

p5 <- 
  kmeans_CT %>% 
  left_join(., st_drop_geometry(CT_parent_vectors), by = "GeoUID") %>% 
  ggplot(aes(x=p_mobility_one_year, y=p_financialized, size=parent_renter, alpha=parent_renter))+
  geom_point(color=col_palette[3])+
  geom_smooth(method="lm", se=FALSE, color="black")+
  scale_x_continuous(name = "Percentage of households having\nmoved in the past year",
                     label = scales::percent)+
  scale_y_continuous("Percentage of financialized\nrental ownership",
                     label = scales::percent, 
                     limits = c(-0.05, 1))+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide = "none")+
  theme_minimal()

p5 <- p5 + stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  label.x = 0.05, label.y = 0.875
)


# Visible minorities and financialized landlords 

# p7 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=p_vm, y=p_financialized, alpha=dwellings, size=dwellings))+
#   geom_point(color=col_palette[5])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Percentage of visible minorities",
#                      label = scales::percent)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p7 <- p7 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )
# # r squared is 0.026 == not included

# Immigrants and financialized landlords

# p8 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=p_immigrants, y=p_financialized, size=dwellings, alpha=dwellings))+
#   geom_point(color=col_palette[6])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Percentage of immigrants",
#                      label = scales::percent)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p8 <- p8 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )
# r2 is 0.038, so not included

# kmeans_CT %>% 
#   na.omit() %>% 
#   mutate(clusters = clusters_test2) %>% 
#   ggplot(aes(x=p_repairs, y=p_financialized))+
#   geom_point(color=col_palette[7])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Percentage of dwellings requiring major repairs",
#                      label = scales::percent)+
#   scale_y_continuous("Percentage of financialized rental ownership",
#                      label = scales::percent)+
#   theme_minimal()

# Distance downtown and financialized landlords 

# p9 <- 
#   kmeans_CT %>% 
#   mutate(distance_dt=distance_dt/1000) %>% 
#   ggplot(aes(x=as.numeric(distance_dt), y=p_financialized, size=dwellings, alpha=dwellings))+
#   geom_point(color=col_palette[1])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Distance from downtown (km)")+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p9 <- p9 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )


# Asking rent and financialized landlords

# p10 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=asking_rent, y=p_financialized, size=dwellings, alpha=dwellings))+
#   geom_point(color=col_palette[6])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Asking rent",
#                      label = scales::dollar)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p10 <- p10 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )

# Percent condo and financialized landlords

# p11 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=p_condo, y=p_financialized, alpha=dwellings, size=dwellings))+
#   geom_point(color=col_palette[6])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Percentage of condos",
#                      label = scales::dollar)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p11 <- p11 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )
# # R squared is 0.047 == not included


# Percent FZ and five more storeys

p12 <-
  kmeans_CT %>%
  left_join(., st_drop_geometry(CT_parent_vectors), by = "GeoUID") %>% 
  ggplot(aes(x=p_five_more_storeys, y=p_financialized, size=parent_renter, alpha=parent_renter))+
  geom_point(color=col_palette[9])+
  geom_smooth(method="lm", se=FALSE, color="black")+
  scale_x_continuous(name = "Percentage of households in apartment\nbuildings of five storeys or more",
                     label = scales::percent)+
  scale_y_continuous("Percentage of financialized\nrental ownership",
                     label = scales::percent,
                     limits = c(-0.05, 1))+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide = "none")+
  theme_minimal()

p12 <- p12 + stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  label.x = 0.05, label.y = 0.875
)

# Percent renters and percent FZ

# p13 <- 
#   kmeans_CT %>% 
#   ggplot(aes(x=p_renter, y=p_financialized, size=dwellings, alpha=dwellings))+
#   geom_point(color=col_palette[9])+
#   geom_smooth(method="lm", se=FALSE, color="black")+
#   scale_x_continuous(name = "Percentage of renter households",
#                      label = scales::percent)+
#   scale_y_continuous("Percentage of financialized\nrental ownership",
#                      label = scales::percent)+
#   scale_size_continuous(guide = "none")+
#   scale_alpha_continuous(guide = "none")+
#   theme_minimal()
# 
# p13 <- p13 + stat_cor(
#   aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   label.x = 0.05, label.y = 0.875
# )
# # R square is 0.024 == not included


# Percent 18-24 and percent FZ

p13 <-
  kmeans_CT %>%
  left_join(., st_drop_geometry(CT_parent_vectors), by = "GeoUID") %>% 
  ggplot(aes(x=p_18_24, y=p_financialized, size=parent_renter, alpha=parent_renter))+
  geom_point(color=col_palette[7])+
  geom_smooth(method="lm", se=FALSE, color="black")+
  scale_x_continuous(name = "Percentage of population aged between 18 and 24 years old",
                     label = scales::percent)+
  scale_y_continuous("Percentage of financialized\nrental ownership",
                     label = scales::percent,
                     limits = c(-0.05, 1))+
  scale_size_continuous(guide = "none")+
  scale_alpha_continuous(guide = "none")+
  theme_minimal()

p13 <- p13 + stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  label.x = 0.05, label.y = 0.875
)


single_regressions <- grid.arrange(p1, p2, p5, p12, p13, nrow = 3)

ggsave("output/figures/single_regressions.pdf", plot = single_regressions, width = 12, 
       height = 8, units = "in", useDingbats = FALSE)


# Multilinear regression analysis ----------------------------------------------

# Make a correlation matrix
library(Hmisc)
library(corrplot)

autocor <- na.omit(kmeans_CT %>% select(-GeoUID, -dwellings, -p_renter, -p_financialized)) %>% 
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
              "P. five years mobility", "P. dwellings in five+ storeys", "Median household income",
              "P. pop 18-24",
              "Change in renter dwellings", "Distance from downtown", "Asking rents", "P. financialized rental ownership"))

reg1 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_condo + p_immigrants + p_vm +
           p_mobility_one_year + distance_dt + change_renter_dwellings + asking_rent + average_value_dwellings, data=reg_kmeans_CT)
reg_kmeans_CT$reg1pred <- predict(reg1)
reg_kmeans_CT$reg1res <- rstandard(reg1)
summary(reg1)

reg2 <- lm(p_financialized ~ p_condo + p_immigrants + p_mobility_one_year + 
             distance_dt + change_renter_dwellings + asking_rent, data=reg_kmeans_CT)
reg_kmeans_CT$reg2pred <- predict(reg2)
reg_kmeans_CT$reg2res <- rstandard(reg2)
summary(reg2)

reg3 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + 
             p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg3pred <- predict(reg3)
reg_kmeans_CT$reg3res <- rstandard(reg3)
summary(reg3)

reg4 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg4pred <- predict(reg4)
reg_kmeans_CT$reg4res <- rstandard(reg4)
summary(reg4)

reg5 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_immigrants +
             p_mobility_one_year + asking_rent, data=reg_kmeans_CT)
reg_kmeans_CT$reg5pred <- predict(reg5)
reg_kmeans_CT$reg5res <- rstandard(reg5)
summary(reg5)

reg6 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + 
             p_immigrants + average_value_dwellings, data=reg_kmeans_CT)
reg_kmeans_CT$reg6pred <- predict(reg6)
reg_kmeans_CT$reg6res <- rstandard(reg6)
summary(reg6)

reg7 <- lm(p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + 
             p_immigrants, data=reg_kmeans_CT)
reg_kmeans_CT$reg7pred <- predict(reg7)
reg_kmeans_CT$reg7res <- rstandard(reg7)
summary(reg7)

reg8 <- lm(`Percent. of financialized landlords` ~ `Percent. renters in housing stress` + `Median rent` + `Percent. one year mobility` + 
             `Percent. immigrants` + `Percent. dwellings in five+ storey buildings`, data=reg_kmeans_CT)
reg_kmeans_CT$reg8pred <- predict(reg8)
reg_kmeans_CT$reg8res <- rstandard(reg8)
summary(reg8)

reg9 <- lm(`Percent. of financialized landlords` ~ `Percent. renters in housing stress` + `Median rent` + `Percent. one year mobility` + 
             `Percent. immigrants` + `Percent. dwellings in five+ storey buildings` + `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg9pred <- predict(reg9)
reg_kmeans_CT$reg9res <- rstandard(reg9)
summary(reg9)

reg10 <- lm(`Percent. of financialized landlords` ~ `Percent. renters in housing stress` + `Median rent` + `Percent. one year mobility` + 
             `Percent. immigrants` + `Percent. dwellings in five+ storey buildings` + `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg10pred <- predict(reg10)
reg_kmeans_CT$reg10res <- rstandard(reg10)
summary(reg10)

reg11 <- lm(`Percent. of financialized landlords` ~ `Percent. renters in housing stress` + `Median rent` + `Five years mobility` + 
              `Percent. immigrants` + `Percent. dwellings in five+ storey buildings` + `Median household income`, data=reg_kmeans_CT)
reg_kmeans_CT$reg11pred <- predict(reg11)
reg_kmeans_CT$reg11res <- rstandard(reg11)
summary(reg11)

reg12 <- lm(`P. financialized rental ownership` ~ `P. renters' housing stress` + `Median rent` + `P. one year mobility` + 
              `P. visible minorities` + `P. dwellings in five+ storeys` + `P. pop 18-24`, data=reg_kmeans_CT)
reg_kmeans_CT$reg12pred <- predict(reg12)
reg_kmeans_CT$reg12res <- rstandard(reg12)
summary(reg12)


stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, type="html", out="regFZ.html")
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

  