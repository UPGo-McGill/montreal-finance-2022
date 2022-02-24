#### ADDITIONAL MODEL ANALYSIS #################################################

# Load libraries ---------------------------------------------------------------

source("R/01_startup.R")

library(ape)
library(spdep)
library(xtable)

# Load data --------------------------------------------------------------------

qs::qload("output/stat_model_data.qsm")

# Exploratory Univariate Diagnostics -------------------------------------------

ggplot(gather(dplyr::select(as_tibble(data_model), 
                            -geometry, 
                            -total, 
                            -n_fin,
                            -GeoUID)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

data_model %>%
  as.tibble() %>%
  dplyr::select(-geometry, -GeoUID) %>%
  gather(-p_fin, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = p_fin)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

data_model %>%
  as_tibble() %>%
  dplyr::select(-geometry, -GeoUID) %>%
  gather(-log_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = log_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

# Exploratory Spatial Diagnostics ----------------------------------------------

OLS_eq <- p_fin ~ n_median_rent + 
  p_stress + 
  average_age +
  p_vm + 
  p_mobility_one_year + 
  p_five_more_storeys + 
  p_built_after_2005
  
binomial_eq <- cbind(n_fin, total) ~ p_stress + 
  n_median_rent + 
  p_mobility_one_year + 
  p_vm + 
  p_five_more_storeys + 
  n_average_age +
  p_built_after_2005

glm_eq <- cbind(n_fin, total) ~ 
  n_median_rent + 
  p_stress + 
  n_average_age +
  p_vm + 
  p_mobility_one_year + 
  p_five_more_storeys + 
  p_built_after_2005


OLS_fit <- lm(OLS_eq, data = data_model)
binomial_fit <- glm(glm_eq,
                    data = data_model, 
                    family = binomial)

queen_adj <- poly2nb(as(data_model, 'Spatial'))

moran_tests <- data_model %>%
  dplyr::select(-c(geometry, median_rent, p_18_24, GeoUID)) %>%
  as.tibble()%>%
  mutate(binomial_res = binomial_fit$residuals) %>%
  dplyr::select(-geometry, -log_financialized) %>%
  map_dfr(~ Moran.I(., nb2mat(queen_adj))) %>%
  mutate(variable = c(colnames(dplyr::select(as.tibble(data_model), 
                                           -c(geometry, median_rent, p_18_24, log_financialized, GeoUID))), "binomial_res")) %>%
  dplyr::select(variable, observed, expected, sd, p.value)

mt_to_file <- moran_tests %>% 
  filter(str_detect(variable, "p_") | 
         str_detect(variable, "^n_median") | 
         str_detect(variable, "^binomial") |
         str_detect(variable, "^n_average")) %>%
  mutate(variable = c("Financialized (%)", 
                      "Median rent",
                      "Renters' housing stress (%)",
                      "average age", 
                      "Visible minorities (%)",
                      "One year mobility (%)", 
                      "Dwelling in five+ stories (%)", 
                      "Units built after 2005 (%)",
                      "Binomial residuals")) %>%
  select(variable, observed, p.value)
         

caption <- "Global Moran's I coefficients for the dependent and independent model variables."
print(xtable(mt_to_file,
             caption = caption,
             type = "latex"), 
      file = "latex/morans_variables.tex")
