# Load libraries ---------------------------------------------------------------

library(ape)
library(spdep)
library(tidyverse)
library(xtable)

# Load data --------------------------------------------------------------------

qs::qload("output/stat_model_data.qsm")

# Exploratory Univariate Diagnostics -------------------------------------------

ggplot(gather(dplyr::select(as_tibble(data_model_f), 
                            -geometry, 
                            -total, 
                            -n_financialized,
                            -GeoUID)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

data_model_f %>%
  as.tibble() %>%
  dplyr::select(-geometry, -GeoUID) %>%
  gather(-p_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = p_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

data_model_f %>%
  as_tibble() %>%
  dplyr::select(-geometry, -GeoUID) %>%
  gather(-log_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = log_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

# Exploratory Spatial Diagnostics ----------------------------------------------

OLS_eq <- p_financialized ~ p_thirty_renter + 
  n_median_rent + 
  p_mobility_one_year + 
  p_vm + 
  p_five_more_storeys + 
  p_18_24

OLS_fit <- lm(OLS_eq, data = data_model_f)

moran_tests <- data_model_f %>%
  dplyr::select(-c(geometry, median_rent, p_18_24, GeoUID)) %>%
  as.tibble()%>%
  mutate(OLS_res = OLS_fit$residuals) %>%
  dplyr::select(-geometry, -log_financialized) %>%
  map_dfr(~ Moran.I(., nb2mat(queen_adj))) %>%
  mutate(variable = c(colnames(dplyr::select(as.tibble(data_model_f), 
                                           -c(geometry, median_rent, p_18_24, log_financialized, GeoUID))), "OLS_res")) %>%
  dplyr::select(variable, observed, expected, sd, p.value)

mt_to_file <- moran_tests %>% 
  filter(str_detect(variable, "p_") | 
         str_detect(variable, "^n_median") | 
         str_detect(variable, "^OLS")) %>%
  mutate(variable = c("% financialized", "% renter's housing stress",
                      "% one year mobility", "% visible minorities",
                      "% dwellings in five+ storeys", "% pop 18-24",
                      "OLS residuals"))

caption <- "Global Moran's I coefficients for the dependent and independent model variables."
print(xtable(mt_to_file,
             caption = caption,
             type = "latex"), 
      file = "morans_variables.tex")
