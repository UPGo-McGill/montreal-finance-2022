# Load libraries ---------------------------------------------------------------

require("parallelly")
require("spdep")
require("spatialreg")
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------

qs::qload("output/datasets.qsm")

# Helper functions -------------------------------------------------------------

scale_center <- function(data, center) {
  scaled_cented <- scale(data, center = center)[,1]
  return(scaled_cented)
}

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

# Process data -----------------------------------------------------------------

data_model <- dataset_CT %>%
  dplyr::select(p_financialized, 
                p_thirty_renter, 
                median_rent, 
                p_mobility_one_year, 
                p_vm,
                p_five_more_storeys,
                p_18_24,
                n_financialized,
                total,
                GeoUID) %>%
  mutate(n_median_rent = stdize(median_rent, na.rm=TRUE),
         log_financialized = ifelse(p_financialized == 0, 
                                    p_financialized, 
                                    log(p_financialized*100)),
         log_18_24 = ifelse(p_18_24 == 0, 
                            p_18_24, 
                            log(p_18_24*100)),
         log_five_more_storeys = ifelse(p_five_more_storeys == 0, 
                                        p_five_more_storeys, 
                                        log(p_five_more_storeys*100)),
         logit_financialized =  ifelse(p_financialized == 0, 
                                       p_financialized, 
                                       (1/1-log(p_financialized*100)))) %>%
  mutate(log_n_median_rent = ifelse(n_median_rent == 0, 
                                    n_median_rent, 
                                    log(n_median_rent*100))) %>%
  mutate(ss_18_24 = scale_center(p_18_24, T),
         ss_median_rent = scale_center(median_rent, T),
         ss_vm = scale_center(p_vm, T),
         ss_five_more_storeys = scale_center(p_five_more_storeys, T),
         ss_thirty_renter = scale_center(p_thirty_renter, T),
         ss_mobility_one_year = scale_center(p_mobility_one_year, T))

# Adjacency structures ---------------------------------------------------------

CTs_to_drop <- c("4620550.02", "4620550.04", "4620550.03", "4620317.04", 
                 "4620317.03", "4620317.02")

data_model_f <- data_model %>%
  na.exclude() %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>%
  filter(total != 0)  %>%
  filter(!GeoUID %in% CTs_to_drop)

queen_adj <- poly2nb(as(data_model_f, 'Spatial'))
queen_adj_listw <- nb2listw(queen_adj) 
BYM_adj_mat <- nb2mat(queen_adj, style='B',zero.policy=TRUE)
isSymmetric(BYM_adj_mat,check.attributes=FALSE)

BYM_adj_list = mat2listw(BYM_adj_mat) 
queen_adj_sf <- as(nb2lines(BYM_adj_list$neighbours, 
                        coords = coordinates(as(data_model_f, 'Spatial'))), 'sf')
queen_adj_sf <- st_set_crs(queen_adj_sf, st_crs(data_model_f))

adjacency_plot <- ggplot(st_as_sf(data_model_f)) + 
  geom_sf(fill = 'white', color = 'grey70') +
  geom_sf(data = queen_adj_sf, color = "black") +
  theme_void()

# Save plots for paper ---------------------------------------------------------

adjplot_fn <- "output/figures/BYM_adjacency_map"
ggsave(
  adjplot_fn,
  device = "jpeg",
  plot = adjacency_plot)


# Save data for models ---------------------------------------------------------

qs::qsavem(data_model_f, 
           queen_adj_listw, 
           queen_adj_sf, 
           BYM_adj_list, 
           file = "output/stat_model_data.qsm", 
           nthreads = parallelly::availableCores())

