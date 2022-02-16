# Load libraries ---------------------------------------------------------------

source("01_startup.R")

require("parallelly")
require("spdep")
require("spatialreg")
library(sf)
library(spdep)
library(tidyverse)

# Load data --------------------------------------------------------------------

qs::qload("output/data.qsm")
qs::qload("output/geometry.qsm")

# Helper functions -------------------------------------------------------------

scale_center <- function(data, center) {
  scaled_cented <- scale(data, center = center)[,1]
  return(scaled_cented)
}

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

# Process data -----------------------------------------------------------------

data_model <- data_CT %>%
  dplyr::select(p_financialized, 
                p_thirty_renter, 
                median_rent, 
                p_mobility_one_year, 
                p_vm,
                p_five_more_storeys,
                p_18_24,
                p_built_after_2005,
                average_age,
                n_financialized,
                total,
                GeoUID) %>%
  mutate(n_median_rent = stdize(median_rent, na.rm=TRUE),
         n_average_age = stdize(average_age, na.rm=TRUE),
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
  filter(total != 0)

queen_adj <- poly2nb(as(data_model_f, 'Spatial'))
queen_adj_listw <- nb2listw(queen_adj) 
BYM_adj_mat <- nb2mat(queen_adj, style='B',zero.policy=TRUE)
isSymmetric(BYM_adj_mat,check.attributes=FALSE)
rownames(BYM_adj_mat) <- data_model_f$GeoUID
colnames(BYM_adj_mat) <- data_model_f$GeoUID

# Add edges between Nun's Island and Verdun
BYM_adj_mat["4620072.00","4620317.02"] <- 1
BYM_adj_mat["4620317.02","4620072.00"] <- 1

# Add edges between Île Bizard and Sainte-Geneviève
BYM_adj_mat["4620540.00","4620550.04"] <- 1
BYM_adj_mat["4620550.04","4620540.00"] <- 1

#BYM_adj_mat["4620540.00","4620550.03"] <- 1
#BYM_adj_mat["4620550.03","4620540.00"] <- 1

#BYM_adj_mat["4620540.00","4620550.02"] <- 1
#BYM_adj_mat["4620550.02","4620540.00"] <- 1

rownames(BYM_adj_mat) <- seq(1:nrow(BYM_adj_mat))
colnames(BYM_adj_mat) <- seq(1:ncol(BYM_adj_mat))

BYM_adj_list = mat2listw(BYM_adj_mat) 
queen_adj_sf <- as(nb2lines(BYM_adj_list$neighbours, 
                        coords = coordinates(as(data_model_f, 'Spatial'))), 'sf')
queen_adj_sf <- st_set_crs(queen_adj_sf, st_crs(data_model_f))

adjacency_plot <- 
  data_model_f %>%
  st_as_sf() |>
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey', alpha=1) +
  geom_sf(data = queen_adj_sf, 
          color = "#FF6600", 
          alpha=0.4,
          show.legend = TRUE) +
  scale_fill_stepsn(name= "Financialized rental units", 
                    colors = col_palette[c(4, 1, 2, 9)],
                    breaks = c(0.15, 0.30, 0.45, 0.60),
                    #values = c(0.2, 0.4, 0.6),
                    na.value = "grey80",
                    limits = c(0, 0.75), oob = scales::squish, 
                    labels = scales::percent) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7))

adjacency_plot

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
           BYM_adj_mat, 
           file = "output/stat_model_data.qsm", 
           nthreads = parallelly::availableCores())

