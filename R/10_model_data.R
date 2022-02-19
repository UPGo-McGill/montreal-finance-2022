#### 10 PREPARE MODEL DATA #####################################################

# Load libraries ---------------------------------------------------------------

source("R/01_startup.R")

library(spdep)
library(spatialreg)


# Load data --------------------------------------------------------------------

qs::qload("output/data.qsm")
qs::qload("output/geometry.qsm")

# Helper functions -------------------------------------------------------------

scale_center <- function(data, center = TRUE) {
  scaled_cented <- scale(data, center = center)[,1]
  return(scaled_cented)
}

stdize <- function(x, ...) (x - min(x, ...)) / (max(x, ...) - min(x, ...))


# Process data -----------------------------------------------------------------

data_model <- 
  data_CT |> 
  select(p_fin, p_stress, median_rent, p_mobility_one_year, 
         p_vm, p_five_more_storeys, p_18_24, p_built_after_2005, average_age,
         n_fin, total, GeoUID) |> 
  mutate(n_median_rent = stdize(median_rent, na.rm = TRUE),
         n_average_age = stdize(average_age, na.rm = TRUE),
         log_financialized = if_else(p_fin == 0, p_fin, log(p_fin * 100)),
         log_18_24 = if_else(p_18_24 == 0, p_18_24, log(p_18_24 * 100)),
         log_five_more_storeys = if_else(p_five_more_storeys == 0, 
                                         p_five_more_storeys, 
                                         log(p_five_more_storeys * 100)),
         logit_financialized =  if_else(p_fin == 0, p_fin, 
                                        (1 / 1 - log(p_fin * 100))),
         log_n_median_rent = if_else(n_median_rent == 0, 
                                     n_median_rent, log(n_median_rent * 100)),
         ss_18_24 = scale_center(p_18_24),
         ss_median_rent = scale_center(median_rent),
         ss_vm = scale_center(p_vm),
         ss_five_more_storeys = scale_center(p_five_more_storeys),
         ss_thirty_renter = scale_center(p_stress),
         ss_mobility_one_year = scale_center(p_mobility_one_year))


# Adjacency structures ---------------------------------------------------------

CTs_to_drop <- c("4620550.02", "4620550.04", "4620550.03", "4620317.04", 
                 "4620317.03", "4620317.02")

data_model <- 
  data_model |> 
  na.exclude() |> 
  filter(!st_is_empty(geometry)) |> 
  st_make_valid() |> 
  filter(total != 0)

queen_adj <- poly2nb(as(data_model, 'Spatial'))
queen_adj_listw <- nb2listw(queen_adj) 
BYM_adj_mat <- nb2mat(queen_adj, style = 'B', zero.policy = TRUE)

if (isSymmetric(BYM_adj_mat, check.attributes = FALSE)) {
  rownames(BYM_adj_mat) <- data_model$GeoUID
  colnames(BYM_adj_mat) <- data_model$GeoUID
}

# Add edges between Nun's Island and Verdun
BYM_adj_mat["4620072.00", "4620317.02"] <- 1
BYM_adj_mat["4620317.02", "4620072.00"] <- 1

# Add edges between Île Bizard and Sainte-Geneviève
BYM_adj_mat["4620540.00", "4620550.04"] <- 1
BYM_adj_mat["4620550.04", "4620540.00"] <- 1

rownames(BYM_adj_mat) <- seq_len(nrow(BYM_adj_mat))
colnames(BYM_adj_mat) <- seq_len(ncol(BYM_adj_mat))

BYM_adj_list <- mat2listw(BYM_adj_mat) 
queen_adj_sf <- as(nb2lines(BYM_adj_list$neighbours, 
                            coords = coordinates(as(data_model, 'Spatial'))), 
                   'sf')
queen_adj_sf <- st_set_crs(queen_adj_sf, st_crs(data_model))


# Save data for models ---------------------------------------------------------

qsavem(data_model, queen_adj_listw, queen_adj_sf, BYM_adj_mat, 
       file = "output/stat_model_data.qsm", nthreads = availableCores())

rm(BYM_adj_list, queen_adj, CTs_to_drop, scale_center, stdize)
