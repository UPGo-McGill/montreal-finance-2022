#### 14 FIGURES FOR APPENDIX ###################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
qload("output/cluster.qsm", nthreads = availableCores())
qload("output/models/extra.qsm", nthreads = availableCores())
qload("output/stat_model_data.qsm", nthreads = availableCores())
brms_linear <- qread("output/models/brms_linear.qs", 
                     nthreads = availableCores())
brms_binomial <- qread("output/models/brms_binomial.qs", 
                       nthreads = availableCores())
brms_bym <- qread("output/models/brms_bym.qs", nthreads = availableCores())

library(patchwork)
library(cluster)
library(factoextra)
library(bayesplot)
fig_alpha <- 0.8


# Figure A1.  Rental unit scatterplot -------------------------------------

fig_A1 <- 
  data_CT |> 
  ggplot(aes(renter, n_rentals)) +
  geom_point(color = col_palette[5], size = 0.8, alpha = 0.7) +
  scale_x_continuous(name = "Renter households (2016 census)",
                     labels = scales::comma) +
  scale_y_continuous(name = "Rental units (2020 PAD scrape)",
                     labels = scales::comma) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figures/figure_A1.png", plot = fig_A1, width = 4.5, dpi = 700,
       height = 4.5, units = "in")


# Figure A2. Cluster diagnostics ------------------------------------------

gap <- 
  data_kmeans |>
  clusGap(FUN = kmeans, nstart = 25, K.max = 10, B = 50)

a2_1 <- 
  gap |> 
  fviz_gap_stat(linecolor = col_palette[5]) +
  ggtitle("Gap statistic") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

a2_2 <- 
  data_kmeans |> 
  fviz_nbclust(kmeans, method = "silhouette", barfill = col_palette[5],
               barcolor = col_palette[5], linecolor = col_palette[5]) +
  ggtitle("Silhouette") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

a2_3 <- 
  data_kmeans |> 
  fviz_nbclust(kmeans, method = "wss",
               barfill = col_palette[5],
               barcolor = col_palette[5],
               linecolor = col_palette[5]) +
  geom_vline(xintercept = 5, linetype = 2, colour = col_palette[5]) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Elbow") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A2 <- a2_1 + a2_2 + a2_3

ggsave("output/figures/figure_A2.png", plot = fig_A2, width = 6.5, dpi = 700,
       height = 3.5, units = "in")


# Figure A3. Posterior predictions ----------------------------------------

n_y_rep <- 100
n_dens_draws <- n_y_rep
ncols <- ncol(pp_bym)
y_ppc_dens <- rep(data_model$p_fin, 3)
pred_to_proportion <- function(draw_m, totals, n) t(t(draw_m[1:n,]) / totals)

y_pred_ppc_dens <- cbind(
  pp_linear[1:n_dens_draws,], 
  pred_to_proportion(pp_bin, data_model$total, n_dens_draws), 
  pred_to_proportion(pp_bym, data_model$total, n_dens_draws))

groups_ppc_dens <- factor(cbind(rep("linear", ncols), 
                                rep("binomial", ncols),
                                rep("binomial-bym2", ncols)), 
                          levels = c('linear', 'binomial', 'binomial-bym2'))

fig_A3 <- 
  ppc_dens_overlay_grouped(y = y_ppc_dens, yrep = y_pred_ppc_dens, 
                           group = groups_ppc_dens, alpha = 0.1, size = 0.2) + 
  geom_vline(xintercept = 0, color = "black", lty = 2, alpha = 0.5) +
  scale_colour_manual(name = NULL, labels = c("Actual", "Predicted"),
                      values = col_palette[c(9, 2)]) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figures/figure_A3.png", plot = fig_A3, width = 6.5, dpi = 700, 
       height = 3.5, units = "in")


# Figure A4. Posterior predictions vs. actual values ----------------------

n_draws_points <- n_y_rep
counts_ppc <- rep(data_model$total, n_y_rep)
y_ppc  <- rep(data_model$p_fin, n_y_rep)

ppc_linear <- 
  tibble(y_hat = as.vector(t(pp_linear[1:n_draws_points,])), y = y_ppc) |> 
  mutate(model = "linear")

ppc_bin <- 
  tibble(y_hat = as.vector(t(pp_bin[1:n_draws_points,])) / counts_ppc, 
         y = y_ppc) |> 
  mutate(model = "binomial")

ppc_bym <- 
  tibble(y_hat = as.vector(t(pp_bym[1:n_draws_points,])) / counts_ppc,
         y = y_ppc) |> 
  mutate(model = "binomial-bym2")

model_ppc_df <- 
  ppc_linear |> 
  bind_rows(ppc_bin, ppc_bym) |> 
  mutate(model = factor(model, 
                        levels = c('linear','binomial', 'binomial-bym2')),
         Prediction = if_else(y_hat < 0, "Less than 0", "Between 0 and 1")) |> 
  rename(predicted = y_hat, actual = y)

fig_A4 <- 
  model_ppc_df |> 
  ggplot(aes(x = predicted, y = actual, color = Prediction)) + 
  geom_point(size = 0.8, alpha = 0.5) +
  facet_grid(cols = vars(model)) +
  geom_hline(yintercept = 0, alpha = 0.7) +
  geom_vline(xintercept = 0, alpha = 0.7) +
  scale_colour_manual(guide = NULL, values = c("#074387", "#FF6600")) + 
  xlab("Predicted") +
  ylab("Actual") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figures/figure_A4.png", plot = fig_A4, width = 6.5, dpi = 700,
       height = 3.5, units = "in")


# Figure A5. Adjacency plot -----------------------------------------------

fig_A5 <-
  data_model |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey') +
  geom_sf(data = queen_adj_sf, color = "#FF6600", alpha = 0.5, size = 0.3) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figures/figure_A5.png", plot = fig_A5, width = 4.5, dpi = 700,
       height = 4.5, units = "in")


# Figure A6. CAR map ------------------------------------------------------

bym_rcar_variance <- 
  brms_bym |> 
  tidybayes::spread_draws(rcar[1:466]) |> 
  group_by(`1:466`) |> 
  summarize(variance = var(rcar))

bym_rcar <- 
  brms_bym |> 
  tidybayes::spread_draws(rcar[1:466]) |> 
  tidybayes::mean_qi() |> 
  left_join(bym_rcar_variance) |> 
  rename(lattice_keys = `1:466`) |> 
  arrange(desc(lattice_keys)) |> 
  left_join(rowid_to_column(data_model, "lattice_keys")) |> 
  mutate(rcarabove_0 = if_else(rcar < 0, 0, rcar))

rcar_alpha <- 0.8
rcar_layout <- "
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
AAABB
CCCCC
"

colors <- 
  scales::col_bin(c(col_palette[c(1, 4)], "grey90", col_palette[c(2, 9)]), 
                  domain = NULL, bins = 11)
col_vals <- colors(1:7)
scale_round <- function(x) sprintf("%.0f", x)

rcar_map <-
  bym_rcar |> 
  st_as_sf() |>
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey', alpha = 1) +
  geom_sf(aes(colour = rcar, fill = after_scale(alpha(colour, fig_alpha))),
          lwd = 0.3) +
  scale_colour_steps2(name = "CAR term by census tract", low = col_palette[1],
                      high = col_palette[9], mid = "grey75",
                      breaks = c(-3, -1, 1, 3)) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom",
        legend.text = element_text(size = 7))

rcar_hist <-
  bym_rcar |> 
  ggplot(aes(x = rcar, colour = after_stat(x), 
             fill = after_scale(alpha(colour, fig_alpha)))) +
  geom_histogram(bins = 30) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_colour_steps2(name = "CAR term by census tract", low = col_palette[1],
                      high = col_palette[9], mid = "grey75",
                      breaks = c(-3, -1, 1, 3)) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        legend.position = "none")

fig_A6 <- rcar_map + rcar_hist + guide_area() + 
  theme(legend.position = "bottom") + 
  plot_layout(design = rcar_layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figures/figure_A6.png", plot = fig_A6, width = 6.5, height = 3.8, dpi = 700,
       units = "in")


# Table 1. Model results --------------------------------------------------

covariate_pars <- c("b_Intercept",
                    "b_n_median_rent", 
                    "b_p_stress",
                    "b_n_average_age",
                    "b_p_vm",
                    "b_p_mobility_one_year",
                    "b_p_five_more_storeys",
                    "b_p_built_after_2005")

coefnames <- c("Intercept",
               "Median rent",
               "Renter housing stress (%)",
               "Average age", 
               "Visible minorities (%)",
               "One year mobility (%)", 
               "Dwellings in 5+ stories (%)", 
               "Units built after 2005 (%)")

mcmcReg(list(brms_linear, brms_binomial, brms_bym),  
        pars = covariate_pars, pointest = "mean",
        coefnames = list(coefnames, coefnames, coefnames))


# Table 2. Moran's I ------------------------------------------------------

glm_eq <- cbind(n_fin, total) ~ n_median_rent + p_stress + n_average_age +
  p_vm + p_mobility_one_year + p_five_more_storeys + p_built_after_2005
binomial_fit <- glm(glm_eq, data = data_model, family = binomial)
queen_adj <- spdep::poly2nb(as(data_model, 'Spatial'))

data_model |> 
  select(-c(geometry, log_financialized, median_rent, p_18_24, GeoUID)) |> 
  st_drop_geometry() |> 
  mutate(binomial_res = binomial_fit$residuals) |> 
  map_dfr(ape::Moran.I, spdep::nb2mat(queen_adj), .id = "variable") |> 
  filter(str_detect(variable, "p_") | 
           str_detect(variable, "^n_median") | 
           str_detect(variable, "^binomial") |
           str_detect(variable, "^n_average")) |> 
  mutate(variable = c("Financialized (%)", 
                      "Median rent",
                      "Renters housing stress (%)",
                      "Average age", 
                      "Visible minorities (%)",
                      "1-year mobility (%)", 
                      "Dwelling in 5+ stories (%)", 
                      "Units built after 2005 (%)",
                      "Binomial residuals")) |> 
  mutate(observed = paste0(scales::comma(observed, 0.01), "*")) |> 
  select(Variable = variable, `Moran's I` = observed) |>
  gt::gt()
