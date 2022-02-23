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

ggsave("output/figures/figure_A1.png", plot = fig_A1, width = 4.5, 
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
  ggtitle("Elbow") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

fig_A2 <- a2_1 + a2_2 + a2_3

ggsave("output/figures/figure_A2.png", plot = fig_A2, width = 6.5, 
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

ggsave("output/figures/figure_A3.png", plot = fig_A3, width = 6.5, 
       height = 3.5, units = "in")














# Adjacency plot ----------------------------------------------------------

adjacency_plot <- 
  data_model |> 
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

adjplot_fn <- "output/figures/BYM_adjacency_map"
ggsave(
  adjplot_fn,
  device = "png",
  plot = adjacency_plot)




# Density -----------------------------------------------------------------




# PPC ---------------------------------------------------------------------

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


model_ppc_df_p <- 
  model_ppc_df |> 
  ggplot(aes(x = predicted, y = actual, color = Prediction)) + 
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(model)) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  geom_vline(xintercept = 0, alpha = 0.8) +
  scale_colour_manual(labels = c("0 to 1", "Less than 0"), 
                      values = c("#074387", "#FF6600"),
                      name = "Predicted") + 
  theme_bw() +
  xlab("Predicted") +
  ylab("Actual")

ggsave("output/figures/model_ppc.png", 
       plot = model_ppc_df_p, 
       width = 8, 
       height = 5, 
       units = "in")


# CAR map -----------------------------------------------------------------


library(patchwork)
qload("output/geometry.qsm", nthreads = availableCores())

bym_rcar_variance <- 
  brms_bym |> 
  spread_draws(rcar[1:466]) |> 
  group_by(`1:466`) |> 
  summarize(variance = var(rcar))

bym_rcar <- 
  brms_bym |> 
  spread_draws(rcar[1:466]) |> 
  mean_qi() |> 
  left_join(bym_rcar_variance) |> 
  rename(lattice_keys = `1:466`) |> 
  arrange(desc(lattice_keys)) |> 
  left_join(rowid_to_column(data_model, "lattice_keys")) |> 
  mutate(rcarabove_0 = if_else(rcar < 0, 0, rcar))

rcar_alpha <- 0.8
rcar_layout <- "
AABB
AABB
AABB
AABB
AABB
CCCC"

colors <- col_bin(col_palette[c(1, 4, 2, 9)], domain = NULL, bins = 11)
col_vals <- colors(c(1,2,3,4,5,6))
scale_round <- function(x) sprintf("%.0f", x)

rcar_map <- 
  bym_rcar |> 
  st_as_sf() |>
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey', alpha = 1) +
  geom_sf(aes(fill = rcar), 
          alpha = rcar_alpha,
          color = "transparent") +
  scale_fill_stepsn(name= "CAR term by census tract", 
                    colors = alpha(col_vals, 0.8),
                    breaks = c(-4,-2, 0, 2, 4),
                    na.value = "grey80",
                    limits = c(-6, 6), oob = squish, 
                    labels = scale_round) +
  gg_bbox(boroughs) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7))

rcar_hist <-
  bym_rcar |> 
  mutate(rcar = round(rcar, 2)) |> 
  mutate(fill = case_when(
    rcar >= 4 ~ "6",
    rcar >= 2 ~ "5",
    rcar >= 0 ~ "4",
    rcar >= -2 ~ "3",
    rcar >= -4 ~ "2",
    rcar >= -6 ~ "1")) |> 
  ggplot(aes(round(rcar, 2), fill = fill, color = fill)) +
  geom_histogram(bins = 30, alpha = rcar_alpha) +
  scale_x_continuous(name = NULL,
                     labels = scale_round,
                     breaks = breaks_extended(n = 14),
                     limits = c(-7, 7)) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = col_vals, guide = NULL) +
  scale_color_manual(values = col_vals, guide = NULL) +
  geom_vline(xintercept = 0, color = "grey86") +
  geom_hline(yintercept = 0, color = "grey86") +
  theme_minimal()

rcar_fig <- rcar_map + rcar_hist + guide_area() + 
  theme(legend.position = "bottom") + 
  plot_layout(design = rcar_layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

ggsave("output/figures/rcar_fig.png", 
       plot = rcar_fig, 
       width = 8, 
       height = 5, 
       units = "in")


## 2.3 Posterior parameter draw ridges -----------------------------------------

# Not used


n_head <- 1000

linear_draws_df <- 
  brms_linear |> 
  as_tibble() |> 
  head(n_head) |> 
  select(all_of(covariate_pars)) |> 
  rename(Intercept = b_Intercept,
         `median rent` = b_n_median_rent,
         `% renters' in stress` = b_p_stress,
         `average age` = b_n_average_age, 
         `% visible minorities` = b_p_vm,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% units built after 2005` = b_p_built_after_2005) |> 
  gather(key = 'estimate', value = 'coefficient') |> 
  mutate(model = 'linear')

bin_draws_df <- 
  brms_binomial |> 
  as_tibble() |> 
  head(n_head) |> 
  select(all_of(covariate_pars)) |> 
  rename(Intercept = b_Intercept,
         `median rent` = b_n_median_rent,
         `% renters' in stress` = b_p_stress,
         `average age` = b_n_average_age, 
         `% visible minorities` = b_p_vm,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% units built after 2005` = b_p_built_after_2005) |> 
  gather(key = 'estimate', value = 'coefficient') |> 
  mutate(model = 'binomial')

bym_draws_df <- 
  brms_bym |> 
  as_tibble() |> 
  head(n_head) |> 
  select(all_of(covariate_pars)) |> 
  rename(Intercept = b_Intercept,
         `median rent` = b_n_median_rent,
         `% renters' in stress` = b_p_stress,
         `average age` = b_n_average_age, 
         `% visible minorities` = b_p_vm,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% units built after 2005` = b_p_built_after_2005) |> 
  gather(key = 'estimate', value = 'coefficient') |> 
  mutate(model = 'binomial-bym2')

model_draws_df <- 
  linear_draws_df |> 
  bind_rows(bin_draws_df, bym_draws_df) |> 
  mutate(model = factor(model, 
                        levels = c('linear','binomial', 'binomial-bym2'))) |> 
  filter(estimate != "Intercept")

p_density_ridges <- 
  ggplot(model_draws_df, 
         aes(x = coefficient, y = estimate, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975),
    scale = 1) + 
  scale_fill_manual(
    name = "Probability", 
    values = alpha(c("#FF6600", "#3399CC", "#FF6600"), 0.5),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) + 
  facet_grid(cols = vars(model), scales = "free_x") +
  geom_vline(xintercept = 0.0, color = "black", alpha = 0.8, lty = 3) + 
  theme_bw() +
  ylab("Estimate") +
  xlab("Coefficient")





## 2.7 Save plots --------------------------------------------------------------


ggsave("output/figures/p_density_ridges.png", 
       plot = p_density_ridges, 
       width = 8, 
       height = 5, 
       units = "in")




