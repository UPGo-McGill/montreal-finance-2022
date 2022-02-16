
#### 12 Statistical models for paper ###########################################

# TODO

# - Change covariates after update from Cloe
# - Rerun models
# - Fix latex output
# - Visualize outcome and save outputs

# 0. Preamble ------------------------------------------------------------------

# 0.1 Libraries and options ----------------------------------------------------

source("01_startup.R")

library(bayesplot)
library(BayesPostEst)
library(brms)
library(ggridges)
library(scales)
library(stats)
library(tidyr)
library(tidybayes)
library(tidyverse)

options(scipen=999)
bayesplot::theme_default()

# 0.2 Load data ----------------------------------------------------------------

qs::qload("output/stat_model_data.qsm")

# 0.3 Helper functions --------------------------------------------------------- 

get_sse <- function(fitted, actual) {
  sse <-  sum((fitted - actual)^2)
  return(sse)
}

plot_fit <- function(fitted, actual) {
  data <- data.frame("predicted" = fitted, "actual" = actual)
  ggplot(data, aes(x = predicted, y = actual)) +
    geom_point(color = "blue", alpha = 0.2) +
    theme_bw()
}

pred_to_proportion <- function(draw_m, totals, n) {
  as_prop <- t(t(draw_m[1:n,])/totals)
  return(as_prop)
}

# 0.4 Color palettes ----------------------------------------------------------- 

get_palette_codes <- function(n) {
  show_col(viridisLite::inferno(n))
  show_col(viridisLite::viridis(n))
  show_col(viridisLite::turbo(n))
}

col_3_inferno <- c("#FF0000A0", "#0000FFA0", "#FF0000A0")
col_3_viridis <- c("#440154ff", "#21908CFF", "#5DC863FF")
col_2_turbo <- c('#39A4FBFF', '#DB3A07FF')

# 1. Models --------------------------------------------------------------------

## 1.1 Frequentist Binomial Regression -----------------------------------------

### 1.1.1 Prep model params ----------------------------------------------------

glm_eq <- cbind(n_financialized, total) ~ p_thirty_renter + 
  n_median_rent + 
  p_mobility_one_year + 
  p_vm + 
  p_five_more_storeys + 
  #p_18_24 +
  n_average_age +
  p_built_after_2005

### 1.1.2 Run model ------------------------------------------------------------

glm_binomial <- glm(glm_eq,
                    data = data_model_f, 
                    family = binomial)
### 1.1.3 Eval model -----------------------------------------------------------

summary(glm_binomial)

sse_glm_bin <- get_sse(glm_binomial$fitted.values, data_model_f$p_financialized)
sse_glm_bin

glm_fit_p <- plot_fit(glm_binomial$fitted.values, 
                      data_model_f$p_financialized)

## 1.2 BRMS Linear Regression --------------------------------------------------

### 1.2.0 General setup --------------------------------------------------------

ndraws = 2000
warmup = 2000
iterations = 8000
seed = 123
chains = 4
cores = 4
inits = "random"
save_m_pars = save_pars(all = TRUE)

covariate_pars <- c("b_p_thirty_renter", 
                    "b_n_median_rent", 
                    "b_p_mobility_one_year",
                    "b_p_vm",
                    "b_p_five_more_storeys",
                    #"b_p_18_24",
                    "b_Intercept",
                    "b_n_average_age",
                    "b_p_built_after_2005")

n_y_rep <- 100
counts_ppc <- rep(data_model_f$total, n_y_rep)
y_ppc  <- rep(data_model_f$p_financialized, n_y_rep)

### 1.2.1 Prep model params ----------------------------------------------------

brms_linear_eq <- p_financialized ~ 
  p_thirty_renter + 
  n_median_rent + 
  p_mobility_one_year + 
  p_vm + 
  p_five_more_storeys + 
  #p_18_24  +
  n_average_age +
  p_built_after_2005

lin_formula <- brmsformula(formula = brms_linear_eq) 
mlinear_priors <- get_prior(lin_formula, data=data_model_f)
mlinear_priors$prior[c(2:7)] <- "normal(0, 2)"

### 1.2.2. Run model -----------------------------------------------------------

brms_linear <- brm(formula = lin_formula, 
                   data = data_model_f,
                   prior = mlinear_priors,
                   warmup = warmup,
                   iter = iterations,
                   seed = seed,
                   chains = chains,
                   cores = cores,
                   inits = inits,
                   save_pars = save_m_pars)
saveRDS(brms_linear, "output/models/brms_linear.rds")

### 1.2.3 Eval model -----------------------------------------------------------

plot(brms_linear, combo = c("dens", "trace"))
pairs(brms_linear)

pp_linear <- posterior_predict(brms_linear, ndraws=ndraws)
get_sse(colMeans(pp_linear), data_model_f$p_financialized)

ppc_dens_overlay_linear_p <- ppc_dens_overlay(data_model_f$p_financialized, 
                                              pp_linear[1:100,],
                                              size = 0.5,
                                              trim=T)
ppc_dens_overlay_linear_p

plot_title <- ggtitle("Posterior distributions for linear regression",
                      "with medians and 80% intervals")
lin_mcmc_coefs <- mcmc_areas(as.matrix(brms_linear),
                             pars = covariate_pars,
                             prob = 0.95) + 
  plot_title +
  vline_0(colour = "orange") +
  theme_bw()

lin_mcmc_coefs

## 1.3. Bayesian binomial regression -------------------------------------------

### 1.3.1 Prep model params ----------------------------------------------------

brms_bin_eq <- n_financialized | trials(total)  ~
  p_thirty_renter + 
  n_median_rent + 
  p_mobility_one_year + 
  p_vm + 
  p_five_more_storeys + 
  #p_18_24 +
  n_average_age +
  p_built_after_2005

brms_bin_formula <- brmsformula(formula = brms_bin_eq, 
                                family = binomial(link = "logit")) 
  
brms_bin_priors <- get_prior(brms_bin_formula, data=data_model_f)
brms_bin_priors$prior[c(2:7)] <- "normal(0, 2)"

### 1.3.2 Run model ------------------------------------------------------------

brms_binomial<- brm(brms_bin_formula,
                    data = data_model_f, 
                    prior=brms_bin_priors,
                    warmup = warmup, 
                    iter = iterations, 
                    chains = chains, 
                    inits = inits, 
                    cores = cores,
                    seed = seed,
                    save_pars = save_m_pars)
saveRDS(brms_binomial, "output/models/brms_binomial.rds")

### 1.3.3 Eval model -----------------------------------------------------------

plot(brms_binomial, combo = c("dens", "trace"))
pairs(brms_binomial)

pp_bin <- posterior_predict(brms_binomial, ndraws=ndraws)
get_sse((colMeans(pp_bin) / data_model_f$total),
        data_model_f$p_financialized)

ppc_dens_bin_p <- ppc_dens_overlay(data_model_f$p_financialized, 
                                   pred_to_proportion(pp_bin,
                                                      data_model_f$total,
                                                      100),
                                   size = 0.5,
                                   trim=T)
ppc_dens_bin_p

plot_title <- ggtitle("Posterior distributions for binomial regression",
                      "with medians and 95% intervals")
bin_mcmc_coefs <- mcmc_areas(as.matrix(brms_binomial),
                             pars = covariate_pars,
                             prob = 0.95) + 
  plot_title +
  vline_0(colour = "orange") +
  theme_bw()
bin_mcmc_coefs

## 1.4. Bayesian binomial regression with BYM2 priors --------------------------

### 1.4.1 Prep model params ----------------------------------------------------

data_model_f$gr <- as.factor(seq.int(nrow(data_model_f)))
brms_bym_formula <- brmsformula(formula = brms_bin_eq, 
                           family = binomial(link = "logit"),
                           autocor = ~ car(w, gr=gr,type = "bym")) 

stan_data2 = list(w=BYM_adj_mat)
brms_bym_priors <- get_prior(brms_bym_formula, 
                             data=data_model_f,
                             data2=stan_data2)
brms_bym_priors$prior[c(2:7)] <- "normal(0, 1)"
brms_bym_priors$prior[10] <- "normal(0, 1)" 
control <- list(max_treedepth = 12,
                adapt_delta = 0.97, 
                stepsize = 0.5)

### 1.4.2 Run model ------------------------------------------------------------

brms_bym <- brm(brms_bym_formula, 
                prior=brms_bym_priors,
                data = data_model_f, 
                data2=stan_data2,
                warmup = warmup, 
                iter = iterations,
                chains = chains, 
                inits = inits, 
                cores = cores,
                seed = seed,
                thin = 1,
                save_pars = save_m_pars,
                control = control)
saveRDS(brms_bym, "output/models/brms_bym.rds")

### 1.4.3 Eval model -----------------------------------------------------------

plot(brms_bym, combo = c("dens", "trace"))
pairs(brms_bym, pars = covariate_pars)

pp_bym <- posterior_predict(brms_bym, ndraws = ndraws)
get_sse((colMeans(pp_bym) / data_model_f$total),
        data_model_f$p_financialized)

ppc_dens_bym_p <- ppc_dens_overlay(data_model_f$p_financialized, 
                                   pred_to_proportion(pp_bym, 
                                                      data_model_f$total,
                                                      100),
                                   size = 0.5,
                                   trim=T)
ppc_dens_bym_p

plot_title <- ggtitle("Posterior distributions for binomial regression",
                      "with medians and 95% intervals")
bym_mcmc_coefs <- mcmc_areas(as.matrix(brms_bym),
                             pars = covariate_pars,
                             prob = 0.95) + 
  plot_title +
  vline_0(colour = "orange") +
  theme_bw()
bym_mcmc_coefs

# 2. Compare Models ------------------------------------------------------------

## 2.1 Output parameter estimates to LATEX -------------------------------------

coefnames <- c("Intercept",
               "Median rent",
               "% renters' housing stress",
               "% one year mobility", 
               "% visible minorities",
               "% dwelling in five+ stories", 
               #"% pop 18-24",
               "% average age", 
               "% units built after 2005")

mcmcReg(list(brms_linear, brms_binomial, brms_bym),  
        pars = covariate_pars,pointest = "mean",
        coefnames = list(coefnames,coefnames,coefnames),
        file = "output/figures/latex/brms_models")

## 2.2 Posterior parameter draws -----------------------------------------------

param_draws_linear <- brms_linear %>% 
  as_draws_df() %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_n_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `average age` = b_n_average_age, 
         `% units built after 2005` = p_built_after_2005)

param_draws_log <- brms_binomial %>%
  as_draws_df() %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_n_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `average age` = b_n_average_age, 
         `% units built after 2005` = p_built_after_2005)

param_draws_bym <- brms_bym %>%
  as_draws_df() %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_n_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `average age` = b_n_average_age, 
         `% units built after 2005` = p_built_after_2005)

combined <- rbind(mcmc_intervals_data(param_draws_linear, prob = 0.95, prob_outer = 1),
                  mcmc_intervals_data(param_draws_log, prob = 0.95, prob_outer = 1),
                  mcmc_intervals_data(param_draws_bym, prob = 0.95, prob_outer = 1))
combined$model <- rep(c("linear", "binomial", "bym"), 
                      each = ncol(param_draws_linear))
combined <- filter(combined, parameter != 'Intercept')

pos <- position_nudge(y = ifelse(
  combined$model == "bym", 0, ifelse(combined$model == "binomial", 0.1, 0.2)))

point_est_p <- combined %>%
  ggplot(aes(x = m, y = parameter, color = model)) + 
  geom_vline(xintercept = 0.0, 
             color="red",
             alpha = 1,
             size = 0.3,
             lty=2) + 
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+
  geom_point(position = pos, size = 1, alpha = 10) +
  scale_colour_manual(breaks = c("linear", "binomial", "bym"),
                      labels = c("linear", "binomial", "bym2"), 
                      values = c( "#3399CC","#CC6699", "#FF6600"),
                      name="Model") + 
  xlab("Estimate") + 
  ylab("Parameter") + 
  theme_minimal()

point_est_p

## 2.3 Posterior parameter draw ridges -----------------------------------------

n_head = 1000

linear_draws_df <- brms_linear %>%
  as_tibble() %>%
  head(n_head) %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_n_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `average age` = b_n_average_age, 
         `% units built after 2005` = p_built_after_2005) %>%
  gather(key='estimate', value='coefficient') %>%
  mutate(model = 'linear')

bin_draws_df <- brms_binomial%>%
  as_tibble() %>%
  head(n_head) %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_n_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `average age` = b_n_average_age, 
         `% units built after 2005` = p_built_after_2005) %>%
  gather(key='estimate', value='coefficient') %>%
  mutate(model = 'binomial')

bym_draws_df <- brms_bym %>%
  as_tibble() %>%
  head(n_head) %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_n_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `average age` = b_n_average_age, 
         `% units built after 2005` = p_built_after_2005) %>%
  gather(key='estimate', value='coefficient') %>%
  mutate(model = 'binomial-bym2')

model_draws_df <- linear_draws_df %>%
  bind_rows(bin_draws_df, bym_draws_df) %>%
  mutate(model = factor(model, 
                        levels = c('linear','binomial', 'binomial-bym2'))) %>%
  filter(estimate != "Intercept")

p_density_ridges <- 
  ggplot(model_draws_df, aes(x = coefficient, 
                           y = estimate,
                           fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975),
    scale=1) + 
  scale_fill_manual(
    name = "Probability", 
    values = alpha(c("#FF6600", "#3399CC", "#FF6600"), 0.5),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) + 
  facet_grid(cols = vars(model), scales = "free_x") +
  geom_vline(xintercept = 0.0, 
             color="black",
             alpha = 0.8,
             lty = 3) + 
  theme_bw() +
  ylab("Estimate") +
  xlab("Coefficient")
p_density_ridges      

## 2.4 Posterior predictions ---------------------------------------------------

n_draws_points = 50

ppc_linear <- tibble(
  y_hat = as.vector(t(pp_linear[1:n_draws_points,])),
  y = y_ppc) %>%
  mutate(model = "linear")

ppc_bin <- tibble(
  y_hat = as.vector(t(pp_bin[1:n_draws_points,])) / counts_ppc,
  y = y_ppc) %>%
  mutate(model = "binomial")

ppc_bym <- tibble(
  y_hat = as.vector(t(pp_bym[1:n_draws_points,])) / counts_ppc,
  y = y_ppc) %>%
  mutate(model = "binomial-bym2")

model_ppc_df <- ppc_linear %>%
  bind_rows(ppc_bin, ppc_bym) %>%
  mutate(model = factor(model, 
                        levels = c('linear','binomial', 'binomial-bym2')),
         Prediction = ifelse(y_hat < 0, "Less than 0", "Between 0 and 1")) %>%
  rename(predicted = y_hat, actual = y)

model_ppc_df_p <- model_ppc_df%>%
  ggplot(aes(x = predicted, y = actual, color = Prediction)) + 
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(model)) +
  geom_hline(yintercept = 0, alpha = 0.8) +
  geom_vline(xintercept = 0, alpha = 0.8) +
  scale_colour_manual(labels = c("0 to 1", "Less than 0"), 
                      values = c("#074387", "#FF6600"),
                      name = "Fitted") + 
  theme_bw() +
  xlab("Fitted") +
  ylab("Actual")

model_ppc_df_p

## 2.5 PPC Density Overlay -----------------------------------------------------

n_dens_draws = n_y_rep
ncols = ncol(pp_bym)
y_ppc_dens <- rep(data_model_f$p_financialized, 3)
y_pred_ppc_dens <- cbind(pp_linear[1:n_dens_draws,], 
                         pred_to_proportion(pp_bin, 
                                            data_model_f$total,
                                            n_dens_draws), 
                         pred_to_proportion(pp_bym, 
                                            data_model_f$total,
                                            n_dens_draws))
groups_ppc_dens <- factor(cbind(rep("linear", ncols), 
                         rep("binomial", ncols),
                         rep("binomial-bym2", ncols)), 
                         levels = c('linear','binomial', 'binomial-bym2'))

ppc_dens_p <- ppc_dens_overlay_grouped(y = y_ppc_dens,
                                       yrep = y_pred_ppc_dens,
                                       group = groups_ppc_dens,
                                       alpha = 0.1,
                                       size=0.2) + 
  scale_colour_manual(
    labels = c("actual", "fitted"),
    values = c("#A80858", "#D87B91"),
    name = "Distributions") +
  theme_bw()
ppc_dens_p$layers <- c(geom_vline(xintercept = 0, 
                                  color = "black",
                                  lty = 2,
                                  alpha = 0.5), 
                       ppc_dens_p$layers)
ppc_dens_p
color_scheme_set(scheme = "blue")

## 2.6 BYM2 CAR terms plotted and mapped ---------------------------------------

bym_rcar_variance <- brms_bym %>%
  spread_draws(rcar[1:466]) %>%
  group_by(`1:466`) %>%
  summarize(variance = var(rcar))

bym_rcar <- brms_bym %>%
  spread_draws(rcar[1:466]) %>%
  mean_qi() %>%
  left_join(bym_rcar_variance) %>%
  rename(lattice_keys = `1:466`) %>%
  arrange(desc(lattice_keys)) %>%
  left_join(rowid_to_column(data_model_f, "lattice_keys")) %>%
  mutate(rcarabove_0 = ifelse(rcar < 0, 0, rcar))

rcar_alpha <- 0.8
rcar_layout <- "
AABB
AABB
AABB
AABB
AABB
CCCC"

colors <- col_bin(palette = col_palette[c(1, 4, 2, 9)], 
                  domain=NULL,
                  bins=11)
col_vals <- colors(c(1,2,3,4,5,6))
scale_round <- function(x) sprintf("%.0f", x)
show_col(col_vals)

rcar_map <- 
  bym_rcar %>%
  st_as_sf() |>
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(fill = 'white', color = 'grey', alpha=1) +
  geom_sf(aes(fill = rcar), 
          alpha=rcar_alpha,
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
rcar_map

rcar_hist <-
  bym_rcar |> 
  mutate(rcar = round(rcar,2)) %>%
  mutate(fill = case_when(
    rcar >= 4 ~ "6",
    rcar >= 2 ~ "5",
    rcar >= 0 ~ "4",
    rcar >= -2 ~ "3",
    rcar >= -4 ~ "2",
    rcar >= -6 ~ "1"
  )) |> 
  ggplot(aes(round(rcar,2), fill = fill, color=fill)) +
  geom_histogram(bins = 30, alpha=rcar_alpha) +
  scale_x_continuous(name = NULL,
                     labels = scale_round,
                     breaks = breaks_extended(n=14),
                     limits = c(-7,7)) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = col_vals, 
                    guide = NULL) +
  scale_color_manual(values = col_vals, 
                     guide = NULL) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal()
rcar_hist

rcar_fig <- rcar_map + rcar_hist + guide_area() + 
  theme(legend.position = "bottom") + 
  plot_layout(design = rcar_layout, guides = "collect") + 
  plot_annotation(tag_levels = "A") 

rcar_fig

## 2.7 Save plots --------------------------------------------------------------

ggsave("output/figures/point_est_p.pdf", plot = point_est_p, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

ggsave("output/figures/p_density_ridges.pdf", plot = p_density_ridges, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

ggsave("output/figures/model_ppc.pdf", plot = model_ppc_df_p, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

ggsave("output/figures/ppc_dens.pdf", plot = ppc_dens_p, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

ggsave("output/figures/rcar_fig.pdf", plot = rcar_fig, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)