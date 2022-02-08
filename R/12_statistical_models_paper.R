
#### 12 Statistical models for paper ###########################################

# 0. Preamble ------------------------------------------------------------------

# 0.1 Libraries and options ----------------------------------------------------

library(bayesplot)
library(BayesPostEst)
library(brms)
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
    geom_point(color = "blue", alpha = 0.2)
}

pred_to_proportion <- function(draw_m, totals) {
  as_prop <- t(t(draw_m)/totals)
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

glm_eq <- cbind(n_financialized, total) ~ ss_thirty_renter + 
  ss_median_rent + 
  ss_mobility_one_year + 
  ss_vm + 
  ss_five_more_storeys + 
  ss_18_24

### 1.1.2 Run model ------------------------------------------------------------

glm_binomial <- glm(glm_eq,
                    data = data_model_f, 
                    family = binomial)
### 1.1.3 Eval model -----------------------------------------------------------

summary(glm_binomial)

sse_glm_bin <- get_sse(glm_binomial$fitted.values, data_model_f$p_financialized)
sse_glm_bin

plot_fit(glm_binomial$fitted.values, data_model_f$p_financialized)

## 1.2 BRMS Linear Regression --------------------------------------------------

### 1.2.0 General setup --------------------------------------------------------

ndraws = 2000

covariate_pars <- c("b_p_thirty_renter", 
                    "b_median_rent", 
                    "b_p_mobility_one_year",
                    "b_p_vm",
                    "b_p_five_more_storeys",
                    "b_p_18_24",
                    "b_Intercept")
n_y_rep <- 100
counts_ppc <- rep(data_model_f$total, n_y_rep)
y_ppc  <- rep(data_model_f$p_financialized, n_y_rep)

### 1.2.1 Prep model params ----------------------------------------------------

brms_linear_eq <- p_financialized ~ 
  p_thirty_renter + median_rent + p_mobility_one_year + 
  p_vm + p_five_more_storeys + p_18_24
lin_formula <- brmsformula(formula = brms_linear_eq) 
mlinear_priors <- get_prior(lin_formula, data=data_model_f)
mlinear_priors$prior[c(2:7)] <- "normal(0, 2)"

### 1.2.2. Run model -----------------------------------------------------------

brms_linear <- brm(formula = lin_formula, 
                   data = data_model_f,
                   prior = mlinear_priors,
                   seed = 123)

### 1.2.3 Eval model -----------------------------------------------------------

pp_linear <- posterior_predict(brms_linear, ndraws=ndraws)
pp_linear_mean <- colMeans(pp_linear)
sse_lin <- get_sse(pp_linear_mean, data_model_f$p_financialized)
sse_lin

ppc_linear <- data.frame(y_hat = pp_linear[1:10,],
                         y = data_model_f$p_financialized)

ppc_dens_overlay_linear_p <- ppc_dens_overlay(data_model_f$p_financialized, 
                                              pp_linear,
                                              size = 0.5,
                                              trim=T)
ppc_dens_overlay_linear_p

plot_title <- ggtitle("Posterior distributions for linear regression",
                      "with medians and 80% intervals")
mcmc_areas(as.matrix(brms_linear),
           pars = covariate_pars,
           prob = 0.95) + 
  plot_title +
  vline_0(colour = "orange") +
  theme_bw()

## 1.3. Bayesian binomial regression -------------------------------------------

### 1.3.1 Prep model params ----------------------------------------------------

brms_log_eq <- n_financialized | trials(total)  ~
  p_thirty_renter + median_rent + p_mobility_one_year + 
  p_vm + p_five_more_storeys + p_18_24
brms_log_formula <- brmsformula(formula = brms_log_eq, 
                                family = binomial(link = "logit")) 
  
brms_log_priors <- get_prior(brms_log_formula, data=data_model_f)
brms_log_priors$prior[c(2:7)] <- "normal(0, 2)"

### 1.3.2 Run model ------------------------------------------------------------

brms_logistic <- brm(brms_log_formula,
                     data = data_model_f, 
                     prior=brms_log_priors,
                     warmup = 1000, 
                     iter = 2000, 
                     chains = 4, 
                     inits = "random", 
                     cores = 4,
                     seed = 123)
### 1.3.3 Eval model -----------------------------------------------------------

plot(brms_logistic, combo = c("dens", "trace"))

pp_log <- posterior_predict(brms_logistic, ndraws=ndraws)
pp_log_mean <- colMeans(pp_log)
sse_log <- get_sse((pp_log_mean / data_model_f$total),
                   data_model_f$p_financialized)
sse_log

ppc_log <- data.frame(y_hat = pp_log_mean/data_model_f$total,
                      y = data_model_f$p_financialized)

ppc_dens_log_p <- ppc_dens_overlay(data_model_f$p_financialized, 
                                   pred_to_proportion(pp_log,
                                                      data_model_f$total),
                                   size = 0.5,
                                   trim=T)
ppc_dens_log_p

plot_title <- ggtitle("Posterior distributions for binomial regression",
                      "with medians and 95% intervals")
mcmc_areas(as.matrix(brms_logistic),
           pars = covariate_pars,
           prob = 0.95) + 
  plot_title +
  vline_0(colour = "orange") +
  theme_bw()

## 1.4. Bayesian binomial regression with BYM2 priors --------------------------

### 1.4.1 Prep model params ----------------------------------------------------

data_model_f$gr <- as.factor(seq.int(nrow(data_model_f)))
brms_bym_formula <- brmsformula(formula = brms_log_eq, 
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

brms_log_bym <- brm(brms_bym_formula, 
                    prior=brms_bym_priors,
                    data = data_model_f, 
                    data2=stan_data2,
                    warmup = 2000, 
                    iter = 8000,
                    chains = 4, 
                    inits = "random", 
                    cores = 4,
                    seed = 123,
                    thin = 1,
                    save_pars = save_pars(all = TRUE),
                    control = control)

### 1.4.3 Eval model -----------------------------------------------------------

plot(brms_log_bym, combo = c("dens", "trace"))

pp_bym <- posterior_predict(brms_log_bym, ndraws = ndraws)
pp_bym_mean <- colMeans(pp_bym)
sse_bym <- get_sse((pp_bym_mean / data_model_f$total)*100,
                   data_model_f$p_financialized*100)
sse_bym

ppc_dens_bym_p <- ppc_dens_overlay(data_model_f$p_financialized, 
                                   pred_to_proportion(pp_bym, 
                                                      data_model_f$total),
                                   size = 0.5,
                                   trim=T)
ppc_dens_bym_p

plot_title <- ggtitle("Posterior distributions for binomial regression",
                      "with medians and 95% intervals")
mcmc_areas(as.matrix(brms_log_bym),
           pars = covariate_pars,
           prob = 0.8) + plot_title

# 2. Compare Models ------------------------------------------------------------

## 2.1 Output parameter estimates to LATEX -------------------------------------

coefnames <- c("% renters' housing stress","Median rent",
               "% one year mobility", "% visible minorities",
               "% dwelling in five+ stories", "% pop18-24",
               "Intercept")

mcmcReg(list(brms_linear = brms_linear, brms_logistic, brms_log_bym),  
        pars = covariate_pars,pointest = "mean",
        coefnames = list(coefnames,coefnames,coefnames))

## 2.2 Posterior parameter draws -----------------------------------------------

param_draws_linear <- brms_linear %>% 
  as_draws_df() %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_p_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         `% pop 18-24` = b_p_18_24)

param_draws_log <- brms_logistic %>%
  as_draws_df() %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_p_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         `% pop 18-24` = b_p_18_24)

param_draws_bym <- brms_log_bym %>%
  as_draws_df() %>%
  dplyr::select(covariate_pars) %>%
  rename(Intercept = b_Intercept,
         `% renters' in stress` = b_p_thirty_renter,
         `median rent` = b_p_median_rent,
         `% visible minorities` = b_p_vm,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% 1 year mob.` = b_p_mobility_one_year,
         `% pop 18-24` = b_p_18_24)

combined <- rbind(mcmc_intervals_data(param_draws_linear),
                  mcmc_intervals_data(param_draws_log),
                  mcmc_intervals_data(param_draws_bym))
combined$model <- rep(c("linear", "binomial", "bym"), 
                      each = ncol(param_draws_linear))
combined <- filter(combined, parameter != 'b_Intercept')

pos <- position_nudge(y = ifelse(
  combined$model == "binomial", 0, ifelse(combined$model == "bym", 0.1, 0.2)))

point_est_p <- combined %>%
  ggplot(aes(x = m, y = parameter, color = model)) + 
  geom_vline(xintercept = 0.0, 
             color="red",
             alpha = 1,
             size = 1,
             lty=2) + 
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+
  geom_point(position = pos, size = 1.5) +
  geom_point(position = pos, color = "black", size = 0.5) +
  scale_colour_manual(labels = c("binomial", "bym", "linear"), 
                      values = col_viridis) + 
  xlab("estimate") + 
  theme_minimal()

point_est_p

## 2.3 Posterior parameter draw ridges -----------------------------------------

linear_draws_df <- brms_linear %>%
  as_tibble() %>%
  dplyr::select(covariate_pars) %>%
  gather(key='estimate', value='coefficient') %>%
  mutate(model = 'linear')

log_draws_df <- brms_logistic %>%
  as_tibble() %>%
  dplyr::select(covariate_pars) %>%
  gather(key='estimate', value='coefficient') %>%
  mutate(model = 'binomial')

bym_draws_df <- brms_log_bym %>%
  as_tibble() %>%
  dplyr::select(covariate_pars) %>%
  gather(key='estimate', value='coefficient') %>%
  mutate(model = 'binomial-bym2')

model_draws_df <- linear_draws_df %>%
  bind_rows(log_draws_df, bym_draws_df) %>%
  mutate(model = factor(model, 
                        levels = c('linear','binomial', 'binomial-bym2'))) 

ggplot(model_draws_df, aes(x = coefficient, 
                           y = estimate,
                           fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975),
    alpha=0.5,
    scale=1.5) + 
  scale_fill_manual(
    name = "Probability", 
    values = alpha(c("#FF0000A0", "#0000FFA0", "#FF0000A0"), 0.5),
    alpha(0.5),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) + 
  facet_grid(cols = vars(model), scales = "free") +
  geom_vline(xintercept = 0.0, 
             color="orange",
             alpha = 0.5) + 
  theme_bw()
      
## 2.4 Posterior predictions ---------------------------------------------------

n_draws_points = n_y_rep

ppc_linear <- tibble(
  y_hat = as.vector(t(pp_linear[1:n_draws_points,])),
  y = y_ppc) %>%
  mutate(model = "linear")

ppc_log <- tibble(
  y_hat = as.vector(t(pp_log[1:n_draws_points,])) / counts_ppc,
  y = y_ppc) %>%
  mutate(model = "binomial")

ppc_bym <- tibble(
  y_hat = as.vector(t(pp_bym[1:n_draws_points,])) / counts_ppc,
  y = y_ppc) %>%
  mutate(model = "binomial-bym2")

model_ppc_df <- ppc_linear %>%
  bind_rows(ppc_log, ppc_bym) %>%
  mutate(model = factor(model, 
                        levels = c('linear','binomial', 'binomial-bym2')),
         Prediction = ifelse(y_hat < 0, "Less than 0", "Between 0 and 1")) %>%
  rename(predicted = y_hat, actual = y)

model_ppc_df_p <- model_ppc_df%>%
  ggplot(aes(x = predicted, y = actual, color = Prediction)) + 
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(model)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_colour_manual(labels = c("Between 0 and 1", "Less than 0"), 
                      values = col_2_turbo) + 
  theme_bw()
  #theme_minimal() +
  #theme(strip.text.x = element_text(size = 15))

model_ppc_df_p

## 2.5 PPC Density Overlay -----------------------------------------------------

n_dens_draws = n_y_rep
y_ppc_dens <- rep(data_model_f$p_financialized, 3)
y_pred_ppc_dens <- cbind(pp_linear[1:n_dens_draws,], 
                         pred_to_proportion(pp_log[1:n_dens_draws,], 
                                            data_model_f$total), 
                         pred_to_proportion(pp_bym[1:n_dens_draws,], 
                                            data_model_f$total))
groups_ppc_dens <- factor(cbind(rep("linear", 460), 
                         rep("binomial", 460),
                         rep("binomial-bym2", 460)), 
                         levels = c('linear','binomial', 'binomial-bym2'))

ppc_dens_p <- ppc_dens_overlay_grouped(y = y_ppc_dens,
                                       yrep = y_pred_ppc_dens,
                                       group = groups_ppc_dens,
                                       alpha = 0.1,
                                       size=0.5) + 
  theme_bw()
ppc_dens_p$layers <- c(geom_vline(xintercept = 0, 
                                  color = col_2_turbo[2],
                                  lty = 2), 
                       ppc_dens_p$layers)
ppc_dens_p

## 2.6 Save plots --------------------------------------------------------------

ggsave("output/figures/point_est_p.pdf", plot = point_est_p, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

ggsave("output/figures/model_ppc.pdf", plot = model_ppc_df_p, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

ggsave("output/figures/ppc_dens.pdf", plot = ppc_dens_p, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)
