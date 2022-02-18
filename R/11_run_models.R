#### 11 STATISTICAL MODELS FOR PAPER ###########################################

# 0. Preamble ------------------------------------------------------------------

# 0.1 Libraries and options ----------------------------------------------------

source("R/01_startup.R")

library(bayesplot)
library(BayesPostEst)
library(brms)
library(ggridges)
library(scales)
library(stats)
library(tidybayes)

options(scipen = 999)


# 0.2 Load data ----------------------------------------------------------------

qs::qload("output/stat_model_data.qsm")


# 0.3 Helper functions --------------------------------------------------------- 

get_sse <- function(fitted, actual) {
  sse <-  sum((fitted - actual) ^ 2)
  return(sse)
}

plot_fit <- function(fitted, actual) {
  data <- data.frame("predicted" = "predicted", "actual" = actual)
  ggplot(data, aes(x = predicted, y = actual)) +
    geom_point(color = "blue", alpha = 0.2) +
    theme_bw()
}

pred_to_proportion <- function(draw_m, totals, n) {
  as_prop <- t(t(draw_m[1:n,]) / totals)
  return(as_prop)
}


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

glm_binomial <- glm(glm_eq, data = data_model, family = binomial)


### 1.1.3 Eval model -----------------------------------------------------------

# Model summary
summary(glm_binomial)

# SSE
get_sse(glm_binomial$fitted.values, data_model$p_financialized)

# Fit plot
plot_fit(glm_binomial$fitted.values, data_model$p_financialized)


## 1.2 BRMS Linear Regression --------------------------------------------------

### 1.2.1 General setup --------------------------------------------------------

ndraws <- 4000
warmup <- 4000
iterations <- 10000
seed <- 123
chains <- 8
cores <- 8
inits <- "random"
save_m_pars <- save_pars(all = TRUE)

covariate_pars <- c("b_Intercept",
                    "b_n_median_rent", 
                    "b_p_thirty_renter",
                    "b_n_average_age",
                    "b_p_vm",
                    "b_p_mobility_one_year",
                    "b_p_five_more_storeys",
                    #"b_p_18_24",
                    "b_p_built_after_2005")


### 1.2.2 Prep model params ----------------------------------------------------

brms_linear_eq <- p_financialized ~ 
  n_median_rent + 
  p_thirty_renter + 
  n_average_age +
  #p_18_24  +
  p_vm + 
  p_mobility_one_year + 
  p_five_more_storeys + 
  p_built_after_2005

lin_formula <- brmsformula(formula = brms_linear_eq) 
mlinear_priors <- get_prior(lin_formula, data = data_model)
mlinear_priors$prior[c(2:7)] <- "normal(0, 2)"


### 1.2.3 Run model ------------------------------------------------------------

brms_linear <- brm(formula = lin_formula, 
                   data = data_model,
                   prior = mlinear_priors,
                   warmup = warmup,
                   iter = iterations,
                   seed = seed,
                   chains = chains,
                   cores = cores,
                   inits = inits,
                   save_pars = save_m_pars)


### 1.2.4 Eval model -----------------------------------------------------------

plot(brms_linear, combo = c("dens", "trace"))
pairs(brms_linear)

pp_linear <- posterior_predict(brms_linear, ndraws = ndraws)
get_sse(colMeans(pp_linear), data_model$p_financialized)

# PPC density overlay
ppc_dens_overlay(data_model$p_financialized, pp_linear[1:100,], size = 0.5,
                 trim = TRUE)

# Posterior distributions
mcmc_areas(as.matrix(brms_linear), pars = covariate_pars, prob = 0.95) + 
  ggtitle("Posterior distributions for linear regression",
          "with medians and 80% intervals") +
  vline_0(colour = "orange") +
  theme_bw()


## 1.3. Bayesian binomial regression -------------------------------------------

### 1.3.1 Prep model params ----------------------------------------------------

brms_bin_eq <- n_financialized | trials(total)  ~
  n_median_rent + 
  p_thirty_renter + 
  n_average_age +
  #p_18_24  +
  p_vm + 
  p_mobility_one_year + 
  p_five_more_storeys + 
  p_built_after_2005

brms_bin_formula <- brmsformula(formula = brms_bin_eq, 
                                family = binomial(link = "logit")) 
  
brms_bin_priors <- get_prior(brms_bin_formula, data=data_model)
brms_bin_priors$prior[c(2:7)] <- "normal(0, 2)"


### 1.3.2 Run model ------------------------------------------------------------

brms_binomial <- brm(brms_bin_formula,
                     data = data_model, 
                     prior = brms_bin_priors,
                     warmup = warmup, 
                     iter = iterations, 
                     chains = chains, 
                     inits = inits, 
                     cores = cores,
                     seed = seed,
                     save_pars = save_m_pars)


### 1.3.3 Eval model -----------------------------------------------------------

plot(brms_binomial, combo = c("dens", "trace"))
pairs(brms_binomial)

pp_bin <- posterior_predict(brms_binomial, ndraws = ndraws)
get_sse((colMeans(pp_bin) / data_model$total), data_model$p_financialized)

# PPC density overlay
ppc_dens_overlay(data_model$p_financialized, 
                 pred_to_proportion(pp_bin, data_model$total, 100),
                 size = 0.5,
                 trim = TRUE)

# Posterior distributions
mcmc_areas(as.matrix(brms_binomial), pars = covariate_pars, prob = 0.95) + 
  ggtitle("Posterior distributions for binomial regression",
          "with medians and 95% intervals") +
  vline_0(colour = "orange") +
  theme_bw()


## 1.4. Bayesian binomial regression with BYM2 priors --------------------------

### 1.4.1 Prep model params ----------------------------------------------------

data_model$gr <- as.factor(seq.int(nrow(data_model)))
brms_bym_formula <- brmsformula(formula = brms_bin_eq, 
                                family = binomial(link = "logit"),
                                autocor = ~ car(w, gr = gr,type = "bym")) 

stan_data2 <- list(w = BYM_adj_mat)
brms_bym_priors <- get_prior(brms_bym_formula, data = data_model, 
                             data2 = stan_data2)
brms_bym_priors$prior[c(2:7)] <- "normal(0, 1)"
brms_bym_priors$prior[10] <- "normal(0, 1)" 
control <- list(max_treedepth = 12, adapt_delta = 0.97, stepsize = 0.5)


### 1.4.2 Run model ------------------------------------------------------------

brms_bym <- brm(brms_bym_formula, 
                prior = brms_bym_priors,
                data = data_model, 
                data2 = stan_data2,
                warmup = warmup, 
                iter = iterations,
                chains = chains, 
                inits = inits, 
                cores = cores,
                seed = seed,
                thin = 1,
                save_pars = save_m_pars,
                control = control)


### 1.4.3 Eval model -----------------------------------------------------------

plot(brms_bym, combo = c("dens", "trace"))
pairs(brms_bym, pars = covariate_pars)

pp_bym <- posterior_predict(brms_bym, ndraws = ndraws)
get_sse((colMeans(pp_bym) / data_model$total), data_model$p_financialized)

# PPC density overlay
ppc_dens_overlay(data_model$p_financialized, 
                 pred_to_proportion(pp_bym, data_model$total, 100),
                 size = 0.5, trim = TRUE)

# Posterior distributions
mcmc_areas(as.matrix(brms_bym), pars = covariate_pars, prob = 0.95) + 
  ggtitle("Posterior distributions for binomial regression",
          "with medians and 95% intervals") +
  vline_0(colour = "orange") +
  theme_bw()


# 2. Compare Models ------------------------------------------------------------

## 2.1 Output parameter estimates to LATEX -------------------------------------

coefnames <- c("Intercept",
               "Median rent",
               "% renters' housing stress",
               "% average age", 
               "% visible minorities",
               "% one year mobility", 
               "% dwelling in five+ stories", 
               "% units built after 2005")

mcmcReg(list(brms_linear, brms_binomial, brms_bym),  
        pars = covariate_pars, pointest = "mean",
        coefnames = list(coefnames, coefnames, coefnames))


## 2.2 Posterior parameter draws -----------------------------------------------

param_draws_linear <- 
  brms_linear |> 
  as_draws_df() |> 
  select(all_of(covariate_pars)) |> 
  rename(Intercept = b_Intercept,
         `median rent` = b_n_median_rent,
         `% renters' in stress` = b_p_thirty_renter,
         `average age` = b_n_average_age, 
         `% visible minorities` = b_p_vm,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% units built after 2005` = b_p_built_after_2005)

param_draws_log <- 
  brms_binomial |> 
  as_draws_df() |> 
  select(all_of(covariate_pars)) |> 
  rename(Intercept = b_Intercept,
         `median rent` = b_n_median_rent,
         `% renters' in stress` = b_p_thirty_renter,
         `average age` = b_n_average_age, 
         `% visible minorities` = b_p_vm,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% units built after 2005` = b_p_built_after_2005)

param_draws_bym <- 
  brms_bym |> 
  as_draws_df() |> 
  select(all_of(covariate_pars)) |> 
  rename(Intercept = b_Intercept,
         `median rent` = b_n_median_rent,
         `% renters' in stress` = b_p_thirty_renter,
         `average age` = b_n_average_age, 
         `% visible minorities` = b_p_vm,
         `% 1 year mob.` = b_p_mobility_one_year,
         #`% pop 18-24` = b_p_18_24,
         `% dwelling in 5+ st.` = b_p_five_more_storeys,
         `% units built after 2005` = b_p_built_after_2005)

combined <- bind_rows(
  mcmc_intervals_data(param_draws_linear, prob = 0.95, prob_outer = 1),
  mcmc_intervals_data(param_draws_log, prob = 0.95, prob_outer = 1),
  mcmc_intervals_data(param_draws_bym, prob = 0.95, prob_outer = 1)) |> 
  mutate(model = rep(c("linear", "binomial", "bym"), 
                     each = ncol(param_draws_linear))) |> 
  filter(parameter != 'Intercept')


# 3.0 Finalize -----------------------------------------------------------------

## 3.0 Save models and outputs -------------------------------------------------

fileConn <- file("output/models/brms_linear.stan")
writeLines(brms_linear$model, fileConn)
close(fileConn)

fileConn <- file("output/models/brms_binomial.stan")
writeLines(brms_binomial$model, fileConn)
close(fileConn)

fileConn <- file("output/models/brms_bym.stan")
writeLines(brms_bym$model, fileConn)
close(fileConn)

qsave(brms_linear, "output/models/brms_linear.qs")
qsave(brms_binomial, "output/models/brms_binomial.qs")
qsave(brms_bym, "output/models/brms_bym.qs", nthreads = availableCores())
qsavem(combined, n_y_rep, pp_linear, pp_bin, pp_bym,
       file = "output/models/extra.qsm")


## 3.1 Clean up ----------------------------------------------------------------

rm(get_sse, plot_fit, pred_to_proportion, glm_eq, ndraws, warmup, iterations, 
   seed, chains, cores, inits, save_m_pars, covariate_pars, brms_linear_eq,
   lin_formula, mlinear_priors, brms_bin_eq, brms_bin_formula, brms_bin_priors,
   brms_bym_formula, brms_bym_priors, control, coefnames, param_draws_linear,
   param_draws_log, param_draws_bym)
