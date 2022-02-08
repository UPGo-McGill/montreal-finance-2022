
#### 12 Statistical models for paper ###########################################

library(bayesplot)
library(BayesPostEst)
library(brms)
library(stats)
library(tidyr)
library(tidybayes)
library(tidyverse)

options(scipen=999)
bayesplot::theme_default()

# Load data --------------------------------------------------------------------

qs::qload("output/stat_model_data.qsm")

# Helper functions ------------------------------------------------------------- 

get_sse <- function(fitted, actual) {
  sse <-  sum((fitted - actual)^2)
  return(sse)
}

plot_fit <- function(fitted, actual) {
  data <- data.frame("predicted" = fitted, "actual" = actual)
  ggplot(data, aes(x = predicted, y = actual)) +
    geom_point(color = "blue", alpha = 0.2)
}

# Models -----------------------------------------------------------------------

## Frequentist Binomial Regression ---------------------------------------------

glm_binomial <- glm(cbind(n_financialized, total) ~ ss_thirty_renter + 
                      ss_median_rent + 
                      ss_mobility_one_year + 
                      ss_vm + 
                      ss_five_more_storeys + 
                      ss_18_24, 
                    data = data_model_f, 
                    family = binomial)
glm_binomial

sse_glm_bin <- get_sse(glm_binomial$fitted.values, data_model_f$p_financialized)
sse_glm_bin

plot_fit(glm_binomial$fitted.values, data_model_f$p_financialized)

## BRMS Linear Regression ------------------------------------------------------

brms_linear_eq <- p_financialized ~ 
  ss_thirty_renter + ss_median_rent + ss_mobility_one_year + 
  ss_vm + ss_five_more_storeys + ss_18_24
lin_formula <- brmsformula(formula = brms_linear_eq) 
mlinear_priors <- get_prior(lin_formula, data=data_model_f)
mlinear_priors$prior[c(2:7)] <- "normal(0, 2)"

brms_linear <- brm(formula = lin_formula, 
                   data = data_model_f,
                   prior = mlinear_priors,
                   seed = 123)

pp_linear <- posterior_predict(brms_linear, draws=1000)
pp_linear_mean <- colMeans(pp_linear)
sse_lin <- get_sse(pp_linear_mean, data_model_f$p_financialized)
sse_lin

ppc_linear <- data.frame(y_hat = pp_linear[1:10,],
                      y = data_model_f$p_financialized)

h <- 0.0
ggplot(ppc_linear, aes(y, y_hat)) + 
  geom_point(color = "blue", alpha = 0.2) +
  geom_hline(yintercept = h, 
             color="red") + 
  geom_text(data=data.frame(x=0,y=h), aes(x, y), 
            label="lower bound", 
            hjust=-5,
            vjust=1.5) +
  theme_bw()

ppc_ecdf_linear_p <- ppc_ecdf_overlay(ppc_linear$y, pp_linear)
ppc_ecdf_linear_p

plot_title <- ggtitle("Posterior distributions for linear regression",
                      "with medians and 80% intervals")
covariate_pars <- c("b_ss_thirty_renter", 
                    "b_ss_median_rent", 
                    "b_ss_mobility_one_year",
                    "b_ss_vm",
                    "b_ss_five_more_storeys",
                    "b_ss_18_24",
                    "b_Intercept")

mcmc_areas(as.matrix(brms_linear),
           pars = covariate_pars,
           prob = 0.8) + plot_title

## Bayesian Linear SAR

## Bayesian GLM ----------------------------------------------------------------

brms_log_eq <- n_financialized | trials(total)  ~
  ss_thirty_renter + ss_median_rent + ss_mobility_one_year + 
  ss_vm + ss_five_more_storeys + ss_18_24
brms_log_formula <- brmsformula(formula = brms_log_eq, 
                                family = binomial(link = "logit")) 
  
brms_log_priors <- get_prior(brms_log_formula, data=data_model_f)
brms_log_priors$prior[c(2:7)] <- "normal(0, 2)"

brms_logistic <- brm(brms_log_formula,
                     data = data_model_f, 
                     prior=brms_log_priors,
                     warmup = 1000, 
                     iter = 2000, 
                     chains = 4, 
                     inits = "random", 
                     cores = 4,
                     seed = 123)

plot(brms_logistic, combo = c("dens", "trace"))

pp_log <- posterior_epred(brms_logistic, draws=1000)
pp_log_mean <- colMeans(pp_log)
sse_log <- get_sse((pp_log_mean / data_model_f$total),
                   data_model_f$p_financialized)
sse_log

ppc_log <- data.frame(y_hat = pp_log_mean/data_model_f$total*100,
                      y = data_model_f$p_financialized*100)

ggplot(ppc_log, aes(y, y_hat)) + 
  geom_point(color = "blue", alpha = 0.2) +
  theme_bw()

ppc_ecdf_log_p <- ppc_ecdf_overlay(ppc_log$y, pp_log)
ppc_ecdf_log_p

plot_title <- ggtitle("Posterior distributions for binomial regression",
                      "with medians and 80% intervals")

mcmc_areas(as.matrix(brms_logistic),
           pars = covariate_pars,
           prob = 0.8) + plot_title


 ## Bayesian GLM with BYM2 priors ----------------------------------------------

data_model_f$gr <- as.factor(seq.int(nrow(data_model_f)))
brms_bym_formula <- brmsformula(formula = brms_log_eq, 
                           family = binomial(link = "logit"),
                           autocor = ~ car(w, gr=gr,type = "bym")) 

stan_data2 = list(w=BYM_adj_mat)
brms_bym_priors <- get_prior(brms_bym_formula, data=data_model_f,data2=stan_data2)
brms_bym_priors$prior[c(2:7)] <- "normal(0, 1)"
brms_bym_priors$prior[10] <- "normal(0, 1)" 
control <- list(max_treedepth = 12,
                adapt_delta = 0.97, 
                stepsize = 0.5)
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

plot(brms_log_bym, combo = c("dens", "trace"))

mcmc_areas(as.matrix(brms_log_bym),
           pars = covariate_pars,
           prob = 0.8) + plot_title


pp_bym <- posterior_epred(brms_log_bym, ndraws = 50)
pp_bym_mean <- colMeans(pp_bym)
sse_bym <- get_sse((pp_bym_mean / data_model_f$total)*100,
                   data_model_f$p_financialized*100)
sse_bym

counts_ppc <- rep(data_model_f$total, 10)
y_ppc  <- rep(data_model_f$p_financialized, 10)

ppc_bym <- tibble(
  y_hat = as.vector(t(pp_bym[1:10,])) / counts_ppc,
  y = y_ppc)

#ppc_bym <- data.frame(y_hat = pp_bym[]data_model_f$total*100,
#                 y = data_model_f$p_financialized*100)

ggplot(ppc_bym, aes(y, y_hat)) + 
  geom_point(color = "blue", alpha = 0.2) +
  theme_bw()

ppc_ecdf_bym_p <- ppc_ecdf_overlay(ppc_bym$y, pp_bym)
ppc_ecdf_bym_p

coefnames <- c("% renters' housing stress","Median rent",
               "% one year mobility", "% visible minorities",
               "% dwelling in five+ stories", "% pop18-24",
               "Intercept")

mcmcReg(list(brms_linear = brms_linear, brms_logistic, brms_log_bym),  
        pars = covariate_pars,pointest = "mean",
        coefnames = list(coefnames,coefnames,coefnames))

# Compare Models ---------------------------------------------------------------

## Posterior draws by coefficient ----------------------------------------------

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
  mutate(model = factor(model, levels = c('linear','binomial', 'binomial-bym2'))) %>% 
  filter(estimate != "b_Intercept")

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
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) + 
  facet_grid(cols = vars(model), scales = "free") +
  geom_vline(xintercept = 0.0, 
             color="orange",
             alpha = 0.5) + 
  theme_bw()
      
## Posterior predictions -------------------------------------------------------

ppc_linear <- tibble(
  y_hat = as.vector(t(pp_linear[1:10,])),
  y = y_ppc) %>%
  mutate(model = "linear")

ppc_log <- tibble(
  y_hat = as.vector(t(pp_log[1:10,])) / counts_ppc,
  y = y_ppc) %>%
  mutate(model = "binomial")

ppc_bym <- tibble(
  y_hat = as.vector(t(pp_bym[1:10,])) / counts_ppc,
  y = y_ppc) %>%
  mutate(model = "binomial-bym2")

model_ppc_df <- ppc_linear %>%
  bind_rows(ppc_log, ppc_bym) %>%
  mutate(model = factor(model, levels = c('linear','binomial', 'binomial-bym2')),
         Prediction = ifelse(y_hat < 0, "Less than 0", "Between 0 and 1")) %>%
  rename(predicted = y_hat, actual = y)
  

ggplot(model_ppc_df, aes(x = predicted, y = actual, color = Prediction)) + 
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(model)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  scale_colour_manual(labels = c("Between 0 and 1", "Less than 0"), 
                      values = c("blue", "red")) + 
  theme_bw()

## Posterior predictions -------------------------------------------------------
