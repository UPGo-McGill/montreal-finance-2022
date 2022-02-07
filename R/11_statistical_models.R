
#### 11 Statistical Models #####################################################

require("spdep")
require("spatialreg")
library(ape)
library(BayesPostEst)
library(brms)
library(car)
library(lmtest)
library(MASS)
library(spdep)
library(spatialreg)
library(tidyr)
library(tidybayes)
library(tidyverse)

options(scipen=999)

# Load data --------------------------------------------------------------------

ct_with_units <- qs::qread("data/CT_with_units.qs")
qs::qload("data/datasets.qsm")

p <- dataset_CT 
#  kmeans_CT %>% 
#  left_join(., ct_with_units %>% dplyr::select(GeoUID, total), by = "GeoUID") %>% 
#  st_as_sf()

#p_disaggregated <- qs::qread("data/buildings.qs") %>%
#  st_join(p, left=FALSE) %>%
#  dplyr::select(-ends_with("y"))


# Preprocessing and Spatial weights --------------------------------------------

scale_center <- function(data, center) {
  scaled_cented <- scale(data, center = center)[,1]
  return(scaled_cented)
}

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

p_model <- p %>%
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

CTs_to_drop <- c("4620550.02", "4620550.04", "4620550.03", "4620317.04", 
                 "4620317.03", "4620317.02")

p_model_f <- p_model %>%
  na.exclude() %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>%
  filter(total != 0)  %>%
  filter(!GeoUID %in% CTs_to_drop)

#%>%
#  filter(!row_number() %in% c(157,178,308))
  
#p_model_f <-  rmapshaper::ms_filter_islands(p_model_f)

#distance.nb <- knn2nb(knearneigh(st_centroid(p_model_f), k=5))
#distance.listw <- nb2listw(distance.nb, zero.policy = TRUE) 
#listw.distance <-distance.listw
#w_mat_dist <- nb2mat(distance.nb, zero.policy = TRUE, style='W')

queen.nonas <- poly2nb(as(p_model_f, 'Spatial'))
queen.listw.nonas <- nb2listw(queen.nonas) 
listw.nonas <-queen.listw.nonas
w_mat <- nb2mat(queen.nonas, style='B',zero.policy=TRUE)
isSymmetric(w_mat,check.attributes=FALSE)

W_list = mat2listw(w_mat) 
nhoods_sf <- st_as_sf(p_model_f)
queen_sf <- as(nb2lines(W_list$neighbours, 
                        coords = coordinates(as(p_model_f, 'Spatial'))), 'sf')
queen_sf <- st_set_crs(queen_sf, st_crs(p_model_f))

adjacency_plot <- ggplot(st_as_sf(p_model_f)) + 
  geom_sf(fill = 'white', color = 'grey70') +
  geom_sf(data = queen_sf, color = "black") +
  theme_void()
adjacency_plot

# Exploratory Univariate Diagnostics -------------------------------------------

ggplot(gather(dplyr::select(as_tibble(p_model_f), 
                            -geometry, 
                            -total, 
                            -n_financialized,
                            -GeoUID)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

p_model_f %>%
  as.tibble() %>%
  dplyr::select(-geometry, -GeoUID) %>%
  gather(-p_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = p_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

p_model_f %>%
  as.tibble() %>%
  dplyr::select(-geometry, -GeoUID) %>%
  gather(-log_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = log_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

# Exploratory Spatial Diagnostics ----------------------------------------------

moran_tests <- p_model_f %>%
  dplyr::select(-c(geometry, median_rent, p_18_24, GeoUID)) %>%
  as.tibble()%>%
  dplyr::select(-geometry, -log_financialized) %>%
  map_dfr(~ ape::Moran.I(., nb2mat(queen.nonas))) %>%
  mutate(variable = colnames(dplyr::select(as.tibble(p_model_f), 
                                   -c(geometry, median_rent, p_18_24, log_financialized, GeoUID)))) %>%
  dplyr::select(variable, observed, expected, sd, p.value)

moran_tests

# OLS Models -------------------------------------------------------------------

reg.eq1 <- log_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + log_five_more_storeys + log_18_24
reg.eq2 <- p_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq3 <- p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq4 <- p_financialized ~ ss_thirty_renter + ss_median_rent + ss_mobility_one_year + ss_vm + ss_five_more_storeys + ss_18_24

reg1 <- lm(reg.eq1, data = p_model_f)
summary(reg1)

reg2 <- lm(reg.eq2, data = p_model_f)
summary(reg2)

reg3 <- lm(reg.eq3, data = p_model_f)
summary(reg3)

reg4 <- lm(reg.eq4, data = p_model_f)
summary(reg4)

p_model_f$reg1_res <- reg1$residuals
p_model_f$reg1_fit <- reg1$fitted.values

p_model_f$reg2_res <- reg2$residuals
p_model_f$reg2_fit <- reg2$fitted.values

p_model_f$reg3_res <- reg3$residuals
p_model_f$reg3_fit <- reg3$fitted.values

p_model_f$reg4_res <- reg4$residuals
p_model_f$reg4_fit <- reg4$fitted.values

sse_reg1 <- sum((p_model_f$reg1_fit - p_model_f$p_financialized)^2)
sse_reg1

sse_reg2 <- sum((p_model_f$reg2_fit - p_model_f$p_financialized)^2)
sse_reg2

sse_reg3 <- sum((p_model_f$reg3_fit - p_model_f$p_financialized)^2)
sse_reg3

sse_reg4 <- sum((p_model_f$reg4_fit - p_model_f$p_financialized)^2)
sse_reg4

# OLS Diagnostics --------------------------------------------------------------

dwtest(reg1)
shapiro.test(p_model_f$reg1_res)
ncvTest(reg1)
qqnorm(p_model_f$reg1_res)
qqline(p_model_f$reg1_res)
hist(p_model_f$reg1_res)

ggplot(p_model_f, aes(reg1_fit, reg1_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(dplyr::select(as_tibble(p_model_f), -c(geometry, GeoUID)), 
       aes(row.names(dplyr::select(as_tibble(p_model_f), -c(geometry, GeoUID))), reg1_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

dwtest(reg2)
shapiro.test(p_model_f$reg2_res)
ncvTest(reg2)
qqnorm(p_model_f$reg2_res)
qqline(p_model_f$reg2_res)
hist(p_model_f$reg2_res)

ggplot(p_model_f, aes(reg2_fit, reg2_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(p_model_f, aes(row.names(p_model_f), reg2_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

dwtest(reg3)
shapiro.test(p_model_f$reg3_res)
ncvTest(reg3)
qqnorm(p_model_f$reg3_res)
qqline(p_model_f$reg3_res)
hist(p_model_f$reg3_res)

ggplot(p_model_f, aes(reg3_fit, reg3_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(p_model_f, aes(row.names(p_model_f), reg3_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

dwtest(reg4)
shapiro.test(p_model_f$reg4_res)
ncvTest(reg4)
qqnorm(p_model_f$reg4_res)
qqline(p_model_f$reg4_res)
hist(p_model_f$reg4_res)

ggplot(p_model_f, aes(reg4_fit, reg4_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(p_model_f, aes(row.names(p_model_f), reg4_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

## Loop OLS --------------------------------------------------------------------

p_model_f$reg4_fit <- round(p_model_f$reg4_fit, 2)

p_model_f_no_outliers_OLS1 <- p_model_f %>%
  filter(reg4_fit >= 0)

reg5 <- lm(reg.eq4, data = p_model_f_no_outliers_OLS1)
summary(reg5)

p_model_f_no_outliers_OLS1$reg5_fit <- round(reg5$fitted.values, 2)
sort(p_model_f_no_outliers_OLS1$reg5_fit,decreasing=F)[1:20]

p_model_f_no_outliers_OLS2 <- round(reg5$fitted.values, 3)


# Logistic models --------------------------------------------------------------

## Frequentist GLM -------------------------------------------------------------
glm_binomial <- stats::glm(cbind(n_financialized, total) ~ ss_thirty_renter + ss_median_rent + ss_mobility_one_year + 
                      ss_vm + ss_five_more_storeys + ss_18_24, data = p_model_f, family = binomial)
glm_binomial

sse_glm_bin <- sum((glm_binomial$fitted.values - p_model_f$p_financialized)^2)
sse_glm_bin

plot(glm_binomial$fitted.values, p_model_f$p_financialized)

## Baysian LM

brms_linear_eq <- p_financialized ~ 
  ss_thirty_renter + ss_median_rent + ss_mobility_one_year + 
  ss_vm + ss_five_more_storeys + ss_18_24
lin_formula <- brmsformula(formula = brms_linear_eq) 
mlinear_priors <- get_prior(lin_formula, data=p_model_f)
mlinear_priors$prior[c(2:7)] <- "normal(0, 2)"

brms_linear <- brm(formula = lin_formula, 
                   data = p_model_f,
                   prior = mlinear_priors,
                   seed = 123)

pp_linear <- posterior_predict(brms_linear, draws=1000)
pp_linear_mean <- colMeans(pp_linear)
sse_lin <- sum((pp_linear_mean - p_model_f$p_financialized)^2)
sse_lin

ppc_linear <- data.frame(y_hat = pp_linear_mean,
                      y = p_model_f$p_financialized)

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
  
brms_log_priors <- get_prior(brms_log_formula, data=p_model_f)
brms_log_priors$prior[c(2:7)] <- "normal(0, 2)"

brms_logistic <- brm(brms_log_formula,
                     data = p_model_f, 
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
sse_log <- sum(((pp_log_mean / p_model_f$total) - p_model_f$p_financialized)^2)
sse_log

ppc_log <- data.frame(y_hat = pp_log_mean/p_model_f$total*100,
                      y = p_model_f$p_financialized*100)

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

#ppc_violin(ppc_log$y, pp_log, alpha = 0, y_draw = "both",
#           size = 1.5, y_alpha = 0.5, y_jitter = 0.33)

 ## Bayesian GLM with BYM2 priors -----------------------------------------------

p_model_f$gr <- as.factor(seq.int(nrow(p_model_f)))
brms_bym_formula <- brmsformula(formula = brms_log_eq, 
                           family = binomial(link = "logit"),
                           autocor = ~ car(w, gr=gr,type = "bym")) 

stan_data2 = list(w=w_mat)
brms_bym_priors <- get_prior(brms_bym_formula, data=p_model_f,data2=stan_data2)
brms_bym_priors$prior[c(2:7)] <- "normal(0, 1)"
brms_bym_priors$prior[10] <- "normal(0, 1)" 
control <- list(max_treedepth = 12,
                adapt_delta = 0.97, 
                stepsize = 0.5)
brms_log_bym <- brm(stan_car_eq, 
                    prior=brms_bym_priors,
                    data = p_model_f, 
                    data2=stan_data2,
                    warmup = 500, 
                    iter = 2000,
                    chains = 4, 
                    inits = "random", 
                    cores = 4,
                    seed = 123,
                    thin = 1,
                    save_pars = save_pars(all = TRUE))

plot(brms_log_bym, combo = c("dens", "trace"))

mcmc_areas(as.matrix(brms_log_bym),
           pars = covariate_pars,
           prob = 0.8) + plot_title


pp_bym <- posterior_epred(brms_log_bym, ndraws = 50)
pp_bym_mean <- colMeans(pp_bym)
sse_bym <- sum(((pp_bym_mean / p_model_f$total)*100 - p_model_f$p_financialized*100)^2)
sse_bym

counts_ppc <- rep(p_model_f$total, 10)
y_ppc  <- rep(p_model_f$p_financialized, 10)

ppc_bym <- tibble(
  y_hat = as.vector(t(pp_bym[1:10,])) / counts_ppc,
  y = y_ppc)

#ppc_bym <- data.frame(y_hat = pp_bym[]p_model_f$total*100,
#                 y = p_model_f$p_financialized*100)

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

# OLS Spatial Diagnostics ------------------------------------------------------

lmMoranTest <- lm.morantest(reg4,listw.nonas)
lmLMtests <- lm.LMtests(reg4, listw.nonas, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

lmMoranTest
lmLMtests

# Spatial Models ---------------------------------------------------------------

## SLX -------------------------------------------------------------------------

OLS_SLX <- lmSLX(reg.eq4, data = p_model_f_no_outliers_1, listw.nonas)
summary(OLS_SLX)

imSLX <- impacts(OLS_SLX, listw=listw.nonas, R=500)
imSLXSum <- summary(imSLX, zstats=TRUE)
imSLXSum

p_model_f$SLX_res <- OLS_SLX$residuals
p_model_f$SLX_fit <- OLS_SLX$fitted.values

sse_slx <- sum((p_model_f$SLX_fit - p_model_f$p_financialized)^2)
sse_slx

plot(p_model_f$SLX_fit, p_model_f$p_financialized)

## SAR -------------------------------------------------------------------------

lmSAR <- lagsarlm(reg.eq4, data = p_model_f, listw.nonas, Durbin = FALSE)
summary(lmSAR)

imSAR <- impacts(lmSAR, listw=listw.nonas,R=500)
imSARSum <- summary(imSAR, zstats=TRUE)
imSARSum

p_model_f$SAR_res <- lmSAR$residuals
p_model_f$SAR_fit <- lmSAR$fitted.values

sse_sar <- sum((p_model_f$SAR_fit - p_model_f$p_financialized)^2)
sse_sar

plot(p_model_f$SAR_fit, p_model_f$p_financialized)

## SLD -------------------------------------------------------------------------

lmDurbin <- lagsarlm(reg.eq4, data = p_model_f, listw.nonas, Durbin = TRUE)
summary(lmDurbin)

imDurbin <- impacts(lmDurbin, listw=listw.nonas,R=500)
imDurbinSum <- summary(imDurbin, zstats=TRUE)
imDurbinSum

p_model_f$SLD_res <- lmDurbin$residuals
p_model_f$SLD_fit <- lmDurbin$fitted.values

sse_sld <- sum((p_model_f$SLD_fit - p_model_f$p_financialized)^2)
sse_sld

plot(p_model_f$SLD_fit, p_model_f$p_financialized)

## Spatial Model Diagnostics ---------------------------------------------------

### SLX ------------------------------------------------------------------------

dwtest(OLS_SLX)
shapiro.test(p_model_f$SLX_res)
ncvTest(OLS_SLX)
qqnorm(p_model_f$SLX_res)
qqline(p_model_f$SLX_res)
hist(p_model_f$SLX_res)

ggplot(p_model_f, aes(SLX_fit, SLX_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(p_model_f, aes(row.names(p_model_f), SLX_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

### SAR ------------------------------------------------------------------------

shapiro.test(p_model_f$SAR_res)
qqnorm(p_model_f$SAR_res)
qqline(p_model_f$SAR_res)
hist(p_model_f$SAR_res)

ggplot(p_model_f, aes(SAR_fit, SAR_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(p_model_f, aes(row.names(p_model_f), SAR_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

### SLD ------------------------------------------------------------------------

shapiro.test(p_model_f$SLD_res)
qqnorm(p_model_f$SLD_res)
qqline(p_model_f$SLD_res)
hist(p_model_f$SLD_res)

ggplot(p_model_f, aes(SLD_fit, SLD_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(p_model_f, aes(row.names(p_model_f), SLD_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

## Impacts ---------------------------------------------------------------------

all_impacts <- data.frame(imSLX$impacts)
colnames(all_impacts) <- c('direct_SLX', 'indirect_SLX', 'total_SLX')
all_impacts[c('direct_SLD', 'indirect_SLD', 'total_SLD')] <- data.frame(imDurbin$res)
all_impacts[c('direct_SAR', 'indirect_SAR', 'total_SAR')]<- data.frame(imSAR$res)

all_impacts[c('direct_SLD_p', 'indirect_SLD_p', 'total_SLD_p')] <- data.frame(imDurbinSum$pzmat)
all_impacts[c('direct_SAR_p', 'indirect_SAR_p', 'total_SAR_p')] <- data.frame(imSARSum$pzmat)
all_impacts[c('direct_SLX_p', 'indirect_SLX_p', 'total_SLX_p')] <- data.frame(imSLXSum$pzmat)

all_impacts <- all_impacts %>%
  dplyr::select(starts_with("direct_SLX"), 
                starts_with("direct_SLD"),
                starts_with("direct_SAR"),
                starts_with("indirect_SLX"), 
                starts_with("indirect_SLD"), 
                starts_with("indirect_SAR"), 
                starts_with("total_SLX"),
                starts_with("total_SLD"),
                starts_with("total_SAR")) %>%
  mutate(across(where(is.numeric), round, 5)) %>%
  mutate(direct_SLX = ifelse(direct_SLX_p < 0.05, direct_SLX, "Not significant"),
         indirect_SLX = ifelse(indirect_SLX_p < 0.05, indirect_SLX, "Not significant"),
         total_SLX = ifelse(total_SLX_p < 0.05, total_SLX, "Not significant"),
         direct_SAR = ifelse(direct_SAR_p < 0.05, direct_SAR, "Not significant"),
         indirect_SAR = ifelse(indirect_SAR_p < 0.05, indirect_SAR, "Not significant"),
         total_SAR = ifelse(total_SAR_p < 0.05, total_SAR, "Not significant"),
         direct_SLD = ifelse(direct_SLD_p < 0.05, direct_SLD, "Not significant"),
         indirect_SLD = ifelse(indirect_SLD_p < 0.05, indirect_SLD, "Not significant"),
         total_SLD = ifelse(total_SLD_p < 0.05, total_SLD, "Not significant"),) %>%
  dplyr::select(-ends_with("_p"))

## Information Criteria --------------------------------------------------------

models = list(reg1, lmSAR, OLS_SLX, lmDurbin)
model_fits <- data.frame(AIC = sapply(models, AIC),
                         AICc = sapply(models, MuMIn::AICc),
                         BIC = sapply(models, BIC))
rownames(model_fits) <- c("OLS", "SAR", "SLX", "SLD")
model_fits
                         