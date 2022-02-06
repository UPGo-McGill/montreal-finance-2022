
#### 11 Statistical Models #####################################################

require("spdep")
require("spatialreg")
library(ape)
library(spdep)
library(lmtest)
library(spatialreg)
library(tidyverse)
library(MASS)
library(car)
library(tidyr)

options(scipen=999)



# Load data --------------------------------------------------------------------

ct_with_units <- qs::qread("data/CT_with_units.qs")

p <- 
  kmeans_CT %>% 
  left_join(., ct_with_units %>% dplyr::select(GeoUID, total), by = "GeoUID") %>% 
  st_as_sf()

p_disaggregated <- qs::qread("data/buildings.qs") %>%
  st_join(p, left=FALSE) %>%
  dplyr::select(-ends_with("y"))


# Preprocessing and Spatial weights --------------------------------------------

p_model <- p %>%
  dplyr::select(p_financialized, 
         p_thirty_renter, 
         median_rent, 
         p_mobility_one_year, 
         p_vm,
         p_five_more_storeys,
         p_18_24,
         total) %>%
  mutate(n_median_rent = stdize(median_rent, na.rm=TRUE),
         log_financialized = ifelse(p_financialized == 0, 
                                    p_financialized, 
                                    log(p_model$p_financialized*100)),
         log_18_24 = ifelse(p_18_24 == 0, 
                            p_18_24, 
                            log(p_18_24*100)),
         log_five_more_storeys = ifelse(p_five_more_storeys == 0, 
                                        p_five_more_storeys, 
                                        log(p_five_more_storeys*100)),
         logit_financialized =  ifelse(p_financialized == 0, 
                                       p_financialized, 
                                       (1/1-log(p_financialized*100))),
         financialized_units = as.integer(round(p_financialized*total,0))) %>%
  mutate(log_n_median_rent = ifelse(n_median_rent == 0, 
                                    n_median_rent, 
                                    log(n_median_rent*100)))

p_model_f <- p_model %>%
  na.exclude() %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>%
  filter(total != 0) %>%
  filter(!row_number() %in% c(157,178,308))
  
p_model_f <-  rmapshaper::ms_filter_islands(p_model_f)

distance.nb <- knn2nb(knearneigh(st_centroid(p_model_f), k=5))
distance.listw <- nb2listw(distance.nb, zero.policy = TRUE) 
listw.distance <-distance.listw
w_mat_dist <- nb2mat(distance.nb, zero.policy = TRUE, style='W')

queen.nonas <- poly2nb(as(p_model_f, 'Spatial'))
queen.listw.nonas <- nb2listw(queen.nonas) 
listw.nonas <-queen.listw.nonas
w_mat <- nb2mat(queen.nonas, style='B',zero.policy=TRUE)
isSymmetric(w_mat,check.attributes=FALSE)

# Exploratory Univariate Diagnostics -------------------------------------------

ggplot(gather(dplyr::select(as.tibble(p_model_f), -geometry)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

p_model_f %>%
  as.tibble() %>%
  dplyr::select(-geometry) %>%
  gather(-p_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = p_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

p_model_f %>%
  as.tibble() %>%
  dplyr::select(-geometry) %>%
  gather(-log_financialized, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = log_financialized)) +
  geom_point() +
  facet_wrap(~ var, scales = "free")

# Exploratory Spatial Diagnostics ----------------------------------------------

moran_tests <- p_model_f %>%
  dplyr::select(-c(geometry, median_rent, p_18_24)) %>%
  as.tibble()%>%
  dplyr::select(-geometry) %>%
  map_dfr(~ ape::Moran.I(., nb2mat(queen.nonas))) %>%
  mutate(variable = colnames(dplyr::select(as.tibble(p_model_f), 
                                   -c(geometry, median_rent, p_18_24)))) %>%
  dplyr::select(variable, observed, expected, sd, p.value)

moran_tests

# OLS Models -------------------------------------------------------------------

reg.eq1 <- log_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + log_five_more_storeys + log_18_24
reg.eq2 <- p_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq3 <- p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq4 <- logit_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24

reg1 <- lm(reg.eq1, data = p_model_f)
summary(reg1)

reg2 <- lm(reg.eq2, data = p_model_f)
summary(reg2)

reg3 <- lm(reg.eq3, data = p_model_f)
summary(reg3)

p_model_f$reg1_res <- reg1$residuals
p_model_f$reg1_fit <- reg1$fitted.values

p_model_f$reg2_res <- reg2$residuals
p_model_f$reg2_fit <- reg2$fitted.values

p_model_f$reg3_res <- reg3$residuals
p_model_f$reg3_fit <- reg3$fitted.values

reg4 <- lm(reg.eq4, data = p_model_f)
summary(reg4)

p_model_f$reg4_res <- reg4$residuals
p_model_f$reg4_fit <- reg4$fitted.values

p_model_f$reg4_res <- reg4$residuals
p_model_f$reg4_fit <- reg4$fitted.values

p_model_f$reg4_res <- reg4$residuals
p_model_f$reg4_fit <- reg4$fitted.values

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

ggplot(p_model_f, aes(row.names(p_model_f), reg1_res)) +
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

# Logistic models --------------------------------------------------------------

## Frequentist GLM -------------------------------------------------------------
m_glm <- stats::glm(cbind(financialized_units, total) ~ p_thirty_renter + median_rent + p_mobility_one_year + 
                      p_vm + p_five_more_storeys + p_18_24, data = p_model_f, family = binomial)
m_glm

## Bayesian GLM ----------------------------------------------------------------

library(brms)
stan_eq <- financialized_units | trials(total) ~ p_thirty_renter + median_rent + p_mobility_one_year + 
  p_vm + p_five_more_storeys + p_18_24

model_logistic_bayes <- brm(stan_eq, 
                            data = p_model_f, 
                            family = binomial(link = "logit"),
                            warmup = 1000, 
                            iter = 2000, 
                            chains = 4, 
                            inits = "0", 
                            cores = 4,
                            seed = 123)

plot(model_logistic_bayes, combo = c("dens", "trace"))

## Bayesian GLM with BYM2 priors -----------------------------------------------

p_model_f$gr <- as.factor(seq.int(nrow(p_model_f)))
stan_car_eq <- brmsformula(formula = stan_eq, 
                           family = binomial(link = "logit"),
                           autocor = ~ car(w, gr=gr,type = "bym2")) 

stan_data2 = list(w=w_mat)
model_logistic_bayes_bym <- brm(stan_car_eq, 
                            data = p_model_f, 
                            data2=stan_data2,
                            warmup = 500, 
                            iter = 2000, 
                            chains = 4, 
                            inits = "0", 
                            cores = 4,
                            seed = 123,
                            control = list(max_treedepth = 20,
                                           adapt_delta = 0.97, 
                                           stepsize = 0.1))

plot(model_logistic_bayes_icar, combo = c("dens", "trace"))

# OLS Spatial Diagnostics ------------------------------------------------------

lmMoranTest <- lm.morantest(reg1,listw.nonas)
lmMoranTest

lmMoranTest <- lm.morantest(reg2,listw.nonas)
lmMoranTest

lmMoranTest <- lm.morantest(reg3,listw.nonas)
lmMoranTest

lmLMtests <- lm.LMtests(reg2, listw.nonas, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMtests

# Spatial Models ---------------------------------------------------------------

## SLX -------------------------------------------------------------------------

OLS_SLX <- lmSLX(reg.eq1, data = p_model_f, listw.nonas)
summary(OLS_SLX)

imSLX <- impacts(OLS_SLX, listw=listw.nonas, R=500)
imSLXSum <- summary(imSLX, zstats=TRUE)
imSLXSum

p_model_f$SLX_res <- OLS_SLX$residuals
p_model_f$SLX_fit <- OLS_SLX$fitted.values

## SAR -------------------------------------------------------------------------

lmSAR <- lagsarlm(reg.eq1, data = p_model_f, listw.nonas, Durbin = FALSE)
summary(lmSAR)

imSAR <- impacts(lmSAR, listw=listw.nonas,R=500)
imSARSum <- summary(imSAR, zstats=TRUE)
imSARSum

p_model_f$SAR_res <- lmSAR$residuals
p_model_f$SAR_fit <- lmSAR$fitted.values

## SLD -------------------------------------------------------------------------

lmDurbin <- lagsarlm(reg.eq1, data = p_model_f, listw.nonas, Durbin = TRUE)
summary(lmDurbin)

imDurbin <- impacts(lmDurbin, listw=listw.nonas,R=500)
imDurbinSum <- summary(imDurbin, zstats=TRUE)
imDurbinSum

p_model_f$SLD_res <- lmDurbin$residuals
p_model_f$SLD_fit <- lmDurbin$fitted.values

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
                         