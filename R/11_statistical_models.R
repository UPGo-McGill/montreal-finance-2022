
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

p <- 
  kmeans_CT %>% 
  left_join(., CT %>% select(GeoUID), by = "GeoUID") %>% 
  st_as_sf()

# Preprocessing and Spatial weights --------------------------------------------

queen.nb <- poly2nb(p) 
rook.nb  <- poly2nb(p, queen=FALSE) 

queen.listw <- nb2listw(queen.nb) 
rook.listw  <- nb2listw(rook.nb) 
listw1 <-  queen.listw


p_model <- p %>%
  dplyr::select(p_financialized, 
         p_thirty_renter, 
         median_rent, 
         p_mobility_one_year, 
         p_vm,
         p_five_more_storeys,
         p_18_24) %>%
  mutate(n_median_rent = stdize(p$median_rent, na.rm=TRUE),
         log_financialized = ifelse(p_model$p_financialized == 0, 
                                    p_model$p_financialized, 
                                    log(p_model$p_financialized*100)),
         log_18_24 = ifelse(p_model$p_18_24 == 0, 
                                    p_model$p_18_24, 
                                    log(p_model$p_18_24*100)))

p_model_f <- p_model %>%
  filter(!is.na(p_18_24)) 

queen.nonas <- poly2nb(p_model_f)
queen.listw.nonas <- nb2listw(queen.nonas) 
listw.nonas <-queen.listw.nonas
nb2mat(queen.nonas)

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

reg.eq1 <- log_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + log_18_24
reg.eq2 <- p_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq3 <- p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24

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

# Model Spatial Diagnostics ----------------------------------------------------

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

OLS_SLX <- lmSLX(reg.eq2, data = p_model_f, listw.nonas)
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

### SLX

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
model_fits
                         