# Load libraries ---------------------------------------------------------------

library(car)
library(lmtest)
library(MASS)
library(spdep)
library(spatialreg)

options(scipen=999)

# Load data --------------------------------------------------------------------

# OLS Models -------------------------------------------------------------------

reg.eq1 <- log_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + log_five_more_storeys + log_18_24
reg.eq2 <- p_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq3 <- p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24
reg.eq4 <- p_financialized ~ ss_thirty_renter + ss_median_rent + ss_mobility_one_year + ss_vm + ss_five_more_storeys + ss_18_24

reg1 <- lm(reg.eq1, data = data_model_f)
summary(reg1)

reg2 <- lm(reg.eq2, data = data_model_f)
summary(reg2)

reg3 <- lm(reg.eq3, data = data_model_f)
summary(reg3)

reg4 <- lm(reg.eq4, data = data_model_f)
summary(reg4)

data_model_f$reg1_res <- reg1$residuals
data_model_f$reg1_fit <- reg1$fitted.values

data_model_f$reg2_res <- reg2$residuals
data_model_f$reg2_fit <- reg2$fitted.values

data_model_f$reg3_res <- reg3$residuals
data_model_f$reg3_fit <- reg3$fitted.values

data_model_f$reg4_res <- reg4$residuals
data_model_f$reg4_fit <- reg4$fitted.values

sse_reg1 <- sum((data_model_f$reg1_fit - data_model_f$p_financialized)^2)
sse_reg1

sse_reg2 <- sum((data_model_f$reg2_fit - data_model_f$p_financialized)^2)
sse_reg2

sse_reg3 <- sum((data_model_f$reg3_fit - data_model_f$p_financialized)^2)
sse_reg3

sse_reg4 <- sum((data_model_f$reg4_fit - data_model_f$p_financialized)^2)
sse_reg4

# OLS Diagnostics --------------------------------------------------------------

dwtest(reg1)
shapiro.test(data_model_f$reg1_res)
ncvTest(reg1)
qqnorm(data_model_f$reg1_res)
qqline(data_model_f$reg1_res)
hist(data_model_f$reg1_res)

ggplot(data_model_f, aes(reg1_fit, reg1_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(dplyr::select(as_tibble(data_model_f), -c(geometry, GeoUID)), 
       aes(row.names(dplyr::select(as_tibble(data_model_f), -c(geometry, GeoUID))), reg1_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

dwtest(reg2)
shapiro.test(data_model_f$reg2_res)
ncvTest(reg2)
qqnorm(data_model_f$reg2_res)
qqline(data_model_f$reg2_res)
hist(data_model_f$reg2_res)

ggplot(data_model_f, aes(reg2_fit, reg2_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(data_model_f, aes(row.names(data_model_f), reg2_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

dwtest(reg3)
shapiro.test(data_model_f$reg3_res)
ncvTest(reg3)
qqnorm(data_model_f$reg3_res)
qqline(data_model_f$reg3_res)
hist(data_model_f$reg3_res)

ggplot(data_model_f, aes(reg3_fit, reg3_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(data_model_f, aes(row.names(data_model_f), reg3_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

dwtest(reg4)
shapiro.test(data_model_f$reg4_res)
ncvTest(reg4)
qqnorm(data_model_f$reg4_res)
qqline(data_model_f$reg4_res)
hist(data_model_f$reg4_res)

ggplot(data_model_f, aes(reg4_fit, reg4_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(data_model_f, aes(row.names(data_model_f), reg4_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

## Loop OLS --------------------------------------------------------------------

data_model_f$reg4_fit <- round(data_model_f$reg4_fit, 2)

data_model_f_no_outliers_OLS1 <- data_model_f %>%
  filter(reg4_fit >= 0)

reg5 <- lm(reg.eq4, data = data_model_f_no_outliers_OLS1)
summary(reg5)

data_model_f_no_outliers_OLS1$reg5_fit <- round(reg5$fitted.values, 2)
sort(data_model_f_no_outliers_OLS1$reg5_fit,decreasing=F)[1:20]

data_model_f_no_outliers_OLS2 <- round(reg5$fitted.values, 3)

# OLS Spatial Diagnostics ------------------------------------------------------

lmMoranTest <- lm.morantest(reg4,queen_adj_listw)
lmLMtests <- lm.LMtests(reg4, queen_adj_listw, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

lmMoranTest
lmLMtests

# Spatial Models ---------------------------------------------------------------

## SLX -------------------------------------------------------------------------

OLS_SLX <- lmSLX(reg.eq4, data = data_model_f, queen_adj_listw)
summary(OLS_SLX)

imSLX <- impacts(OLS_SLX, listw=queen_adj_listw, R=500)
imSLXSum <- summary(imSLX, zstats=TRUE)
imSLXSum

data_model_f$SLX_res <- OLS_SLX$residuals
data_model_f$SLX_fit <- OLS_SLX$fitted.values

sse_slx <- sum((data_model_f$SLX_fit - data_model_f$p_financialized)^2)
sse_slx

plot(data_model_f$SLX_fit, data_model_f$p_financialized)

## SAR -------------------------------------------------------------------------

lmSAR <- lagsarlm(reg.eq4, data = data_model_f, queen_adj_listw, Durbin = FALSE)
summary(lmSAR)

imSAR <- impacts(lmSAR, listw=queen_adj_listw,R=500)
imSARSum <- summary(imSAR, zstats=TRUE)
imSARSum

data_model_f$SAR_res <- lmSAR$residuals
data_model_f$SAR_fit <- lmSAR$fitted.values

sse_sar <- sum((data_model_f$SAR_fit - data_model_f$p_financialized)^2)
sse_sar

plot(data_model_f$SAR_fit, data_model_f$p_financialized)

## SLD -------------------------------------------------------------------------

lmDurbin <- lagsarlm(reg.eq4, data = data_model_f, queen_adj_listw, Durbin = TRUE)
summary(lmDurbin)

imDurbin <- impacts(lmDurbin, listw=queen_adj_listw,R=500)
imDurbinSum <- summary(imDurbin, zstats=TRUE)
imDurbinSum

data_model_f$SLD_res <- lmDurbin$residuals
data_model_f$SLD_fit <- lmDurbin$fitted.values

sse_sld <- sum((data_model_f$SLD_fit - data_model_f$p_financialized)^2)
sse_sld

plot(data_model_f$SLD_fit, data_model_f$p_financialized)

## Spatial Model Diagnostics ---------------------------------------------------

### SLX ------------------------------------------------------------------------

dwtest(OLS_SLX)
shapiro.test(data_model_f$SLX_res)
ncvTest(OLS_SLX)
qqnorm(data_model_f$SLX_res)
qqline(data_model_f$SLX_res)
hist(data_model_f$SLX_res)

ggplot(data_model_f, aes(SLX_fit, SLX_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(data_model_f, aes(row.names(data_model_f), SLX_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

### SAR ------------------------------------------------------------------------

shapiro.test(data_model_f$SAR_res)
qqnorm(data_model_f$SAR_res)
qqline(data_model_f$SAR_res)
hist(data_model_f$SAR_res)

ggplot(data_model_f, aes(SAR_fit, SAR_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(data_model_f, aes(row.names(data_model_f), SAR_res)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

### SLD ------------------------------------------------------------------------

shapiro.test(data_model_f$SLD_res)
qqnorm(data_model_f$SLD_res)
qqline(data_model_f$SLD_res)
hist(data_model_f$SLD_res)

ggplot(data_model_f, aes(SLD_fit, SLD_res)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

ggplot(data_model_f, aes(row.names(data_model_f), SLD_res)) +
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

