
install.packages("spdep")
install.packages("spatialreg")
library(ape)
library(spdep)
library(spatialreg)
library(tidyverse)
library(MASS)
library(car)
library(tidyr)

options(scipen=999)

p <- 
  kmeans_CT %>% 
  left_join(., CT %>% select(GeoUID), by = "GeoUID") %>% 
  st_as_sf()

# 8. Moran's test

w <- poly2nb(p, row.names=p$GeoUID)
class(w)
summary(w)
str(w)
wm <- nb2mat(w, style='B')
n <- nrow(p)
y <- p$p_financialized
ybar <- mean(y)

# dy <- y - ybar
# g <- expand.grid(dy, dy)
# yiyj <- g[,1] * g[,2]

yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj

pm <- matrix(yiyj, ncol=n)

pmw <- pm * wm
pmw

spmw <- sum(pmw)
spmw

smw <- sum(wm)
sw  <- spmw / smw

vr <- n / sum(dy^2)
MI <- vr * sw
MI
EI <- -1/(n-1)
EI

ww <-  nb2listw(w, style='B')
ww

moran(p$p_financialized, ww, n=length(ww$neighbours), S0=Szero(ww))

moran.test(p$p_financialized, ww, randomisation=FALSE)
moran.mc(p$p_financialized, ww, nsim=99)

# 9. Regression Models

# 9.3. Spatial weights

queen.nb <- poly2nb(p) 
rook.nb  <- poly2nb(p, queen=FALSE) 

queen.listw <- nb2listw(queen.nb) 
rook.listw  <- nb2listw(rook.nb) 

listw1 <-  queen.listw

# 10. Classical approach

# 10.2. OLS

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

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

var_tests <- p_model_f %>%
  dplyr::select(-c(geometry, median_rent, p_18_24)) %>%
  as.tibble()%>%
  dplyr::select(-geometry) %>%
  map_dfr(~ ape::Moran.I(., nb2mat(queen.nonas))) %>%
  mutate(variable = colnames(dplyr::select(as.tibble(p_model_f), 
                                   -c(geometry, median_rent, p_18_24)))) %>%
  dplyr::select(variable, observed, expected, sd, p.value)

reg.eq1 <- p_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + log_18_24

reg1 <- lm(reg.eq1, data = p_model_f)
summary(reg1)

# 10.2.1. Check residual spatial dependence

lmMoranTest <- lm.morantest(reg1,listw.nonas)
lmMoranTest

lmLMtests <- lm.LMtests(reg1, listw.nonas, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMtests

OLS_SLX <- lmSLX(reg.eq1, data = p_model_f, listw.nonas)
summary(OLS_SLX)

imSLX <- impacts(OLS_SLX, listw=listw.nonas, R=500)
imSLXSum <- summary(imSLX, zstats=TRUE)
imSLXSum


lmSAR <- lagsarlm(reg.eq1, data = p_model_f, listw.nonas, Durbin = FALSE)
summary(lmSAR)

imSAR <- impacts(lmSAR, listw=listw.nonas,R=500)
imSARSum <- summary(imSAR, zstats=TRUE)
imSARSum

lmDurbin <- lagsarlm(reg.eq1, data = p_model_f, listw.nonas, Durbin = TRUE)
summary(lmDurbin)

imDurbin <- impacts(lmDurbin, listw=listw.nonas,R=500)
imDurbinSum <- summary(imDurbin, zstats=TRUE)
imDurbinSum

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

models = list(reg1, lmSAR, OLS_SLX, lmDurbin)
model_fits <- data.frame(AIC = sapply(models, AIC),
                         AICc = sapply(models, MuMIn::AICc),
                         BIC = sapply(models, BIC))
model_fits
                         