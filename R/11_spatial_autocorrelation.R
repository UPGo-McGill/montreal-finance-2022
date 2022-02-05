
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

reg.eq1 <- log_financialized ~ p_thirty_renter + n_median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + log_18_24

reg1 <- lm(reg.eq1, data = p_model_f)
summary(reg1)

# 10.2.1. Check residual spatial dependence

lmMoranTest <- lm.morantest(reg1,listw.nonas)
lmMoranTest

lmLMtests <- lm.LMtests(reg1, listw.nonas, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMtests

lmlag <- lagsarlm(reg.eq1, data = p_model_f, listw.nonas, Durbin = FALSE)
summary(lmlag)

lmDurbin <- lagsarlm(reg.eq1, data = p_model_f, listw.nonas, Durbin = TRUE)
summary(lmDurbin)

imDurbin <- impacts(lmDurbin, listw=listw.nonas,R=500)
summary(imDurbin, zstats=TRUE)
