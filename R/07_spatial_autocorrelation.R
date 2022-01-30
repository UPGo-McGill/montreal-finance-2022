
install.packages("spdep")
install.packages("spatialreg")
library(spdep)
library(spatialreg)

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

p

reg.eq1 <- p_financialized ~ p_thirty_renter + median_rent + p_mobility_one_year + p_vm + p_five_more_storeys + p_18_24

reg1 <- lm(reg.eq1, data = p)
summary(reg1)

# 10.2.1. Check residual spatial dependence

lmMoranTest <- lm.morantest(reg1,listw1)
lmMoranTest

lmLMtests <- lm.LMtests(reg1, listw1, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
lmLMtests
