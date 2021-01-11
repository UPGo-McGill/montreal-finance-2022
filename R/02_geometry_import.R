#### 02 GEOMETRY IMPORT ########################################################

source("R/01_startup.R")
library(cancensus)
library(osmdata)

# Montreal DAs ------------------------------------------------------------

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = "2466023"), level = "DA",
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, Dwellings) %>% 
  set_names(c("GeoUID", "dwellings", "geometry")) %>% 
  st_set_agr("constant")


# Montreal boroughs -------------------------------------------------------

boroughs_raw <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") %>% 
  filter(TYPE == "Arrondissement") %>% 
  select(borough = NOM) %>% 
  st_set_agr("constant") %>% 
  st_transform(32618) 

boroughs <- 
  boroughs_raw %>% 
  st_intersection(province)

boroughs <- 
  DA %>% 
  select(dwellings) %>% 
  st_interpolate_aw(boroughs, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(dwellings) %>% 
  cbind(boroughs, .) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  arrange(borough)


# Montreal CSD ------------------------------------------------------------

city <-
  boroughs_raw %>% 
  st_combine() %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_union() %>%
  smoothr::fill_holes(400)


# Save output -------------------------------------------------------------

save(DA, boroughs, boroughs_raw, city, streets, streets_downtown, 
     file = "output/geometry.Rdata")