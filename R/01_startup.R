#### 01 STARTUP ################################################################

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(sf)
library(stringr)
library(tidyr)
library(future)


# Set global variables ----------------------------------------------------

#if (Sys.info()["sysname"] != "Windows") {plan(multiprocess)}
col_palette <- 
  c("#FF6600", "#CC6699", "#3399CC", "#FFCC66", "#074387", "#6EEB83", "#008A43", "#FFD500", "#A80858")
#scales::show_col(col_palette)

#library(wesanderson)
#col_wes <- wes_palette("Zissou1", 10, type = "continuous")
#scales::show_col(col_wes)

  
  

  

