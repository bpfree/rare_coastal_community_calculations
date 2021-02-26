################################
# Coastal Community Calculations
################################

######################################################
######################################################

### 0. Preparing the environment and packages

# Clean environment
rm(list = ls())

# Preparing packages
if (!require("pacman")) install.packages("pacman")

# Load packages
pacman::p_load(berryFunctions,dplyr,raster,rgdal,sf,sp, stringr)

######################################################
######################################################

### 1. Set directories
boundary_dir <- "data//gadm36_levels.gpkg"
data_dir <- "data//rare_community"

## country directories
bra <- "data//country//bra"
fsm <- "data//country//fsm"
idn <- "data//country//idn"
mar <- "data//country//mar"
moz <- "data//country//moz"
phl <- "data//country//phl"
plw <- "data//country//plw"

## Output directory
rare_dir <- "data//r_dump"

### 2. Import data
bra_sedac <- st_read(dsn = bra, layer = "bra_sedac")
bra_level4_para <- st_read(dsn = bra, layer = "bra_level4_para")
fsm_sedac <- st_read(dsn = fsm, layer = "fsm_sedac")
gtm_sedac <- st_read(dsn = mar, layer = "gtm_sedac") %>%
  dplyr::rename(iso3 = ISOALPHA,
                country = COUNTRYNM,
                lvl1_nm = NAME1,
                lvl2_nm = NAME2,
                lvl3_nm = NAME3,
                lvl4_nm = NAME4,
                long = CENTROID_X,
                lat = CENTROID_Y) %>%
  dplyr::select(iso3,
                country,
                lvl1_nm,
                lvl2_nm,
                lvl3_nm,
                lvl4_nm,
                long,
                lat)
hnd_sedac <- st_read(dsn = mar, layer = "hnd_sedac")
idn_sedac <- st_read(dsn = idn, layer = "idn_sedac")
moz_sedac <- st_read(dsn = moz, layer = "moz_sedac")
phl_sedac <- st_read(dsn = phl, layer = "phl_sedac")
plw_sedac <- st_read(dsn = plw, layer = "plw_sedac")
