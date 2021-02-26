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

### Set CRS
crs = 3395 # WGS84 World Mercator

######################################################
######################################################

### 1. Set directories

## country directories
bra <- "data//country//bra"
fsm <- "data//country//fsm"
idn <- "data//country//idn"
mar <- "data//country//mar"
moz <- "data//country//moz"
phl <- "data//country//phl"
plw <- "data//country//plw"

## output directories
coastal_dir <- "data//coastal"

### 2. Data import
## Coastal communities by ArcGIS "Select by Location"
bra_coastal_communities_Arc <- st_read(dsn = bra, layer = "bra_lvl3full_coast")
fsm_coastal_communities_Arc <- st_read(dsn = fsm, layer = "fsm_lvl1full_coast") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = TYPE_1,
                eng1_typ = ENGTYPE_1) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ, eng1_typ)
gtm_coastal_communities_Arc <- st_read(dsn = mar, layer = "gtm_lvl2full_coast")  %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl2_nm = NAME_2,
                lvl2_typ = TYPE_2,
                eng2_typ = ENGTYPE_2) %>%
  dplyr::mutate(lvl1_typ = "Department") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ, eng2_typ)
hnd_coastal_communities_Arc <- st_read(dsn = mar, layer = "hnd_lvl3full_coast")
idn_coastal_communities_Arc <- st_read(dsn = idn, layer = "idn_lvl4full_coast")
moz_coastal_communities_Arc <- st_read(dsn = moz, layer = "moz_lvl3full_coast") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = NL_NAME_1,
                lvl2_nm = NAME_2,
                lvl2_typ = NL_NAME_2,
                lvl3_nm = NAME_3,
                lvl3_typ = TYPE_3,
                eng3_typ = ENGTYPE_3) %>%
  dplyr::mutate(lvl1_typ = "Province",
                lvl2_typ = "Districts") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ, eng3_typ)
phl_coastal_communities_Arc <- st_read(dsn = phl, layer = "phl_lvl3full_coast") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = NL_NAME_1,
                lvl2_nm = NAME_2,
                lvl2_typ = NL_NAME_2,
                lvl3_nm = NAME_3,
                lvl3_typ = TYPE_3,
                eng3_typ = ENGTYPE_3) %>%
  dplyr::mutate(lvl1_typ = "Province",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ, eng3_typ)
plw_coastal_communities_Arc <- st_read(dsn = plw, layer = "plw_lvl1full_coast") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = TYPE_1,
                eng1_typ = ENGTYPE_1) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ, eng1_typ)

## Coastal communities by R "Intersection"
bra_coastal_communities_R <- st_read(dsn = bra, layer = "bra_level3_coastal_communities")
bra_para_coastal_communities_R <- st_read(dsn = bra, layer = "bra_level4_coastal_communities")
fsm_coastal_communities_R <- st_read(dsn = fsm, layer = "fsm_level1_coastal_communities")
gtm_coastal_communities_R <- st_read(dsn = mar, layer = "gtm_level2_coastal_communities")
hnd_coastal_communities_R <- st_read(dsn = mar, layer = "hnd_level3_coastal_communities")
idn_coastal_communities_R <- st_read(dsn = idn, layer = "idn_level4_coastal_communities")
moz_coastal_communities_R <- st_read(dsn = moz, layer = "moz_level3_coastal_communities")
phl_coastal_communities_R <- st_read(dsn = phl, layer = "phl_level3_coastal_communities")
plw_coastal_communities_R <- st_read(dsn = plw, layer = "plw_level1_coastal_communities")

### X. Export data for later analysis
st_write(obj = bra_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "bra_coastal_communities.shp"), append = F)
st_write(obj = fsm_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "fsm_coastal_communities.shp"), append = F)
st_write(obj = gtm_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "gtm_coastal_communities.shp"), append = F)
st_write(obj = hnd_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "hnd_coastal_communities.shp"), append = F)
st_write(obj = idn_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "idn_coastal_communities.shp"), append = F)
st_write(obj = moz_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "moz_coastal_communities.shp"), append = F)
st_write(obj = phl_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "phl_coastal_communities.shp"), append = F)
st_write(obj = plw_coastal_communities_Arc, dsn = paste0(coastal_dir, "/", "plw_coastal_communities.shp"), append = F)

st_write(obj = bra_coastal_communities_R, dsn = paste0(coastal_dir, "/", "bra_coastal_communities_r.shp"), append = F)
st_write(obj = bra_para_coastal_communities_R, dsn = paste0(coastal_dir, "/", "bra_para_coastal_communities_r.shp"), append = F)
st_write(obj = fsm_coastal_communities_R, dsn = paste0(coastal_dir, "/", "fsm_coastal_communities_r.shp"), append = F)
st_write(obj = gtm_coastal_communities_R, dsn = paste0(coastal_dir, "/", "gtm_coastal_communities_r.shp"), append = F)
st_write(obj = hnd_coastal_communities_R, dsn = paste0(coastal_dir, "/", "hnd_coastal_communities_r.shp"), append = F)
st_write(obj = idn_coastal_communities_R, dsn = paste0(coastal_dir, "/", "idn_coastal_communities_r.shp"), append = F)
st_write(obj = moz_coastal_communities_R, dsn = paste0(coastal_dir, "/", "moz_coastal_communities_r.shp"), append = F)
st_write(obj = phl_coastal_communities_R, dsn = paste0(coastal_dir, "/", "phl_coastal_communities_r.shp"), append = F)
st_write(obj = plw_coastal_communities_R, dsn = paste0(coastal_dir, "/", "plw_coastal_communities_r.shp"), append = F)
