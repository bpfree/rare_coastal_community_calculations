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

### 2. Data import
## Coastal boundaries
bra_10km_coast <- st_read(dsn = bra, layer = "bra_10km_coast")
fsm_10km_coast <- st_read(dsn = fsm, layer = "fsm_10km_coast")
gtm_10km_coast <- st_read(dsn = mar, layer = "gtm_10km_coast")
hnd_10km_coast <- st_read(dsn = mar, layer = "hnd_10km_coast")
idn_10km_coast <- st_read(dsn = idn, layer = "idn_10km_coast")
moz_10km_coast <- st_read(dsn = moz, layer = "moz_10km_coast")
phl_10km_coast <- st_read(dsn = phl, layer = "phl_10km_coast")
plw_10km_coast <- st_read(dsn = plw, layer = "plw_10km_coast")

## Lowest level data
bra_level3 <- st_read(dsn = bra, layer = "bra_level3")
bra_level4 <- st_read(dsn = bra, layer = "bra_level4_para")
fsm_level1 <- st_read(dsn = fsm, layer = "fsm_level1")
gtm_level2 <- st_read(dsn = mar, layer = "gtm_level2")
hnd_level3 <- st_read(dsn = mar, layer = "hnd_level3")
idn_level4 <- st_read(dsn = idn, layer = "idn_level4")
moz_level3 <- st_read(dsn = moz, layer = "moz_level3")
phl_level3 <- st_read(dsn = phl, layer = "phl_level3")
plw_level1 <- st_read(dsn = plw, layer = "plw_level1")

### 3. Transform data to be in projected coordinate system
## Coastal boundaries
bra_coast10km_wgs84 <- st_transform(bra_10km_coast, crs)
fsm_coast10km_wgs84 <- st_transform(fsm_10km_coast, crs)
gtm_coast10km_wgs84 <- st_transform(gtm_10km_coast, crs)
hnd_coast10km_wgs84 <- st_transform(hnd_10km_coast, crs)
idn_coast10km_wgs84 <- st_transform(idn_10km_coast, crs)
moz_coast10km_wgs84 <- st_transform(moz_10km_coast, crs)
phl_coast10km_wgs84 <- st_transform(phl_10km_coast, crs)
plw_coast10km_wgs84 <- st_transform(plw_10km_coast, crs)

## Lowest level data
bra_level3_wgs84 <- st_transform(bra_level3, crs)
bra_level4_wgs84 <- st_transform(bra_level4, crs)
fsm_level1_wgs84 <- st_transform(fsm_level1, crs)
gtm_level2_wgs84 <- st_transform(gtm_level2, crs)
hnd_level3_wgs84 <- st_transform(hnd_level3, crs)
idn_level4_wgs84 <- st_transform(idn_level4, crs)
moz_level3_wgs84 <- st_transform(moz_level3, crs)
phl_level3_wgs84 <- st_transform(phl_level3, crs)
plw_level1_wgs84 <- st_transform(plw_level1, crs)

### 4. Select coastal communities within coastal area of interest
bra_level3_coast <- st_intersection(bra_level3_wgs84, bra_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)
bra_level4_coast <- st_intersection(bra_level4_wgs84, bra_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)
fsm_level1_coast <- st_intersection(fsm_level1_wgs84, fsm_coast10km_wgs84) %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = TYPE_1,
                eng1_typ = ENGTYPE_1) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ, eng1_typ)
gtm_level2_coast <- st_intersection(gtm_level2_wgs84, gtm_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)
hnd_level3_coast <- st_intersection(hnd_level3_wgs84, hnd_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)
idn_level4_coast <- st_intersection(st_make_valid(idn_level4_wgs84), st_make_valid(idn_coast10km_wgs84)) %>%
  dplyr::select(-GID_0, -NAME_0)
moz_level3_coast <- st_intersection(moz_level3_wgs84, moz_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)
phl_level3_coast <- st_intersection(phl_level3_wgs84, phl_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0, -GID_0_1, -NAME_0_1)
plw_level1_coast <- st_intersection(plw_level1_wgs84, plw_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0, -GID_0_1, -NAME_0_1)


### 5. Export coastal community data
st_write(obj = bra_level3_coast, dsn = paste0(bra, "/", "bra_level3_coastal_communities.shp"), append = F)
st_write(obj = bra_level4_coast, dsn = paste0(bra, "/", "bra_level4_coastal_communities.shp"), append = F)
st_write(obj = fsm_level1_coast, dsn = paste0(fsm, "/", "fsm_level1_coastal_communities.shp"), append = F)
st_write(obj = gtm_level2_coast, dsn = paste0(mar, "/", "gtm_level2_coastal_communities.shp"), append = F)
st_write(obj = hnd_level3_coast, dsn = paste0(mar, "/", "hnd_level3_coastal_communities.shp"), append = F)
st_write(obj = idn_level4_coast, dsn = paste0(idn, "/", "idn_level4_coastal_communities.shp"), append = F)
st_write(obj = moz_level3_coast, dsn = paste0(moz, "/", "moz_level3_coastal_communities.shp"), append = F)
st_write(obj = phl_level3_coast, dsn = paste0(phl, "/", "phl_level3_coastal_communities.shp"), append = F)
st_write(obj = plw_level1_coast, dsn = paste0(plw, "/", "plw_level1_coastal_communities.shp"), append = F)
