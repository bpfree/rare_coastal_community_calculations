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
## National boundary
bra_level0 <- st_read(dsn = bra, layer = "bra_level0")
fsm_level0 <- st_read(dsn = fsm, layer = "fsm_level0")
gtm_level0 <- st_read(dsn = mar, layer = "gtm_level0")
hnd_level0 <- st_read(dsn = mar, layer = "hnd_level0")
idn_level0 <- st_read(dsn = idn, layer = "idn_level0")
moz_level0 <- st_read(dsn = moz, layer = "moz_level0")
phl_level0 <- st_read(dsn = phl, layer = "phl_level0")
plw_level0 <- st_read(dsn = plw, layer = "plw_level0")

### 3. Transform data to be in projected coordinate system
## National boundaries
bra_level0_wgs84 <- st_transform(bra_level0, crs)
fsm_level0_wgs84 <- st_transform(fsm_level0, crs)
gtm_level0_wgs84 <- st_transform(gtm_level0, crs)
hnd_level0_wgs84 <- st_transform(hnd_level0, crs)
idn_level0_wgs84 <- st_transform(idn_level0, crs)
moz_level0_wgs84 <- st_transform(moz_level0, crs)
phl_level0_wgs84 <- st_transform(phl_level0, crs)
plw_level0_wgs84 <- st_transform(plw_level0, crs)

### 4. Convert boundary (polygon) to line (polyline)
bra_level0_polyline <- st_cast(x = bra_level0_wgs84, to = "MULTILINESTRING")
fsm_level0_polyline <- st_cast(x = fsm_level0_wgs84, to = "MULTILINESTRING")
gtm_level0_polyline <- st_cast(x = gtm_level0_wgs84, to = "MULTILINESTRING")
hnd_level0_polyline <- st_cast(x = hnd_level0_wgs84, to = "MULTILINESTRING")
idn_level0_polyline <- st_cast(x = idn_level0_wgs84, to = "MULTILINESTRING")
moz_level0_polyline <- st_cast(x = moz_level0_wgs84, to = "MULTILINESTRING")
phl_level0_polyline <- st_cast(x = phl_level0_wgs84, to = "MULTILINESTRING")
plw_level0_polyline <- st_cast(x = plw_level0_wgs84, to = "MULTILINESTRING")

### 5. Buffer boundary
bra_10km <- st_buffer(bra_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
fsm_10km <- st_buffer(fsm_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
gtm_10km <- st_buffer(gtm_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
hnd_10km <- st_buffer(hnd_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
idn_10km <- st_buffer(idn_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
moz_10km <- st_buffer(moz_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
phl_10km <- st_buffer(phl_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
plw_10km <- st_buffer(plw_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 6. Fix to only land area
bra_10km_land <- st_intersection(bra_10km, bra_level0_wgs84)
fsm_10km_coast <- st_intersection(fsm_10km, fsm_level0_wgs84)
gtm_10km_land <- st_intersection(gtm_10km, gtm_level0_wgs84)
hnd_10km_land <- st_intersection(hnd_10km, hnd_level0_wgs84)
idn_10km_land <- st_intersection(idn_10km, idn_level0_wgs84)
moz_10km_land <- st_intersection(moz_10km, moz_level0_wgs84)
phl_10km_coast <- st_intersection(phl_10km, phl_level0_wgs84)
plw_10km_coast <- st_intersection(plw_10km, plw_level0_wgs84)

### 7. Export data
st_write(obj = bra_10km, dsn = paste0(bra, "/", "bra_10km.shp"), append = F)
st_write(obj = bra_10km_land, dsn = paste0(bra, "/", "bra_10km_land.shp"), append = F)

st_write(obj = fsm_10km, dsn = paste0(fsm, "/", "fsm_10km.shp"), append = F)
st_write(obj = fsm_10km_coast, dsn = paste0(fsm, "/", "fsm_10km_coast.shp"), append = F)

st_write(obj = gtm_10km, dsn = paste0(mar, "/", "gtm_10km.shp"), append = F)
st_write(obj = gtm_10km_land, dsn = paste0(mar, "/", "gtm_10km_land.shp"), append = F)

st_write(obj = hnd_10km, dsn = paste0(mar, "/", "hnd_10km.shp"), append = F)
st_write(obj = hnd_10km_land, dsn = paste0(mar, "/", "hnd_10km_land.shp"), append = F)

st_write(obj = idn_10km, dsn = paste0(idn, "/", "idn_10km.shp"), append = F)
st_write(obj = idn_10km_land, dsn = paste0(idn, "/", "idn_10km_land.shp"), append = F)

st_write(obj = moz_10km, dsn = paste0(moz, "/", "moz_10km.shp"), append = F)
st_write(obj = moz_10km_land, dsn = paste0(moz, "/", "moz_10km_land.shp"), append = F)

st_write(obj = phl_10km, dsn = paste0(phl, "/", "phl_10km.shp"), append = F)
st_write(obj = phl_10km_coast, dsn = paste0(phl, "/", "phl_10km_coast.shp"), append = F)

st_write(obj = plw_10km, dsn = paste0(plw, "/", "plw_10km.shp"), append = F)
st_write(obj = plw_10km_coast, dsn = paste0(plw, "/", "plw_10km_coast.shp"), append = F)

