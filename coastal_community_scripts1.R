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
crs = 4326 # WGS84

######################################################
######################################################

### 1. Set directories
boundary_dir <- "data//gadm36_levels.gpkg"

## country directories
bra <- "data//country//bra"
fsm <- "data//country//fsm"
idn <- "data//country//idn"
mar <- "data//country//mar"
moz <- "data//country//moz"
phl <- "data//country//phl"
plw <- "data//country//plw"

## Output directory
rare_dir <- "data//rare_community"
  
### 2. Import data
## Brazil
bra_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "BRA")
bra_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "BRA")
bra_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "BRA")
bra_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "BRA") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = NL_NAME_1,
                lvl2_nm = NAME_2,
                lvl2_typ = NL_NAME_2,
                lvl3_nm = NAME_3,
                lvl3_typ = TYPE_3,
                eng3_typ = ENGTYPE_3) %>%
  dplyr::mutate(lvl1_typ = "State",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ, eng3_typ)
bra_level4_para <- st_read(dsn = bra, layer = "bra_level4_para_raw") %>%
  dplyr::rename(lvl4_nm = LOCALIDADE,
                lvl2_nm = MUNICIPIO) %>%
  dplyr::mutate(iso3 = "BRA",
                country = "Brazil",
                lvl2_typ = "Municipality",
                lvl4_typ = "Locality") %>%
  dplyr::select(iso3, country,
                lvl2_nm, lvl2_typ,
                lvl4_nm, lvl4_typ)

## Federated States of Micronesia
fsm_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "FSM")
fsm_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "FSM")

## Guatemala
gtm_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "GTM")
gtm_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "GTM")
gtm_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "GTM") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = NL_NAME_1,
                lvl2_nm = NAME_2,
                lvl2_typ = NL_NAME_2,
                eng2_typ = ENGTYPE_2) %>%
  dplyr::mutate(lvl1_typ = "Department",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ, eng2_typ)

## Honduras
hnd_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "HND")
hnd_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "HND")
hnd_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "HND") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = NL_NAME_1,
                lvl2_nm = NAME_2,
                lvl2_typ = NL_NAME_2,
                eng2_typ = ENGTYPE_2) %>%
  dplyr::mutate(lvl1_typ = "Department",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ, eng2_typ)

hnd_level3 <- st_read(dsn = mar, layer = "hnd_level3_un") %>%
  dplyr::mutate(iso3 = "HND",
                lvl1_typ = "Department",
                lvl2_typ = "Municipality",
                lvl3_typ = "Aldea",
                eng3_typ = "Village") %>%
  dplyr::rename(lvl1_nm = admin1Name,
                lvl2_nm = admin2Name,
                lvl3_nm = admin3Name,
                country = admin0Name) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ, eng3_typ)

## Indonesia
idn_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "IDN")
idn_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "IDN")
idn_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "IDN")
idn_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "IDN") %>%
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
                lvl2_typ = "Regency") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ, eng3_typ)

idn_level4 <- st_read(dsn = idn, layer = "idn_level4_un") %>%
  dplyr::mutate(iso3 = "IDN",
                lvl1_typ = "Province",
                lvl2_typ = "Municipality",
                lvl3_typ = "District",
                lvl4_typ = "Village",
                eng4_typ = "Village") %>%
  dplyr::rename(lvl1_nm = ADM1_EN,
                lvl2_nm = ADM2_EN,
                lvl3_nm = ADM3_EN,
                lvl4_nm = ADM4_EN,
                country = ADM0_EN) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ,
                lvl4_nm, lvl4_typ, eng4_typ)

## Mozambique
moz_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "MOZ")
moz_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "MOZ")
moz_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "MOZ")
moz_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "MOZ") %>%
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
                lvl2_typ = "District") %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl3_nm, lvl3_typ, eng3_typ)

## Palau
plw_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "PLW")
plw_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "PLW") %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = TYPE_1,
                eng1_typ = ENGTYPE_1) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ, eng1_typ)

## Philippines
phl_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "PHL")
phl_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "PHL")
phl_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "PHL")
phl_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "PHL") %>%
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

### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)
head(rare_communities)

bra_communities <- rare_communities %>%
  dplyr::filter(country_code == "BRA") %>%
  dplyr::rename(iso3 = country_code,
               level1 = level1_name,
               level2 = level2_name,
               level3 = level3_name,
               village = level4_name,
               long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

fsm_communities <- rare_communities %>%
  dplyr::filter(country_code == "FSM") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

gtm_communities <- rare_communities %>%
  dplyr::filter(country_code == "GTM") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

hnd_communities <- rare_communities %>%
  dplyr::filter(country_code == "HND") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

idn_communities <- rare_communities %>%
  dplyr::filter(country_code == "IDN") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

moz_communities <- rare_communities %>%
  dplyr::filter(country_code == "MOZ") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

phl_communities <- rare_communities %>%
  dplyr::filter(country_code == "PHL") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

plw_communities <- rare_communities %>%
  dplyr::filter(country_code == "PLW") %>%
  dplyr::rename(iso3 = country_code,
                level1 = level1_name,
                level2 = level2_name,
                level3 = level3_name,
                village = level4_name,
                long = lon) %>%
  dplyr::select(country,
                iso3,
                village,
                level1,
                level2,
                level3,
                ff_community,
                lat,
                long)

### 4. Export data
st_write(obj = bra_level0, dsn = paste0(bra, "/", "bra_level0.shp"), append = F)
st_write(obj = bra_level1, dsn = paste0(bra, "/", "bra_level1.shp"), append = F)
st_write(obj = bra_level2, dsn = paste0(bra, "/", "bra_level2.shp"), append = F)
st_write(obj = bra_level3, dsn = paste0(bra, "/", "bra_level3.shp"), append = F)
st_write(obj = bra_level3, dsn = paste0(bra, "/", "bra_level3.shp"), append = F)
st_write(obj = bra_level4_para, dsn = paste0(bra, "/", "bra_level4_para.shp"), append = F)
write.csv(bra_communities, file = paste0(rare_dir, "/", "bra_communities.csv"))

st_write(obj = fsm_level0, dsn = paste0(fsm, "/", "fsm_level0.shp"), append = F)
st_write(obj = fsm_level1, dsn = paste0(fsm, "/", "fsm_level1.shp"), append = F)
write.csv(fsm_communities, file = paste0(rare_dir, "/", "fsm_communities.csv"))

st_write(obj = gtm_level0, dsn = paste0(mar, "/", "gtm_level0.shp"), append = F)
st_write(obj = gtm_level1, dsn = paste0(mar, "/", "gtm_level1.shp"), append = F)
st_write(obj = gtm_level2, dsn = paste0(mar, "/", "gtm_level2.shp"), append = F)
write.csv(gtm_communities, file = paste0(rare_dir, "/", "gtm_communities.csv"))

st_write(obj = hnd_level0, dsn = paste0(mar, "/", "hnd_level0.shp"), append = F)
st_write(obj = hnd_level1, dsn = paste0(mar, "/", "hnd_level1.shp"), append = F)
st_write(obj = hnd_level2, dsn = paste0(mar, "/", "hnd_level2.shp"), append = F)
st_write(obj = hnd_level3, dsn = paste0(mar, "/", "hnd_level3.shp"), append = F)
write.csv(hnd_communities, file = paste0(rare_dir, "/", "hnd_communities.csv"))

st_write(obj = idn_level0, dsn = paste0(idn, "/", "idn_level0.shp"), append = F)
st_write(obj = idn_level1, dsn = paste0(idn, "/", "idn_level1.shp"), append = F)
st_write(obj = idn_level2, dsn = paste0(idn, "/", "idn_level2.shp"), append = F)
st_write(obj = idn_level3, dsn = paste0(idn, "/", "idn_level3.shp"), append = F)
st_write(obj = idn_level4, dsn = paste0(idn, "/", "idn_level4.shp"), append = F)
write.csv(idn_communities, file = paste0(rare_dir, "/", "idn_communities.csv"))

st_write(obj = moz_level0, dsn = paste0(moz, "/", "moz_level0.shp"), append = F)
st_write(obj = moz_level1, dsn = paste0(moz, "/", "moz_level1.shp"), append = F)
st_write(obj = moz_level2, dsn = paste0(moz, "/", "moz_level2.shp"), append = F)
st_write(obj = moz_level3, dsn = paste0(moz, "/", "moz_level3.shp"), append = F)
write.csv(moz_communities, file = paste0(rare_dir, "/", "moz_communities.csv"))

st_write(obj = plw_level0, dsn = paste0(plw, "/", "plw_level0.shp"), append = F)
st_write(obj = plw_level1, dsn = paste0(plw, "/", "plw_level1.shp"), append = F)
write.csv(plw_communities, file = paste0(rare_dir, "/", "plw_communities.csv"))

st_write(obj = phl_level0, dsn = paste0(phl, "/", "phl_level0.shp"), append = F)
st_write(obj = phl_level1, dsn = paste0(phl, "/", "phl_level1.shp"), append = F)
st_write(obj = phl_level2, dsn = paste0(phl, "/", "phl_level2.shp"), append = F)
st_write(obj = phl_level3, dsn = paste0(phl, "/", "phl_level3.shp"), append = F)
write.csv(phl_communities, file = paste0(rare_dir, "/", "phl_communities.csv"))
