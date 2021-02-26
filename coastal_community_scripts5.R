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
coastal_dir <- "data//coastal"

## output directory
table_dir <- "data//coastal//coastal_summary_tables"
output_dir <- "reporting_tables"

### 2. Load cleaned coastal data
bra_coastal_communities <- st_read(coastal_dir, "bra_coastal_communities")
bra_para_coastal_communities <- st_read(coastal_dir, "bra_para_coastal_communities_r")
fsm_coastal_communities <- st_read(coastal_dir, "fsm_coastal_communities")
gtm_coastal_communities <- st_read(coastal_dir, "gtm_coastal_communities")
hnd_coastal_communities <- st_read(coastal_dir, "hnd_coastal_communities")
idn_coastal_communities <- st_read(coastal_dir, "idn_coastal_communities")
moz_coastal_communities <- st_read(coastal_dir, "moz_coastal_communities")
phl_coastal_communities <- st_read(coastal_dir, "phl_coastal_communities")
plw_coastal_communities <- st_read(coastal_dir, "plw_coastal_communities")

### 3. Entire country coastal calculations
## Brazil
bra_lvl1 <- bra_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl1_typ = "State") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
bra_lvl2 <- bra_coastal_communities %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
bra_lvl3 <- bra_coastal_communities %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl3_typ = "District") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)
bra_lvl4 <- bra_para_coastal_communities %>%
  count(lvl4_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl4_typ = "Village") %>%
  dplyr::select(iso3, lvl4_typ, lvl4_nm, count)

## Federated States of Micronesia
fsm_lvl1 <- fsm_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "FSM",
                lvl1_typ = "State") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)

## Guatemala
gtm_lvl1 <- gtm_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "GTM",
                lvl1_typ = "Department") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
gtm_lvl2 <- gtm_coastal_communities %>%
  count(lvl2_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "GTM",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)

## Honduras
hnd_lvl1 <- hnd_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "HND",
                lvl1_typ = "Department") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
hnd_lvl2 <- hnd_coastal_communities %>%
  count(lvl2_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "HND",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
hnd_lvl3 <- hnd_coastal_communities %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "HND",
                lvl3_typ = "Village") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

## Indonesia
idn_lvl1 <- idn_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl1_typ = "Province") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
idn_lvl2 <- idn_coastal_communities %>%
  count(lvl2_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
idn_lvl3 <- idn_coastal_communities %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl3_typ = "District") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)
idn_lvl4 <- idn_coastal_communities %>%
  count(lvl4_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl4_typ = "Village") %>%
  dplyr::select(iso3, lvl4_typ, lvl4_nm, count)

## Mozambique
moz_lvl1 <- moz_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "MOZ",
                lvl1_typ = "Province") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
moz_lvl2 <- moz_coastal_communities %>%
  count(lvl2_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "MOZ",
                lvl2_typ = "District") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
moz_lvl3 <- moz_coastal_communities %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "MOZ",
                lvl3_typ = "Locality") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

## Philippines
phl_lvl1 <- phl_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PHL",
                lvl1_typ = "Province") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
phl_lvl2 <- phl_coastal_communities %>%
  count(lvl2_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PHL",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
phl_lvl3 <- phl_coastal_communities %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PHL",
                lvl3_typ = "Village") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

## Palau
plw_lvl1 <- plw_coastal_communities %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PLW",
                lvl1_typ = "State") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)


### 4. Make data null
## Level 1
st_geometry(bra_lvl1) <- NULL
st_geometry(fsm_lvl1) <- NULL
st_geometry(gtm_lvl1) <- NULL
st_geometry(hnd_lvl1) <- NULL
st_geometry(idn_lvl1) <- NULL
st_geometry(moz_lvl1) <- NULL
st_geometry(phl_lvl1) <- NULL
st_geometry(plw_lvl1) <- NULL

## Level 2
st_geometry(bra_lvl2) <- NULL
st_geometry(gtm_lvl2) <- NULL
st_geometry(hnd_lvl2) <- NULL
st_geometry(idn_lvl2) <- NULL
st_geometry(moz_lvl2) <- NULL
st_geometry(phl_lvl2) <- NULL

## Level 3
st_geometry(bra_lvl3) <- NULL
st_geometry(hnd_lvl3) <- NULL
st_geometry(idn_lvl3) <- NULL
st_geometry(moz_lvl3) <- NULL
st_geometry(phl_lvl3) <- NULL

## Level 4
st_geometry(bra_lvl4) <- NULL
st_geometry(idn_lvl4) <- NULL

### 5. Combine level data
## Level 1
rare_lvl1 <- rbind(bra_lvl1,
                   fsm_lvl1,
                   gtm_lvl1,
                   hnd_lvl1,
                   idn_lvl1,
                   moz_lvl1,
                   phl_lvl1,
                   plw_lvl1)

## Level 2
rare_lvl2 <- rbind(bra_lvl2,
                   gtm_lvl2,
                   hnd_lvl2,
                   idn_lvl2,
                   moz_lvl2,
                   phl_lvl2)

## Level 3
rare_lvl3 <- rbind(bra_lvl3,
                   hnd_lvl3,
                   idn_lvl3,
                   moz_lvl3,
                   phl_lvl3)

## Villages
bra_vill <- bra_lvl4 %>%
  dplyr::rename(village = lvl4_nm) %>%
  dplyr::select(-lvl4_typ)
hnd_vill <- hnd_lvl3 %>%
  dplyr::rename(village = lvl3_nm) %>%
  dplyr::select(-lvl3_typ)
idn_vill <- idn_lvl4 %>%
  dplyr::rename(village = lvl4_nm) %>%
  dplyr::select(-lvl4_typ)
moz_vill <- moz_lvl3 %>%
  dplyr::rename(village = lvl3_nm) %>%
  dplyr::select(-lvl3_typ)
phl_vill <- phl_lvl3 %>%
  dplyr::rename(village = lvl3_nm) %>%
  dplyr::select(-lvl3_typ)

rare_villages <- rbind(bra_vill,
                       hnd_vill,
                       idn_vill,
                       moz_vill,
                       phl_vill)

### 6. Save data
write.csv(bra_lvl1, file = paste0(table_dir, "/", "bra_level1_summary.csv"))
write.csv(bra_lvl2, file = paste0(table_dir, "/", "bra_level2_summary.csv"))
write.csv(bra_lvl3, file = paste0(table_dir, "/", "bra_level3_summary.csv"))
write.csv(bra_lvl4, file = paste0(table_dir, "/", "bra_level4_summary.csv"))

write.csv(fsm_lvl1, file = paste0(table_dir, "/", "fsm_level1_summary.csv"))

write.csv(gtm_lvl1, file = paste0(table_dir, "/", "gtm_level1_summary.csv"))
write.csv(gtm_lvl2, file = paste0(table_dir, "/", "gtm_level2_summary.csv"))

write.csv(hnd_lvl1, file = paste0(table_dir, "/", "hnd_level1_summary.csv"))
write.csv(hnd_lvl2, file = paste0(table_dir, "/", "hnd_level2_summary.csv"))
write.csv(hnd_lvl3, file = paste0(table_dir, "/", "hnd_level3_summary.csv"))

write.csv(idn_lvl1, file = paste0(table_dir, "/", "idn_level1_summary.csv"))
write.csv(idn_lvl2, file = paste0(table_dir, "/", "idn_level2_summary.csv"))
write.csv(idn_lvl3, file = paste0(table_dir, "/", "idn_level3_summary.csv"))
write.csv(idn_lvl4, file = paste0(table_dir, "/", "idn_level4_summary.csv"))

write.csv(moz_lvl1, file = paste0(table_dir, "/", "moz_level1_summary.csv"))
write.csv(moz_lvl2, file = paste0(table_dir, "/", "moz_level2_summary.csv"))
write.csv(moz_lvl3, file = paste0(table_dir, "/", "moz_level3_summary.csv"))

write.csv(phl_lvl1, file = paste0(table_dir, "/", "phl_level1_summary.csv"))
write.csv(phl_lvl2, file = paste0(table_dir, "/", "phl_level2_summary.csv"))
write.csv(phl_lvl3, file = paste0(table_dir, "/", "phl_level3_summary.csv"))

write.csv(plw_lvl1, file = paste0(table_dir, "/", "plw_level1_summary.csv"))

write.csv(rare_lvl1, file = paste0(output_dir, "/", "rare_level1_summary.csv"))
write.csv(rare_lvl2, file = paste0(output_dir, "/", "rare_level2_summary.csv"))
write.csv(rare_lvl3, file = paste0(output_dir, "/", "rare_level3_summary.csv"))
write.csv(rare_villages, file = paste0(output_dir, "/", "rare_villages_summary.csv"))
