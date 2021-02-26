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

## coastal directory
table_dir <- "data//coastal//coastal_summary_tables"

## Rare community directory
rare_dir <- "data//rare_community"

## Report directory
report_dir <- "reporting_tables"

### 2. Import data
## Rare location sheets
rare_bra <- read.csv(paste(rare_dir, "bra_communities.csv", sep = "/"), as.is = T) %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_fsm <- read.csv(paste(rare_dir, "fsm_communities.csv", sep = "/"), as.is = T) %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_gtm <- read.csv(paste(rare_dir, "gtm_communities.csv", sep = "/"), as.is = T) %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_hnd <- read.csv(paste(rare_dir, "hnd_communities.csv", sep = "/"), as.is = T)  %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_idn <- read.csv(paste(rare_dir, "idn_communities.csv", sep = "/"), as.is = T)  %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_moz <- read.csv(paste(rare_dir, "moz_communities.csv", sep = "/"), as.is = T)  %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_phl <- read.csv(paste(rare_dir, "phl_communities.csv", sep = "/"), as.is = T)  %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

rare_plw <- read.csv(paste(rare_dir, "plw_communities.csv", sep = "/"), as.is = T)  %>%
  dplyr::select(iso3, country, village,
                level1, level2, level3,
                ff_community,
                lat, long)

## Import country level data
bra_lvl1_coast_count <- read.csv(paste(table_dir, "bra_level1_summary.csv", sep = "/"), as.is = T)
bra_lvl2_coast_count <- read.csv(paste(table_dir, "bra_level2_summary.csv", sep = "/"), as.is = T)
bra_lvl3_coast_count <- read.csv(paste(table_dir, "bra_level3_summary.csv", sep = "/"), as.is = T)

fsm_lvl1_coast_count <- read.csv(paste(table_dir, "fsm_level1_summary.csv", sep = "/"), as.is = T)

gtm_lvl1_coast_count <- read.csv(paste(table_dir, "gtm_level1_summary.csv", sep = "/"), as.is = T)
gtm_lvl2_coast_count <- read.csv(paste(table_dir, "gtm_level2_summary.csv", sep = "/"), as.is = T)

hnd_lvl1_coast_count <- read.csv(paste(table_dir, "hnd_level1_summary.csv", sep = "/"), as.is = T)
hnd_lvl2_coast_count <- read.csv(paste(table_dir, "hnd_level2_summary.csv", sep = "/"), as.is = T)
hnd_lvl3_coast_count <- read.csv(paste(table_dir, "hnd_level3_summary.csv", sep = "/"), as.is = T)

idn_lvl1_coast_count <- read.csv(paste(table_dir, "idn_level1_summary.csv", sep = "/"), as.is = T)
idn_lvl2_coast_count <- read.csv(paste(table_dir, "idn_level2_summary.csv", sep = "/"), as.is = T)
idn_lvl3_coast_count <- read.csv(paste(table_dir, "idn_level3_summary.csv", sep = "/"), as.is = T)
idn_lvl4_coast_count <- read.csv(paste(table_dir, "idn_level4_summary.csv", sep = "/"), as.is = T)

moz_lvl1_coast_count <- read.csv(paste(table_dir, "moz_level1_summary.csv", sep = "/"), as.is = T)
moz_lvl2_coast_count <- read.csv(paste(table_dir, "moz_level2_summary.csv", sep = "/"), as.is = T)
moz_lvl3_coast_count <- read.csv(paste(table_dir, "moz_level3_summary.csv", sep = "/"), as.is = T)

phl_lvl1_coast_count <- read.csv(paste(table_dir, "phl_level1_summary.csv", sep = "/"), as.is = T)
phl_lvl2_coast_count <- read.csv(paste(table_dir, "phl_level2_summary.csv", sep = "/"), as.is = T)
phl_lvl3_coast_count <- read.csv(paste(table_dir, "phl_level3_summary.csv", sep = "/"), as.is = T)

plw_lvl1_coast_count <- read.csv(paste(table_dir, "plw_level1_summary.csv", sep = "/"), as.is = T)

### Analyze country level data
## Brazil
bra_coast_lvl1 <- bra_lvl1_coast_count %>%
  count(lvl1_nm)
bra_coast_lvl1_count <- nrow(bra_coast_lvl1)

bra_coast_lvl2 <- bra_lvl2_coast_count %>%
  count(lvl2_nm)
bra_coast_lvl2_count <- nrow(bra_coast_lvl2)

bra_coast_lvl3 <- bra_lvl3_coast_count %>%
  count(lvl3_nm)
bra_coast_lvl3_count <- nrow(bra_coast_lvl3)

## FSM
fsm_coast_lvl1 <- fsm_lvl1_coast_count %>%
  count(lvl1_nm)
fsm_coast_lvl1_count <- nrow(fsm_coast_lvl1)

## Guatemala
gtm_coast_lvl1 <- gtm_lvl1_coast_count %>%
  count(lvl1_nm)
gtm_coast_lvl1_count <- nrow(gtm_coast_lvl1)

gtm_coast_lvl2 <- gtm_lvl2_coast_count %>%
  count(lvl2_nm)
gtm_coast_lvl2_count <- nrow(gtm_coast_lvl2)

## Honduras
hnd_coast_lvl1 <- hnd_lvl1_coast_count %>%
  count(lvl1_nm)
hnd_coast_lvl1_count <- nrow(hnd_coast_lvl1)

hnd_coast_lvl2 <- hnd_lvl2_coast_count %>%
  count(lvl2_nm)
hnd_coast_lvl2_count <- nrow(hnd_coast_lvl2)

hnd_coast_lvl3 <- hnd_lvl3_coast_count %>%
  count(lvl3_nm)
hnd_coast_lvl3_count <- nrow(hnd_coast_lvl3)

## Indonesia
idn_coast_lvl1 <- idn_lvl1_coast_count %>%
  count(lvl1_nm)
idn_coast_lvl1_count <- nrow(idn_coast_lvl1)

idn_coast_lvl2 <- idn_lvl2_coast_count %>%
  count(lvl2_nm)
idn_coast_lvl2_count <- nrow(idn_coast_lvl2)

idn_coast_lvl3 <- idn_lvl3_coast_count %>%
  count(lvl3_nm)
idn_coast_lvl3_count <- nrow(idn_coast_lvl3)

idn_coast_lvl4 <- idn_lvl4_coast_count %>%
  count(lvl4_nm)
idn_coast_lvl4_count <- nrow(idn_coast_lvl4)

## Mozambique
moz_coast_lvl1 <- moz_lvl1_coast_count %>%
  count(lvl1_nm)
moz_coast_lvl1_count <- nrow(moz_coast_lvl1)

moz_coast_lvl2 <- moz_lvl2_coast_count %>%
  count(lvl2_nm)
moz_coast_lvl2_count <- nrow(moz_coast_lvl2)

moz_coast_lvl3 <- moz_lvl3_coast_count %>%
  count(lvl3_nm)
moz_coast_lvl3_count <- nrow(moz_coast_lvl3)

## Philippines
phl_coast_lvl1 <- phl_lvl1_coast_count %>%
  count(lvl1_nm)
phl_coast_lvl1_count <- nrow(phl_coast_lvl1)

phl_coast_lvl2 <- phl_lvl2_coast_count %>%
  count(lvl2_nm)
phl_coast_lvl2_count <- nrow(phl_coast_lvl2)

phl_coast_lvl3 <- phl_lvl3_coast_count %>%
  count(lvl3_nm)
phl_coast_lvl3_count <- nrow(phl_coast_lvl3)

## Palau
plw_coast_lvl1 <- plw_lvl1_coast_count %>%
  count(lvl1_nm)
plw_coast_lvl1_count <- nrow(plw_coast_lvl1)

### analyze data
## Brazil
# Village
bra_rare_village <- rare_bra %>%
  count(village)
bra_rare_village <- nrow(bra_rare_village)
# Level 1
bra_rare_lvl1 <- rare_bra %>%
  count(level1)
bra_rare_lvl1_count <- nrow(bra_rare_lvl1)
# Level 2
bra_rare_lvl2 <- rare_bra %>%
  count(level2)
bra_rare_lvl2_count <- nrow(bra_rare_lvl2)
# Level 3
bra_rare_lvl3 <- rare_bra %>%
  count(level3)
bra_rare_lvl3_count <- nrow(bra_rare_lvl3)



## Federated States of Micronesia
# Village
fsm_rare_village <- rare_fsm %>%
  count(village)
fsm_rare_village <- nrow(fsm_rare_village)
# Level 1
fsm_rare_lvl1 <- rare_fsm %>%
  count(level1)
fsm_rare_lvl1_count <- nrow(fsm_rare_lvl1)
# Level 2
fsm_rare_lvl2 <- rare_fsm %>%
  count(level2)
fsm_rare_lvl2_count <- nrow(fsm_rare_lvl2)
# Level 3
fsm_rare_lvl3 <- rare_fsm %>%
  count(level3)
fsm_rare_lvl3_count <- nrow(fsm_rare_lvl3)



## Guatemala
# Village
gtm_rare_village <- rare_gtm %>%
  count(village)
gtm_rare_village <- nrow(gtm_rare_village)
# Level 1
gtm_rare_lvl1 <- rare_gtm %>%
  count(level1)
gtm_rare_lvl1_count <- nrow(gtm_rare_lvl1)
# Level 2
gtm_rare_lvl2 <- rare_gtm %>%
  count(level2)
gtm_rare_lvl2_count <- nrow(gtm_rare_lvl2)
# Level 3
gtm_rare_lvl3 <- rare_gtm %>%
  count(level3)
gtm_rare_lvl3_count <- nrow(gtm_rare_lvl3)


## Honduras
# Village
hnd_rare_village <- rare_hnd %>%
  count(village)
hnd_rare_village <- nrow(hnd_rare_village)
# Level 1
hnd_rare_lvl1 <- rare_hnd %>%
  count(level1)
hnd_rare_lvl1_count <- nrow(hnd_rare_lvl1)
# Level 2
hnd_rare_lvl2 <- rare_hnd %>%
  count(level2)
hnd_rare_lvl2_count <- nrow(hnd_rare_lvl2)
# Level 3
hnd_rare_lvl3 <- rare_hnd %>%
  count(level3)
hnd_rare_lvl3_count <- nrow(hnd_rare_lvl3)


## Indonesia
# Village
idn_rare_village <- rare_idn %>%
  count(village)
idn_rare_village <- nrow(idn_rare_village)
# Level 1
idn_rare_lvl1 <- rare_idn %>%
  count(level1)
idn_rare_lvl1_count <- nrow(idn_rare_lvl1)
# Level 2
idn_rare_lvl2 <- rare_idn %>%
  count(level2)
idn_rare_lvl2_count <- nrow(idn_rare_lvl2)
# Level 3
idn_rare_lvl3 <- rare_idn %>%
  count(level3)
idn_rare_lvl3_count <- nrow(idn_rare_lvl3)



## Mozambique
# Village
moz_rare_village <- rare_moz %>%
  count(village)
moz_rare_village <- nrow(moz_rare_village)
# Level 1
moz_rare_lvl1 <- rare_moz %>%
  count(level1)
moz_rare_lvl1_count <- nrow(moz_rare_lvl1)
# Level 2
moz_rare_lvl2 <- rare_moz %>%
  count(level2)
moz_rare_lvl2_count <- nrow(moz_rare_lvl2)
# Level 3
moz_rare_lvl3 <- rare_moz %>%
  count(level3)
moz_rare_lvl3_count <- nrow(moz_rare_lvl3)


## Philippines
# Village
phl_rare_village <- rare_phl %>%
  count(village)
phl_rare_village <- nrow(phl_rare_village)
# Level 1
phl_rare_lvl1 <- rare_phl %>%
  count(level1)
phl_rare_lvl1_count <- nrow(phl_rare_lvl1)
# Level 2
phl_rare_lvl2 <- rare_phl %>%
  count(level2)
phl_rare_lvl2_count <- nrow(phl_rare_lvl2)
# Level 3
phl_rare_lvl3 <- rare_phl %>%
  count(level3)
phl_rare_lvl3_count <- nrow(phl_rare_lvl3)


## Palau
# Village
plw_rare_village <- rare_plw %>%
  count(village)
plw_rare_village <- nrow(plw_rare_village)
# Level 1
plw_rare_lvl1 <- rare_plw %>%
  count(level1)
plw_rare_lvl1_count <- nrow(plw_rare_lvl1)
# Level 2
plw_rare_lvl2 <- rare_plw %>%
  count(level2)
plw_rare_lvl2_count <- nrow(plw_rare_lvl2)
# Level 3
plw_rare_lvl3 <- rare_plw %>%
  count(level3)
plw_rare_lvl3_count <- nrow(plw_rare_lvl3)




### Construct the tables
bra_rare_count <- matrix(c("Brazil", "BRA",
                           bra_coast_lvl1_count, bra_rare_lvl1_count,
                           bra_coast_lvl2_count, bra_rare_lvl2_count,
                           bra_coast_lvl3_count, bra_rare_lvl3_count, 
                           NA, bra_rare_village),
                         ncol = 10, byrow = TRUE)


fsm_rare_count <- matrix(c("Micronesia", "FSM", 
                           fsm_coast_lvl1_count, fsm_rare_lvl1_count,
                           NA, fsm_rare_lvl2_count,
                           NA, fsm_rare_lvl3_count,
                           NA, fsm_rare_village), 
                         ncol = 10, byrow = TRUE)


gtm_rare_count <- matrix(c("Guatemala", "GTM",
                           gtm_coast_lvl1_count, gtm_rare_lvl1_count,
                           gtm_coast_lvl2_count, gtm_rare_lvl2_count,
                           NA, gtm_rare_lvl3_count,
                           NA, gtm_rare_village), 
                         ncol = 10, byrow = TRUE)


hnd_rare_count <- matrix(c("Honduras", "HND",
                           hnd_coast_lvl1_count, hnd_rare_lvl1_count,
                           hnd_coast_lvl2_count, hnd_rare_lvl2_count,
                           hnd_coast_lvl3_count, hnd_rare_lvl3_count,
                           NA, hnd_rare_village), 
                         ncol = 10, byrow = TRUE)


idn_rare_count <- matrix(c("Indonesia", "IDN",
                           idn_coast_lvl1_count, idn_rare_lvl1_count,
                           idn_coast_lvl2_count, idn_rare_lvl2_count,
                           idn_coast_lvl3_count, idn_rare_lvl3_count,
                           idn_coast_lvl4_count, idn_rare_village), 
                         ncol = 10, byrow = TRUE)


moz_rare_count <- matrix(c("Mozambique", "MOZ",
                           moz_coast_lvl1_count, moz_rare_lvl1_count,
                           moz_coast_lvl2_count, moz_rare_lvl2_count,
                           moz_coast_lvl3_count, moz_rare_lvl3_count,
                           NA, moz_rare_village), 
                         ncol = 10, byrow = TRUE)


phl_rare_count <- matrix(c("Philippines", "PHL",
                           phl_coast_lvl1_count, phl_rare_lvl1_count,
                           phl_coast_lvl2_count, phl_rare_lvl2_count,
                           phl_coast_lvl3_count, phl_rare_lvl3_count,
                           NA, phl_rare_village), 
                         ncol = 10, byrow = TRUE)


plw_rare_count <- matrix(c("Palau", "PLW",
                           plw_coast_lvl1_count, plw_rare_lvl1_count,
                           NA, plw_rare_lvl2_count,
                           NA, plw_rare_lvl3_count,
                           NA, plw_rare_village), 
                         ncol = 10, byrow = TRUE)

summary_table <- as.data.frame(rbind(bra_rare_count,
                       fsm_rare_count,
                       gtm_rare_count,
                       hnd_rare_count,
                       idn_rare_count,
                       moz_rare_count,
                       phl_rare_count,
                       plw_rare_count)) %>%
  dplyr::rename(country = V1,
                iso3 = V2,
                lvl1_ctry = V3,
                lvl1_rare = V4,
                lvl2_ctry = V5,
                lvl2_rare = V6,
                lvl3_ctry = V7,
                lvl3_rare = V8,
                lvl4_ctry = V9,
                lvl4_rare = V10) %>%
  dplyr::mutate(lvl1_pct = as.numeric(lvl1_rare) / as.numeric(lvl1_ctry) * 100,
                lvl2_pct = as.numeric(lvl2_rare) / as.numeric(lvl2_ctry) * 100,
                lvl3_pct = as.numeric(lvl3_rare) / as.numeric(lvl3_ctry) * 100,
                lvl4_pct = as.numeric(lvl4_rare) / as.numeric(lvl4_ctry) * 100)


### Export summary table
write.csv(summary_table, file = paste0(report_dir, "/", "rare_country_summary_table.csv"))
