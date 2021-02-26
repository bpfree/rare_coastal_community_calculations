################################
# Coastal Community Calculations
################################

################################
# Federated States of Micronesia
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
boundary_dir <- "data//gadm36_levels.gpkg"
rare_dir <- "data//rare_community"

## Output directory
fsm_dir <- "data//fsm_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
fsm_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "FSM")
fsm_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "FSM")

### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)

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

### 4. Export data
st_write(obj = fsm_level0, dsn = paste0(fsm_dir, "/", "fsm_level0.shp"), append = F)
st_write(obj = fsm_level1, dsn = paste0(fsm_dir, "/", "fsm_level1.shp"), append = F)
write.csv(fsm_communities, file = paste0(fsm_dir, "/", "fsm_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
fsm_level0_wgs84 <- st_transform(fsm_level0, crs)
fsm_level1_wgs84 <- st_transform(fsm_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
fsm_level0_polyline <- st_cast(x = fsm_level0_wgs84, to = "MULTILINESTRING")
fsm_level1_polyline <- st_cast(x = fsm_level1_wgs84, to = "MULTILINESTRING")

### 7. Buffer boundary
fsm_10km <- st_buffer(fsm_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 8. Fix to only land area
fsm_10km_coast <- st_intersection(fsm_10km, fsm_level0_wgs84)

### 9. Export data
st_write(obj = fsm_10km, dsn = paste0(fsm_dir, "/", "fsm_10km.shp"), append = F)
st_write(obj = fsm_10km_coast, dsn = paste0(fsm_dir, "/", "fsm_10km_coast.shp"), append = F)

######################################################
######################################################

### 10. Import coast data
fsm_coast10km_wgs84 <- st_read(dsn = fsm_dir, layer = "fsm_10km_coast") %>%
  st_transform(crs)

### 11. Transform data into WGS84 World Mercator for intersection
fsm_level1_wgs84 <- st_transform(fsm_level1, crs)

### 12. Communities within 10km coast buffer
fsm_level1_coast <- st_intersection(fsm_level1_wgs84, fsm_coast10km_wgs84) %>%
  dplyr::rename(iso3 = GID_0,
                country = NAME_0,
                lvl1_nm = NAME_1,
                lvl1_typ = TYPE_1,
                eng1_typ = ENGTYPE_1) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ, eng1_typ)

### 13. Export coastal community data
st_write(obj = fsm_level1_coast, dsn = paste0(fsm_dir, "/", "fsm_level1_coastal_communities.shp"), append = F)

######################################################
######################################################

### 14. Coastal community calculations across area of interest 
## State
fsm_lvl1 <- fsm_level1_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "FSM",
                lvl1_typ = "State") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)

### 15. Make geometries NULL to generate table
## Level 1 -- state
st_geometry(fsm_lvl1) <- NULL

### 16. Save tables
write.csv(fsm_lvl1, file = paste0(fsm_dir, "/", "fsm_level1_summary.csv"))

######################################################
######################################################

### 17. Analyze country level data
## State counts
fsm_coast_lvl1 <- fsm_lvl1 %>%
  count(lvl1_nm)
fsm_coast_lvl1_count <- nrow(fsm_coast_lvl1)

### 18. Analyze Rare community data
## Village
fsm_rare_village <- fsm_communities %>%
  count(village)
fsm_rare_village <- nrow(fsm_rare_village)

## Level 1 -- state
fsm_rare_lvl1 <- fsm_communities %>%
  count(level1)
fsm_rare_lvl1_count <- nrow(fsm_rare_lvl1)

## Level 2 -- municipality
fsm_rare_lvl2 <- fsm_communities %>%
  count(level2)
fsm_rare_lvl2_count <- nrow(fsm_rare_lvl2)

## Level 3 -- district
fsm_rare_lvl3 <- fsm_communities %>%
  count(level3)
fsm_rare_lvl3_count <- nrow(fsm_rare_lvl3)

### 19. Construct tables for Federated States of Micronesia
fsm_rare_count <- matrix(c("Federated States of Micronesia", "FSM",
                           fsm_coast_lvl1_count, fsm_rare_lvl1_count,
                           NA, fsm_rare_lvl2_count,
                           NA, fsm_rare_lvl3_count, 
                           NA, fsm_rare_village),
                         ncol = 10, byrow = TRUE)

fsm_summary_table <- as.data.frame(fsm_rare_count) %>%
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

### 20. Export summary table
write.csv(fsm_summary_table, file = paste0(fsm_dir, "/", "fsm_country_summary_table.csv"))
