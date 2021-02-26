################################
# Coastal Community Calculations
################################

################################
# Guatemala
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
gtm_dir <- "data//gtm_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
gtm_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "GTM")
gtm_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "GTM",
                NAME_1 == "Izabal")
gtm_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "GTM",
                NAME_1 == "Izabal") %>%
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

### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)

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

### 4. Export data
st_write(obj = gtm_level0, dsn = paste0(gtm_dir, "/", "gtm_level0.shp"), append = F)
st_write(obj = gtm_level1, dsn = paste0(gtm_dir, "/", "gtm_level1.shp"), append = F)
st_write(obj = gtm_level2, dsn = paste0(gtm_dir, "/", "gtm_level2.shp"), append = F)

write.csv(gtm_communities, file = paste0(gtm_dir, "/", "gtm_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
gtm_level0_wgs84 <- st_transform(gtm_level0, crs)
gtm_level1_wgs84 <- st_transform(gtm_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
gtm_level0_polyline <- st_cast(x = gtm_level0_wgs84, to = "MULTILINESTRING")
gtm_level1_polyline <- st_cast(x = gtm_level1_wgs84, to = "MULTILINESTRING")

### 7. Buffer national and state boundary
gtm_10km <- st_buffer(gtm_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
gtm_izabal_10km <- st_buffer(gtm_level1_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 8. Fix to only land area
gtm_10km_land <- st_intersection(gtm_10km, gtm_level0_wgs84)
gtm_izabal_10km_land <- st_intersection(gtm_izabal_10km, gtm_level1_wgs84)

### 9. Export data
st_write(obj = gtm_10km, dsn = paste0(gtm_dir, "/", "gtm_10km.shp"), append = F)
st_write(obj = gtm_10km_land, dsn = paste0(gtm_dir, "/", "gtm_10km_land.shp"), append = F)
st_write(obj = gtm_izabal_10km, dsn = paste0(gtm_dir, "/", "gtm_izabal_10km.shp"), append = F)
st_write(obj = gtm_izabal_10km_land, dsn = paste0(gtm_dir, "/", "gtm_izabal_10km_land.shp"), append = F)

######################################################
######################################################

### 10. Import coast data
gtm_coast10km_wgs84 <- st_read(dsn = gtm_dir, layer = "gtm_10km_coast") %>%
  st_transform(crs)
gtm_izabal_coast10km_wgs84 <- st_read(dsn = gtm_dir, layer = "gtm_izabal_10km_coast") %>%
  st_transform(crs)

### Transform data into WGS84 World Mercator for intersection
gtm_level2_wgs84 <- st_transform(gtm_level2, crs)

### 12. Communities within 10km coast buffer
gtm_level2_coast <- st_intersection(gtm_level2_wgs84, gtm_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0, -GID_0_1, -NAME_0_1)

### 13. Export coastal community data
st_write(obj = gtm_level2_coast, dsn = paste0(gtm_dir, "/", "gtm_level2_coastal_communities.shp"), append = F)

######################################################
######################################################

### 14. Coastal community calculations across area of interest 
## Department
gtm_lvl1 <- gtm_level2_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "GTM",
                lvl1_typ = "Department") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
## Municipality
gtm_lvl2 <- gtm_level2_coast %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "GTM",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)

### 15. Make geometries NULL to generate table
## Level 1 -- department
st_geometry(gtm_lvl1) <- NULL

## Level 2 -- municipality
st_geometry(gtm_lvl2) <- NULL

### 16. Save tables
write.csv(gtm_lvl1, file = paste0(gtm_dir, "/", "gtm_level1_summary.csv"))
write.csv(gtm_lvl2, file = paste0(gtm_dir, "/", "gtm_level2_summary.csv"))

######################################################
######################################################

### 17. Analyze country level data
## Department counts
gtm_coast_lvl1 <- gtm_lvl1 %>%
  count(lvl1_nm)
gtm_coast_lvl1_count <- nrow(gtm_coast_lvl1)

## Municipality counts
gtm_coast_lvl2 <- gtm_lvl2 %>%
  count(lvl2_nm)
gtm_coast_lvl2_count <- nrow(gtm_coast_lvl2)

### 18. Analyze Rare community data
## Village
gtm_rare_village <- gtm_communities %>%
  count(village)
gtm_rare_village <- nrow(gtm_rare_village)

## Level 1 -- department
gtm_rare_lvl1 <- gtm_communities %>%
  count(level1)
gtm_rare_lvl1_count <- nrow(gtm_rare_lvl1)

## Level 2 -- municipality
gtm_rare_lvl2 <- gtm_communities %>%
  count(level2)
gtm_rare_lvl2_count <- nrow(gtm_rare_lvl2)

## Level 3 -- 
gtm_rare_lvl3 <- gtm_communities %>%
  count(level3)
gtm_rare_lvl3_count <- nrow(gtm_rare_lvl3)

### 19. Construct tables for Guatemala
gtm_rare_count <- matrix(c("Guatemala", "GTM",
                           gtm_coast_lvl1_count, gtm_rare_lvl1_count,
                           gtm_coast_lvl2_count, gtm_rare_lvl2_count,
                           NA, gtm_rare_lvl3_count, 
                           NA, gtm_rare_village),
                         ncol = 10, byrow = TRUE)

gtm_summary_table <- as.data.frame(gtm_rare_count) %>%
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
write.csv(gtm_summary_table, file = paste0(gtm_dir, "/", "gtm_country_summary_table.csv"))
