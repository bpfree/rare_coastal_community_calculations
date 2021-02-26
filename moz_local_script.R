################################
# Coastal Community Calculations
################################

################################
# Mozambique
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
moz_dir <- "data//moz_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
moz_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "MOZ")
moz_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "MOZ",
                NAME_1 %in% c("Cabo Delgado",
                              "Gaza",
                              "Inhambane",
                              "Maputo",
                              "Maputo City",
                              "Nampula",
                              "Sofala",
                              "Zambezia"))
moz_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "MOZ",
                NAME_1 %in% c("Cabo Delgado",
                              "Gaza",
                              "Inhambane",
                              "Maputo",
                              "Maputo City",
                              "Nampula",
                              "Sofala",
                              "Zambezia"))
moz_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "MOZ",
                NAME_1 %in% c("Cabo Delgado",
                              "Gaza",
                              "Inhambane",
                              "Maputo",
                              "Maputo City",
                              "Nampula",
                              "Sofala",
                              "Zambezia")) %>%
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


### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)

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

### 4. Export data
st_write(obj = moz_level0, dsn = paste0(moz_dir, "/", "moz_level0.shp"), append = F)
st_write(obj = moz_level1, dsn = paste0(moz_dir, "/", "moz_level1.shp"), append = F)
st_write(obj = moz_level2, dsn = paste0(moz_dir, "/", "moz_level2.shp"), append = F)
st_write(obj = moz_level3, dsn = paste0(moz_dir, "/", "moz_level3.shp"), append = F)

write.csv(moz_communities, file = paste0(moz_dir, "/", "moz_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
moz_level0_wgs84 <- st_transform(moz_level0, crs)
moz_level1_wgs84 <- st_transform(moz_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
moz_level0_polyline <- st_cast(x = moz_level0_wgs84, to = "MULTILINESTRING")
moz_level1_polyline <- st_cast(x = moz_level1_wgs84, to = "MULTILINESTRING")

### 7. Dissolve the boundary files
moz_level1_wgs84 <- moz_level1_wgs84 %>%
  dplyr::mutate(area = st_area(moz_level1_wgs84)) %>%
  dplyr::summarise(area = sum(area))

moz_level1_polyline <- moz_level1_polyline %>%
  dplyr::group_by(GID_0) %>%
  dplyr::summarise()

### 8. Buffer national and state boundary
moz_10km <- st_buffer(moz_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
moz_prov_10km <- st_buffer(moz_level1_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 8. Fix to only land area
moz_10km_land <- st_intersection(moz_10km, moz_level0_wgs84)
moz_prov_10km_land <- st_intersection(moz_prov_10km, moz_level1_wgs84) %>%
  dplyr::select(GID_0)

### 9. Export data
st_write(obj = moz_10km, dsn = paste0(moz_dir, "/", "moz_10km.shp"), append = F)
st_write(obj = moz_10km_land, dsn = paste0(moz_dir, "/", "moz_10km_land.shp"), append = F)
st_write(obj = moz_prov_10km, dsn = paste0(moz_dir, "/", "moz_prov_10km.shp"), append = F)
st_write(obj = moz_prov_10km_land, dsn = paste0(moz_dir, "/", "moz_prov_10km_land.shp"), append = F)

######################################################
######################################################

### 10. Import coast data
moz_coast10km_wgs84 <- st_read(dsn = moz_dir, layer = "moz_10km_coast") %>%
  st_transform(crs)
moz_prov_coast10km_wgs84 <- st_read(dsn = moz_dir, layer = "moz_prov_10km_coast") %>%
  st_transform(crs)

### Transform data into WGS84 World Mercator for intersection
moz_level3_wgs84 <- st_transform(moz_level3, crs)

### 12. Communities within 10km coast buffer
moz_level3_coast <- st_intersection(moz_level3_wgs84, moz_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)

### 13. Export coastal community data
st_write(obj = moz_level3_coast, dsn = paste0(moz_dir, "/", "moz_level3_coastal_communities.shp"), append = F)

######################################################
######################################################

### 14. Coastal community calculations across area of interest 
## Province
moz_lvl1 <- moz_level3_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "MOZ",
                lvl1_typ = "Province") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
## District
moz_lvl2 <- moz_level3_coast %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "MOZ",
                lvl2_typ = "District") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)

## Village
moz_lvl3 <- moz_level3_coast %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "MOZ",
                lvl3_typ = "Locality") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

### 15. Make geometries NULL to generate table
## Level 1 -- department
st_geometry(moz_lvl1) <- NULL

## Level 2 -- municipality
st_geometry(moz_lvl2) <- NULL

## Level 3 -- aldea
st_geometry(moz_lvl3) <- NULL

## Villages
moz_vill <- moz_lvl3 %>%
  dplyr::rename(village = lvl3_nm) %>%
  dplyr::select(-lvl3_typ)

### 16. Save tables
write.csv(moz_lvl1, file = paste0(moz_dir, "/", "moz_level1_summary.csv"))
write.csv(moz_lvl2, file = paste0(moz_dir, "/", "moz_level2_summary.csv"))
write.csv(moz_lvl3, file = paste0(moz_dir, "/", "moz_level3_summary.csv"))

######################################################
######################################################

### 17. Analyze country level data
## Province counts
moz_coast_lvl1 <- moz_lvl1 %>%
  count(lvl1_nm)
moz_coast_lvl1_count <- nrow(moz_coast_lvl1)

## District counts
moz_coast_lvl2 <- moz_lvl2 %>%
  count(lvl2_nm)
moz_coast_lvl2_count <- nrow(moz_coast_lvl2)

## Locality counts
moz_coast_lvl3 <- moz_lvl3 %>%
  count(lvl3_nm)
moz_coast_lvl3_count <- nrow(moz_coast_lvl3)

### 18. Analyze Rare community data
## Village
moz_rare_village <- moz_communities %>%
  count(village)
moz_rare_village <- nrow(moz_rare_village)

## Level 1 -- department
moz_rare_lvl1 <- moz_communities %>%
  count(level1)
moz_rare_lvl1_count <- nrow(moz_rare_lvl1)

## Level 2 -- municipality
moz_rare_lvl2 <- moz_communities %>%
  count(level2)
moz_rare_lvl2_count <- nrow(moz_rare_lvl2)

## Level 3 -- locality
moz_rare_lvl3 <- moz_communities %>%
  count(level3)
moz_rare_lvl3_count <- nrow(moz_rare_lvl3)

### 19. Construct tables for Mozambique
moz_rare_count <- matrix(c("Mozambique", "MOZ",
                           moz_coast_lvl1_count, moz_rare_lvl1_count,
                           moz_coast_lvl2_count, moz_rare_lvl2_count,
                           moz_coast_lvl3_count, moz_rare_lvl3_count, 
                           NA, moz_rare_village),
                         ncol = 10, byrow = TRUE)

moz_summary_table <- as.data.frame(moz_rare_count) %>%
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
write.csv(moz_summary_table, file = paste0(moz_dir, "/", "moz_country_summary_table.csv"))
