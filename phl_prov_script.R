################################
# Coastal Community Calculations
################################

################################
# Philippines
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
phl_dir <- "data//phl_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
phl_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "PHL")
phl_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "PHL",
                NAME_1 %in% c("Antique",
                              "Camarines Norte",
                              "Camarines Sur",
                              "Cebu",
                              "Iloilo",
                              "Leyte",
                              "Negros Occidental",
                              "Negros Oriental",
                              "Occidental Mindoro",
                              "Siquijor",
                              "Sorsogon",
                              "Southern Leyte",
                              "Surigao del Norte",
                              "Surigao del Sur",
                              "Zamboanga Sibugay"))
phl_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "PHL",
                NAME_1 %in% c("Antique",
                              "Camarines Norte",
                              "Camarines Sur",
                              "Cebu",
                              "Iloilo",
                              "Leyte",
                              "Negros Occidental",
                              "Negros Oriental",
                              "Occidental Mindoro",
                              "Siquijor",
                              "Sorsogon",
                              "Southern Leyte",
                              "Surigao del Norte",
                              "Surigao del Sur",
                              "Zamboanga Sibugay"))
phl_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "PHL",
                NAME_1 %in% c("Antique",
                              "Camarines Norte",
                              "Camarines Sur",
                              "Cebu",
                              "Iloilo",
                              "Leyte",
                              "Negros Occidental",
                              "Negros Oriental",
                              "Occidental Mindoro",
                              "Siquijor",
                              "Sorsogon",
                              "Southern Leyte",
                              "Surigao del Norte",
                              "Surigao del Sur",
                              "Zamboanga Sibugay")) %>%
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

### 4. Export data
st_write(obj = phl_level0, dsn = paste0(phl_dir, "/", "phl_level0.shp"), append = F)
st_write(obj = phl_level1, dsn = paste0(phl_dir, "/", "phl_level1.shp"), append = F)
st_write(obj = phl_level2, dsn = paste0(phl_dir, "/", "phl_level2.shp"), append = F)
st_write(obj = phl_level3, dsn = paste0(phl_dir, "/", "phl_level3.shp"), append = F)

write.csv(phl_communities, file = paste0(phl_dir, "/", "phl_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
phl_level0_wgs84 <- st_transform(phl_level0, crs)
phl_level1_wgs84 <- st_transform(phl_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
phl_level0_polyline <- st_cast(x = phl_level0_wgs84, to = "MULTILINESTRING")
phl_level1_polyline <- st_cast(x = phl_level1_wgs84, to = "MULTILINESTRING")

### 7. Dissolve the boundary files
phl_level1_wgs84 <- phl_level1_wgs84 %>%
  dplyr::mutate(area = st_area(phl_level1_wgs84)) %>%
  dplyr::summarise(area = sum(area))

phl_level1_polyline <- phl_level1_polyline %>%
  dplyr::group_by(GID_0) %>%
  dplyr::summarise()

### 8. Buffer national and state boundary
phl_10km <- st_buffer(phl_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
phl_prov_10km <- st_buffer(phl_level1_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 9a. Fix to only land area
phl_10km_land <- st_intersection(phl_10km, phl_level0_wgs84)
phl_prov_10km_coast <- st_intersection(phl_prov_10km, phl_level1_wgs84) %>%
  dplyr::select(GID_0)

### 9b. Remove the inland boundaries to keep it to the 
phl_prov_10km_coast <- st_intersection(phl_prov_10km_coast, phl_10km_land)

### 10. Export data
st_write(obj = phl_10km, dsn = paste0(phl_dir, "/", "phl_10km.shp"), append = F)
st_write(obj = phl_10km_land, dsn = paste0(phl_dir, "/", "phl_10km_coast.shp"), append = F)
st_write(obj = phl_prov_10km, dsn = paste0(phl_dir, "/", "phl_prov_10km.shp"), append = F)
st_write(obj = phl_prov_10km_coast, dsn = paste0(phl_dir, "/", "phl_prov_10km_coast.shp"), append = F)

######################################################
######################################################

### 11. Import coast data
phl_coast10km_wgs84 <- st_read(dsn = phl_dir, layer = "phl_10km_coast") %>%
  st_transform(crs)
phl_prov_coast10km_wgs84 <- st_read(dsn = phl_dir, layer = "phl_prov_10km_coast") %>%
  st_transform(crs)

### Transform data into WGS84 World Mercator for intersection
phl_level3_wgs84 <- st_transform(phl_level3, crs)

### 12. Communities within 10km coast buffer
phl_level3_coast <- st_intersection(phl_level3_wgs84, phl_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)

### 13. Export coastal community data
st_write(obj = phl_level3_coast, dsn = paste0(phl_dir, "/", "phl_level3_coastal_communities.shp"), append = F)

######################################################
######################################################

### 14. Coastal community calculations across area of interest 
## Province
phl_lvl1 <- phl_level3_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PHL",
                lvl1_typ = "Province") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
## Municipality
phl_prov_10km_coast <- phl_level3_coast %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PHL",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)

## Village
phl_lvl3 <- phl_level3_coast %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "PHL",
                lvl3_typ = "Locality") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

### 15. Make geometries NULL to generate table
## Level 1 -- province
st_geometry(phl_lvl1) <- NULL

## Level 2 -- municipality
st_geometry(phl_lvl2) <- NULL

## Level 3 -- village
st_geometry(phl_lvl3) <- NULL

## Villages
phl_vill <- phl_lvl3 %>%
  dplyr::rename(village = lvl3_nm) %>%
  dplyr::select(-lvl3_typ)

### 16. Save tables
write.csv(phl_lvl1, file = paste0(phl_dir, "/", "phl_level1_summary.csv"))
write.csv(phl_lvl2, file = paste0(phl_dir, "/", "phl_level2_summary.csv"))
write.csv(phl_lvl3, file = paste0(phl_dir, "/", "phl_level3_summary.csv"))

######################################################
######################################################

### 17. Analyze country level data
## Department counts
phl_coast_lvl1 <- phl_lvl1 %>%
  count(lvl1_nm)
phl_coast_lvl1_count <- nrow(phl_coast_lvl1)

## Municipality counts
phl_coast_lvl2 <- phl_lvl2 %>%
  count(lvl2_nm)
phl_coast_lvl2_count <- nrow(phl_coast_lvl2)

## Locality counts
phl_coast_lvl3 <- phl_lvl3 %>%
  count(lvl3_nm)
phl_coast_lvl3_count <- nrow(phl_coast_lvl3)

### 18. Analyze Rare community data
## Village
phl_rare_village <- phl_communities %>%
  count(village)
phl_rare_village <- nrow(phl_rare_village)

## Level 1 -- department
phl_rare_lvl1 <- phl_communities %>%
  count(level1)
phl_rare_lvl1_count <- nrow(phl_rare_lvl1)

## Level 2 -- municipality
phl_rare_lvl2 <- phl_communities %>%
  count(level2)
phl_rare_lvl2_count <- nrow(phl_rare_lvl2)

## Level 3 -- village
phl_rare_lvl3 <- phl_communities %>%
  count(level3)
phl_rare_lvl3_count <- nrow(phl_rare_lvl3)

### 19. Construct tables for Philippines
phl_rare_count <- matrix(c("Philippines", "PHL",
                           phl_coast_lvl1_count, phl_rare_lvl1_count,
                           phl_coast_lvl2_count, phl_rare_lvl2_count,
                           phl_coast_lvl3_count, phl_rare_lvl3_count, 
                           NA, phl_rare_village),
                         ncol = 10, byrow = TRUE)

phl_summary_table <- as.data.frame(phl_rare_count) %>%
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
write.csv(phl_summary_table, file = paste0(phl_dir, "/", "phl_country_summary_table.csv"))
