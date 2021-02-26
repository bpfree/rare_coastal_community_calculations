################################
# Coastal Community Calculations
################################

################################
# Honduras
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
hnd_dir <- "data//hnd_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
hnd_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "HND")
hnd_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "HND",
                NAME_1 %in% c("Atlántida",
                              "Colón",
                              "Cortés",
                              "Gracias a Dios",
                              "Islas de la Bahía"))
hnd_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "HND",
                NAME_1 %in% c("Atlántida",
                              "Colón",
                              "Cortés",
                              "Gracias a Dios",
                              "Islas de la Bahía")) %>%
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

hnd_level3 <- st_read(dsn = hnd_dir, layer = "hnd_level3_un") %>%
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

### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)

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

### 4. Export data
st_write(obj = hnd_level0, dsn = paste0(hnd_dir, "/", "hnd_level0.shp"), append = F)
st_write(obj = hnd_level1, dsn = paste0(hnd_dir, "/", "hnd_level1.shp"), append = F)
st_write(obj = hnd_level2, dsn = paste0(hnd_dir, "/", "hnd_level2.shp"), append = F)
st_write(obj = hnd_level3, dsn = paste0(hnd_dir, "/", "hnd_level3.shp"), append = F)

write.csv(hnd_communities, file = paste0(hnd_dir, "/", "hnd_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
hnd_level0_wgs84 <- st_transform(hnd_level0, crs)
hnd_level1_wgs84 <- st_transform(hnd_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
hnd_level0_polyline <- st_cast(x = hnd_level0_wgs84, to = "MULTILINESTRING")
hnd_level1_polyline <- st_cast(x = hnd_level1_wgs84, to = "MULTILINESTRING")

### 7. Dissolve the boundary files
hnd_level1_wgs84 <- hnd_level1_wgs84 %>%
  dplyr::mutate(area = st_area(hnd_level1_wgs84)) %>%
  dplyr::summarise(area = sum(area))

hnd_level1_polyline <- hnd_level1_polyline %>%
  dplyr::group_by(GID_0) %>%
  dplyr::summarise()

### 8. Buffer national and state boundary
hnd_10km <- st_buffer(hnd_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
hnd_dept_10km <- st_buffer(hnd_level1_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 9. Fix to only land area
hnd_10km_land <- st_intersection(hnd_10km, hnd_level0_wgs84)
hnd_dept_10km_land <- st_intersection(hnd_dept_10km, hnd_level1_wgs84) %>%
  dplyr::select(GID_0)

### 10. Export data
st_write(obj = hnd_10km, dsn = paste0(hnd_dir, "/", "hnd_10km.shp"), append = F)
st_write(obj = hnd_10km_land, dsn = paste0(hnd_dir, "/", "hnd_10km_land.shp"), append = F)
st_write(obj = hnd_dept_10km, dsn = paste0(hnd_dir, "/", "hnd_dept_10km.shp"), append = F)
st_write(obj = hnd_dept_10km_land, dsn = paste0(hnd_dir, "/", "hnd_dept_10km_land.shp"), append = F)

######################################################
######################################################

### 11. Import coast data
hnd_coast10km_wgs84 <- st_read(dsn = hnd_dir, layer = "hnd_10km_coast") %>%
  st_transform(crs)
hnd_dept_coast10km_wgs84 <- st_read(dsn = hnd_dir, layer = "hnd_dept_10km_coast") %>%
  st_transform(crs)

### 12. Transform data into WGS84 World Mercator for intersection
hnd_level3_wgs84 <- st_transform(hnd_level3, crs)

### 13. Communities within 10km coast buffer
hnd_level3_coast <- st_intersection(hnd_level3_wgs84, hnd_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0)

### 14. Export coastal community data
st_write(obj = hnd_level3_coast, dsn = paste0(hnd_dir, "/", "hnd_level3_coastal_communities.shp"), append = F)

######################################################
######################################################

### 15. Coastal community calculations across area of interest 
## Department
hnd_lvl1 <- hnd_level3_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "HND",
                lvl1_typ = "Department") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
## Municipality
hnd_lvl2 <- hnd_level3_coast %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "HND",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)

## Village
hnd_lvl3 <- hnd_level3_coast %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "HND",
                lvl3_typ = "Village") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

### 16. Make geometries NULL to generate table
## Level 1 -- department
st_geometry(hnd_lvl1) <- NULL

## Level 2 -- municipality
st_geometry(hnd_lvl2) <- NULL

## Level 3 -- aldea
st_geometry(hnd_lvl3) <- NULL

## Villages
hnd_vill <- hnd_lvl3 %>%
  dplyr::rename(village = lvl3_nm) %>%
  dplyr::select(-lvl3_typ)

### 17. Save tables
write.csv(hnd_lvl1, file = paste0(hnd_dir, "/", "hnd_level1_summary.csv"))
write.csv(hnd_lvl2, file = paste0(hnd_dir, "/", "hnd_level2_summary.csv"))
write.csv(hnd_lvl3, file = paste0(hnd_dir, "/", "hnd_level3_summary.csv"))

######################################################
######################################################

### 18. Analyze country level data
## Department counts
hnd_coast_lvl1 <- hnd_lvl1 %>%
  count(lvl1_nm)
hnd_coast_lvl1_count <- nrow(hnd_coast_lvl1)

## Municipality counts
hnd_coast_lvl2 <- hnd_lvl2 %>%
  count(lvl2_nm)
hnd_coast_lvl2_count <- nrow(hnd_coast_lvl2)

## Aldea counts
hnd_coast_lvl3 <- hnd_lvl3 %>%
  count(lvl3_nm)
hnd_coast_lvl3_count <- nrow(hnd_coast_lvl3)

### 19. Analyze Rare community data
## Village
hnd_rare_village <- hnd_communities %>%
  count(village)
hnd_rare_village <- nrow(hnd_rare_village)

## Level 1 -- department
hnd_rare_lvl1 <- hnd_communities %>%
  count(level1)
hnd_rare_lvl1_count <- nrow(hnd_rare_lvl1)

## Level 2 -- municipality
hnd_rare_lvl2 <- hnd_communities %>%
  count(level2)
hnd_rare_lvl2_count <- nrow(hnd_rare_lvl2)

## Level 3 -- aldea
hnd_rare_lvl3 <- hnd_communities %>%
  count(level3)
hnd_rare_lvl3_count <- nrow(hnd_rare_lvl3)

### 20. Construct tables for Honduras
hnd_rare_count <- matrix(c("Honduras", "HND",
                           hnd_coast_lvl1_count, hnd_rare_lvl1_count,
                           hnd_coast_lvl2_count, hnd_rare_lvl2_count,
                           hnd_coast_lvl3_count, hnd_rare_lvl3_count, 
                           NA, hnd_rare_village),
                         ncol = 10, byrow = TRUE)

hnd_summary_table <- as.data.frame(hnd_rare_count) %>%
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

### 21. Export summary table
write.csv(hnd_summary_table, file = paste0(hnd_dir, "/", "hnd_country_summary_table.csv"))
