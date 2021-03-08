################################
# Coastal Community Calculations
################################

################################
# Indonesia
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
idn_dir <- "data//idn_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
idn_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "IDN")

idn_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "IDN",
                NAME_1 %in% c("Sulawesi Utara",
                              "Sulawesi Tenggara"))
idn_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "IDN",
                NAME_1 %in% c("Sulawesi Utara",
                              "Sulawesi Tenggara"))
idn_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "IDN",
                NAME_1 %in% c("Sulawesi Utara",
                              "Sulawesi Tenggara")) %>%
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

idn_level4 <- st_read(dsn = idn_dir, layer = "idn_level4_un") %>%
  dplyr::filter(ADM1_EN %in% c("Sulawesi Utara",
                               "Sulawesi Tenggara")) %>%
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

### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)

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

### 4. Export data
st_write(obj = idn_level0, dsn = paste0(idn_dir, "/", "idn_level0.shp"), append = F)
st_write(obj = idn_level1, dsn = paste0(idn_dir, "/", "idn_level1.shp"), append = F)
st_write(obj = idn_level2, dsn = paste0(idn_dir, "/", "idn_level2.shp"), append = F)
st_write(obj = idn_level3, dsn = paste0(idn_dir, "/", "idn_level3.shp"), append = F)
st_write(obj = idn_level4, dsn = paste0(idn_dir, "/", "idn_level4.shp"), append = F)
write.csv(idn_communities, file = paste0(idn_dir, "/", "idn_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
idn_level0_wgs84 <- st_transform(idn_level0, crs)
idn_level1_wgs84 <- st_transform(idn_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
idn_level0_polyline <- st_cast(x = idn_level0_wgs84, to = "MULTILINESTRING")
idn_level1_polyline <- st_cast(x = idn_level1_wgs84, to = "MULTILINESTRING")

### 7. Dissolve the boundary files
idn_level1_wgs84 <- idn_level1_wgs84 %>%
  dplyr::mutate(area = st_area(idn_level1_wgs84)) %>%
  dplyr::summarise(area = sum(area))

idn_level1_polyline <- idn_level1_polyline %>%
  dplyr::group_by(GID_0) %>%
  dplyr::summarise()

### 8. Buffer national and state boundary
idn_10km <- st_buffer(idn_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
idn_dept_10km <- st_buffer(idn_level1_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 9a. Fix to only land area
idn_10km_land <- st_intersection(st_make_valid(idn_10km), st_make_valid(idn_level0_wgs84))
idn_dept_10km_land <- st_intersection(idn_dept_10km, idn_level1_wgs84)

### 9b. Remove the inland boundaries to keep it to the Indonesia states of interest side
idn_dept_10km_coast <- st_intersection(st_make_valid(idn_dept_10km_land), st_make_valid(idn_10km_land)) %>%
  dplyr::select(GID_0, area)

### 10. Export data
st_write(obj = idn_10km, dsn = paste0(idn_dir, "/", "idn_10km.shp"), append = F)
st_write(obj = idn_10km_land, dsn = paste0(idn_dir, "/", "idn_10km_land.shp"), append = F)
st_write(obj = idn_dept_10km, dsn = paste0(idn_dir, "/", "idn_dept_10km.shp"), append = F)
st_write(obj = idn_dept_10km_coast, dsn = paste0(idn_dir, "/", "idn_dept_10km_coast.shp"), append = F)

######################################################
######################################################

### 11. Import coast data
idn_coast10km_wgs84 <- st_read(dsn = idn_dir, layer = "idn_10km_land") %>%
  st_transform(crs)
dept_coast10km_wgs84 <- st_read(dsn = idn_dir, layer = "idn_dept_10km_coast") %>%
  st_transform(crs)

### 12. Transform data into WGS84 World Mercator for intersection
idn_level3_wgs84 <- st_transform(idn_level3, crs)
dept_level4_wgs84 <- st_transform(idn_level4, crs)

### 13. Communities within 10km coast buffer
idn_level3_coast <- st_intersection(idn_level3_wgs84, idn_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0, 
                -GID_0_1, -NAME_0_1)
dept_level4_coast <- st_intersection(dept_level4_wgs84, dept_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -area)

### 14. Export coastal community data
st_write(obj = idn_level3_coast, dsn = paste0(idn_dir, "/", "idn_level3_coastal_communities.shp"), append = F)
st_write(obj = dept_level4_coast, dsn = paste0(idn_dir, "/", "dept_level4_coastal_communities.shp"), append = F)

######################################################
######################################################

### 15. Coastal community calculations across area of interest 
## Department
idn_lvl1 <- idn_level3_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl1_typ = "Department") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
## Municipality
idn_lvl2 <- idn_level3_coast %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
## District 
idn_lvl3 <- idn_level3_coast %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl3_typ = "District") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

## Department where Rare works
dept_lvl4 <- dept_level4_coast %>%
  count(lvl4_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "IDN",
                lvl4_typ = "Locality") %>%
  dplyr::select(iso3, lvl4_typ, lvl4_nm, count)

### 16. Make geometries NULL to generate table
## Level 1 -- state
st_geometry(idn_lvl1) <- NULL

## Level 2 -- municipality
st_geometry(idn_lvl2) <- NULL

## Level 3 -- district
st_geometry(idn_lvl3) <- NULL

## Level 4 -- village
st_geometry(dept_lvl4) <- NULL

### 17. Save tables
write.csv(idn_lvl1, file = paste0(idn_dir, "/", "idn_level1_summary.csv"))
write.csv(idn_lvl2, file = paste0(idn_dir, "/", "idn_level2_summary.csv"))
write.csv(idn_lvl3, file = paste0(idn_dir, "/", "idn_level3_summary.csv"))

write.csv(dept_lvl4, file = paste0(idn_dir, "/", "dept_level4_summary.csv"))

######################################################
######################################################

### 18. Analyze country level data
## Department counts
idn_coast_lvl1 <- idn_lvl1 %>%
  count(lvl1_nm)
idn_coast_lvl1_count <- nrow(idn_coast_lvl1)

## Municipality counts
idn_coast_lvl2 <- idn_lvl2 %>%
  count(lvl2_nm)
idn_coast_lvl2_count <- nrow(idn_coast_lvl2)

## District counts
idn_coast_lvl3 <- idn_lvl3 %>%
  count(lvl3_nm)
idn_coast_lvl3_count <- nrow(idn_coast_lvl3)

## Rare Department count
dept_coast_lvl4 <- dept_lvl4 %>%
  count(lvl4_nm)
dept_coast_lvl4_count <- nrow(dept_coast_lvl4)

### 19. Analyze Rare community data
## Village
idn_rare_village <- idn_communities %>%
  count(village)
idn_rare_village <- nrow(idn_rare_village)

## Level 1 -- state
idn_rare_lvl1 <- idn_communities %>%
  count(level1)
idn_rare_lvl1_count <- nrow(idn_rare_lvl1)

## Level 2 -- municipality
idn_rare_lvl2 <- idn_communities %>%
  count(level2)
idn_rare_lvl2_count <- nrow(idn_rare_lvl2)

## Level 3 -- district
idn_rare_lvl3 <- idn_communities %>%
  count(level3)
idn_rare_lvl3_count <- nrow(idn_rare_lvl3)

### 20. Construct tables for Indonesia
idn_rare_count <- matrix(c("Indonesia", "IDN",
                           idn_coast_lvl1_count, idn_rare_lvl1_count,
                           idn_coast_lvl2_count, idn_rare_lvl2_count,
                           idn_coast_lvl3_count, idn_rare_lvl3_count, 
                           dept_coast_lvl4_count, idn_rare_village),
                         ncol = 10, byrow = TRUE)

idn_summary_table <- as.data.frame(idn_rare_count) %>%
  dplyr::rename(country = V1,
                iso3 = V2,
                lvl1_ctry = V3,
                lvl1_rare = V4,
                lvl2_ctry = V5,
                lvl2_rare = V6,
                lvl3_ctry = V7,
                lvl3_rare = V8,
                lvl4_dept = V9,
                lvl4_rare = V10) %>%
  dplyr::mutate(lvl1_pct = as.numeric(lvl1_rare) / as.numeric(lvl1_ctry) * 100,
                lvl2_pct = as.numeric(lvl2_rare) / as.numeric(lvl2_ctry) * 100,
                lvl3_pct = as.numeric(lvl3_rare) / as.numeric(lvl3_ctry) * 100,
                lvl4_pct = as.numeric(lvl4_rare) / as.numeric(lvl4_dept) * 100)

### 21. Export summary table
write.csv(idn_summary_table, file = paste0(idn_dir, "/", "idn_country_summary_table.csv"))
