################################
# Coastal Community Calculations
################################

################################
# Brazil
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
brazil_dir <- "data//brazil_data"

######################################################
######################################################

### 2. Import and clean data
## NOTE:: Remove filters for state to get complete country data / change summary table file name
bra_level0 <- st_read(dsn = boundary_dir, layer = "level0") %>%
  dplyr::filter(GID_0 == "BRA")
bra_level1 <- st_read(dsn = boundary_dir, layer = "level1") %>%
  dplyr::filter(GID_0 == "BRA",
                NAME_1 == "Pará")
bra_level2 <- st_read(dsn = boundary_dir, layer = "level2") %>%
  dplyr::filter(GID_0 == "BRA",
                NAME_1 == "Pará")
bra_level3 <- st_read(dsn = boundary_dir, layer = "level3") %>%
  dplyr::filter(GID_0 == "BRA",
                NAME_1 == "Pará") %>%
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
bra_level4_para <- st_read(dsn = brazil_dir, layer = "bra_level4_para_raw") %>%
  dplyr::rename(lvl4_nm = LOCALIDADE,
                lvl2_nm = MUNICIPIO) %>%
  dplyr::mutate(iso3 = "BRA",
                country = "Brazil",
                lvl2_typ = "Municipality",
                lvl4_typ = "Locality") %>%
  dplyr::select(iso3, country,
                lvl2_nm, lvl2_typ,
                lvl4_nm, lvl4_typ)

### 3. Rare community data
rare_communities <- read.csv(paste(rare_dir, "rare_footprint_global_02192021.csv", sep= "/"), as.is = T)

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

### 4. Export data
st_write(obj = bra_level0, dsn = paste0(brazil_dir, "/", "bra_level0.shp"), append = F)
st_write(obj = bra_level1, dsn = paste0(brazil_dir, "/", "bra_level1.shp"), append = F)
st_write(obj = bra_level2, dsn = paste0(brazil_dir, "/", "bra_level2.shp"), append = F)
st_write(obj = bra_level3, dsn = paste0(brazil_dir, "/", "bra_level3.shp"), append = F)
st_write(obj = bra_level3, dsn = paste0(brazil_dir, "/", "bra_level3.shp"), append = F)
st_write(obj = bra_level4_para, dsn = paste0(brazil_dir, "/", "bra_level4_para.shp"), append = F)
write.csv(bra_communities, file = paste0(brazil_dir, "/", "bra_communities.csv"))

######################################################
######################################################

### 5. Transform data in a projected coordinate system
bra_level0_wgs84 <- st_transform(bra_level0, crs)
bra_level1_wgs84 <- st_transform(bra_level1, crs)

### 6. Convert boundary (polygon) to line (polyline)
bra_level0_polyline <- st_cast(x = bra_level0_wgs84, to = "MULTILINESTRING")
bra_level1_polyline <- st_cast(x = bra_level1_wgs84, to = "MULTILINESTRING")

### 7. Buffer national and state boundary
brazil_10km <- st_buffer(bra_level0_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right
para_10km <- st_buffer(bra_level1_polyline, 10000, singleSide = FALSE) # if TRUE --> positive = left, negative = right

### 8. Fix to only land area
bra_10km_land <- st_intersection(brazil_10km, bra_level0_wgs84)
para_10km_land <- st_intersection(para_10km, bra_level1_wgs84)

### 9. Export data
st_write(obj = brazil_10km, dsn = paste0(brazil_dir, "/", "bra_10km.shp"), append = F)
st_write(obj = bra_10km_land, dsn = paste0(brazil_dir, "/", "bra_10km_land.shp"), append = F)
st_write(obj = para_10km, dsn = paste0(brazil_dir, "/", "para_10km.shp"), append = F)
st_write(obj = para_10km_land, dsn = paste0(brazil_dir, "/", "para_10km_land.shp"), append = F)

######################################################
######################################################

### 10. Import coast data
bra_coast10km_wgs84 <- st_read(dsn = brazil_dir, layer = "bra_10km_coast") %>%
  st_transform(crs)
para_coast10km_wgs84 <- st_read(dsn = brazil_dir, layer = "para_10km_coast") %>%
  st_transform(crs)

### Transform data into WGS84 World Mercator for intersection
bra_level3_wgs84 <- st_transform(bra_level3, crs)
para_level4_wgs84 <- st_transform(bra_level4_para, crs)

### 12. Communities within 10km coast buffer
bra_level3_coast <- st_intersection(bra_level3_wgs84, bra_coast10km_wgs84) %>%
  dplyr::select(-GID_0, -NAME_0, 
                -GID_0_1, -NAME_0_1)
para_level4_coast <- st_intersection(para_level4_wgs84, para_coast10km_wgs84) %>%
  dplyr::rename(lvl1_nm = NAME_1,
                lvl1_typ = TYPE_1) %>%
  dplyr::select(iso3, country,
                lvl1_nm, lvl1_typ,
                lvl2_nm, lvl2_typ,
                lvl4_nm, lvl4_typ)

### 13. Export coastal community data
st_write(obj = bra_level3_coast, dsn = paste0(brazil_dir, "/", "bra_level3_coastal_communities.shp"), append = F)
st_write(obj = para_level4_coast, dsn = paste0(brazil_dir, "/", "para_level4_coastal_communities.shp"), append = F)

######################################################
######################################################

### 14. Coastal community calculations across area of interest 
## State
bra_lvl1 <- bra_level3_coast %>%
  count(lvl1_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl1_typ = "State") %>%
  dplyr::select(iso3, lvl1_typ, lvl1_nm, count)
## Municipality
bra_lvl2 <- bra_level3_coast %>%
  count(lvl2_nm)  %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl2_typ = "Municipality") %>%
  dplyr::select(iso3, lvl2_typ, lvl2_nm, count)
## District 
bra_lvl3 <- bra_level3_coast %>%
  count(lvl3_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl3_typ = "District") %>%
  dplyr::select(iso3, lvl3_typ, lvl3_nm, count)

## State where Rare works
para_lvl4 <- para_level4_coast %>%
  count(lvl4_nm) %>%
  dplyr::rename(count = n) %>%
  dplyr::mutate(iso3 = "BRA",
                lvl4_typ = "Locality") %>%
  dplyr::select(iso3, lvl4_typ, lvl4_nm, count)

### 15. Make geometries NULL to generate table
## Level 1 -- state
st_geometry(bra_lvl1) <- NULL

## Level 2 -- municipality
st_geometry(bra_lvl2) <- NULL

## Level 3 -- district
st_geometry(bra_lvl3) <- NULL

## Level 4 -- Para
st_geometry(para_lvl4) <- NULL

### 16. Save tables
write.csv(bra_lvl1, file = paste0(brazil_dir, "/", "bra_level1_summary.csv"))
write.csv(bra_lvl2, file = paste0(brazil_dir, "/", "bra_level2_summary.csv"))
write.csv(bra_lvl3, file = paste0(brazil_dir, "/", "bra_level3_summary.csv"))

write.csv(para_lvl4, file = paste0(brazil_dir, "/", "para_level4_summary.csv"))

######################################################
######################################################

### 17. Analyze country level data
## State counts
bra_coast_lvl1 <- bra_lvl1 %>%
  count(lvl1_nm)
bra_coast_lvl1_count <- nrow(bra_coast_lvl1)

## Municipality counts
bra_coast_lvl2 <- bra_lvl2 %>%
  count(lvl2_nm)
bra_coast_lvl2_count <- nrow(bra_coast_lvl2)

## District counts
bra_coast_lvl3 <- bra_lvl3 %>%
  count(lvl3_nm)
bra_coast_lvl3_count <- nrow(bra_coast_lvl3)

## Rare State count
para_coast_lvl4 <- para_lvl4 %>%
  count(lvl4_nm)
para_coast_lvl4_count <- nrow(para_coast_lvl4)

### 18. Analyze Rare community data
## Village
bra_rare_village <- bra_communities %>%
  count(village)
bra_rare_village <- nrow(bra_rare_village)

## Level 1 -- state
bra_rare_lvl1 <- bra_communities %>%
  count(level1)
bra_rare_lvl1_count <- nrow(bra_rare_lvl1)

## Level 2 -- municipality
bra_rare_lvl2 <- bra_communities %>%
  count(level2)
bra_rare_lvl2_count <- nrow(bra_rare_lvl2)

## Level 3 -- district
bra_rare_lvl3 <- bra_communities %>%
  count(level3)
bra_rare_lvl3_count <- nrow(bra_rare_lvl3)

### 19. Construct tables for Brazil
bra_rare_count <- matrix(c("Brazil", "BRA",
                           bra_coast_lvl1_count, bra_rare_lvl1_count,
                           bra_coast_lvl2_count, bra_rare_lvl2_count,
                           bra_coast_lvl3_count, bra_rare_lvl3_count, 
                           para_coast_lvl4_count, bra_rare_village),
                         ncol = 10, byrow = TRUE)

bra_summary_table <- as.data.frame(bra_rare_count) %>%
  dplyr::rename(country = V1,
                iso3 = V2,
                lvl1_ctry = V3,
                lvl1_rare = V4,
                lvl2_ctry = V5,
                lvl2_rare = V6,
                lvl3_ctry = V7,
                lvl3_rare = V8,
                lvl4_para = V9,
                lvl4_rare = V10) %>%
  dplyr::mutate(lvl1_pct = as.numeric(lvl1_rare) / as.numeric(lvl1_ctry) * 100,
                lvl2_pct = as.numeric(lvl2_rare) / as.numeric(lvl2_ctry) * 100,
                lvl3_pct = as.numeric(lvl3_rare) / as.numeric(lvl3_ctry) * 100,
                lvl4_pct = as.numeric(lvl4_rare) / as.numeric(lvl4_para) * 100)

### 20. Export summary table
write.csv(bra_summary_table, file = paste0(brazil_dir, "/", "bra_country_summary_table.csv"))
