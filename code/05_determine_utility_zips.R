#-------------------------------------------------
# PSPS: Determine utility zips
# June 2025
#-------------------------------------------------

# setup -------------------------------------------------
library(pacman)
pacman::p_load(sf, tidyverse)
repo_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/")

ca_zips <- st_read(paste0(repo_dir, "exposure_data/ca_zip.geojson"))
utility_polys <- st_read(paste0(repo_dir, "exposure_data/utility_areas_serviced/ElectricLoadServingEntities_IOU_POU.shp"))

# get zips served by PGE, SDGE, SCE -------------------------------------------------
ca_zips <- ca_zips %>% select(c("ZIP_CODE", "geometry"))

utilities <- c( "Southern California Edison",
                "Pacific Gas & Electric Company",                    
                "San Diego Gas & Electric")
utility_polys_sm <- utility_polys %>% 
    filter(Utility %in% utilities) %>% 
    select(c("Utility", "geometry"))

# set both to the same CRS -------------------------------------------------
utility_polys_sm <- st_transform(utility_polys_sm, crs = st_crs(ca_zips))

# validate and fix geometry issues
utility_polys_sm <- st_make_valid(utility_polys_sm)
ca_zips <- st_make_valid(ca_zips)

# get zips served by PGE, SDGE, SCE -------------------------------------------------
utility_zips <- st_intersection(ca_zips, utility_polys_sm)
utility_zips <- utility_zips %>% 
    select(c("ZIP_CODE")) %>% 
    st_drop_geometry() %>% 
    unique() # in case a zip is served by multiple utilities

# write out -------------------------------------------------
write_csv(utility_zips, paste0(repo_dir, "exposure_data/utility_zips.csv"))
