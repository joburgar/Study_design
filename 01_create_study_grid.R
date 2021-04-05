# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#####################################################################################
# 01_create_study_grid.R
# script to create fishnet polygon with grid of x size, based on Area of Interest (AOI)
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 03-Apr-2021
#####################################################################################

.libPaths("C:/Program Files/R/R-4.0.5/library") # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# Load Packages
list.of.packages <- c("tidyverse", "lubridate","bcdata", "bcmaps","sp","sf", "rgdal", "readxl", "Cairo",
                      "OpenStreetMap", "ggmap", "nngeo")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

###--- Area of Interest (AOI) is larger than Study Area
# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910 
aoi <- st_read(dsn = "./data", layer = "BDOW_removalsites_20210330") # %>% st_transform(crs = 3005)
aoi_utm <- st_transform(aoi, crs=26910) # to have in metres for specifying grid cell size

# plot as check and to ensure correct AOI for fishnet
# ggplot()+
#   geom_sf(data = aoi, aes(fill=as.factor(OBJECTID)))

aoi_1km_grid <- st_make_grid(st_bbox(aoi_utm), cellsize=1000, square=TRUE) # 1km (1000 m) grid for entire AOI (rectangle)
sa_1km_grid <- st_make_grid(aoi_utm %>% filter(OBJECTID==3), cellsize = 1000, square = TRUE) # 1km grid for study area (sa)

# plot as check
# ggplot()+
#   geom_sf(data = sa_1km_grid) +
#   geom_sf(data = aoi_utm, aes(fill=as.factor(OBJECTID)))

sa_1km_points <- st_point_on_surface(sa_1km_grid)

# plot as check
ggplot()+
  geom_sf(data = sa_1km_grid) +
  geom_sf(data=sa_1km_points) +
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA)

# now have spatial grid, spatial points and study area boundary objects
# can proceed to Task 2 - load covariates and join to spatial points
  
