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
                      "OpenStreetMap", "ggmap", "nngeo", "raster")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

###--- function to create study grid
# note that this function creates square grids (use square=FALSE for hexagons)
# this function dissolves multipolygon into single polygon
# assumes original shapefile is the entire aoi; remove the st_union and st_combine code if want separate study areas within aoi

create_study_grid <- function (dsn=dsn, layer=layer, cellsize=cellsize, output=output){
  
  aoi <- st_read(dsn=dsn, layer=layer) %>% st_transform(crs = 3005) # ensures poly is in Albers
  aoi <- aoi %>% 
    summarise(across(geometry, ~ st_union(.))) %>%
    summarise(across(geometry, ~ st_combine(.)))
  aoi_utm <- st_transform(aoi, crs=26910) # to have in metres for specifying grid cell size
  aoi_grid <- sa_grid <- st_make_grid(st_bbox(aoi_utm), cellsize=cellsize, square=TRUE) #  grid for entire AOI (rectangle)
  
  sa_points <- st_point_on_surface(sa_grid)  # if using portion of aoi
  sa_points <- st_intersection(sa_points, aoi_utm)
  
  st_write(aoi_utm, paste0(getwd(),"/out/",output,"_utm.shp"), delete_layer = TRUE)
  st_write(aoi_grid %>% st_intersection(aoi_utm), paste0(getwd(),"/out/",output,"_point.shp"), delete_layer = TRUE)
  st_write(sa_points, paste0(getwd(),"/out/",output,"_grid.shp"), delete_layer = TRUE)
  
   return(list(aoi_utm, aoi_grid, sa_points))
}

#####################################################################################

###--- Area of Interest (AOI) is larger than Study Area
# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910 

cellsize <- 1000 # owls
cellsize <- 2000 # elk


SKA_MAN_ARU <- create_study_grid(dsn= "./data", layer = "Skagis_Manning_ARU_Release_Area_IanVersion", 
                                 cellsize = 1000, output = "SKA_MAN_ARU")


# plot as check
ggplot()+
  geom_sf(data = SKA_MAN_ARU[[2]] %>% st_intersection(SKA_MAN_ARU[[1]])) +
  geom_sf(data = SKA_MAN_ARU[[3]]) +
  geom_sf(data = SKA_MAN_ARU[[1]], lwd=2, col="red", fill=NA)

# now have spatial grid, spatial points and study area boundary objects
# can proceed to Task 2 - load covariates and join to spatial points
  
