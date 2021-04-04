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
# 02_join_covariates.R
# script to load and join covariates to aoi / sa spatial points created in Task 1
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 03-Apr-2021
#####################################################################################

# load covariates from bcmaps
# digital elevation raster
aoi_raster <- cded_raster(aoi) 

# watercourses at 5M
aoi.wtrcrs <- watercourses_5M() %>% st_intersection(aoi)

# load covariates from bcdata
# using the bc data warehouse option to clip to aoi

# transportation layer (Digital Road Atlas)
bcdc_search("road", res_format = "wms")

aoi.DRA <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# approved WHAs
bcdc_search("WHA", res_format = "wms")

aoi.WHA <- bcdc_query_geodata("b19ff409-ef71-4476-924e-b3bcf26a0127") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# vegetation data (VRI)
bcdc_search("VRI", res_format = "wms")

aoi.VRI <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()


# plot as check
ggplot()+
  geom_sf(data=aoi.WHA, aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data = sa_1km_grid, fill=NA) +
  geom_sf(data=sa_1km_points) +
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA)+
  geom_sf(data=wtrcrs5M, lwd=1.5, col="blue") +
  geom_sf(data=aoi.DRA, lwd=0.8, col="brown")

# plot as check
ggplot()+
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA)+
  geom_sf(data=aoi.VRI, aes(fill=SHRUB_HEIGHT))

# data is loading properly, now do a join to the spatial points in study area

# keep only sampling locations within WHA
# retain points >100 m from large lakes, rivers, and roads
# exclude points with too steep slope (are we going to aim for this?)
# random stratify remaining based on VRI veg height data
# prioritise within VRI category
# check spatial distribution to ensure sampling across landscape