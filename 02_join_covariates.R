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
# bcdc_search("road", res_format = "wms")

aoi.DRA <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# approved WHAs
# bcdc_search("WHA", res_format = "wms")

aoi.WHA <- bcdc_query_geodata("b19ff409-ef71-4476-924e-b3bcf26a0127") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# vegetation data (VRI)
# bcdc_search("VRI", res_format = "wms")

aoi.VRI <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()


# plot as check
ggplot()+
  geom_sf(data=aoi.WHA, aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data = sa_1km_grid, fill=NA) +
  geom_sf(data=sa_1km_points) +
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA)+
  geom_sf(data=aoi.wtrcrs, lwd=1.5, col="blue") +
  geom_sf(data=aoi.DRA, lwd=0.8, col="brown")

aoi.VRI %>% filter(!is.na(PROJ_HEIGHT_1)) %>%  
  summarise(mean = mean(PROJ_HEIGHT_1), min = min(PROJ_HEIGHT_1), max=max(PROJ_HEIGHT_1), sd = sd(PROJ_HEIGHT_1))
#  mean = 22.8, min =  0.1, max = 54.1, sd  = 8.89 

aoi.VRI$PROJ_HEIGHT_1_cat <- as.factor(ifelse(aoi.VRI$PROJ_HEIGHT_1 < 10, "H0-10",
                                      ifelse(aoi.VRI$PROJ_HEIGHT_1 < 20, "H10-20",
                                             ifelse(aoi.VRI$PROJ_HEIGHT_1 < 30, "H20-30",
                                                    ifelse(aoi.VRI$PROJ_HEIGHT_1 < 40, "H30-40",
                                                           ifelse(aoi.VRI$PROJ_HEIGHT_1 < 50, "H40-50",
                                                                  ifelse(aoi.VRI$PROJ_HEIGHT_1<60, "H50-60")))))))# remove NAs
aoi.VRI <- aoi.VRI[complete.cases(aoi.VRI$PROJ_HEIGHT_1),]


# determine area within each release site with tree height in various classes
# first create an area field
aoi.VRI$area <- st_area(aoi.VRI)
sa.VRI <- st_intersection(aoi.VRI, aoi %>% filter(OBJECTID==3))

# plot to check - clipped the Anderson release area
names(sa.VRI)
ggplot()+
  geom_sf(data = sa.VRI, aes(fill=PROJ_HEIGHT_1_cat, col=NA)) +
  scale_fill_brewer(palette="Greens") +
  scale_color_brewer(palette="Greens") +
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA) +
  theme(legend.title=element_blank())

proj_hgt_area <- as.data.frame(sa.VRI %>% group_by(PROJ_HEIGHT_1_cat) %>% 
                                 summarise(area = sum(area)) %>% st_drop_geometry())

proj_hgt_area$area_km2 <- as.numeric(proj_hgt_area$area/1000)
proj_hgt_area$prop <- proj_hgt_area$area_km2 / sum(proj_hgt_area$area_km2)

proj_hgt_area %>% select(prop) %>% round(2)
# want to have similar proportions of sites within various age classes

# prop
# 1 0.08
# 2 0.14
# 3 0.44
# 4 0.31
# 5 0.03
# so, if we have 60 ARUs, aim for 5 in H0-10, 8 in H10-20, 26 in H20-30, 19 in H30-40, 2 in H40+


sa_smpl_lcns <- as.data.frame(st_coordinates(sa_1km_points))

# need to change all line features to utm so can calculate distance in m
# retain points >100 m from large lakes, rivers, and roads
#- watercourses
sa.wtrcrs <- aoi.wtrcrs %>% st_intersection(aoi %>% filter(OBJECTID==3)) %>% st_transform(crs=26910)
sa.wtrcrs.dist <- st_nn(sa_1km_points, sa.wtrcrs, k=1, returnDist = T)

sa_smpl_lcns$wtr_dist <- unlist(sa.wtrcrs.dist$dist)
sa_smpl_lcns$wtr_type <- unlist(sa.wtrcrs.dist$nn)
sa_smpl_lcns$wtr_type <- sa.wtrcrs$name_en[match(sa_smpl_lcns$wtr_type,rownames(sa.wtrcrs))]

sa_smpl_lcns$wtr_use <- as.factor(ifelse(sa_smpl_lcns$wtr_dist>100,"yes","no"))
  
#- roads
sa.DRA <- aoi.DRA %>% st_intersection(aoi %>% filter(OBJECTID==3)) %>% st_transform(crs=26910)
sa.road.dist <- st_nn(sa_1km_points, sa.DRA, k=1, returnDist = T)

sa_smpl_lcns$road_dist <- unlist(sa.road.dist$dist)
sa_smpl_lcns$road_type <- unlist(sa.road.dist$nn)
sa_smpl_lcns$road_type <- sa.DRA$FEATURE_TYPE[match(sa_smpl_lcns$road_type,rownames(sa.DRA))]
summary(as.factor(sa_smpl_lcns$road_type))

sa_smpl_lcns$road_use <- as.factor(case_when(sa_smpl_lcns$road_type=="Road" & sa_smpl_lcns$road_dist<100 ~ "no",
                                             TRUE ~ "yes"))

#- WHA
# keep only sampling locations within WHA
sa.WHA.dist <- st_nn(sa_1km_points, aoi.WHA %>% st_transform(crs=26910), k=1, returnDist = T)
sa_smpl_lcns$WHA_dist <- unlist(sa.WHA.dist$dist) 

sa_smpl_lcns$WHA_use <- as.factor(ifelse(sa_smpl_lcns$WHA_dist==0,"yes","no"))

###--- sampling locations available to use
sa_smpl_lcns$options <- as.factor(ifelse(sa_smpl_lcns$wtr_use=="yes" & 
                                           sa_smpl_lcns$road_use=="yes" &
                                           sa_smpl_lcns$WHA_use=="yes", "available","exclude"))

# exclude points with too steep slope (are we going to aim for this?)
# random stratify remaining based on VRI veg height data
# prioritise within VRI category
# check spatial distribution to ensure sampling across landscape

#- VRI
# add in type of Veg
sa.VRI <- sa.VRI %>% st_transform(crs=26910)
sa.VRI.dist <- st_nn(sa_1km_points, sa.VRI, k=1, returnDist = T)

# check the range of age classes in study area
sa.VRI$PROJ_AGE_1
sa.VRI.row <- unlist(sa.VRI.dist$nn)
sa.VRI.age <- sa.VRI %>% filter(rownames(sa.VRI) %in% sa.VRI.row) %>% select(PROJ_AGE_1) %>% st_drop_geometry()
summary(sa.VRI.age) # ranges from 2 to 374 years, median = 144, mean = 148.6
hist(sa.VRI.age$PROJ_AGE_1, breaks = 50)

sa_smpl_lcns$veg_dist <- unlist(sa.VRI.dist$dist)
sa_smpl_lcns$veg_height <- unlist(sa.VRI.dist$nn)
sa_smpl_lcns$veg_height <- sa.VRI$PROJ_HEIGHT_1[match(sa_smpl_lcns$veg_height,rownames(sa.VRI))]

###--- create sf object from sampling location data frame
sa_smpl_lcns.sf <- st_as_sf(sa_smpl_lcns, coords = c("X","Y"), crs = 26910)
sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm %>% filter(OBJECTID==3)) %>% select(-Nmae, -SHAPE_Leng, -SHAPE_Area)
sa_smpl_lcns.sf %>% count(options) # only 73 available sampling locations, 83 excluded based on proximity to roads/river and WHA occurrence
sa_smpl_lcns.sf %>% filter(options=="available") %>% summarise(min(veg_height), mean(veg_height))


# plot to check - clipped the Anderson release area
ggplot()+
  geom_sf(data = sa.VRI, aes(fill=PROJ_HEIGHT_1_cat, col=NA)) +
  scale_fill_brewer(palette="Greens") +
  scale_color_brewer(palette="Greens") +
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA) +
  geom_sf(data = sa_smpl_lcns.sf %>% filter(options=="available") ,size = 2, shape = 23, fill = "darkred") +
  theme(legend.title=element_blank())

# export shapefile
st_write(sa_smpl_lcns.sf, paste0(getwd(),"/out/Anderson_ARU_opts.shp"))

# plot to check
ggplot()+
  geom_sf(data=aoi.WHA %>% st_transform(crs=26910), aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data = sa_smpl_lcns.sf %>% filter(options=="available")) +
  geom_sf(data = aoi %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA)+
  geom_sf(data=sa.wtrcrs, lwd=1.5, col="blue") +
  geom_sf(data=sa.DRA %>% filter(FEATURE_TYPE=="Road"), lwd=0.8, col="brown") +
  theme(legend.position = "none")
ggsave("Anderson_ARU_options.png",plot=last_plot(), dpi=300)
