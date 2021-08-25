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

###--- function to retrieve geodata from BCGW

retrieve_geodata_aoi <- function (ID=ID){
  aoi.geodata <- bcdc_query_geodata(ID) %>%
    filter(BBOX(st_bbox(aoi))) %>%
    collect()
  aoi.geodata <- aoi.geodata %>% st_intersection(aoi)
  aoi.geodata$Area_km2 <- st_area(aoi.geodata)*1e-6
  aoi.geodata <- drop_units(aoi.geodata)
  return(aoi.geodata)
}

#####################################################################################

aoi <- SP_cam[[1]] %>% st_transform(crs = 3005)
aoi_utm <- SP_cam[[2]] %>% st_intersection(aoi %>% st_transform(26910))
sa_points <- SP_cam[[3]]

# load covariates from bcmaps
# digital elevation raster
aoi_raster <- cded_raster(aoi) 
plot(aoi_raster)
plot(SP_cam[[3]] %>% st_transform(crs = 4326), add= TRUE) # as a check

aoi.cded <- rasterToPoints(aoi_raster) # convert to points for join
aoi.cded.sf <- st_as_sf(as.data.frame(aoi.cded), coords = c("x","y"), crs = 4326) # create spatial layer
aoi.cded.utm <- st_transform(aoi.cded.sf, crs = 26910) # convert to utm for join distance


# load covariates from bcdata
# using the bc data warehouse option to clip to aoi
aoi <- aoi %>% st_transform(3005)

# biogeoclimatic zones
# bcdc_search("Biogeoclimatic zone", res_format = "wms")
# 3: BEC Map (other, wms, kml)
# ID: f358a53b-ffde-4830-a325-a5a03ff672c3
# Name: bec-map
aoi.BEC <- retrieve_geodata_aoi(ID = "f358a53b-ffde-4830-a325-a5a03ff672c3")

# ggplot()+
#   geom_sf(data=aoi.BEC)
# st_write(aoi.BEC, paste0(GISDir,"/LIDAR_BEC.shp"), delete_layer = TRUE)

# watercourses layer
# bcdc_search("NTS BC River", res_format = "wms")
aoi.RLW <- retrieve_geodata_aoi(ID = "414be2d6-f4d9-4f32-b960-caa074c6d36b") 

# FWA_WATERSHED_GROUPS Freshwater Atlas Watershed Boundaries
# bcdc_search("freshwater", res_format = "wms")
aoi.FWA <- retrieve_geodata_aoi(ID = "ab758580-809d-4e11-bb2c-df02ac5465c9")

# transportation layer (Digital Road Atlas)
# bcdc_search("road", res_format = "wms")
aoi.DRA <- retrieve_geodata_aoi(ID = "bb060417-b6e6-4548-b837-f9060d94743e")

# transportation layer (Digital Road Atlas)
# bcdc_search("train", res_format = "wms")
aoi.train <- retrieve_geodata_aoi(ID = "4ff93cda-9f58-4055-a372-98c22d04a9f8")
# no trains in study area

# approved WHAs & UWRs (GAR Orders)
# bcdc_search("WHA", res_format = "wms")
aoi.WHA <- retrieve_geodata_aoi(ID = "b19ff409-ef71-4476-924e-b3bcf26a0127")

# bcdc_search("UWR", res_format = "wms")
aoi.UWR <- retrieve_geodata_aoi(ID = "712bd887-7763-4ed3-be46-cdaca5640cc1")

# vegetation data (VRI)
# bcdc_search("VRI", res_format = "wms")
aoi.VRI <- retrieve_geodata_aoi(ID = "2ebb35d8-c82f-4a17-9c96-612ac3532d55")


ggplot()+
  geom_sf(data=aoi.WHA, aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data = aoi, lwd=2, col="red", fill=NA)+
  geom_sf(data=aoi.RLW, lwd=1, col="blue") +
  geom_sf(data=aoi.DRA, lwd=0.8, col="gray") +
  geom_sf(data=sa_points) 

aoi.VRI %>% filter(!is.na(PROJ_HEIGHT_1)) %>%  
  summarise(mean = mean(PROJ_HEIGHT_1), min = min(PROJ_HEIGHT_1), max=max(PROJ_HEIGHT_1), sd = sd(PROJ_HEIGHT_1))
#  mean = 22.8, min = 0.1, max = 54.1, sd  = 8.89 # Anderson
#  mean = 25.5, min = 0.2, max = 63.5, sd = 13.2 # Skwawka
#  mean = 30.1, min = 0.2, max = 56.6  sd = 8.63 # Skagit - Manning
aoi.VRI$PROJ_HEIGHT_1_cat <- as.factor(ifelse(aoi.VRI$PROJ_HEIGHT_1 < 10, "H0-10",
                                      ifelse(aoi.VRI$PROJ_HEIGHT_1 < 20, "H10-20",
                                             ifelse(aoi.VRI$PROJ_HEIGHT_1 < 30, "H20-30",
                                                    ifelse(aoi.VRI$PROJ_HEIGHT_1 < 40, "H30-40",
                                                           ifelse(aoi.VRI$PROJ_HEIGHT_1 < 50, "H40-50", "H50+"))))))# remove NAs
aoi.VRI <- aoi.VRI[complete.cases(aoi.VRI$PROJ_HEIGHT_1),]


# determine area within each release site with tree height in various classes
# first create an area field
aoi.VRI$area <- st_area(aoi.VRI)
#sa.VRI <- st_intersection(aoi.VRI, aoi %>% filter(OBJECTID==3))
sa.VRI <- st_intersection(aoi.VRI, aoi)

# plot to check - clipped the Anderson release area
names(sa.VRI)
ggplot()+
  geom_sf(data = sa.VRI, aes(fill=PROJ_HEIGHT_1_cat, col=NA)) +
  scale_fill_brewer(palette="Greens") +
  scale_color_brewer(palette="Greens") +
  geom_sf(data = aoi_utm , lwd=1, col="red", fill=NA) +
  theme(legend.title=element_blank())

proj_hgt_area <- as.data.frame(sa.VRI %>% group_by(PROJ_HEIGHT_1_cat) %>% 
                                 summarise(area = sum(area)) %>% st_drop_geometry())

proj_hgt_area$area_km2 <- as.numeric(proj_hgt_area$area/1000)
proj_hgt_area$prop <- proj_hgt_area$area_km2 / sum(proj_hgt_area$area_km2)

proj_hgt_area %>% dplyr::select(prop) %>% round(2)
# want to have similar proportions of sites within various age classes

# prop
# 1 0.08
# 2 0.14
# 3 0.44
# 4 0.31
# 5 0.03
# so, if we have 60 ARUs, aim for 5 in H0-10, 8 in H10-20, 26 in H20-30, 19 in H30-40, 2 in H40+

sa_smpl_lcns <- as.data.frame(st_coordinates(sa_points))

# need to change all line features to utm so can calculate distance in m
# retain points >100 m from large lakes, rivers, and roads

#- watercourses
sa.wtrcrs <- aoi.RLW %>% st_intersection(aoi) %>% st_transform(crs=26910)
sa.wtrcrs.dist <- st_nn(sa_points, sa.wtrcrs, k=1, returnDist = T)
sa_smpl_lcns$wtr_dist <- unlist(sa.wtrcrs.dist$dist)
sa_smpl_lcns$wtr_type <- unlist(sa.wtrcrs.dist$nn)
sa_smpl_lcns$wtr_type <- sa.wtrcrs$DESCRIPTION[match(sa_smpl_lcns$wtr_type,rownames(sa.wtrcrs))]

#- roads
sa.DRA <- aoi.DRA %>% st_transform(crs=26910)
sa.road.dist <- st_nn(sa_points, sa.DRA, k=1, returnDist = T)
sa_smpl_lcns$road_dist <- unlist(sa.road.dist$dist)
sa_smpl_lcns$road_type <- unlist(sa.road.dist$nn)
sa_smpl_lcns$road_type <- sa.DRA$FEATURE_TYPE[match(sa_smpl_lcns$road_type,rownames(sa.DRA))]
summary(as.factor(sa_smpl_lcns$road_type))


# distance to Trans Canada
# sa.HWY.dist <- st_nn(sa_points, sa.DRA %>% filter(HIGHWAY_ROUTE_NUMBER==1), k=1, returnDist = T)
# sa_smpl_lcns$HWY_dist <- unlist(sa.HWY.dist$dist)

# distance to train
# sa.train.dist <- st_nn(sa_points, aoi.train %>% st_transform(crs=26910), k=1, returnDist = T)
# sa_smpl_lcns$train_dist <- unlist(sa.train.dist$dist)


#- FWA
sa.FWA.dist <- st_nn(sa_points, aoi.FWA %>% st_transform(crs=26910), k=1, returnDist = T)
sa_smpl_lcns$FWA_dist <- unlist(sa.FWA.dist$dist)
sa_smpl_lcns$FWA_type <- unlist(sa.FWA.dist$nn)
sa_smpl_lcns$FWA_type <- aoi.FWA$WATERSHED_GROUP_CODE[match(sa_smpl_lcns$FWA_type,rownames(aoi.FWA))]
summary(as.factor(sa_smpl_lcns$FWA_type))
summary(sa_smpl_lcns$FWA_dist)

#- GAR protection
# sampling location distance to WHAs
sa.WHA.dist <- st_nn(sa_points, aoi.WHA %>% st_transform(crs=26910), k=1, returnDist = T)
sa_smpl_lcns$WHA_dist <- unlist(sa.WHA.dist$dist) 

# sampling location distance to UWRs
# sa.UWR.dist <- st_nn(sa_smpl_lcns, aoi.UWR %>% st_transform(crs=26910), k=1, returnDist = T)
# sa_smpl_lcns$UWR_dist <- unlist(sa.UWR.dist$dist)

#- Elevation
sa.elev.dist <- st_nn(sa_points, aoi.cded.utm, k=1, returnDist = T)
sa_smpl_lcns$elev <- unlist(sa.elev.dist$nn)
sa_smpl_lcns$elev <- aoi.cded.utm$elevation[match(sa_smpl_lcns$elev,rownames(aoi.cded.utm))]
summary(sa_smpl_lcns$elev)

# exclude points with too steep slope (are we going to aim for this?)
# random stratify remaining based on VRI veg height data
# prioritise within VRI category
# check spatial distribution to ensure sampling across landscape

#- VRI
# add in type of Veg
sa.VRI <- sa.VRI %>% st_transform(crs=26910)
sa.VRI.dist <- st_nn(sa_points, sa.VRI, k=1, returnDist = T)

# check the range of age classes in study area
sa.VRI$PROJ_AGE_1
sa.VRI.row <- unlist(sa.VRI.dist$nn)
sa.VRI.age <- sa.VRI %>% filter(rownames(sa.VRI) %in% sa.VRI.row) %>% dplyr::select(PROJ_AGE_1) %>% st_drop_geometry()
summary(sa.VRI.age) # ranges from 39 to 324 years, median = 164, mean = 175
hist(sa.VRI.age$PROJ_AGE_1, breaks = 20)

sa_smpl_lcns$veg_dist <- unlist(sa.VRI.dist$dist)
sa_smpl_lcns$veg_height <- unlist(sa.VRI.dist$nn)
sa_smpl_lcns$veg_height <- sa.VRI$PROJ_HEIGHT_1[match(sa_smpl_lcns$veg_height,rownames(sa.VRI))]

sa_smpl_lcns$veg_age <- unlist(sa.VRI.dist$nn)
sa_smpl_lcns$veg_age <- sa.VRI$PROJ_AGE_1[match(sa_smpl_lcns$veg_age,rownames(sa.VRI))]


# # ###--- sampling locations available to use
# names(sa_smpl_lcns)
# sa_smpl_lcns %>% count(veg_dist)
# sa_smpl_lcns$locations <- as.factor(ifelse(#sa_smpl_lcns$WHA_dist < 1 &
#   sa_smpl_lcns$veg_dist==0 & sa_smpl_lcns$veg_height > 25 &
#     sa_smpl_lcns$wtr_dist > 100 & 
#     # sa_smpl_lcns$train_dist > 1000 &
#     # sa_smpl_lcns$HWY_dist > 1000, 
#     sa_smpl_lcns$elev < 1200 &
#     between(sa_smpl_lcns$road_dist, 100, 2500), "available","exclude"))
# 
# sa_smpl_lcns %>% count(locations) # 154 possible locations within the 3 study areas
# nrow(sa_smpl_lcns)
# # 67 available and 98 excluded
# 
# sa_smpl_lcns$priority <- as.factor(ifelse(sa_smpl_lcns$locations=="available" &
#                                             sa_smpl_lcns$elev < 1000, 1,
#                                           ifelse(sa_smpl_lcns$locations=="exclude",3,2)))
# 
# sa_smpl_lcns %>% group_by(priority) %>% count(locations) # 154 possible locations within the 3 study areas
# 
# ###--- create sf object from sampling location data frame
# sa_smpl_lcns.sf <- st_as_sf(sa_smpl_lcns, coords = c("X","Y"), crs = 26910)
# sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm)
# # sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm) #%>% select(-Nmae, -SHAPE_Leng, -SHAPE_Area)
# 
# 
# sa_smpl_lcns.sf %>% count(locations) # 110 available sampling locations, 83 excluded based on proximity to roads/river and WHA occurrence
# sa_smpl_lcns.sf %>% filter(locations=="available") %>% summarise(min(veg_height), mean(veg_height), max(veg_height))
# 
# # sa_smpl_lcns.sf %>% filter(locations=="available") %>% count(OBJECTID)
# # sa_smpl_lcns.sf  %>% filter(locations=="available")%>% group_by(OBJECTID) %>% count(Nmae)
# # sa_smpl_lcns.sf  %>% group_by(priority) %>% count(OBJECTID) # where 1 = Uzltius, 2 = Spuzzum and 3 = Anderson
# 
# # plot to check - clipped the appropriate area
# ggplot()+
#   geom_sf(data = sa.VRI, aes(fill=PROJ_HEIGHT_1_cat, col=NA)) +# use color=NA to remove border lines
#   scale_fill_brewer(palette="Greens") +
#   scale_color_brewer(palette="Greens") +
#   # geom_sf(data = sa_smpl_lcns.sf ,size = 2, shape = 23, fill = "darkred") +
#   geom_sf(data = sa_smpl_lcns.sf %>% filter(priority=="1") ,size = 2, shape = 23, fill = "darkred") +
#   geom_sf(data = aoi, size=2, col="blue", fill=NA)+
#   geom_sf(data = aoi_utm, lwd=0.5, col="gray", fill=NA) +
#     theme(legend.title=element_blank())
# # ggsave("out/Skagit_priority_grid.png",plot=last_plot(), dpi=300)
# ggsave("Skagit_ALL_options_grid.png",plot=last_plot(), dpi=300)
# 
# 
# # export shapefile
# st_write(sa_smpl_lcns.sf, paste0(getwd(),"/out/SKA_MAN_smpln_opts.shp"), delete_layer = TRUE)
# 
# # plot to check
# ggplot()+
#   geom_sf(data=aoi.WHA %>% st_transform(crs=26910))+  
#   geom_sf(data = sa_smpl_lcns.sf %>% filter(priority!="priority"), aes(fill=priority, col=priority)) +
#   # geom_sf(data = sa_smpl_lcns.sf %>% filter(elev>0), col="red") +
#   geom_sf(data = aoi %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA)+
#   geom_sf(data = aoi %>% filter(OBJECTID!=3), lwd=2, col="blue", fill=NA)+
#   geom_sf(data=sa.wtrcrs, lwd=1.5, col="blue") +
#   geom_sf(data=sa.DRA %>% filter(FEATURE_TYPE=="Road"), lwd=0.5, col="brown") +
#   theme(legend.position = "none")
# # ggsave("Anderson_ARU_options.png",plot=last_plot(), dpi=300)
# ggsave("Owl_ARU_priority.png",plot=last_plot(), dpi=300)
# 
# st_write(sa_smpl_lcns.sf  %>% st_transform(crs=4326), "lcns.kml", driver = "kml", delete_dsn = TRUE)
# 
# avail <- sa_smpl_lcns.sf %>% filter(locations=="available")%>% st_transform(crs=4326)
# st_write(avail  %>% st_transform(crs=4326), "avail.kml", driver = "kml", delete_dsn = TRUE)
# 
# st_write(avail %>% dplyr::select(NAME = priority), "out/SKA_priority.kml", driver = "kml", delete_dsn = TRUE)
# st_write(smp_elk %>% filter(Options!="truck") %>% st_transform(crs=4326) %>% select(NAME = Name), "data/elk_aerial_atv_sites.kml", driver = "kml", delete_dsn = TRUE)
# 
# 
# sa_smpl_lcns.sf <- st_read(dsn="./out", layer="Owl_smpln_opts")

###### For Stanley Park Coyote camera project

# get the layers in the kml
input_file <- 'data/SP_CoyoteAttacks.kml'
CoyoteAttacks_sf <- read_sf(input_file) %>% st_transform(crs=26910) %>% st_zm(drop=TRUE, what="ZM") # convert to utm for grid size

ggplot()+
  geom_sf(data=aoi_utm)+
  geom_sf(data=CoyoteAttacks_sf)


# determine distance of sampling locations to attacks
sa.attack.dist <- st_nn(sa_points, CoyoteAttacks_sf, k=1, returnDist = T)
sa_smpl_lcns$attack_dist <- unlist(sa.attack.dist$dist) 


glimpse(sa_smpl_lcns)
sa_smpl_lcns %>% group_by(wtr_type) %>% summarise(min=min(wtr_dist), mean=mean(wtr_dist), max=max(wtr_dist))
sa_smpl_lcns %>% group_by(road_type) %>% summarise(min=min(road_dist), mean=mean(road_dist), max=max(road_dist))
sa_smpl_lcns %>% group_by(FWA_type) %>% summarise(min=min(FWA_dist), mean=mean(FWA_dist), max=max(FWA_dist))
sa_smpl_lcns %>% summarise(min=min(attack_dist), mean=mean(attack_dist), max=max(attack_dist))

# ###--- sampling locations available to use
names(sa_smpl_lcns)
sa_smpl_lcns$locations <- as.factor(ifelse(sa_smpl_lcns$attack_dist < 500 &
  sa_smpl_lcns$road_dist < 100 &
    between(sa_smpl_lcns$wtr_dist, 50, 500), "available","exclude"))

sa_smpl_lcns %>% count(locations) # 154 possible locations within the 3 study areas
nrow(sa_smpl_lcns)
# 99 available and 70 excluded

sa_smpl_lcns$priority <- as.factor(ifelse(sa_smpl_lcns$locations=="available" &
                                            sa_smpl_lcns$elev < 1000, 1,
                                          ifelse(sa_smpl_lcns$locations=="exclude",3,2)))

sa_smpl_lcns %>% group_by(priority) %>% count(locations) # 154 possible locations within the 3 study areas

###--- create sf object from sampling location data frame
sa_smpl_lcns.sf <- st_as_sf(sa_smpl_lcns, coords = c("X","Y"), crs = 26910)
sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm)
# sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm) #%>% select(-Nmae, -SHAPE_Leng, -SHAPE_Area)


sa_smpl_lcns.sf %>% count(locations) # 110 available sampling locations, 83 excluded based on proximity to roads/river and WHA occurrence

# export shapefile
st_write(sa_smpl_lcns.sf, paste0(getwd(),"/out/SP_smpln_opts.shp"), delete_layer = TRUE)

# plot to check
ggplot()+
  geom_sf(data=aoi_utm, col="gray", lwd=0.5)+
  geom_sf(data = aoi, lwd=1, col="black", fill=NA)+
  geom_sf(data=sa.DRA, aes(fill=FEATURE_TYPE, col=FEATURE_TYPE)) +
  geom_sf(data=sa.wtrcrs, col="blue", fill="lightblue")+
  geom_sf(data=CoyoteAttacks_sf, col="darkred")+
  geom_sf(data = sa_smpl_lcns.sf %>% filter(locations=="available"), col="black", pch=18, cex=2)+
  ggtitle("Potential Camera Locations \n Stanley Park") 
ggsave("out/SP_smpl_lcns.png",plot=last_plot(), dpi=300)


st_write(smp_elk %>% filter(Options!="truck") %>% st_transform(crs=4326) %>% select(NAME = Name), "data/elk_aerial_atv_sites.kml", driver = "kml", delete_dsn = TRUE)

st_write(sa_smpl_lcns.sf  %>% st_transform(crs=4326), "SP_lcns.kml", driver = "kml", delete_dsn = TRUE)


st_write(sa_smpl_lcns.sf %>% filter(locations=="available"), "out/SP_avail_lcns.kml", driver = "kml", delete_dsn = TRUE)


sa_smpl_lcns.sf <- st_read(dsn="./out", layer="Owl_smpln_opts")

#####################################################################################
###--- view OSM data and download appropriate section for study area
#- EPU polygon shapefile
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
aoi <- st_read(dsn=GISDir, layer="EPU_Sechelt_Peninsula")

aoi_latlon <- st_transform(aoi, crs=4326)
st_bbox(aoi_latlon)

LAT1 = st_bbox(aoi_latlon)[2] ; LAT2 = st_bbox(aoi_latlon)[4]
LON1 = st_bbox(aoi_latlon)[3] ; LON2 = st_bbox(aoi_latlon)[1]

#our background map
map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[6],
               mergeTiles = TRUE)

## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

###--- for elk camera project
# smpl_plot_2021 <- OpenStreetMap::autoplot.OpenStreetMap(map.latlon)  +
#   labs(title = "Potential Sampling Locations - 2021",x = "Longitude", y="Latitude")+
#   geom_point(data=smp_elk, 
#              aes(x=Longitude, y=Latitude, fill=Options), size=3, shape=21)+
#   # geom_point(data=AARU[AARU$options=="available",],
#   #            aes(x=Longitude, y=Latitude), size=4, shape=21, fill="blue") +
#   theme(legend.position = "bottom")
# smpl_plot_2021

camlocn <- read.csv("C:/Users/JBURGAR/R/Analysis/Elk_sightability/data/camera_actual_location_SP.csv", header=TRUE)
camlocn.sf <- st_as_sf(camlocn, coords = c("Longitude","Latitude"), crs = 4326) # create spatial layer

camlocn_plot_2021 <- OpenStreetMap::autoplot.OpenStreetMap(map.latlon)  +
  labs(title = "Camera Locations - 2021",x = "Longitude", y="Latitude")+
  geom_point(data=sa_smpl_lcns,
             aes(x=Longitude, y=Latitude, fill=Deploy_Type), size=3, shape=21)+
  theme(legend.position = "bottom", legend.title = element_blank())
camlocn_plot_2021


Cairo(file="C:/Users/JBURGAR/R/Analysis/Elk_sightability/out/elk_camlocn_plot_2021_low.PNG",
      type="png",
      width=1600,
      height=1200,
      pointsize=10,
      bg="white",
      dpi=200)
camlocn_plot_2021
dev.off()


## create point data
#- smaller polygons
sa_smpl_lcns_latlon <- st_transform(sa_smpl_lcns.sf, crs=4326)
sa_smpl_lcns_latlon$Longitude <- st_coordinates(sa_smpl_lcns_latlon)[,1]
sa_smpl_lcns_latlon$Latitude <- st_coordinates(sa_smpl_lcns_latlon)[,2]

# #- Anderson polygon
# AARU <- st_read(dsn="./out", layer="Anderson_ARU_opts") %>% st_transform(crs=4326)
# AARU$Longitude <- st_coordinates(AARU)[,1]
# AARU$Latitude <- st_coordinates(AARU)[,2]

smpl_plot_2021 <- OpenStreetMap::autoplot.OpenStreetMap(map.latlon)  +
  labs(title = "Potential Sampling Locations - 2021",x = "Longitude", y="Latitude")+
  geom_point(data=sa_smpl_lcns_latlon[sa_smpl_lcns_latlon$locations=="available",], 
             aes(x=Longitude, y=Latitude, fill=OBJECTID), size=4, shape=21)+
  # geom_point(data=AARU[AARU$options=="available",],
  #            aes(x=Longitude, y=Latitude), size=4, shape=21, fill="blue") +
  theme(legend.position = "none")
smpl_plot_2021

Cairo(file="out/Owl_smpl_plot_2021.PNG",
      type="png",
      width=3000,
      height=2200,
      pointsize=15,
      bg="white",
      dpi=300)
smpl_plot_2021
dev.off()

# plot to check
ggplot()+
  geom_sf(data=aoi.WHA %>% st_transform(crs=26910), aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data = sa_smpl_lcns.sf %>% filter(options=="available")) +
  geom_sf(data = aoi %>% filter(OBJECTID!=3), lwd=2, col="red", fill=NA)+
  geom_sf(data=sa.wtrcrs, lwd=1.5, col="blue") +
  geom_sf(data=sa.DRA %>% filter(FEATURE_TYPE=="Road"), lwd=0.8, col="brown") +
  theme(legend.position = "none")

glimpse(sa_smpl_lcns.sf)
ggplot()+
  geom_sf(data = sa_smpl_lcns.sf %>% filter(priority=="1"), col="red") +
  geom_sf(data = sa_smpl_lcns.sf %>% filter(priority=="2"), col="blue") +
  #geom_sf(data = sa_smpl_lcns.sf %>% filter(priority=="3"), col="black") +
  theme(legend.position = "none")


# #- Anderson polygon
actual_lcn <- read.csv("data/2021_Deployment_schedule_Jul21bc.csv")
head(actual_lcn)
names(actual_lcn)
actual_lcn.sf <- st_as_sf(actual_lcn, coords = c("Utm.e","Untm.n"), crs = 26910) # create spatial layer
ggplot()+
  geom_sf(data = actual_lcn.sf)


st_write(actual_lcn.sf %>% st_transform(crs=4326), "out/actual_lcns.kml", driver = "kml", delete_dsn = TRUE)
st_write(actual_lcn.sf, paste0(getwd(),"/out/Owl_actual_lcns.shp"), delete_layer = TRUE)
# st_write(aoi_grid, paste0(getwd(),"/out/Owl_smpln_grid.shp"), delete_layer = TRUE)

actual_lcn.dist <- st_nn(actual_lcn.sf, actual_lcn.sf, k=2, returnDist = T)
actual_lcn.dist <- unlist(actual_lcn.dist[2])
head(actual_lcn.dist)
actual_lcn.dist <- actual_lcn.dist[actual_lcn.dist > 0]
length(actual_lcn.dist)
min(actual_lcn.dist); mean(actual_lcn.dist); max(actual_lcn.dist) # 94 m; 657 m; 2400 m
sd(actual_lcn.dist)/sqrt(length(actual_lcn.dist)) # SE = 29 m

