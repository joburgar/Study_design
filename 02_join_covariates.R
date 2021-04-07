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
plot(aoi_raster)
plot(sa_points %>% st_transform(crs = 4326), add= TRUE) # as a check

aoi.cded <- rasterToPoints(aoi_raster) # convert to points for join
aoi.cded.sf <- st_as_sf(as.data.frame(aoi.cded), coords = c("x","y"), crs = 4326) # create spatial layer
aoi.cded.utm <- st_transform(aoi.cded.sf, crs = 26910) # convert to utm for join distance


# load covariates from bcdata
# using the bc data warehouse option to clip to aoi

# watercourses layer
# bcdc_search("NTS BC River", res_format = "wms")
aoi.RLW <- bcdc_query_geodata("450d4230-c552-4b61-add9-43ff2f870f59") %>%  #NTS  River, Lake and Wetland
  filter(INTERSECTS(aoi)) %>%
  collect()

# FWA_WATERSHED_GROUPS Freshwater Atlas Watershed Boundaries
# bcdc_search("freshwater", res_format = "wms")
aoi.FWA <- bcdc_query_geodata("ab758580-809d-4e11-bb2c-df02ac5465c9") %>%  #FWA_WATERSHED_GROUPS
  filter(INTERSECTS(aoi)) %>%
  collect()
glimpse(aoi.FWA)

# transportation layer (Digital Road Atlas)
# bcdc_search("road", res_format = "wms")
aoi.DRA <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# approved WHAs & UWRs (GAR Orders)
# bcdc_search("WHA", res_format = "wms")
aoi.WHA <- bcdc_query_geodata("b19ff409-ef71-4476-924e-b3bcf26a0127") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# bcdc_search("UWR", res_format = "wms")
aoi.UWR <- bcdc_query_geodata("712bd887-7763-4ed3-be46-cdaca5640cc1") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# vegetation data (VRI)
# bcdc_search("VRI", res_format = "wms")
aoi.VRI <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%  
  filter(INTERSECTS(aoi)) %>%
  collect()

# plot as check
# glimpse(aoi_raster$elevation)
# ggplot(aoi_raster, aes(extent, y)) +
#   geom_raster(aes(fill = elevation))


ggplot()+
  geom_sf(data=aoi.WHA, aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data=sa_points) +
  geom_sf(data = aoi_utm, lwd=2, col="red", fill=NA)+
  geom_sf(data=aoi.RLW, lwd=1, col="blue") +
  geom_sf(data=aoi.DRA, lwd=0.8, col="brown")

aoi.VRI %>% filter(!is.na(PROJ_HEIGHT_1)) %>%  
  summarise(mean = mean(PROJ_HEIGHT_1), min = min(PROJ_HEIGHT_1), max=max(PROJ_HEIGHT_1), sd = sd(PROJ_HEIGHT_1))
#  mean = 22.8, min =  0.1, max = 54.1, sd  = 8.89 # Anderson
#  mean = 25.5, min = 0.2, max = 63.5, sd = 13.2 # Skwawka

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
  geom_sf(data = aoi_utm %>% filter(OBJECTID==3), lwd=2, col="red", fill=NA) +
  geom_sf(data = aoi_utm %>% filter(OBJECTID!=3), lwd=2, col="blue", fill=NA) +
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
aoi.RLW %>% count(FCODE)
# sa.wtrcrs <- aoi.wtrcrs %>% st_intersection(aoi %>% filter(OBJECTID==3)) %>% st_transform(crs=26910)
sa.wtrcrs <- aoi.RLW %>% st_intersection(aoi) %>% st_transform(crs=26910)
# no wtrcrs at 5M scale in two smaller cells
sa.wtrcrs.dist <- st_nn(sa_points, sa.wtrcrs, k=1, returnDist = T)

sa_smpl_lcns$wtr_dist <- unlist(sa.wtrcrs.dist$dist)
sa_smpl_lcns$wtr_type <- unlist(sa.wtrcrs.dist$nn)
sa_smpl_lcns$wtr_type <- sa.wtrcrs$FCODE[match(sa_smpl_lcns$wtr_type,rownames(sa.wtrcrs))]


# sa_smpl_lcns$wtr_use <- as.factor(ifelse(sa_smpl_lcns$wtr_dist>100,"yes","no"))
  
#- roads
# sa.DRA <- aoi.DRA %>% st_intersection(aoi %>% filter(OBJECTID==3)) %>% st_transform(crs=26910)
sa.DRA <- aoi.DRA %>% st_transform(crs=26910)
sa.road.dist <- st_nn(sa_points, sa.DRA, k=1, returnDist = T)

sa_smpl_lcns$road_dist <- unlist(sa.road.dist$dist)
sa_smpl_lcns$road_type <- unlist(sa.road.dist$nn)
sa_smpl_lcns$road_type <- sa.DRA$FEATURE_TYPE[match(sa_smpl_lcns$road_type,rownames(sa.DRA))]
summary(as.factor(sa_smpl_lcns$road_type))

sa_smpl_lcns$road_use <- as.factor(case_when(sa_smpl_lcns$road_type=="Road" & sa_smpl_lcns$road_dist<100 ~ "no",
                                             TRUE ~ "yes"))


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
sa.UWR.dist <- st_nn(sa_points, aoi.UWR %>% st_transform(crs=26910), k=1, returnDist = T)
sa_smpl_lcns$UWR_dist <- unlist(sa.UWR.dist$dist) 

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
summary(sa.VRI.age) # ranges from 2 to 374 years, median = 144, mean = 148.6
hist(sa.VRI.age$PROJ_AGE_1, breaks = 20)

sa_smpl_lcns$veg_dist <- unlist(sa.VRI.dist$dist)
sa_smpl_lcns$veg_height <- unlist(sa.VRI.dist$nn)
sa_smpl_lcns$veg_height <- sa.VRI$PROJ_HEIGHT_1[match(sa_smpl_lcns$veg_height,rownames(sa.VRI))]


# ###--- sampling locations available to use
sa_smpl_lcns$options <- as.factor(ifelse(sa_smpl_lcns$wtr_dist>1 &
                                           #between(sa_smpl_lcns$road_dist,10,1000) &
                                           between(sa_smpl_lcns$elev,1,1000), "available","exclude"))

###--- create sf object from sampling location data frame
sa_smpl_lcns.sf <- st_as_sf(sa_smpl_lcns, coords = c("X","Y"), crs = 26910)
# sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm %>% filter(OBJECTID==3)) %>% select(-Nmae, -SHAPE_Leng, -SHAPE_Area)
# sa_smpl_lcns.sf <- st_intersection(sa_smpl_lcns.sf, aoi_utm) #%>% select(-Nmae, -SHAPE_Leng, -SHAPE_Area)


sa_smpl_lcns.sf %>% count(options) # only 73 available sampling locations, 83 excluded based on proximity to roads/river and WHA occurrence
sa_smpl_lcns.sf %>% filter(options=="available") %>% summarise(min(veg_height), mean(veg_height), max(veg_height))

# sa_smpl_lcns.sf %>% filter(options=="available") %>% count(OBJECTID)

# plot to check - clipped the appropriate area
ggplot()+
  geom_sf(data = sa.VRI, aes(fill=PROJ_HEIGHT_1_cat, col=NA)) +
  scale_fill_brewer(palette="Greens") +
  scale_color_brewer(palette="Greens") +
  geom_sf(data = sa_smpl_lcns.sf %>% filter(options=="available") ,size = 2, shape = 23, fill = "darkred") +
  geom_sf(data = aoi_grid, size=0.5, col="gray", fill=NA)+
  geom_sf(data = aoi_utm %>% filter(OBJECTID!=3), lwd=2, col="blue", fill=NA) +
    theme(legend.title=element_blank())
ggsave("Skwawka_options_grid.png",plot=last_plot(), dpi=300)


# export shapefile
st_write(sa_smpl_lcns.sf, paste0(getwd(),"/out/SecheltPen_smpln_opts.shp"), delete_layer = TRUE)

# plot to check
ggplot()+
  geom_sf(data=aoi.WHA %>% st_transform(crs=26910), aes(fill=COMMON_SPECIES_NAME), color=NA)+  # use color=NA to remove border lines
  geom_sf(data = sa_smpl_lcns.sf %>% filter(options=="available")) +
  # geom_sf(data = sa_smpl_lcns.sf %>% filter(elev>0), col="red") +
  geom_sf(data = aoi %>% filter(OBJECTID!=3), lwd=2, col="red", fill=NA)+
  geom_sf(data=sa.wtrcrs, lwd=1.5, col="blue") +
  geom_sf(data=sa.DRA %>% filter(FEATURE_TYPE=="Road"), lwd=0.8, col="brown") +
  theme(legend.position = "none")
# ggsave("Anderson_ARU_options.png",plot=last_plot(), dpi=300)
ggsave("SU_ARU_options.png",plot=last_plot(), dpi=300)

#####################################################################################
###--- view OSM data and download appropriate section for study area
aoi_latlon <- st_transform(aoi, crs=4326)
st_bbox(aoi_latlon)

LAT1 = st_bbox(aoi_latlon)[2] ; LAT2 = st_bbox(aoi_latlon)[4]
LON1 = st_bbox(aoi_latlon)[3] ; LON2 = st_bbox(aoi_latlon)[1]

#our background map
map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[3],
               mergeTiles = TRUE)

## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

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
  geom_point(data=sa_smpl_lcns_latlon[sa_smpl_lcns_latlon$options=="available",], 
             aes(x=Longitude, y=Latitude, fill=elev), size=4, shape=21)+
  # geom_point(data=AARU[AARU$options=="available",], 
  #            aes(x=Longitude, y=Latitude), size=4, shape=21, fill="blue") +
  # theme(legend.position = "none")
smpl_plot_2021

Cairo(file="out/SechPen_smpl_plot_2021.PNG",
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
