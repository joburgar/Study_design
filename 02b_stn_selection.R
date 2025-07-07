#####################################################################################
# 02b_stn_selection.R
# script to join remaining covariates and stratify site selection
# created for EC_MMP project, focus on Manning Park and Skagit areas, expanding outwards
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 3-July-2025
#####################################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive

# Load Packages
list.of.packages <- c("tidyverse","sf","terra","purrr")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

# 1. Read vector data
GIS_Dir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/MMP/2.Data/Camera_ARU_Deployments/Ecological_Corridors/"
EC_MMP_Fquad <- st_read(dsn = GIS_Dir, layer = "BC_meso_Fquad_EC")

# 2. Load raster
raster_fDir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/GIS_Sharespace/Elevation/"
BC_elev <- rast(file.path(raster_fDir, "elevation_BC_clip.tif"))

# 3. Calculate centroid (or use st_point_on_surface for guaranteed in-polygon points)
centroids <- st_point_on_surface(EC_MMP_Fquad)

# 4. Transform centroids to raster CRS (this is critical!)
centroids <- st_transform(centroids, crs(BC_elev))

# 5. Convert to terra format and extract
centroids_vect <- vect(centroids)
elev_vals <- extract(BC_elev, centroids_vect)

# 6. Join elevation values back (removing the ID column from extract)
centroids$elevation <- elev_vals[, 2]  # 2nd column = raster value

# 7. Optional: Join elevation to original polygons (by row index)
EC_MMP_Fquad$elevation <- centroids$elevation

################################################################################
## For site selection for on-site protocol
# need to be at least 1 km apart - doable if 1 per grid cell (36 km2)
# could also go with 3-4 cameras per grid cell and it would still fit within CCMP methods
# pick 


ggplot(data = EC_MMP_Fquad %>% filter(CCMP_prior != "CCMP_OOS")) +
  geom_sf(aes(fill = CCMP_prior)) +
  scale_fill_viridis_d(option = "plasma", na.value = "grey80") +  # <- discrete scale
  theme_minimal() +
  labs(title = "CCMP Prior by Fquad", fill = "CCMP Prior")


hist(EC_MMP_Fquad$elevation[EC_MMP_Fquad$CCMP_prior != "CCMP_OOS"])

###################################
centroids <- st_centroid(EC_MMP_Fquad)

# Step 2: Transform centroids to lat/lon (EPSG:4326)
centroids_latlon <- st_transform(centroids, crs = 4326)

# Step 3: Extract latitudes
latitudes <- st_coordinates(centroids_latlon)[, 2]

# Step 4: Filter the original object by these latitudes
sf_filtered <- EC_MMP_Fquad[latitudes < 49.3795, ]

# 4. Add logical column to original sf_object
EC_MMP_Fquad <- EC_MMP_Fquad %>%
  mutate(use_cell = Y_centroid < 548189) # about as far north as Boston Bar

# Step 5: Plot
ggplot() +
  geom_sf(data = EC_MMP_Fquad, aes(fill = use_cell), color = "black") +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "skyblue")) +
  labs(title = "Grid Cells Flagged for Use (Below 49.3795° N)") +
  theme_minimal()

EC_MMP_Fquad %>% filter(use_cell==TRUE) %>% group_by(ECpoly) %>% count(CCMP_prior) %>% st_drop_geometry()

# Plot, colored by CCMP_prior, below ~ Boston Bar, only in NCFP and with paved roads
ggplot() +
  geom_sf(data = EC_MMP_Fquad %>% filter(use_cell==TRUE) %>% filter(ECpoly=="NCFP") %>% filter(Paved_Dnst>0), 
          aes(fill = CCMP_prior), color = "black") +
  scale_fill_viridis_d(name = "CCMP Priority", option = "D") +
  labs(title = "Grid Cells by CCMP Priority - paved road") +
  theme_minimal()

ggplot() +
  geom_sf(data = EC_MMP_Fquad %>% filter(use_cell==TRUE) %>% filter(ECpoly=="NCFP") %>% filter(Unpvd_Dnst>0), 
          aes(fill = CCMP_prior), color = "black") +
  scale_fill_viridis_d(name = "CCMP Priority", option = "D") +
  labs(title = "Grid Cells by CCMP Priority - unpaved road") +
  theme_minimal()

ggplot() +
  geom_sf(data = EC_MMP_Fquad %>% filter(use_cell==TRUE) %>% filter(ECpoly=="NCFP") %>% filter(ParkTrail=="Yes"), 
          aes(fill = CCMP_prior), color = "black") +
  scale_fill_viridis_d(name = "CCMP Priority", option = "D") +
  labs(title = "Grid Cells by CCMP Priority - park trail") +
  theme_minimal()

####################################################
### PILOT STUDY DESIGN
# Part of the NCFP ecological cooridor polygon
# Below ~49.8 degrees N (548189 as the Y centroid in Albers)
# With a paved road in the 36 km2 cell
# yields 138 potential sites
# aim is to have 3-4 cameras per cell, spaced a min of 1 km apart, preferably ~3 km

EC_MMP_Fquad %>% filter(use_cell==TRUE) %>% filter(ECpoly=="NCFP") %>% filter(Paved_Dnst>0)

EC_MMP_pot <- EC_MMP_Fquad %>% filter(use_cell==TRUE) %>% filter(ECpoly=="NCFP") %>% filter(Paved_Dnst>0)
hist(EC_MMP_pot$elevation)


#elevation bins
EC_MMP_pot <- EC_MMP_pot %>%
  mutate(elevation_bin = case_when(
    elevation >= 0    & elevation < 500   ~ "1.0–500",
    elevation >= 500  & elevation < 1000  ~ "2.500–1000",
    elevation >= 1000 & elevation < 1500  ~ "3.1000–1500",
    elevation >= 1500 & elevation <= 2000 ~ "4.1500–2000",
    TRUE ~ NA_character_  # 
  ))

ggplot(EC_MMP_pot, aes(x = elevation_bin)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Elevation Bin", y = "Count", title = "Elevation Bins") +
  theme_minimal()

#road density bins
EC_MMP_pot <- EC_MMP_pot %>%
  mutate(Paved_Dnst_bin = case_when(
    Paved_Dnst >= 0    & Paved_Dnst < 0.25   ~ "1.Low Paved Density",
    Paved_Dnst >= 0.25    & Paved_Dnst < 0.5   ~ "2.Med Paved Density",
    Paved_Dnst >= 0.5    & Paved_Dnst < 2.0   ~ "3.High Paved Density",
    TRUE ~ NA_character_  # 
  ))

ggplot(EC_MMP_pot, aes(x = Paved_Dnst_bin)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Paved Road Density Bin", y = "Count", title = "Paved Density Bins") +
  theme_minimal()



###################################################################################
# load grizzly data
# values: 0 = low and 16 = high
GB_spring <- rast(file.path(GIS_Dir, "hsf_16quant_burgar_AOI_spring.tif"))
GB_summer <- rast(file.path(GIS_Dir, "hsf_16quant_burgar_AOI_summer.tif"))
GB_fall <- rast(file.path(GIS_Dir, "hsf_16quant_burgar_AOI_fall.tif"))
GB_latefall <- rast(file.path(GIS_Dir, "hsf_16quant_burgar_AOI_latefall.tif"))

# Summarize for mean value, then reclassify to bins and get the mode of each bin

# Define the matrix of breaks and new values
rcl <- matrix(c(
  0,    4.0,  1,
  4.0,  8.0,  2,
  8.0,  12.0,  3,
  12.0, 16.01,  4 # upper limit slightly higher than max
), ncol = 3, byrow = TRUE)

# Define a mode function
get_mode <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA)
  ux_tab <- table(ux)
  as.numeric(names(ux_tab)[which.max(ux_tab)])
}

## work through the files
v <- EC_MMP_pot      # your polygon shapefile

r <- GB_spring       # your raster

# Extract raster values within each polygon and summarize
summary_table <- extract(r, v, fun = mean, na.rm = TRUE)

r <- classify(r, rcl)
plot(r, col = terrain.colors(4), main = "Binned Raster: Low to High")

# Extract mode per polygon
mode_table <- extract(r, v, fun = get_mode)

summary_table$mode <- mode_table[,2]
names(summary_table)[2] <- "mean"

# add into polygon file
EC_MMP_pot$GB_spring_mean <- summary_table$mean
EC_MMP_pot$GB_spring_mode <- summary_table$mode

### come back to this point - EC_MMP_pot is now saved in project directory
# # Save
# saveRDS(EC_MMP_pot, "EC_MMP_pot.rds")

# Load
# EC_MMP_pot <- readRDS("EC_MMP_pot.rds")

# Create a ranking for sites - aiming for 100 cameras
# each cell will have an average of 3 cameras
# so that makes for ~33 sites (3*33=99 cameras)
# needs to have at least half within park trails (30 cells)
# balance the park trails so join with CCMP_priority and have 
# logistically need paved roads in each cell
# need to rank so that all park trails are included
# rank by bins of road density (paved and unpaved)
# rank by bins of grizzly bear habitat (aim for minimum of 5 cells with each mode)
# grizzly bear habitat will also be selected at micro site level
# rank by elevation bins (>1000 m for CCMP)
EC_MMP_pot %>% group_by(CCMP_prior) %>% count(ParkTrail) %>% st_drop_geometry()
EC_MMP_pot %>% count(Paved_Dnst_bin) %>% st_drop_geometry()
EC_MMP_pot %>% count(elevation_bin) %>% st_drop_geometry()
hist(EC_MMP_pot$Unpvd_Dnst)
hist(EC_MMP_pot$Paved_Dnst)


ggplot() +
  geom_sf(data = EC_MMP_pot %>% filter(use_cell==TRUE) %>% filter(ECpoly=="NCFP") %>% filter(ParkTrail=="Yes"), 
          aes(fill = CCMP_prior), color = "black") +
  scale_fill_viridis_d(name = "CCMP Priority", option = "D") +
  labs(title = "Grid Cells by CCMP Priority - park trail") +
  theme_minimal()

glimpse(EC_MMP_pot)

#  Create a Cross-Stratification Table
cells <- expand.grid(
  # CCMP_prior = c("2526_1", "2526_2", "NotPrior"),
  ParkTrail = c("No", "Yes"),
  elevation_bin = c("1.0–500", "2.500–1000", "3.1000–1500", "4.1500–2000"),
  Paved_Dnst_bin = c("1.Low Paved Density", "2.Med Paved Density", "3.High Paved Density")
)


# Add a stratification ID to data and a random variable to select stratum
EC_MMP_pot <- EC_MMP_pot %>%
  mutate(strata_id = paste(ParkTrail, elevation_bin, Paved_Dnst_bin, sep = "_"))

# Assuming you already created strata_id
EC_MMP_pot <- EC_MMP_pot %>%
  mutate(random_val = runif(n())) %>%  # Assign a random number to each row
  arrange(strata_id, random_val)       # Sort by strata then random value

sampled_data <- EC_MMP_pot %>%
  group_by(strata_id) %>%
  slice_head(n = 3) %>%  # Pick the top 3 random ones per stratum
  ungroup()


sampled_data %>% count(CCMP_prior) %>% st_drop_geometry()
sampled_data %>% count(ParkTrail) %>% st_drop_geometry()
sampled_data %>% count(elevation_bin) %>% st_drop_geometry()
sampled_data %>% count(Paved_Dnst_bin) %>% st_drop_geometry()

sampled_data %>% count(strata_id)%>% st_drop_geometry()
write.csv(sampled_data, "sampled_data.csv", row.names = FALSE)



st_write(EC_MMP_pot, "EC_MMP_pot.shp", delete_layer = TRUE)
st_write(EC_MMP_pot, "EC_MMP_pot.kml", driver = "KML", delete_dsn = TRUE)

