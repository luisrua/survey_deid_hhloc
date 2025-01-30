### DEIDENTIFICATION OF HOUSEHOLD LOCATIONS FROM SURVEY DATASETS ###
### BASED ON MICS GIS INITITATIVE METHODOLOGY ###
# https://mics.unicef.org/news/geographic-data-mics-anonymisation-and-dissemination
## LUIS DE LA RUA -- DEC 2024 ##

## HH LOCATIONS DISPLACEMENT FOR 2021 TONGA HIES ##

## INCLUDING CLUSTER CREATION ##

# SETTINGS  --------------------------------------------------------------------
source("setup.R") 

# Paths
dir <- "C:/Users/luisr/SPC/SDD GIS - Documents/Deidentification/TongaHIES21_test/"
layers <- "C:/Users/luisr/SPC/SDD GIS - Documents/Deidentification/TongaHIES21_test/layers/"

# IMPORT DATASET AND LAYERS AND PROCESS LATER
# Import hhlocations dataset
hhloc_dta <- read_dta(paste0(dir,"0_Cover.dta"))

# IMPORT ADMIN BOUNDARIES
ab <- vect(paste0(layers,"TON_dsid_4326.gpkg")) ## This needs to be replaced each time
# Import block layer to ensure that the block ids entered on questionnaire are correct
block_pol <- vect("C:/Users/luisr/SPC/SDD GIS - Documents/Surveys/2020 TON HIES/layers/TON_2020HIES_GPSchks.gpkg",layer = "2021HIES_EA framework")

# 1. PROCESS SURVEY DATASET -----------------------------------------

# Clean what we do not need and rename some of the variables
hhloc_dta <- hhloc_dta %>% 
  select(c(interview__key, island, rururb, district, village, block, buildingGPS__Latitude,buildingGPS__Longitude)) %>% 
  rename(x = buildingGPS__Longitude,
         y = buildingGPS__Latitude,
         ur = rururb) %>% 
  print()

# Convert to spatial object
hhloc <- vect(hhloc_dta, geom = c("x","y"), crs="EPSG:4326")
plot(hhloc)

# plot hhloc using different colour based on block code
plot(hhloc, col = hhloc$block)

# Reproject to metric
metric_crs <- crs("EPSG:3832")
hhloc <- project(hhloc, metric_crs)
crs(hhloc)

writeVector(hhloc, paste0(layers,"cover.gpkg"), overwrite=T)


# 2. PROCESS ADMIN BOUNDARIES LAYERS AND PROCESS -------------------------

# Reproject to metric
ab <- project(ab,metric_crs)
block_pol <- project(block_pol, metric_crs)

head(ab)
head(block_pol)
# Standardize admin unit codes
ab <- ab %>% 
  mutate(abid = dsid) %>% 
  print()

block_pol <- block_pol %>% 
  mutate (block_geo = blkid ) %>% 
  select(block_geo)


# 3. GENERATE CLUSTERS FROM HHLOCATIONS DATASET PRIOR TO DISPLACEMENT PROCESS ----------

# Capture admin unit codes
hhloc_sjoin <- hhloc %>%
  mutate(abid = terra::extract(ab, hhloc)[, "abid"]) %>%  # extract admin unit code 
  mutate(block_geo = terra ::extract(block_pol,hhloc)[,"block_geo"]) # extract block code from block boundaries

# Find points out of the admin boundaries
blkid_na <- hhloc_sjoin %>% 
  as_tibble() %>% 
  filter(is.na(block_geo)) %>% 
  print()

# Remove hhlocs out of boundaries
hhloc_sjoin <- hhloc_sjoin %>% 
  filter(!is.na(block_geo))

# Check inconsistencies with codes
issue_blk <- blkid_na <- hhloc_sjoin %>% 
  as_tibble() %>% 
  filter(block_geo != block) %>% 
  print()

table(hhloc_sjoin$block_geo)
table(hhloc_sjoin$block)
table(hhloc_sjoin$village)

# Check unique identifier
isid(as.data.frame(hhloc_sjoin), vars="interview__key")

# For this dataset ur = 1 -> Urban ur = 2 -> Rural
head(hhloc_sjoin)
table(hhloc_sjoin$ur)

hhloc_sjoin <- hhloc_sjoin %>%
  rename (urint = ur) %>% 
  mutate(ur = ifelse(urint == 1, "R", "U")) %>% 
  select(-urint)

table(hhloc_sjoin$ur)

# How many points we have in each block including NAS and convert into dataframe
block_summ <- hhloc_sjoin %>% 
  as.data.frame() %>% 
  group_by(block_geo) %>% 
  summarize(n = n()) %>% 
  print()

# Group points in the same block into polygon and then calculate centroids
polygons <- hhloc_sjoin %>%
  st_as_sf() %>% 
  group_by(block_geo) %>%
  summarise(geometry = st_convex_hull(st_union(geometry)), .groups = "drop") %>%
  st_as_sf()

# There is the possibility of having clusters formed by single point or a line. We deal with this
# identifying these cases and converting them into polygons generating a 1m buffer

# Buffer points to small polygons (e.g., with a 1m buffer)

not_polygon <- polygons %>%
  filter(st_geometry_type(.) != "POLYGON") %>%
  st_buffer(dist = 1)  # Adjust the distance accordingly

# Combine the points and lines buffered back into polygons
polygons <- rbind(not_polygon, polygons[st_geometry_type(polygons) == "POLYGON", ])

polygons

# Generate centroids for the polygons generated 
clust_centr <- terra::centroids(vect(polygons), inside =T )


# Plot hh locations, polygons and centroids for checking purposes
 # Plot convex hulls
tm_shape(st_as_sf(hhloc)) +
  tm_dots(col = "block", palette = "Set1", title = "Block") + # Plot points
  tm_shape(st_as_sf(clust_centr)) +
  tm_dots(col = "red") + 
  tm_shape(polygons) +
  tm_polygons(col = "lightblue", border.col = "black", alpha = 0.5) + 
  tm_shape(st_as_sf(block_pol)) + 
  tm_polygons(border.col = "orange", alpha = 0)+
  tm_basemap("Esri.WorldImagery") 
  

# Complete dataframe for the following operations. making 1:n join
clust <- clust_centr %>% 
  left_join(as.data.frame(hhloc_sjoin), by = "block_geo", 
            keep = F, multiple = "first") %>% 
  select(-(interview__key))


# Random select 1% of the Rural points and rename them as R1
# Randomly sample 1% of entries where UR = R
set.seed(123)  # For reproducibility

random_selection <- clust %>%
  filter(ur == "R") %>%       # Filter rows where UR = R
  slice_sample(n = max(1, round(nrow(.) * 0.01))) %>%  # Ensure at least 1 row is selected
  mutate(ur = "R1")

# Inspect the result
as_tibble(random_selection)

# Merge with original hhlocations dataset identifying the Rural points that are R1
clust_to_displ <- clust %>%
  filter(!(block_geo %in% random_selection$block_geo)) %>%  # Remove original rows in the selection
  rbind(random_selection)  
table(clust_to_displ$ur)


#  4. HH LOCATIONS DEIDENTIFICATION PROCESS -------------------------------------
points <- clust_to_displ

### Include different displacement methods based on Urban / Rural Strata also including 
# the 1% extra displacement for Rural dwellings

# Function to displace a single point with conditions based on `UR` value
displace_point_with_ur_check <- function(point, ab, max_distances) {
  original_abid <- point$abid
  ur_value <- point$ur  # Extract the UR value for the current point
  
  # Get the corresponding max displacement distance based on UR value
  max_distance <- max_distances[[ur_value]]
  
  if (is.null(max_distance)) {
    stop(paste("Invalid UR value:", ur_value))
  }
  
  repeat {
    # Generate random displacement
    distance <- runif(1, 0, max_distance)
    angle <- runif(1, 0, 360)
    angle_rad <- angle * pi / 180
    
    # Calculate new coordinates
    x_displacement <- distance * cos(angle_rad)
    y_displacement <- distance * sin(angle_rad)
    new_coords <- crds(point) + cbind(x_displacement, y_displacement)
    
    # Create temporary point
    temp_point <- vect(new_coords, type = "points", crs = crs(point))
    
    # Extract new iid
    new_abid <- terra::extract(ab, temp_point)[, "abid"]
    
    # Check if new iid matches original
    if (!is.na(new_abid) && new_abid == original_abid) {
      return(temp_point)
    }
  }
}

# Set maximum displacement distances for each UR category
max_distances <- list(
  U = 2000,   # Displacement 1 to 2000m for UR = U
  R = 5000,   # Displacement 1 to 5000m for UR = R
  R1 = 10000  # Displacement 1 to 10000m for UR = R1
)

# Ensure CRS consistency between points and admin_units
points <- project(points, crs(ab))

# Initialize an empty list to store displaced points
displaced_points_list <- list()

# Loop through each point
tic()
for (i in 1:nrow(points)) {
  # Displace the point based on UR and check admin unit
  displaced_points_list[[i]] <- displace_point_with_ur_check(points[i, ], ab, max_distances)
}

# Combine displaced points into a single SpatVector
displaced_points <- do.call(rbind, displaced_points_list)

# Ensure displaced points keep their original attributes
values(displaced_points) <- values(points)

toc()

# writeVector(displaced_points, paste0(layers,"/displaced_hhloc.gpkg"), overwrite = TRUE)

#  PREPARE DATA TO BE INCLUDED INTO MICRODATA DATASET ------------------------
output <- displaced_points

# Project into standard projection
output <- project(output, "EPSG:4326")

# Clean fields are not used and create x y coords fields
output <- output %>% 
  select(c(block_geo)) %>%
  mutate(x_cluster_displaced = crds(.)[,1],
         y_cluster_displaced = crds(.)[,2]) %>% 
  print()

plot(output)

final_data <- hhloc_sjoin %>% 
  as.data.frame() %>% 
  left_join(as.data.frame(output), by = "block_geo") %>% 
  select(-"block_geo")
  

# Export as gpkg and as csv too

write.csv(final_data, paste0(layers,"displaced_hhloc_4326.csv"))

# 5. Evaluate deidentification process -----------------------------------------
# Need to do more research on this, contact people from MICS GIS Initiative????


# 6. Analyse displacement from geographic perspective ------------------------
original_df <- as.data.frame(clust)
displaced_df <- as.data.frame(displaced_points)

# Calculate paths between original and displaced points 
paths <- list() 

for (i in 1:nrow(original_df)){ 
  # Extract points with matching ids 
  orig <- clust[clust$block_geo == original_df$block_geo[i], ] 
  disp <- displaced_points[displaced_points$block_geo == original_df$block_geo[i], ] 
  
  # Create a line from the original to displaced point 
  line <- rbind(crds(orig), crds(disp))  # Coordinates of the two points 
  paths[[i]] <- vect(line, type = "lines", crs = crs(clust)) 
  # Get interviewer__key id for each of the lines
  paths[[i]]$block_geo <- original_df$block_geo[i]
}
# Combine list into single dataset
paths_vect <- do.call(rbind, paths)

# Calculate distances
paths_vect <- paths_vect %>% 
  mutate(length_m = terra::perim(paths_vect)) %>% 
  print()

# Merge with original dataset to get Urban rural classification
paths_vect <- paths_vect %>% 
  left_join(as.data.frame(points) %>% 
              select(block_geo, ur), by="block_geo") %>% 
  print()

paths_vect_stat <- paths_vect %>% 
  as.data.frame() %>%
  group_by(ur) %>% 
  summarise(disp_mean = mean(length_m), 
            disp_sd = sd(length_m), 
            dips_count = n(),
            disp_max = max(length_m),
            disp_min = min(length_m),
            disp_range = max(length_m) - min(length_m))

paths_vect_stat

# 7. Map results -----------------------------------------------------------
# Show map original and displaced points joined by the paths of a urban district dsid 11
tmap_mode("view")  # Set to interactive mode

# Example urban area
urban_example_map <-   tm_shape(st_as_sf(displaced_points)) + tm_dots(col = "red") +
  tm_shape(st_as_sf(hhloc)) + tm_dots(col = "blue") +
  tm_shape(st_as_sf(paths_vect)) + tm_lines(col = "#50fe00", lwd = 1) + 
  tm_shape(st_as_sf(ab)) + tm_borders(col = "orange", lwd = 3) + 
  tm_shape(polygons) +
  tm_polygons(col = "lightblue", border.col = "black", alpha = 0.2) +  # Plot convex hulls
  tm_basemap("Esri.WorldImagery") +
  tm_view(bbox = st_bbox(ab %>% filter(dsid == 11)))

urban_example_map

# Example rural area
rural_example_map <-tm_shape(st_as_sf(hhloc)) + tm_dots(col = "blue") +
  tm_shape(st_as_sf(displaced_points)) + tm_dots(col = "red") +
  tm_shape(st_as_sf(paths_vect)) + tm_lines(col = "#50fe00", lwd = 1) + 
  tm_shape(st_as_sf(ab)) + tm_borders(col = "orange", lwd = 3) + 
  tm_shape(polygons) +
  tm_polygons(col = "lightblue", border.col = "black", alpha = 0.2) +  # Plot convex hulls
  tm_basemap("Esri.WorldImagery") +
  tm_view(bbox = st_bbox(ab %>% filter(dsid == 25)))


rural_example_map

# Save maps on image format
tmap_save(urban_example_map, filename = paste0(layers,"urban_example_map.png"), width = 10, height = 10, dpi = 300)
tmap_save(rural_example_map, filename = paste0(layers,"rural_example_map.png"), width = 10, height = 10, dpi = 300)



