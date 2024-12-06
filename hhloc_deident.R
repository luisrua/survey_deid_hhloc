### DEIDENTIFICATION OF HOUSEHOLD LOCATIONS FROM SURVEY DATASETS ###
### BASED ON MICS GIS INITITATIVE METHODOLOGY ###
# https://mics.unicef.org/news/geographic-data-mics-anonymisation-and-dissemination
## LUIS DE LA RUA -- DEC 2024 ##


# SETTINGS  --------------------------------------------------------------------
source("setup.R") 

# Paths
dir <- "C:/Users/luisr/SPC/SDD GIS - Documents/Deidentification/test/"
layers <- "C:/Users/luisr/SPC/SDD GIS - Documents/Deidentification/test/layers/"

# 1. IMPORT AND PROCESS SURVEY DATASET -----------------------------------------
# Import fake dataset
hhloc_csv <- read.csv(paste0(dir,"dataset/fake_mics_locations_4326.csv"))

# Convert to spatial object
hhloc <- vect(hhloc_csv, geom = c("x","y"), crs="EPSG:4326")

# Reproject to metric
metric_crs <- crs("EPSG:3832")
hhloc <- project(hhloc, metric_crs)
crs(hhloc)
# 2. IMPORT ADMINISTRATIVE BOUNDARY LAYERS AND PROCESS -------------------------
village <- vect(paste0(layers,"kir_vid_mics24_4326.gpkg"))
island <- vect(paste0(layers,"kir_iid_mics24_4326.gpkg"))

# Reproject to metric
village <- project(village,metric_crs)
island <- project(island, metric_crs)



# 3. PREPARE HHLOCATIONS DATASET PRIOR DISPLACEMENT PROCESS --------------------

# Capture vid and EA codes
hhloc_sjoin <- hhloc %>%
  mutate(
    vid = terra::extract(village, hhloc)[, "vid"], # Replace "vid" with the column name in `village`
    iid = terra::extract(island,hhloc)[,"iid"]
  ) %>% 
  rename(hhid = rand_point_id)
isid(as.data.frame(hhloc_sjoin), vars="hhid")


# For this test case we assign Urban Rural Strata from island code 
hhloc_init <- hhloc_sjoin %>% 
  mutate( ur = ifelse(iid %in% c("7","22"), "U","R"))

table(hhloc_init$ur)
head(hhloc_init)

# Random select 1% of the Rural points and rename them as R1
# Randomly sample 1% of entries where UR = R
set.seed(123)  # For reproducibility

random_selection <- hhloc_init %>%
  filter(ur == "R") %>%       # Filter rows where UR = R
  slice_sample(prop = 0.01) %>%   # Randomly select 1% of rows
  mutate(ur = "R1")

# Inspect the result
print(random_selection)

# Merge with original hhlocations dataset identifying the Rural points that are R1
hhloc_to_displ <- hhloc_init %>%
  filter(!(hhid %in% random_selection$hhid)) %>%  # Remove original rows in the selection
  rbind(random_selection)  
table(hhloc_to_displ$ur)


#  4. HH LOCATIONS DEIDENTIFICATION PROCESS -------------------------------------
points <- hhloc_to_displ
admin_units <- island

# go back to the simple deidentification process and start from there

# Generate random distances (0 to 5000 meters) and angles (0 to 360 degrees)
set.seed(123)  # For reproducibility
n_points <- nrow(points)  # Number of points
distances <- runif(n_points, 0, 5000)  # Random distances
angles <- runif(n_points, 0, 360)  # Random angles in degrees

# Convert angles to radians for trigonometric functions
angles_rad <- angles * pi / 180

# Calculate new coordinates
x_displacement <- distances * cos(angles_rad)  # X displacement
y_displacement <- distances * sin(angles_rad)  # Y displacement
new_coords <- crds(points) + cbind(x_displacement, y_displacement)

# Create a new displaced point layer
displaced_points <- vect(new_coords, crs = crs(points), type = "points")

# Copy attributes from original points to the displaced points
values(displaced_points) <- values(points)

# Save the displaced points to a new shapefile
writeVector(displaced_points, paste0(layers,"/displaced_test1.gpkg"), overwrite = TRUE)

# Inspect the result
plot(points, col = "blue", pch = 20, main = "Original vs Displaced Points")
points(displaced_points, col = "red", pch = 20)
legend("topright", legend = c("Original", "Displaced"), col = c("blue", "red"), pch = 20)



# Lets include the admin unit test --------------
head(points)
# Function to displace a single point and ensure it stays on the original island
# Function to displace a single point
# Function to displace a single point and check if it remains within the original admin unit
displace_point_with_iid_check <- function(point, admin_units, max_distance = 5000) {
  original_iid <- point$iid
  
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
    new_iid <- terra::extract(admin_units, temp_point)[, "iid"]
    
    # Check if new iid matches original
    if (!is.na(new_iid) && new_iid == original_iid) {
      return(temp_point)
    }
  }
}

# Ensure CRS consistency between points and admin_units
points <- project(points, crs(admin_units))

# Initialize an empty list to store displaced points
displaced_points_list <- list()

# Loop through each point
for (i in 1:nrow(points)) {
  # Displace the point
  displaced_points_list[[i]] <- displace_point_with_iid_check(points[i, ], admin_units)
}

# Combine displaced points into a single SpatVector
displaced_points <- do.call(rbind, displaced_points_list)

# Ensure displaced points keep their original attributes
values(displaced_points) <- values(points)

plot(displaced_points)
head(displaced_points)

writeVector(displaced_points, paste0(layers,"/displaced_test1.gpkg"), overwrite = TRUE)



### Include different displacement methods based on Urban / Rural Strata
# Function to displace a single point with conditions based on `UR` value
displace_point_with_ur_check <- function(point, admin_units, max_distances) {
  original_iid <- point$iid
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
    new_iid <- terra::extract(admin_units, temp_point)[, "iid"]
    
    # Check if new iid matches original
    if (!is.na(new_iid) && new_iid == original_iid) {
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
points <- project(points, crs(admin_units))

# Initialize an empty list to store displaced points
displaced_points_list <- list()

# Loop through each point
tic()
for (i in 1:nrow(points)) {
  # Displace the point based on UR and check admin unit
  displaced_points_list[[i]] <- displace_point_with_ur_check(points[i, ], admin_units, max_distances)
}

# Combine displaced points into a single SpatVector
displaced_points <- do.call(rbind, displaced_points_list)

# Ensure displaced points keep their original attributes
values(displaced_points) <- values(points)

toc()

writeVector(displaced_points, paste0(layers,"/displaced_test2.gpkg"), overwrite = TRUE)



#  EVALUATE DEIDENTIFICATION PROCESS -----------------------------------------
#  PREPARE DATA TO BE INCLUDED INTO MICRODATA DATASET ------------------------