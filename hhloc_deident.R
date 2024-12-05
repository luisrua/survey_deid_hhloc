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

# 2. IMPORT ADMINISTRATIVE BOUNDARY LAYERS AND PROCESS -------------------------
village <- vect(paste0(layers,"kir_vid_mics24_4326.gpkg"))
island <- vect(paste0(layers,"kir_iid_mics24_4326.gpkg"))

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

#  HH LOCATIONS DEIDENTIFICATION PROCESS -------------------------------------


#  EVALUATE DEIDENTIFICATION PROCESS -----------------------------------------
#  PREPARE DATA TO BE INCLUDED INTO MICRODATA DATASET ------------------------