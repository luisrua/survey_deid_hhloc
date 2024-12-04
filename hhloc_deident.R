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

# Capture vid and EA codes

hhloc_sjoin <- hhloc %>%
  mutate(
    vid = terra::extract(village, hhloc)[, "vid"], # Replace "vid" with the column name in `village`
    iid = terra::extract(island,hhloc)[,"iid"]
  )

# For this case we assign Urban Rural Strata from 
hhloc_init <- hhloc_sjoin %>% 
  mutate( ur = ifelse(vid %in% c(7,22), "U","R"))

# Random select 1% of the Rural points and rename them as R1


head(hhloc_sjoin)
#  HH LOCATIONS DEIDENTIFICATION PROCESS -------------------------------------
#  EVALUATE DEIDENTIFICATION PROCESS -----------------------------------------
#  PREPARE DATA TO BE INCLUDED INTO MICRODATA DATASET ------------------------