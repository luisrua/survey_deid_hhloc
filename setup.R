# This script is for functionality that is very commonly used across
# the actual analysis scripts 

# counter for checking if we have run setup.R before
if(!exists(".setup_counter")){
  .setup_counter <- 0
  quiet <- FALSE
} else {
  quiet <- TRUE
}

# LIBRARIES 
# General tools
library("dplyr")
library("tidyr")
library("tidyverse")
library("readxl")
library("lubridate")
library("janitor")
library("tictoc") # processing time
library("purrr")
library("eeptools")
library("haven") # to read stata datasets

# For spatial analysis
library("terra")
library("sp")
library("sf")
library("tidyterra")
library("conflicted")
library("raster")

# library("spcstyle") # See https://github.com/PacificCommunity/sdd-spcstyle-r-package
# library("scales")
# library("rsdmx")
# library("glue")

# library("ISOcodes")
# library("patchwork")   # layout multiple charts in one image
# library("ggrepel")     # add tet labels to points without overlapping

# CONFLICT WITH FUNCTIONS 
conflict_prefer("select", "dplyr", quiet = quiet)
conflict_prefer("filter", "dplyr", quiet = quiet)
conflict_prefer("year", "lubridate", quiet = quiet)
conflict_prefer("first", "dplyr", quiet = quiet)
conflict_prefer("lag", "dplyr", quiet = quiet)
conflict_prefer("intersect","base", quiet=quiet)

# silence a ubiquitous and annoying message
options(dplyr.summarise.inform = FALSE)
