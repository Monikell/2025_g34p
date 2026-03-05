
# Note: all paths assume that the folder containing this R script
# is the working root directory
#
# Note: using PhotoGea to assit with cleaning
# https://eloch216.github.io/PhotoGEA/index.html

# Issues -----------------------------------------------------------------------


###############################################################################X
# Directory -----
###############################################################################X
setwd("~/git/2025_g34p/scripts")


###############################################################################X
# Libraries ------
###############################################################################X
# general
library(stringr)
library(tidyverse)
library(readr)
library(lubridate)

# data processing
library(photosynthesis)
library(plantecophys)
library(PhotoGEA)
library(lattice)

###############################################################################X
## meta data
###############################################################################X

data_meta <- read.csv("../data_meta/plant_ids.csv")
data_meta_photo <- data_meta %>%
  select(individual, c3_c4, phosphorous_p1.p4)
data_meta_photo <- rename(data_meta_photo, id = individual)



###############################################################################X
## cleaning ------
## Merge, clean, and update LI-6800 files into a single DF
###############################################################################X

# load the data
folder_path_li6800 <- "../data/li_6800/day/"

li6800_files <- list.files(
  path = folder_path_li6800,
  full.names = TRUE,
  recursive = TRUE)

# # Load each file, storing the result in a list; added the source file name.
# gasex_exdf_list <- lapply(li6800_files, function(fpath) {
#   df <- read_gasex_file(fpath, "time")
#   df$source_file <- basename(li6800_files)
#   df
# })

gasex_exdf_list <- lapply(li6800_files, function(fpath) {
  read_gasex_file(fpath, 'time')
})


# Get the names of all columns that are present in all of the data files
columns_to_keep <- do.call(identify_common_columns, gasex_exdf_list)

# Extract just these columns
gasex_exdf_list <- lapply(gasex_exdf_list, function(x) {
  x[ , columns_to_keep, TRUE]
})

# Use `rbind` to combine all the data
gasex_data <- do.call(rbind, gasex_exdf_list)

# add in photosynthetic pathway info
gasex_data$main_data <- gasex_data$main_data %>%
  left_join(data_meta_photo, by = "id")


# Make sure the data meets basic requirements
check_response_curve_data(gasex_data, 'id', 96, 'CO2_r_sp')



# Add a new column called `curve_npts` that stores the number of points in each
# response curve
gasex_data <- do.call(rbind, by(gasex_data, gasex_data[, 'id'], function(x) {
  x[, 'curve_npts'] <- nrow(x)
  x
}))


# Choose points to remove, depending on how many points are in the curve
gasex_data$main_data <- gasex_data$main_data %>%
  filter(curve_npts == 96)
