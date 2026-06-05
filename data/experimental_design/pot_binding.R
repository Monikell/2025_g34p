## project: G34P
## date: June 4 2026 
## Author: Monika Kelley
## Purpose: assess root binding in pots

## Notes: 
### pot volume size oer pot = 1.589 L

# working directory ------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readxl)

# data -------------------------------------------------------------------------
shoots <- read_excel("../biomass/biomass_shoots.xlsx")
roots <- read_excel("../biomass/biomass_roots_v2.xlsx")

# data manipulation ------------------------------------------------------------

shoots$type <- NULL
roots$type <- NULL
biomass_total <- full_join(shoots, roots, by = "individual")

## making 1 note column 
df$new_column <- paste(df$column1, df$column2, sep = " ")
biomass_total$notes <- paste(biomass_total$notes.x, 
                             biomass_total$notes.y, 
                             sep = "; ")

