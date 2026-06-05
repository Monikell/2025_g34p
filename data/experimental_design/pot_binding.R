## project: G34P
## date: June 4 2026 
## Author: Monika Kelley
## Purpose: assess root binding in pots

## Notes: 
### pot volume size oer pot = 1.589 L

# working directory ------------------------------------------------------------
setwd("~/git/2025_g34p/data")


# libraries --------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

# data -------------------------------------------------------------------------
shoots <- read_excel("../biomass/biomass_shoots.xlsx")
roots <- read_excel("../biomass/biomass_roots_v2.xlsx")
meta_data <- read.csv("../data_meta/plant_ids.csv")


# data manipulation ------------------------------------------------------------
shoots$type <- NULL
roots$type <- NULL
biomass_total <- full_join(shoots, roots, by = "individual")


# calculations -----------------------------------------------------------------
## total biomass count
biomass_total$biomass_total <- biomass_total$biomass_shoots_g + 
  biomass_total$biomass_roots_g

## total plant biomass to pot volume ratio (goal 1 g L-1; Poorter et al 2012)
biomass_total$biomass_pot_volume_ratio <- biomass_total$biomass_total / 1.589

## add in metadata 
biomass_total <- left_join(biomass_total, meta_data, by = "individual")


# visual -----------------------------------------------------------------------

biomass_total$species_code

ggplot(biomass_total,aes(phosphorous_p1.p4, biomass_pot_volume_ratio)) +
  geom_boxplot()

ggplot(biomass_total, aes(phosphorous_p1.p4, biomass_pot_volume_ratio)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = species_code, shape = species_code), 
              size = 3)

