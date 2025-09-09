## 2025/09/08
## phenolgy observations of G34P
## Monika Kelley

# set working directory --------------------------------------------------------
setwd("~/git/2025_g34p/scripts")

# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)

# data -------------------------------------------------------------------------

## phenology data
meta_pheno <- read.csv("../data/phenology/form-1__g34p-phenology.csv")
data_pheno <- read.csv("../data/phenology/branch-1__phenology.csv")

## plant and block information
meta_plant <- read.csv("../data_meta/plant_ids.csv")

# cleaning ---------------------------------------------------------------------

## merging meta data with data
data_pheno_full <- left_join(data_pheno, meta_pheno, 
                             by = c('ec5_branch_owner_uuid' = 'ec5_uuid'))


## creating single id column 
data_pheno_full <- data_pheno_full %>%
  mutate(
    id = case_when(
      X5_id_scan == "" ~ as.character(X6_id_number),
      TRUE ~ str_extract(X5_id_scan, "\\d+$")
    ),
    id = as.numeric(id)
  )


## adding plant and block information
data_pheno_full <- left_join(data_pheno_full, meta_plant,
                             by = c('id' = 'individual'))


## removing NA value form id, removes 1 row, which was just a note. 
data_pheno_full <- data_pheno_full %>%
  drop_na('id')


## adding weeks
data_pheno_full_weeks <- data_pheno_full %>%
  mutate(week = case_when(
    X1_date %in% c("14/08/2025", "16/08/2025") ~ "week 1",
    X1_date %in% c("21/08/2025") ~ "week 2",
    X1_date %in% c("28/08/2025") ~ "week 3",
    TRUE ~ "other"
  ))


# plotting ---------------------------------------------------------------------

## filtering data we want to focus on
data_pheno_full_weeks_filtered <- data_pheno_full_weeks %>%
  filter(week %in% c("week 1", "week 2", "week 3"),
         phosphorous_p1.p4 %in% c("p1", "p4"))


## plotting general leaf counts
ggplot(data_pheno_full_weeks_filtered, 
       aes(x = as.factor(week), 
           y = X7_leaf_count, 
           fill = as.factor(phosphorous_p1.p4))) +
  geom_boxplot() +
  facet_wrap( ~ shade_00s_70s)


## plotting c3 vs. c4 general leaf counts
ggplot(data_pheno_full_weeks_filtered, 
       aes(x = as.factor(week), 
           y = X7_leaf_count, 
           fill = as.factor(phosphorous_p1.p4))) +
  geom_boxplot() +
  facet_wrap( ~ c3_c4)


## c3 vs. c4 plant height
ggplot(data_pheno_full_weeks_filtered, 
       aes(x = as.factor(week), 
           y = X8_height_cm, 
           fill = as.factor(phosphorous_p1.p4))) +
  geom_boxplot() +
  facet_wrap( ~ c3_c4)


## species plant height
ggplot(data_pheno_full_weeks_filtered, 
       aes(x = as.factor(week), 
           y = X7_leaf_count, 
           fill = as.factor(phosphorous_p1.p4))) +
  geom_boxplot() +
  facet_wrap( ~ c3_c4 + usda_code)
