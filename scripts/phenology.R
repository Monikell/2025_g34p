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
library(readxl)

# data -------------------------------------------------------------------------

## phenology data
meta_pheno <- read.csv("../data/phenology/form-1__g34p-phenology.csv")
data_pheno <- read.csv("../data/phenology/branch-1__phenology.csv")
data_pheno_mid <- read_excel("../data/phenology/plant_height_mid.xlsx")
data_pheno_last <- read_excel("../data/phenology/plant_height_last_date.xlsx")

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
      X6_id_scan == "" ~ as.character(X7_id_number),
      TRUE ~ str_extract(X6_id_scan, "\\d+$")
    ),
    id = as.numeric(id)
  )

## adding plant and block information
data_pheno_full <- left_join(data_pheno_full, meta_plant,
                             by = c('id' = 'individual'))

## removing NA value form id, removes 1 row, which was just a note. 
data_pheno_full <- data_pheno_full %>%
  drop_na('id')

## updating column names 
data_pheno_full <- data_pheno_full %>%
  rename(height = X9_height_cm,
         date = X1_date)


## removing extra columns
data_pheno_last <- data_pheno_last %>%
  select(date, id, height, notes)
  

## merging data sets entered in Excel
data_pheno_mid_last <- bind_rows(data_pheno_mid, data_pheno_last)


## combining the notes column
data_pheno_mid_last <- data_pheno_mid_last %>%
  unite(col = "notes", note, notes, 
        remove = TRUE,
        na.rm = TRUE)

# ## merge excel and epicollect data into one!
# data_pheno_full <- bind_rows(data_pheno_full, data_pheno_mid_last)

## convert dates into dates
data_pheno_full <- data_pheno_full %>%
  mutate(date = as.Date(date, 
                           format = "%d/%m/%Y"))

## convert dates into dates
data_pheno_mid_last <- data_pheno_mid_last %>%
  mutate(date = as.Date(date, 
                           format = "%d/%m/%Y"))

## mergining epicollect and excel data together
data_pheno_full <- cbind(data_pheno_full, data_pheno_mid_last)


## adding week #
data_pheno_full$week_number <- strftime(data_pheno_full$X1_date, 
                                        format ='%V')


# ## adding weeks
# data_pheno_full_weeks <- data_pheno_full %>%
#   mutate(week = case_when(
#     X1_date %in% c("14/08/2025", "16/08/2025") ~ "week 1",
#     X1_date %in% c("21/08/2025") ~ "week 2",
#     X1_date %in% c("28/08/2025") ~ "week 3",
#     TRUE ~ "other"
#   ))


# prepping data for plotting
data_pheno_full$week_number <- as.factor(data_pheno_full$week_number)
data_pheno_full$phosphorous_p1.p4 <- as.factor(data_pheno_full$phosphorous_p1.p4)
data_pheno_full$shade_00s_70s <- as.factor(data_pheno_full$shade_00s_70s)

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




## height how does that look through the weeks (v1)
ggplot(data_pheno_full, aes(x = week_number, 
                            y = X9_height_cm,
                            fill = phosphorous_p1.p4)) +
  geom_boxplot() +
  facet_wrap(~ shade_00s_70s)

