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

## li600 data 
data_li600_v1 <- read.csv("../data/li_600/g34p_light_v2/2025-10-05/g34p_light_v2_mrk_2025_10_07_15_21_46_1.csv")

## plant and block information
meta_plant <- read.csv("../data_meta/plant_ids.csv")

# Phenology --------------------------------------------------------------------
## cleaning --------------------------------------------------------------------

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


## removing NA value form id, removes 1 row, which was just a note. 
data_pheno_full <- data_pheno_full %>%
  drop_na('id')

## updating column names 
data_pheno_full <- data_pheno_full %>%
  rename(height = X9_height_cm,
         date = X1_date)

## removing extra columns
data_pheno_full <- data_pheno_full %>% 
  select(id, date, height)

data_pheno_last <- data_pheno_last %>%
  select(date, id, height)
  
data_pheno_mid <- data_pheno_mid %>%
  select(date, id, height)

## convert dates into dates
data_pheno_full <- data_pheno_full %>%
  mutate(date = as.Date(date, 
                        format = "%d/%m/%Y"))

data_pheno_last <- data_pheno_last %>%
  mutate(date = as.Date(date, 
                        format = "%d/%m/%Y"))

data_pheno_mid <- data_pheno_mid %>%
  mutate(date = as.Date(date, 
                        format = "%d/%m/%Y"))


## merging data sets
data_pheno_heights <- bind_rows(data_pheno_full, data_pheno_last, 
                                data_pheno_mid)


## add in metadata
data_pheno_heights <- left_join(data_pheno_heights, meta_plant,
                             by = c('id' = 'individual'))


## adding week #
data_pheno_heights$week_number <- strftime(data_pheno_heights$date, 
                                        format ='%V')


## renaming some of the silly column names I used 
data_pheno_heights <- data_pheno_heights %>%
  rename(trt_p = phosphorous_p1.p4,
         trt_light = shade_00s_70s,
         photopath = c3_c4)

## prepping data for plotting
data_pheno_heights$week_number <- as.factor(data_pheno_heights$week_number)
data_pheno_heights$phosphorous_p1.p4 <- as.factor(data_pheno_heights$trt_p)
data_pheno_heights$shade_00s_70s <- as.factor(data_pheno_heights$trt_light)



## figures ---------------------------------------------------------------------

# figures data prep ----------------------------------------------------------##
## prepping data, trying to isolate just weeks with biggest observations
filtered_data <- data_pheno_heights %>%
  group_by(week_number) %>%
  filter(n_distinct(id) > 190)

## checking to see unique numbers and names
test_v2 <- filtered_data %>% 
  group_by(week_number) %>%
  summarise(unique_ids = n_distinct(id))

## removing the weird weeks 
filtered_data <- filtered_data %>% 
  filter(date != "2025-08-25")

## removing not p1 or p4
filtered_data_p1_p4 <- filtered_data %>% 
  filter(trt_p %in% c("p1", "p4"))


filtered_data <- data %>%
  filter(column_name %in% c("value1", "value2"))

### height ---------------------------------------------------------------------

data_pheno_heights

## c3 vs. c4 plant height
ggplot(data_pheno_heights_weeks_filtered, 
       aes(x = as.factor(week), 
           y = X8_height_cm, 
           fill = as.factor(phosphorous_p1.p4))) +
  geom_boxplot() +
  facet_wrap( ~ c3_c4)



## species plant height
ggplot(data_pheno_heights_weeks_filtered, 
       aes(x = as.factor(week), 
           y = X7_leaf_count, 
           fill = as.factor(phosphorous_p1.p4))) +
  geom_boxplot() +
  facet_wrap( ~ c3_c4 + usda_code)


## height x weeks: species (v1, straight line)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot () +
  facet_wrap(~species_code)

## height x weeks: species 
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = FALSE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: species (v2, curved lines)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "loess", color = "black", se = TRUE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: species (v3, different shades)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(color = trt_light, group = trt_light),
              method = "lm", se = FALSE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: species (v4, different shades, cute curves)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(color = trt_light, group = trt_light),
              method = "loess", se = FALSE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)



## height x weeks: species (v4, different shades, cute curves) ----------------# fig.3
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(color = trt_light, group = trt_light),
              method = "loess", se = FALSE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)




## height x weeks: species (v4, different shades, cute curves) ----------------# fig.4
ggplot(filtered_data_p1_p4, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(color = trt_light, group = trt_light),
              method = "loess", se = FALSE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)




## height x weeks: species (v5)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(color = trt_light, group = trt_light),
              method = "loess", se = FALSE) +
  facet_wrap(~ species_code + trt_p) +
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: shade treatments (v1)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot() +
  facet_wrap(~ trt_light) + 
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: shade treatments (v2)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(aes(color = trt_light), width = 0.2, alpha = 0.5) +
  geom_smooth(aes(color = trt_light, group = trt_light),
              method = "loess", se = FALSE) +  
  facet_wrap(~ trt_light + trt_p) + 
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: shade treatments (v4) -------------------------------------## fig.1) shade x trt_p (w/ trends)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_smooth(aes(color = trt_p, group = trt_p),
              method = "loess", se = FALSE) +
  facet_grid(~ trt_light) +
  scale_fill_grey(start = 0.9, end = 0.3)


## height x weeks: shade treatments (v4) -------------------------------------## fig.2) shade x trt p x species (w/ trends)
ggplot(filtered_data, aes(x = week_number, y = height, fill = trt_p)) +
  geom_boxplot(alpha = 0.8) +
  geom_smooth(aes(color = trt_p, group = trt_p),
              method = "loess", se = FALSE) +
  facet_wrap(~ species_code) +
  scale_fill_grey(start = 0.9, end = 0.3)




# write.csv(data_pheno_heights, "../data/r_data/pheight.csv")



# li600 ------------------------------------------------------------------------