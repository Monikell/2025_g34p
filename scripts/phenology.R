## 2025/09/08
## phenolgy observations of G34P
## Monika Kelley



# libraries --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)

# data -------------------------------------------------------------------------

data_pheno_meta <- read.csv("../data/phenology/form-1__g34p-phenology.csv")
data_pheno <- read.csv("../data/phenology/branch-1__phenology.csv")

# cleaning ---------------------------------------------------------------------

## mergining meta data with data
colnames(data_pheno)
colnames(data_pheno_meta)

data_pheno_full <- left_join(data_pheno, data_pheno_meta, 
                             by = c('ec5_branch_owner_uuid' = 'ec5_uuid'))


## creating single id column 

data <- data_pheno_full %>%
  mutate(
    ID = case_when(
      # If X5_id_scan is missing/blank → take X6
      X5_id_scan == "" ~ as.character(X6_id_number),
      
      # Otherwise extract the last number from X5
      TRUE ~ str_extract(X5_id_scan, "\\d+$")
    ),
    # Convert everything to numeric
    ID = as.numeric(ID)
  )



## plotting --------------------------------------------------------------------


head(data)



