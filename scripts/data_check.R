

# libraries
library(photosynthesis)
library(readr)
library(dplyr)
library(gasanalyzer)
library(ggplot2)
library(units)


# read in data -----------------------------------------------------------------


## using "read_licor", doesn't work on a few of the machines -----------------------------------------------------------------------

## read individual files
albert_9.20 <- read_6800_txt("../data/li_6800/g34p_albert/2025-09-20-0934_logdata_albert")
gibson_9.20 <- read.delim("../data/li_6800/g34p_gibson/2025-09-20-1105_logdata", skip = 63, header = TRUE)



## Creating file paths for each machine
albert <- "../data/li_6800/g34p_albert/"
gibson <- "../data/li_6800/g34p_gibson/"
ozzie <- "../data/li_6800/g34p_ozzie/"
stan <- "../data/li_6800/g34p_stan/"
yadi <- "../data/li_6800/g34p_yadi/"


# Albert files
## 1 list out all the files
files_albert <- list.files(albert, full.names = TRUE)

## 2 only pull the non-excel files (txt files are not reading as ".txt")
files_albert <- files_albert[!grepl("\\.xlsx?$", files_albert, 
                                    ignore.case = TRUE)]

## 3 applying the read lifore to the files
data_albert <- lapply(files_albert, read_6800_txt)


## 4 making one big albert files to check ID's
data_albert_df <- bind_rows(data_albert, .id = "file_id")



# Gibson files 
## 1
files_gibson <- list.files(gibson, full.names = TRUE)

## 2
files_gibson <- files_gibson[!grepl("\\.xlsx?$", files_gibson, 
                                    ignore.case = TRUE)]

## 3
data_gibson <- lapply(files_gibson, read_6800_txt)


## 4
data_gibson_df <- bind_rows(data_gibson, .id = "file_id")




# Ozzie files
## 1
files_ozzie <- list.files(ozzie, full.names = TRUE)

## 2
files_ozzie <- files_ozzie[!grepl("\\.xlsx?$", files_ozzie, 
                                    ignore.case = TRUE)]
## 3
data_ozzie <- lapply(files_ozzie, read_6800_txt)

## 4
data_ozzie_df <- bind_rows(data_ozzie, .id = "file_id")



# Yadi files
## 1
files_yadi <- list.files(yadi, full.names = TRUE)

## 2
files_yadi <- files_yadi[!grepl("\\.xlsx?$", files_yadi, 
                                  ignore.case = TRUE)]
## 3
data_yadi <- lapply(files_yadi, read_6800_txt)

## 4
data_yadi_df <- bind_rows(data_yadi, .id = "file_id")




# Stan files
## 1
files_stan <- list.files(stan, full.names = TRUE)

## 2
files_stan <- files_stan[!grepl("\\.xlsx?$", files_stan, 
                                ignore.case = TRUE)]
## 3
data_stan <- lapply(files_stan, read_6800_txt)

## 4
data_stan_df <- bind_rows(data_stan, .id = "file_id")



##### just want a list of the ID values

ids_stan <- unique(data_stan_df$UserDefCon.id)
ids_gibson <- unique(data_gibson_df$UserDefCon.id)
ids_yadi <- unique(data_yadi_df$UserDefCon.id)
ids_albert <- unique(data_albert_df$UserDefCon.id)
ids_ozzie <- unique(data_ozzie_df$UserDefCon.id)


ids_09.28 <- combine(ids_albert, ids_gibson, ids_yadi, ids_albert, ids_yadi)



## quick look at values, cause yadi is concering

ggplot(data_yadi_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50))

ggplot(data_stan_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50))

ggplot(data_albert_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50))

ggplot(data_gibson_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50))
