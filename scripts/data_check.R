## Date: ~ 2025/09/25
## Author: Monika Kelley 
## Project: G34P (grasses C3 vs. C4, phosphorous manipulation & allocation)
## Purpose: Verify all (or most individuals) have been sampled for ACI/ Rdark, 
## prior to ending the experiment. 


# Issue keyword ----------------------------------------------------------------

## LEMONPIE (run ANOVA to see how much of a difference there is)

# libraries --------------------------------------------------------------------
library(photosynthesis)
library(readr)
library(dplyr)
library(gasanalyzer)
library(ggplot2)
library(units)
library(patchwork)


# data -------------------------------------------------------------------------

## individual plant ids
id_list_plants <- read.csv("../data_meta/plant_ids.csv")

# data li-6800 aci curves ------------------------------------------------------


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


## extracting ID list ---
ids_stan <- unique(data_stan_df$UserDefCon.id)
ids_gibson <- unique(data_gibson_df$UserDefCon.id)
ids_yadi <- unique(data_yadi_df$UserDefCon.id)
ids_albert <- unique(data_albert_df$UserDefCon.id)
ids_ozzie <- unique(data_ozzie_df$UserDefCon.id)


## rough look at data ---
mean(data_yadi_df$GasEx.A)
mean(data_stan_df$GasEx.A)
mean(data_albert_df$GasEx.A)
mean(data_gibson_df$GasEx.A)
mean(data_ozzie_df$GasEx.A)


## fig for ref
fig_yadi <- ggplot(data_yadi_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50)) +
  labs(title = "yadi A")

fig_stan <- ggplot(data_stan_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50)) +
  labs(title = "stan A")

fig_albert <- ggplot(data_albert_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50)) +
  labs(title = "albert A")

fig_gibson <- ggplot(data_gibson_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50)) +
  labs(title = "gibson A")

fig_ozzie <- ggplot(data_ozzie_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1 , 50)) +
  labs(title = "ozzie A")

## look at em kinda next to each other
fig_stan + fig_yadi + fig_albert + fig_stan + fig_gibson
 

## check ids ---
## getting unique ID's per day (log file)
ids_yadi <- data_yadi_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_stan <- data_stan_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_ozzie <- data_ozzie_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_albert <- data_albert_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_gibson <- data_gibson_df %>%
  distinct(SysObs.Filename, UserDefCon.id)


## adding machine names for clarity
ids_yadi$machine <- "yadi"
ids_stan$machine <- "stan"
ids_ozzie$machine <- "ozzie"
ids_albert$machine <- "albert"
ids_gibson$machine <- "gibson"


id_list_licor <- rbind(ids_yadi, ids_stan, ids_ozzie, ids_albert, ids_gibson)

write.csv(id_list_licor, file = "../data/id_list_licor.csv")



## creation of MIA licor data --------------------------------------------------

## updating column names to match
id_list_licor <- id_list_licor %>%
  rename(individual = UserDefCon.id)

## compare data
missing_licor_data <- anti_join(id_list_plants, id_list_licor, 
                                by = "individual")

missing_licor_data

write.csv(missing_licor_data, "../data/missing_licor_data.csv")


## running anova --- ########################################################### LEMONPIE



# data dark respiration curves -------------------------------------------------

## Creating file paths for each machine
albert_dark <- "../data/li_6800/g34p_albert_dark/"
gibson_dark <- "../data/li_6800/g34p_gibson_dark/"
ozzie_dark <- "../data/li_6800/g34p_ozzie_dark/"
stan_dark <- "../data/li_6800/g34p_ozzie_dark/"
yadi_dark <- "../data/li_6800/g34p_yadi_dark/"

# SLA file names
files_sla <- list.files("../data/sla/", full.names = TRUE)
files_sla <- as.data.frame(files_sla)
write.csv(files_sla, "../data/files_sla.csv")

# Albert files
## 1 list out all the files
files_albert_dark <- list.files(albert_dark, full.names = TRUE)

## 2 only pull the non-excel files (txt files are not reading as ".txt")
files_albert_dark <- files_albert_dark[!grepl("\\.xlsx?$", files_albert_dark, 
                                    ignore.case = TRUE)]

## 3 applying the read lifore to the files
data_albert_dark <- lapply(files_albert_dark, read_6800_txt)


## 4 making one big albert files to check ID's
data_albert_dark_df <- bind_rows(data_albert_dark, .id = "file_id")



# Gibson files 
## 1
files_gibson_dark <- list.files(gibson_dark, full.names = TRUE)

## 2
files_gibson_dark <- files_gibson_dark[!grepl("\\.xlsx?$", files_gibson_dark, 
                                    ignore.case = TRUE)]

## 3
data_gibson_dark <- lapply(files_gibson_dark, read_6800_txt)


## 4
data_gibson_dark_df <- bind_rows(data_gibson_dark, .id = "file_id")




# Ozzie files
## 1
files_ozzie_dark <- list.files(ozzie_dark, full.names = TRUE)

## 2
files_ozzie_dark <- files_ozzie_dark[!grepl("\\.xlsx?$", files_ozzie_dark, 
                                  ignore.case = TRUE)]
## 3
data_ozzie_dark <- lapply(files_ozzie_dark, read_6800_txt)

## 4
data_ozzie_dark_df <- bind_rows(data_ozzie_dark, .id = "file_id")



# Yadi files
## 1
files_yadi_dark <- list.files(yadi_dark, full.names = TRUE)

## 2
files_yadi_dark <- files_yadi_dark[!grepl("\\.xlsx?$", files_yadi_dark, 
                                ignore.case = TRUE)]
## 3
data_yadi_dark <- lapply(files_yadi_dark, read_6800_txt)

## 4
data_yadi_dark_df <- bind_rows(data_yadi_dark, .id = "file_id")




# Stan files
## 1
files_stan_dark <- list.files(stan_dark, full.names = TRUE)

## 2
files_stan_dark <- files_stan_dark[!grepl("\\.xlsx?$", files_stan_dark, 
                                ignore.case = TRUE)]
## 3
data_stan_dark <- lapply(files_stan_dark, read_6800_txt)

## 4
data_stan_dark_df <- bind_rows(data_stan_dark, .id = "file_id")



## extracting ID list ---
ids_stan_dark <- unique(data_stan_dark_df$UserDefCon.id)
ids_gibson_dark <- unique(data_gibson_dark_df$UserDefCon.id)
ids_yadi_dark <- unique(data_yadi_dark_df$UserDefCon.id)
ids_albert_dark <- unique(data_albert_dark_df$UserDefCon.id)
ids_ozzie_dark <- unique(data_ozzie_dark_df$UserDefCon.id)



## rough look at data ---
mean(data_yadi_dark_df$GasEx.A)
mean(data_stan_dark_df$GasEx.A)
mean(data_albert_dark_df$GasEx.A)
mean(data_gibson_dark_df$GasEx.A)
mean(data_ozzie_dark_df$GasEx.A)


## fig for ref
fig_yadi_dark <- ggplot(data_yadi_dark_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA,
               fill = "#555559") +
  coord_cartesian(ylim = c(-10, 1)) +
  labs(title = "yadi dark A")

fig_stan_dark <- ggplot(data_stan_dark_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA,
               fill = "#555559") +
  coord_cartesian(ylim = c(-10, 1)) +
  labs(title = "stan dark A")

fig_albert_dark <- ggplot(data_albert_dark_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA,
               fill = "#555559") +
  coord_cartesian(ylim = c(-10, 1)) +
  labs(title = "albert dark A")

fig_gibson_dark <- ggplot(data_gibson_dark_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA,
               fill = "#555559") +
  coord_cartesian(ylim = c(-10, 1)) +
  labs(title = "gibson dark A")

fig_ozzie_dark <- ggplot(data_ozzie_dark_df, aes(SysObs.Filename, GasEx.A)) +
  geom_boxplot(outlier.shape = NA,
               fill = "#555559") +
  coord_cartesian(ylim = c(-10, 1)) +
  labs(title = "ozzie dark A")

## look at em kinda next to each other
fig_stan_dark + fig_yadi_dark + fig_albert_dark + fig_stan_dark + fig_gibson_dark


## check ids ---
## getting unique ID's per day (log file)
ids_yadi_dark <- data_yadi_dark_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_stan_dark <- data_stan_dark_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_ozzie_dark <- data_ozzie_dark_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_albert_dark <- data_albert_dark_df %>%
  distinct(SysObs.Filename, UserDefCon.id)

ids_gibson_dark <- data_gibson_dark_df %>%
  distinct(SysObs.Filename, UserDefCon.id)


## adding machine names for clarity
ids_yadi_dark$machine <- "yadi"
ids_stan_dark$machine <- "stan"
ids_ozzie_dark$machine <- "ozzie"
ids_albert_dark$machine <- "albert"
ids_gibson_dark$machine <- "gibson"


id_list_licor_dark <- rbind(ids_yadi_dark, 
                            ids_stan_dark, 
                            ids_ozzie_dark, 
                            ids_albert_dark, 
                            ids_gibson_dark)

write.csv(id_list_licor_dark, file = "../data/id_list_licor_dark.csv")


## creation of MIA licor data --------------------------------------------------

## updating column names to match
id_list_licor_dark <- id_list_licor_dark %>%
  rename(individual = UserDefCon.id)

## compare data
missing_licor_data_dark <- anti_join(id_list_plants, id_list_licor_dark, 
                                by = "individual")

missing_licor_data_dark

write.csv(missing_licor_data_dark, "../data/missing_licor_data_dark.csv")
