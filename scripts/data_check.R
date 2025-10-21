## Date: ~ 2025/09/25
## Author: Monika Kelley 
## Project: G34P (grasses C3 vs. C4, phosphorous manipulation & allocation)
## Purpose: Verify what individuals have been samples for all the things! 


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
library(readxl)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)

# data -------------------------------------------------------------------------

## individual plant ids
id_list_plants <- read.csv("../data_meta/plant_ids.csv")

# data: gas exchange ----------------------------------------------------------- 

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

## mrk notes made
# write.csv(id_list_licor, file = "../data/id_list_licor.csv")

head(id_list_licor)

## MIA: gas exchange -----------------------------------------------------------

## updating column names to match
id_list_licor <- id_list_licor %>%
  rename(individual = UserDefCon.id)

## compare data
missing_licor_data <- anti_join(id_list_plants, id_list_licor, 
                                by = "individual")


## mrk notes made
# write.csv(missing_licor_data, "../data/missing_licor_data.csv")


## running anova --- ########################################################### LEMONPIE



# data: dark-resp --------------------------------------------------------------

## Creating file paths for each machine
albert_dark <- "../data/li_6800/g34p_albert_dark/"
gibson_dark <- "../data/li_6800/g34p_gibson_dark/"
ozzie_dark <- "../data/li_6800/g34p_ozzie_dark/"
stan_dark <- "../data/li_6800/g34p_ozzie_dark/"
yadi_dark <- "../data/li_6800/g34p_yadi_dark/"


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


head(data_albert_dark_df)


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


## mrk notes made
# write.csv(id_list_licor_dark, file = "../data/id_list_licor_dark.csv")


## MIA: dark-resp --------------------------------------------------------------

## updating column names to match
id_list_licor_dark <- id_list_licor_dark %>%
  rename(individual = UserDefCon.id)

## compare data
missing_licor_data_dark <- anti_join(id_list_plants, id_list_licor_dark, 
                                by = "individual")

missing_licor_data_dark


## mrk notes made
write.csv(missing_licor_data_dark, "../data/mia_data/mia_rdark_data.csv")

## other MIA methods -----------------------------------------------------------

### dates ----------------------------------------------------------------------
## lump by dates
id_list_licor_summary <- id_list_licor %>%
  mutate(date = str_extract(SysObs.Filename, "\\d{4}-\\d{2}-\\d{2}")) %>%
  group_by(date) %>%
  summarise(individuals = list(unique(individual))) %>%
  ungroup()


ids_day_li6800 <- id_list_licor_summary %>% tidyr::unnest(individuals)



id_list_licor_dark_summary <- id_list_licor_dark %>%
  mutate(date = str_extract(SysObs.Filename, "\\d{4}-\\d{2}-\\d{2}")) %>%
  group_by(date) %>%
  summarise(individuals = list(unique(individual))) %>%
  ungroup()


ids_night_li6800 <- id_list_licor_dark_summary %>% tidyr::unnest(individuals)


head(ids_night_li6800)
head(ids_day_li6800)


## trying to see about individual that don't overlap dates or something
mia_dark_date <- anti_join(ids_night_li6800, ids_day_li6800, 
                                     by = "individuals", "date")


# write.csv(ids_night_li6800, "../data/mia_data/dark_mia_investigation/li6800_dark_ids.csv")
# write.csv(ids_day_li6800, "../data/mia_data/dark_mia_investigation/li6800_light_ids.csv")


### times ----------------------------------------------------------------------

## stan
curve_times_stan_dark <- data_stan_dark_df %>%
  group_by(UserDefCon.id, SysObs.Filename) %>%
  summarise(
    start_time = min(SysObs.Time),
    end_time   = max(SysObs.Time),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    .groups = "drop"
  )


## albert
curve_times_albert_dark <- data_albert_dark_df %>%
  group_by(UserDefCon.id, SysObs.Filename) %>%
  summarise(
    start_time = min(SysObs.Time),
    end_time   = max(SysObs.Time),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    .groups = "drop"
  )


## yadi
curve_times_yadi_dark <- data_yadi_dark_df %>%
  group_by(UserDefCon.id, SysObs.Filename) %>%
  summarise(
    start_time = min(SysObs.Time),
    end_time   = max(SysObs.Time),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    .groups = "drop"
  )

## ozzie
curve_times_ozzie_dark <- data_ozzie_dark_df %>%
  group_by(UserDefCon.id, SysObs.Filename) %>%  
  summarise(
    start_time = min(SysObs.Time),
    end_time   = max(SysObs.Time),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    .groups = "drop"
  )

## gibson
curve_times_gibson_dark <- data_gibson_dark_df %>%
  group_by(UserDefCon.id, SysObs.Filename) %>%
  summarise(
    start_time = min(SysObs.Time),
    end_time   = max(SysObs.Time),
    duration_s = as.numeric(difftime(end_time, start_time, units = "secs")),
    .groups = "drop"
  )




## adding machine names for clarity
curve_times_yadi_dark$machine <- "yadi"
curve_times_stan_dark$machine <- "stan"
curve_times_ozzie_dark$machine <- "ozzie"
curve_times_albert_dark$machine <- "albert"
curve_times_gibson_dark$machine <- "gibson"


curve_times_dark <- rbind(curve_times_yadi_dark, 
                          curve_times_stan_dark, 
                          curve_times_ozzie_dark, 
                          curve_times_albert_dark, 
                          curve_times_gibson_dark)

# write.csv(curve_times_dark, "../data/mia_data/dark_mia_investigation/curve_times_dark.csv")


# data: light-flo --------------------------------------------------------------

## Creating file paths for light fluoro
files_li600 <- "../data/li_600/export_2025_10_18_142238/g34p_light_v2/"


# 1: list all Excel files in that directory *and all sub-folders*
files_li600_all <- list.files(
  path = files_li600,
  pattern = "\\.xlsx?$",
  full.names = TRUE,          
  recursive = TRUE, # dive into the sub-folders
  ignore.case = TRUE
)

# 2: read & combine
data_li600_df <- map_dfr(
  files_li600_all,
  ~ read_excel(.x),
  .id = "file_id"
)


# 3: pull only the unique barcodes and id's
li600_ids <- data_li600_df %>% 
  distinct(USERDEF...7, USERDEF...8,SYS...2, SYS...3)



 
## MIA: light-flo --------------------------------------------------------------

## updating column names to match
li600_ids <- li600_ids %>%
  rename(individual = USERDEF...7,
         barcode = USERDEF...8, 
         time = SYS...2,
         date = SYS...3)

## removing first 2 odd rows
li600_ids <- li600_ids[-c(1,2), ]

li600_ids$individual <- as.integer(li600_ids$individual)

unique(li600_ids$individual)

## compare data
missing_li600_data <- anti_join(id_list_plants, li600_ids, 
                                by = "individual")

## mrk notes made
# write.csv(missing_li600_data, "../data/missing_li600_data.csv")
write.csv(data_li600_df, "../data/li600_data.csv")



# data: dark-flo ---------------------------------------------------------------

## Creating file paths for light fluoro
files_li600_dark <- "../data/li_600/export_2025_10_18_142238/g34p_dark/"


# 1: list all Excel files in that directory *and all sub-folders*
files_li600_all_dark <- list.files(
  path = files_li600_dark,
  pattern = "\\.xlsx?$",
  full.names = TRUE,          
  recursive = TRUE, # dive into the sub-folders
  ignore.case = TRUE
)

# 2: read & combine
data_li600_dark_df <- map_dfr(
  files_li600_all_dark,
  ~ read_excel(.x),
  .id = "file_id"
)

## barcode/ id issue (didn't sep. until later)
# write.csv(data_li600_dark_df, "../data/mia_data/data_li600_dark.csv")

li600_dark_ids_updated <- read.csv("../data/mia_data/data_li600_dark_v2.csv")

li600_dark_ids_updated$barcode

# 3: pull only the unique barcodes and id's
li600_dark_ids <- li600_dark_ids_updated %>% 
  distinct(individual, barcode)

## MIA: dark-flo ---------------------------------------------------------------


## compare data
mia_li600_dark_data <- anti_join(id_list_plants, li600_dark_ids, 
                                by = "individual")


## reupload try agian



## mrk notes made
write.csv(mia_li600_dark_data, "../data/mia_data/mia_li600_dark_data.csv")
# write.csv(data_li600_df, "../data/li600_data.csv")








## MIA: SLA --------------------------------------------------------------------

## 1: grab the file name
files_sla <- list.files("../data/sla/", pattern = "\\.jpg$", full.names = TRUE)


## 2: remove the file paths
files_sla_ids <- basename(files_sla)
  
## 3: removes the .jpg by only (* = ignores anything after that)
files_sla_ids <- sub("^([0-9]+).*", "\\1", files_sla_ids)

## 4: covert them to numeric
files_sla_ids <- as.numeric(files_sla_ids)

## 5: convert to a dataframe
files_sla_ids <- as.data.frame(files_sla_ids)

files_sla_ids

## 5: updating column names to match 
files_sla_ids <- files_sla_ids %>%
  rename(individual = files_sla_ids)


## who is missing
mia_sla_data <- anti_join(id_list_plants, files_sla_ids, 
          by = "individual")

## mrk notes made
# write.csv(mia_sla_data, "../data/missing_data_check/mia_sla_data.csv")

