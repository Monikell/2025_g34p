

# libraries
install.packages("photosynthesis")
library(photosynthesis)

library(readr)
library(dplyr)


# read in data -----------------------------------------------------------------


## using "read_licor", doesn't work on a few of the machines -----------------------------------------------------------------------

## read individual files
albert_9.20 <- read_licor("../data/li_6800/g34p_albert/2025-09-20-0934_logdata_albert")



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
data_albert <- lapply(files_albert, read_licor)

## 4 making one big albert files to check ID's
data_albert_df <- bind_rows(data_albert, .id = "file_id")



# Gibson files 
## 1
files_gibson <- list.files(gibson, full.names = TRUE)

## 2
files_gibson <- files_gibson[!grepl("\\.xlsx?$", files_gibson, 
                                    ignore.case = TRUE)]
## 3
data_gibson <- lapply(files_gibson, read_licor)

## 4
data_gibson_df <- bind_rows(data_gibson, .id = "file_id")



# Ozzie files
## 1
files_ozzie <- list.files(ozzie, full.names = TRUE)

## 2
files_ozzie <- files_ozzie[!grepl("\\.xlsx?$", files_ozzie, 
                                    ignore.case = TRUE)]
## 3
data_ozzie <- lapply(files_ozzie, read_licor)

## 4
data_ozzie_df <- bind_rows(data_ozzie, .id = "file_id")



# Yadi files
## 1
files_yadi <- list.files(yadi, full.names = TRUE)

## 2
files_yadi <- files_yadi[!grepl("\\.xlsx?$", files_yadi, 
                                  ignore.case = TRUE)]
## 3
data_yadi <- lapply(files_yadi, read_licor)

## 4
data_yadi_df <- bind_rows(data_yadi, .id = "file_id")




# Stan files
## 1
files_stan <- list.files(stan, full.names = TRUE)

## 2
files_stan <- files_stan[!grepl("\\.xlsx?$", files_stan, 
                                ignore.case = TRUE)]
## 3
data_stan <- lapply(files_stan, read_licor)

## 4
data_stan_df <- bind_rows(data_stan, .id = "file_id")


unique(data_albert_df$id)
unique(data_ozzie_df$id)


## using GasanalyzeR

install.packages("gasanalyzer")
library(gasanalyzer)

exampleFiles <- system.file("extdata", package = "gasanalyzer")
import_factory_cals(exampleFiles)

xlsxfile <- read_6800_xlsx(paste(exampleFiles, "lowo2.xlsx", sep = "/"))
txtfile <- read_6800_txt(paste(exampleFiles, "lowo2", sep = "/"))


all.equal(txtfile[!excludeCols], xlsxfile[!excludeCols], tolerance = 1e-6)


# default points:
update_geom_defaults("point", list(shape = 1, size = 3))

# convenience function to change plot labels automatically:
nice_labels <- function(plt) {
  plt$labels <- var2label(plt$labels)
  plt
}

#generate some descriptive stats  and plotmath it with a call to var2label:
descr <- colMeans(xlsxfile[c("LeafQ.Qin", "Meas.Tleaf", "Const.Oxygen")])
dlist <- var2label(rep(names(descr), 2),
                   use_units = rep(c(FALSE, TRUE), each = 3),
                   val = c(rep(NA, 3), as.character(descr)),
                   group = c("", "")) |>
  setNames(letters[1:6]) |> unlist() |>
  substitute(a==d*","~b==e*","~c==f, env = _ ) |> as.expression()

AvsCi1 <- xlsxfile |> ggplot(aes(GasEx.Ci, GasEx.A)) + geom_point() +
  labs(title = dlist)

nice_labels(AvsCi1)