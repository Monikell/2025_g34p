library(tidyr)
library(dplyr)
library(ggplot2)

meta_data <- read.csv("../data_meta/plant_ids.csv")
df_biomass <- read.csv("../data/biomass/biomass_issues.csv")


df_biomass <- left_join(df_biomass, meta_data)

df_biomass$rootshoot <- df_biomass$biomass_belowground_g/ df_biomass$biomass_aboveground_g


ggplot(df_biomass, aes(x = phosphorous_p1.p4, y = biomass_aboveground_g, fill = shade_00s_70s)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ c3_c4 ~ shade_00s_70s) + 
  coord_cartesian(ylim = c(0, 0.2)) +
  theme_minimal()



ggplot(df_biomass, aes(x = phosphorous_p1.p4, y = biomass_belowground_g, fill = shade_00s_70s)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ c3_c4 ~ shade_00s_70s) + 
  theme_minimal()


ggplot(df_biomass, aes(x = phosphorous_p1.p4, y = rootshoot, fill = shade_00s_70s)) +
  geom_boxplot() +
  facet_wrap(~ c3_c4 ~ shade_00s_70s) + 
  theme_minimal()
