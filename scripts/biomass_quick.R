
###############################################################################X
# libraries --------------------------------------------------------------------
###############################################################################X

library(tidyr)
library(dplyr)
library(ggplot2)


###############################################################################X
# data -------------------------------------------------------------------------
###############################################################################X

meta_data <- read.csv("../data_meta/plant_ids.csv")
df_biomass <- read.csv("../data/biomass/biomass_sort.csv")
df_biomass_sla <- read.csv("../data/biomass/biomass_sla.csv")
df_biomass_roots <- read.csv("../data/biomass/biomass_roots.csv")

df_biomass_v2 <- read.csv("../data/biomass/biomass_v2.csv")



## data cleaning ---------------------------------------------------------------

# df_biomass <- left_join(df_biomass, meta_data)

df_biomass_qc <- full_join(df_biomass_v2, df_biomass_sla, 
                           by = c("individual", "biomass_shoot"))
# 
# df_biomass_qc <- left_join(df_biomass_qc, df_biomass_roots, 
#                            by = c("individual", "biomass_roots"))




write.csv(df_biomass_qc, "../data/r_data/df_biomass_qc.csv")






df_biomass_qc$biomass_roots
df_biomass_roots$biomass_roots





df_biomass$rootshoot <- df_biomass$biomass_belowground_g/ df_biomass$biomass_aboveground_g










###############################################################################X
# figures ----------------------------------------------------------------------
###############################################################################X


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
