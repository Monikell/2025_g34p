# monika_analyses.R. Script merges biomass datasheet with
# metadata file, then calculates some crude biomass partitioning
# results. Then, script analyzes data to determine main belowground
# allocation responses to treatment combinations


###############################################################################x
# directory ---------
###############################################################################x
setwd("~/git/2025_g34p/scripts")


###############################################################################x
# Libraries and data cleaning ---------
###############################################################################x

# Libraries
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(multcomp)
library(readxl)

###############################################################################x
# data ---------
###############################################################################x
# Read biomass and meta-data files
meta_data <- read.csv("../data_meta/plant_ids.csv") %>%
  mutate(id = as.character(individual))

# read in root data
biomass_data_roots <- read_excel("../data/biomass/biomass_roots_v2.xlsx")
biomass_data_shoots <- read_excel("../data/biomass/biomass_shoots.xlsx")
biomass_data_shoots$og_order <- NULL
biomass_data_shoots$type <- NULL
biomass_data_roots$type <- NULL

biomass_data <- left_join(biomass_data_roots, biomass_data_shoots)


# Merge meta-data with biomass data to append treatments and species
compiled_df <- biomass_data %>%
  left_join(meta_data, by = "individual") %>%
  mutate(shade = factor(ifelse(shade_00s_70s == "00s", 0, 70), 
                        levels = c(0, 70)),
         p.trt = ifelse(phosphorous_p1.p4 == "p1", 0,
                        ifelse(phosphorous_p1.p4 == "p2", 15,
                               ifelse(phosphorous_p1.p4 == "p3", 31,
                                      ifelse(phosphorous_p1.p4 == 
                                               "p4", 62, NA))))) %>%
  dplyr::select(individual, shade, p.trt, usda_code, c3_c4, block, rep,
                biomass_shoots_g, biomass_roots_g)

compiled_df$p.trt <- factor(compiled_df$p.trt, 
                             levels = c(0, 15, 31, 62))








###############################################################################x
# Above-ground biomass model ---------
###############################################################################x

hist(compiled_df$biomass_shoots_g) 
hist(log(compiled_df$biomass_shoots_g)) # for hard skewed right data
hist(sqrt(compiled_df$biomass_shoots_g)) # for moderate right skewed data


agb_model <- lmer(sqrt(biomass_shoots_g) ~ shade * p.trt * c3_c4 + 
                    (1 | usda_code) + (1 | block), data = compiled_df)

# Check model assumptions
plot(agb_model)
qqnorm(residuals(agb_model))
qqline(residuals(agb_model))
densityPlot(residuals(agb_model))
shapiro.test(residuals(agb_model))
outlierTest(agb_model)

# Model output
summary(agb_model)
Anova(agb_model) #shade, p.trt ***
# r.squaredGLMM(agb_model)

# Pairwise comparisons
emmeans(agb_model, pairwise~shade) # shade <0.0001
emmeans(agb_model, pairwise~p.trt) #
emmip(agb_model, ~ p.trt, CIs = TRUE)
emmip(agb_model, ~ shade, CIs = TRUE)

cld(emmeans(agb_model, pairwise~shade))
cld(emmeans(agb_model, pairwise~p.trt))

cld(emmeans(agb_model, pairwise ~ shade*c3_c4))


ggplot(compiled_df, aes(x = shade, y = biomass_shoots_g)) +
  facet_grid( ~c3_c4) +
  geom_boxplot() 
# biomass increase with more sun


ggplot(compiled_df, aes(x = p.trt, y = biomass_shoots_g,
                        fill = c3_c4)) +
  facet_grid( ~shade) +
  geom_boxplot() 

ggplot(compiled_df, aes(x = p.trt, y = biomass_shoots_g,
                        fill = p.trt)) + 
  geom_boxplot()
# p increase biomass 

ggplot(compiled_df, aes(x = p.trt, y = biomass_shoots_g,
                        fill = p.trt)) + 
  geom_boxplot()


ggplot(compiled_df, aes(x = c3_c4, y = biomass_shoots_g, 
                        fill = c3_c4)) +
  facet_grid(~p.trt) +
  geom_boxplot()

###############################################################################x
# Below-ground biomass model ---------
###############################################################################x

hist(biomass_data$biomass_roots_g)
hist(log(biomass_data$biomass_roots_g))
hist(sqrt(biomass_data$biomass_roots_g))

sort(biomass_data$biomass_roots_g)

bgb_model <- lmer(sqrt(biomass_roots_g) ~ shade * p.trt * c3_c4 + 
                    (1 | usda_code) + (1 | block), data = compiled_df)



# Check model assumptions
plot(bgb_model)
qqnorm(residuals(bgb_model))
qqline(residuals(bgb_model))
densityPlot(residuals(bgb_model))
shapiro.test(residuals(bgb_model))
outlierTest(bgb_model)

# Model output
summary(bgb_model)
Anova(bgb_model) # shade, p.trt ***

# Pairwise comparisons
emmeans(bgb_model, pairwise~shade)
# Pairwise comparisons
emmeans(bgb_model, pairwise~shade) # shade <0.0001
emmeans(bgb_model, pairwise~p.trt) # 0 x 15
emmip(bgb_model, ~ p.trt, CIs = TRUE)
emmip(bgb_model, ~ shade, CIs = TRUE)


ggplot(compiled_df, aes(x = shade, y = biomass_roots_g)) +
  geom_boxplot()

ggplot(compiled_df, aes(x = p.trt, y = biomass_roots_g,
                        fill = p.trt)) + 
  geom_boxplot()





compiled_df$biomass_roots_g


ggplot(compiled_df, aes(x = p.trt, y = biomass_roots_g,
                        fill = c3_c4)) +
  facet_grid( ~shade) +
  geom_boxplot() 





## Greater below-ground biomass in full sun treatment

test(emtrends(bgb_model, ~1, "p.trt"))
## Belowground biomass increases with increasing P fertilization

test(emtrends(bgb_model, pairwise~shade, "p.trt"))
## P fertilization effect is only observed under 0% shade cover

cld(emmeans(bgb_model, pairwise~shade*c3_c4, type = "response"))
## Stronger below-ground biomass response to shade in C4 species compared
## to C3 species


# Shade cover treatment
shade_plot <- ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_roots_g > 0), 
       aes(x = shade, y = log(biomass_roots_g), fill = shade)) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#FFDD44", "#117733"),
                    labels = c(expression("C"["3"]),
                               expression("C"["4"]))) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "Light increases root biomass",
       x = "Shade cover (%)",
       y = "ln Root biomass (unitless)") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))


# P fertilization
pfert_plot <- ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_roots_g > 0), 
       aes(x = as.factor(p.trt), y = log(biomass_roots_g), fill = as.factor(p.trt))) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#CEFFFF", "#D5C304", "#F94902", "#A80003")) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "P does not change root biomass",
       x = "P fertilization (ppm)",
       y = "ln Root biomass (unitless)") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))

# Photosynthetic pathway
photo_plot <- ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_roots_g > 0), 
                     aes(x = c3_c4, y = log(biomass_roots_g), fill = c3_c4)) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#117733", "#FFDD44")) +
  scale_x_discrete(labels = c(expression("C"["3"]),
                              expression("C"["4"]))) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "Photo. pathway does not alter root biomass",
       x = "Photosynthetic pathway",
       y = "ln Root biomass (unitless)") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))

# png("../Zaara_plots.png", width = 20, height = 5, units = "in", res = 600)
# ggarrange(shade_plot, pfert_plot, photo_plot,
#           nrow = 1, ncol = 3)
# dev.off()
# 



###############################################################################x
# Root:shoot model -------------
###############################################################################x

## adding root: shoot
compiled_df$root_shoot <- compiled_df$biomass_roots_g / 
  compiled_df$biomass_shoots_g

compiled_df_cleaned <- na.omit(compiled_df)

## checking data
hist(compiled_df$root_shoot)
hist(log(compiled_df$root_shoot))
hist(sqrt(compiled_df$root_shoot))


rootshoot_model <- lmer(sqrt(root_shoot) ~ shade * p.trt * c3_c4 + 
                          (1 | usda_code) + (1 | block), 
                        data = compiled_df_cleaned)

# Check model assumptions
plot(rootshoot_model)
qqnorm(residuals(rootshoot_model))
qqline(residuals(rootshoot_model))
densityPlot(residuals(rootshoot_model))
shapiro.test(residuals(rootshoot_model))
outlierTest(rootshoot_model)

# Model output
summary(rootshoot_model)
Anova(rootshoot_model) # shade and p.trt
r.squaredGLMM(rootshoot_model)

# Pairwise comparisons
cld(emmeans(rootshoot_model, pairwise ~ shade)) # bigger in sun
cld(emmeans(rootshoot_model, pairwise ~ p.trt)) # 0 diff from all
## Root:shoot is greater at 0% shade cover

cld(emmeans(rootshoot_model, pairwise~shade*c3_c4, type = "response"))
# C3 and C4 responded differently

# test(emtrends(rootshoot_model, ~shade*c3_c4, "p.trt"))

## P fertilization effect is only observed under 0% shade cover and
## is entirely driven by C4 species



ggplot(compiled_df, aes(x = shade, y = root_shoot)) +
  geom_boxplot() +
  facet_grid(~ c3_c4)# more shoot in full-sun

ggplot(compiled_df, aes(x = p.trt, y = root_shoot,
                        fill = p.trt)) + 
  geom_boxplot() +
  facet_grid( ~c3_c4)

# p treatment and shade treatment. global root:shoot ratio


###############################################################################x
# figures ----------
###############################################################################x

compiled_df_cleaned$root_shoot

## ROOT:SHOOT ------------------------------------

# Shade cover treatment (ROOT:SHOOT)
ggplot(data = subset(compiled_df_cleaned, !is.na(c3_c4) & root_shoot > 0), 
                     aes(x = shade, y = log(root_shoot), fill = shade)) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#FFDD44", "#636363"),
                    labels = c(expression("C"["3"]),
                               expression("C"["4"]))) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "Light increases root:shoot (more roots than shoots)",
       x = "Shade cover (%)",
       y = "ROOT:SHOOT") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))


# P fertilization
ggplot(data = subset(compiled_df_cleaned, !is.na(c3_c4) & root_shoot > 0), 
                     aes(x = as.factor(p.trt), y = log(root_shoot), fill = as.factor(p.trt))) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#CEFFFF", "#D5C304", "#F94902", "#A80003")) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "P does not change root:shoot",
       x = "P fertilization (ppm)",
       y = "ROOT:SHOOT") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))

# Photosynthetic pathway
ggplot(data = subset(compiled_df_cleaned, !is.na(c3_c4) & root_shoot > 0), 
                     aes(x = c3_c4, y = log(root_shoot), fill = c3_c4)) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#117733", "#FFDD44")) +
  scale_x_discrete(labels = c(expression("C"["3"]),
                              expression("C"["4"]))) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "Photo. pathway does not alter root biomass",
       x = "Photosynthetic pathway",
       y = "ROOT:SHOOT") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))



## SHOOT ------------------------------------

compiled_df$biomass_shoots_g

# Shade cover treatment (ROOT:SHOOT)
ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_shoots_g > 0), 
       aes(x = shade, y = log(biomass_shoots_g), fill = shade)) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#FFDD44", "#117733"),
                    labels = c(expression("C"["3"]),
                               expression("C"["4"]))) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "Light increases shoots",
       x = "Shade cover (%)",
       y = "SHOOTS") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))


# P fertilization
ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_shoots_g > 0), 
       aes(x = as.factor(p.trt), y = log(biomass_shoots_g), fill = as.factor(p.trt))) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#CEFFFF", "#D5C304", "#F94902", "#A80003")) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "P does not change shoots",
       x = "P fertilization (ppm)",
       y = "SHOOT") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))

# Photosynthetic pathway
ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_shoots_g > 0), 
       aes(x = c3_c4, y = log(biomass_shoots_g), fill = c3_c4)) +
  geom_boxplot(outliers = FALSE) +
  #(data = photo_shade, aes(label = .group, y = 18),
  #          position = position_dodge(0.75), size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#117733", "#FFDD44")) +
  scale_x_discrete(labels = c(expression("C"["3"]),
                              expression("C"["4"]))) +
  scale_y_continuous(limits = c(-6, 3), breaks = seq(-6, 3, 3)) +
  labs(title = "Photo. pathway does not alter SHOOT BIOMASS",
       x = "Photosynthetic pathway",
       y = "ROOT:SHOOT") +
  guides(fill = "none") +
  theme_classic(base_size = 20) +
  theme(title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.title = element_text(face = "bold", size = 20),
        axis.text = element_text(size = 20))



ggplot(compiled_df, aes(x = shade, y = biomass_shoots_g, fill = shade)) +
  geom_boxplot()
