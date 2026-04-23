# monika_analyses.R. Script merges biomass datasheet with
# metadata file, then calculates some crude biomass partitioning
# results. Then, script analyzes data to determine main belowground
# allocation responses to treatment combinations

##############################################################
# Libraries and data cleaning
##############################################################

# Libraries
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(multcomp)

# Read biomass and meta-data files
meta_data <- read.csv("../data/plant_ids.csv") %>%
  mutate(id = as.character(individual))

biomass_data <- read.csv("../data/biomass_roots.csv")
head(biomass_data)
# biomass_data <- read.csv("../data/biomass.csv") %>%
#   filter(use_yn == "y") %>% # filter to only include non-duplicates
#   mutate(root_shoot = ifelse(is.na(biomass_belowground_g) | 
#                                is.na(biomass_aboveground_g),
#                              NA,
#                              biomass_belowground_g / biomass_aboveground_g)) %>%
#   dplyr::select(id, biomass_aboveground_g, biomass_belowground_g,
#                 root_shoot)
  
# Merge meta-data with biomass data to append treatments and species


compiled_df <- biomass_data %>%
  left_join(meta_data, by = "individual") %>%
  mutate(shade = factor(ifelse(shade_00s_70s == "00s", 0, 70), levels = c(0, 70)),
         p.trt = ifelse(phosphorous_p1.p4 == "p1", 0,
                        ifelse(phosphorous_p1.p4 == "p2", 15,
                               ifelse(phosphorous_p1.p4 == "p3", 31,
                                      ifelse(phosphorous_p1.p4 == "p4", 62, NA))))) %>%
  dplyr::select(individual, shade, p.trt, usda_code, c3_c4, block, rep,
                biomass_roots)


##############################################################
# Aboveground biomass model
##############################################################
compiled_df$agb_g[c(12, 120, 307)] <- NA

agb_model <- lmer(sqrt(agb_g) ~ shade * p.trt * c3_c4 + (1 | usda_code) + (1 | block),
                  data = compiled_df)

# Check model assumptions
plot(agb_model)
qqnorm(residuals(agb_model))
qqline(residuals(agb_model))
densityPlot(residuals(agb_model))
shapiro.test(residuals(agb_model))
outlierTest(agb_model)

# Model output
summary(agb_model)
Anova(agb_model)
r.squaredGLMM(agb_model)

# Pairwise comparisons
emmeans(agb_model, pairwise~shade)
## Greater aboveground biomass in full sun treatment

##############################################################
# Belowground biomass model
##############################################################
compiled_df$biomass_roots[c(261, 269, 298, 300, 303, 306, 308, 329, 369)] <- NA

bgb_model <- lmer(log(biomass_roots) ~ shade * p.trt * c3_c4 + (1 | usda_code) + (1 | block),
                  data = subset(compiled_df, biomass_roots > 0))

# Check model assumptions
plot(bgb_model)
qqnorm(residuals(bgb_model))
qqline(residuals(bgb_model))
densityPlot(residuals(bgb_model))
shapiro.test(residuals(bgb_model))
outlierTest(bgb_model)

# Model output
summary(bgb_model)
Anova(bgb_model)

# Pairwise comparisons
emmeans(bgb_model, pairwise~shade)
## Greater belowground biomass in full sun treatment

test(emtrends(bgb_model, ~1, "p.trt"))
## Belowground biomass increases with increasing P fertilization

test(emtrends(bgb_model, pairwise~shade, "p.trt"))
## P fertilization effect is only observed under 0% shade cover

cld(emmeans(bgb_model, pairwise~shade*c3_c4, type = "response"))
## Stronger belowground biomass response to shade in C4 species compared
## to C3 species


# Shade cover treatment
shade_plot <- ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_roots > 0), 
       aes(x = shade, y = log(biomass_roots), fill = shade)) +
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
pfert_plot <- ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_roots > 0), 
       aes(x = as.factor(p.trt), y = log(biomass_roots), fill = as.factor(p.trt))) +
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
photo_plot <- ggplot(data = subset(compiled_df, !is.na(c3_c4) & biomass_roots > 0), 
                     aes(x = c3_c4, y = log(biomass_roots), fill = c3_c4)) +
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

png("../Zaara_plots.png", width = 20, height = 5, units = "in", res = 600)
ggarrange(shade_plot, pfert_plot, photo_plot,
          nrow = 1, ncol = 3)
dev.off()



##############################################################
# Root:shoot model
##############################################################
compiled_df$root_shoot[c(11, 29, 264, 272)] <- NA

rootshoot_model <- lmer(sqrt(root_shoot) ~ shade * p.trt * c3_c4 + (1 | usda_code) + (1 | block),
                  data = compiled_df)

# Check model assumptions
plot(rootshoot_model)
qqnorm(residuals(rootshoot_model))
qqline(residuals(rootshoot_model))
densityPlot(residuals(rootshoot_model))
shapiro.test(residuals(rootshoot_model))
outlierTest(rootshoot_model)

# Model output
summary(rootshoot_model)
Anova(rootshoot_model)
r.squaredGLMM(rootshoot_model)

# Pairwise comparisons
emmeans(rootshoot_model, pairwise~shade)
## Root:shoot is greater at 0% shade cover

test(emtrends(rootshoot_model, ~1, "p.trt"))
## Root:shoot increases with increasing P fertilization

test(emtrends(rootshoot_model, pairwise~shade, "p.trt"))
## P fertilization effect is only observed under 0% shade cover

cld(emmeans(bgb_model, pairwise~shade*c3_c4, type = "response"))
## Stronger root:shoot response to shade in C4 species compared
## to C3 species

test(emtrends(rootshoot_model, ~shade*c3_c4, "p.trt"))
## P fertilization effect is only observed under 0% shade cover and
## is entirely driven by C4 species

