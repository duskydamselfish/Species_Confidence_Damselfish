##### Color and Damselfish #####

##### Metadata #####
# Trial: chronological number of each trial
# Date: Date of each trial
# Location: location of each trial- "Sailing School" is common name for Ta'ahiamanu beaach
# Time: Time of each trial
# Beaufort: ranked weather conditions of each trial using Beaufort scale
# Rain: marked yes or no regarding presence of rain
# Habitat: noted whether flushing area was a secluded coral bommie or part of contiguous reef habitat
# Depth: measured the depth of each trial's sibject individual with 2 m pole marked with cm increments
# Treatment_color: recorded treatment color used in each trial
# Fish_size: estimated by flusher based on training with 50 trials estimating objects of varying sizes in the same habitat, error was ± 3 cm
# Group_size: flusher counted number of individual damselfish occupying each trial habitat
# Observer: name of the flusher for each trial
# ED_cm: extension distance: length of the stimulus extended from flusher when subject individual initiated flight
# SD_cm: starting distance: distance between flusher and subject individual's initial position measured with 2 m stimulus pole marked in cm increments
# FID: flight initiation distance: extension distance subtracted from starting distance
# Notes: extra notes on each trial if needed
# Temperature_C: measured by third observer during each substrate measurement with a thermometer attached to the quadrant
# Live_coral_cover: number of intercepts on quadrant that third observer marked as "live coral cover"
# Dead_coral_cover: number of intercepts on quadrant that third observer marked as "dead coral cover"
# Algae_cover: number of intercepts on quadrant that third observer marked as "algae cover"
# Rubble_cover: number of intercepts on quadrant that third observer marked as "rubble cover"
# Sand: number of intercepts on quadrant that third observer marked as "sand"
# Sum: sum of substrate cover tallies from each trial- each trial had 81 tallies
# Hard_substrate_cover: summed "live coral cover" and "dead coral cover" tallies to quantify hard substrate
# RGB_color_difference: Euclidean distance between red, green, and blue color values of the stimulus and the background for each trial
# HSB_color_difference: Euclidean distance between the hue, saturation, and brightness values of the stimulus and background for each trial





##### Setup #####

#load packages needed
library(emmeans)
library(performance)
library(see)
library(patchwork)
library(ggplot2)
library(rsq)
library(nloptr)

#set working directory
setwd("~/Desktop")

#load in raw data
color_data <- read.csv("Color_and_Damselfish_Data_Sheet_31Jan2022.csv", header=T)
size_data <- read.csv("size_error.csv", header=T)

#remove all SD lower than 141
color_data2 <- color_data[color_data$SD_cm >= 141,]

#remove beaufort 3
data_final <- color_data2[color_data$Beaufort <= 2,]

#remove N/A rows within the data
data_final <- subset(data_final, !is.na(data_final$Trial))

##### Calculating Means and Standard Deviation of Data Set Values #####
mean(data_final$Fish_size_cm)
mean(data_final$Group_size)
mean(data_final$SD_cm)
mean(data_final$Hard_substrate_cover)
sd(data_final$Fish_size_cm)
sd(data_final$Group_size)
sd(data_final$SD_cm)
sd(data_final$Hard_substrate_cover)

##### Calculating error for flusher's fish size estimation #####
# size_data contains the following columns:
# Trial: trial number of each size estimation during training exercise 
# Guess: Estimation of object size in cm
# Actual: Measured object size in cm
# Difference: Difference between estimation and measurement in cm

t.test(size_data$Guess, size_data$Actual, paired = TRUE, conf.level = .95) #t test for error

##### Graphical Analysis #####
#checking data distributions
hist(data_final$FID)
hist(data_final$SD_cm)
hist(data_final$Fish_size_cm)
hist(data_final$Group_size)
boxplot(data_final$FID~data_final$Treatment_color)
boxplot(data_final$RGB_color_difference~data_final$Treatment_color)
boxplot(data_final$HSB_color_difference~data_final$Treatment_color)
hist(data_final$HSB_color_difference)

##### Contingency Table Treatment and Rain - Not Significant #####
table(data_final$Treatment_color, data_final$Rain)
chisq.test(data_final$Treatment_color, data_final$Rain)

##### Contingency Table Treatment and Beaufort - Not Significant #####
table(data_final$Treatment_color, data_final$Beaufort)
chisq.test(data_final$Treatment_color, data_final$Beaufort)

##### Correlation Matrix - checking for collinearity #####
data_cor <- subset(data_final, select = c(SD_cm, Hard_substrate_cover, Depth_cm, Fish_size_cm, Group_size))
data_cor <- subset(data_cor, !is.na(data_cor$Depth_cm)) #Remove NA from data
cor(data_cor)





##### Testing for Species Confidence Hypothesis #####

##### Fitting Linear Models #####
m1 <- lm(FID ~ Treatment_color + SD_cm + Hard_substrate_cover + Depth_cm + Fish_size_cm + Group_size + Beaufort + Treatment_color*SD_cm + Treatment_color*Hard_substrate_cover + Treatment_color*Fish_size_cm + Treatment_color*Group_size, data = data_final)

check_model(m1) #checking assumptions of model
anova(m1) #checks if main effects/interactions are significant
summary(m1) #breakdown of estimates - allow you to make predication equation
rsq.partial(m1) #gives partial R^2 which is the effect size for each predictor in the model

##### Easier to View Partial R-squared #####
library("rsq")
rp <-rsq.partial(m1)
vec1 <- rp$variable
vec2 <- rp$partial.rsq
part_r2 <- data.frame(vec1, vec2)
View(part_r2)
rsq.partial(m1, adj=FALSE, type=c("v"))

##### EMMEANS #####

contrast <- emmeans(m1, "Treatment_color") 
pairs(contrast)

##### EMMEANS Cohen's D #####
pwpm(contrast)
pwpm(contrast, means = FALSE, flip = TRUE, reverse = TRUE,       side = ">", delta = 0.05, adjust = "none")

eff_size(contrast, sigma = sigma(m1), edf = df.residual(m1))

##### Testing EMMEANS #####
testing <- emmeans(m1, ~ "Treatment_color")
pairs(testing)
testing_plot <- plot <- plot(testing, horizontal=F, comparisons=T) + theme_bw()
contrast(testing, interaction = T)

##### Ran EMTRENDS and found nothing significant #####
emtrends(m1, pairwise ~ "Treatment_color", var = "Hard_substrate_cover") #emtrends with hard substrate





##### Testing Color Difference Hypothesis #####

# simple model with HSB color difference as the dependent variable and treatment color as the independent variable, tested whether our treatments were significantly different from each other in terms of hue, saturation, and brightness values
m2 <- lm(HSB_color_difference ~ Treatment_color,data = data_final)
m2

check_model(m2) #checking assumptions of model
anova(m2) #checks if main effects/interactions are significant
summary(m2) #breakdown of estimates - allow you to make predication equation

# simple model with RGB color difference as the dependent variable and treatment color as the independent variable, tested whether our treatments were significantly different from each other in terms of red, green, and blue values
m3 <- lm(RGB_color_difference ~ Treatment_color,data = data_final)
m3

check_model(m3) #checking assumptions of model
anova(m3) #checks if main effects/interactions are significant
summary(m3) #breakdown of estimates - allow you to make predication equation

# refitted original linear model that tested our main hypothesis substituting HSB color difference for treatment color as the independent variable to compare adjusted R squared values as an explanation of variance in FID
m4 <- lm(FID ~ HSB_color_difference + SD_cm + Hard_substrate_cover + Depth_cm + Fish_size_cm + Group_size + Beaufort + HSB_color_difference*SD_cm + HSB_color_difference*Hard_substrate_cover + HSB_color_difference*Fish_size_cm + HSB_color_difference*Group_size, data = data_final)
m4

check_model(m4) #checking assumptions of model
anova(m4) #checks if main effects/interactions are significant
summary(m4) #breakdown of estimates - allow you to make predication equation
rsq.partial(m4)

# refitted original linear model that tested our main hypothesis substituting RGB color difference for treatment color as the independent variable to compare adjusted R squared values as an explanation of variance in FID
m5 <- lm(FID ~ RGB_color_difference + SD_cm + Hard_substrate_cover + Depth_cm + Fish_size_cm + Group_size + Beaufort + RGB_color_difference*SD_cm + RGB_color_difference*Hard_substrate_cover + RGB_color_difference*Fish_size_cm + RGB_color_difference*Group_size, data = data_final)
m5

check_model(m5) #checking assumptions of model
anova(m5) #checks if main effects/interactions are significant
summary(m5) #breakdown of estimates - allow you to make predication equation
rsq.partial(m5)

# refitted original linear model that tested our main hypothesis adding HSB color difference as an additional independent variable to contro for color difference while testing for treatment color.
m6 <- lm(FID ~ Treatment_color + SD_cm + Hard_substrate_cover + Depth_cm + Fish_size_cm + Group_size + HSB_color_difference + Beaufort + Treatment_color*SD_cm + Treatment_color*Hard_substrate_cover + Treatment_color*Fish_size_cm + Treatment_color*Group_size, data = data_final)
m6

check_model(m6) #checking assumptions of model
anova(m6) #checks if main effects/interactions are significant
summary(m6) #breakdown of estimates - allow you to make predication equation
rsq.partial(m6)

# refitted original linear model that tested our main hypothesis adding RGB color difference as an additional independent variable to contro for color difference while testing for treatment color
m7 <- lm(FID ~ Treatment_color + SD_cm + Hard_substrate_cover + Depth_cm + Fish_size_cm + Group_size + RGB_color_difference + Beaufort + Treatment_color*SD_cm + Treatment_color*Hard_substrate_cover + Treatment_color*Fish_size_cm + Treatment_color*Group_size, data = data_final)
m7

check_model(m7) #checking assumptions of model
anova(m7) #checks if main effects/interactions are significant
summary(m7) #breakdown of estimates - allow you to make predication equation
rsq.partial(m7)

