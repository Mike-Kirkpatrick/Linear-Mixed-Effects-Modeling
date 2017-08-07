
###############################################
# Example Code for Linear Mixed Effects Model #
###############################################

# There are 6 Variables
# Class_Term = the unique ID for each record (each class runs once per term)
# Class = A course (e.g., BIO100)
# Term  = Quarter system
# Mode  = class modality - either online or onsite
# Pre   = average exam score for each Class_Term for time 1
# Post  = average exam score for each Class_Term for time 2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Import Data and Transform from WIDE to TALL #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Clear entire environment
rm(list=ls())

#load data
library(readxl)
setwd("C:/Users/mkirkpatrick/Google Drive/Statistics n Such/R/Analyses for Reference/Linear Mixed Effects - Repeated Measures/")
data <- read_excel("Pre-Test Post-Test data.xlsx")


### Need to "stack" the Pre and Post columns and rename Time
library(tidyr)
data_long <- gather(data, time, Score, Pre:Post)

# Make time a factor and change the order of it so Pre is listed first (this is for graphing later)
data_long$Time <- factor(data_long$time, levels = c("Pre","Post"))


#~~~~~~~~~~~~~~~~~~~~~~~~#
# Descriptive Statistics #
#~~~~~~~~~~~~~~~~~~~~~~~~#
library(psych)
with(data_long,describeBy(Score,group = list(Time), mat=T, digits=2))
with(data_long,describeBy(Score,group = list(Time,Mode), mat=T, digits=2))
with(data_long,describeBy(Score,group = list(Time,Class), mat=T, digits=2))
with(data_long,describeBy(Score,group = list(Mode,Class), mat=T, digits=2))
with(data_long,describeBy(Score,group = list(Mode,Class,Time), mat=T, digits=2)) #Groups are really small at this level


#~~~~~~~~~~~~~~~~#
# Model Building #
#~~~~~~~~~~~~~~~~#

library(lme4)
library(lmerTest)
library(lsmeans)

# Time is the within-subjects factor
# Mode and Class are the between-subjects factor

# Random factors are Class_Term and Time
# Fixed Factors (the predictors that you want to analyze) are Mode, Class and Time

# Intercept only model
lm0 <- lmer(Score ~ 1 + (1 | Class_Term),data = data_long,REML=FALSE)

lm1 <- lmer(Score ~ Mode*Time + (Time | Class_Term), data_long, REML=FALSE)
anova(lm1)

lm2 <- lmer(Score ~ Class*Time + (Time | Class_Term),data = data_long,REML=FALSE)
anova(lm2)

lm3 <- lmer(Score ~ Mode + Class + Time + Mode:Time + Class:Time + (Time | Class_Term),data = data_long,REML=FALSE)
anova(lm3)

lm4 <- lmer(Score ~ Mode + Class + Time + Mode:Time + Class:Time + Mode:Class + (Time | Class_Term),data = data_long,REML=FALSE)
anova(lm4)

lm5 <- lmer(Score ~ Mode*Class*Time + (Time | Class_Term),data = data_long,REML=FALSE)
anova(lm5)

# Compare each model
# Each model is a sig improvement from the last except for the final model.
# Going with model 4 because of this and because the 3 way interaction was non-sig
anova(lm0,lm1,lm2,lm3,lm4,lm5)


#FINAL MODEL
lm4 <- lmer(Score ~ Mode + Class + Time + Mode:Time + Class:Time + Mode:Class + (Time | Class_Term),data = data_long,REML=FALSE)
anova(lm4)
summary(lm4)




#~~~~~~~~~~~~~~~~~~~#
# Interaction Plots #
#~~~~~~~~~~~~~~~~~~~#

library(ggplot2)

##### Export Images #####
pdf("Graphs of Interactions.pdf")

# Time*Mode
lsm <- lsmeans(lm4, pairwise ~ Time | Mode)
lsm <- summary(lsm$lsmeans)

ggplot(lsm, aes(x=Time, y=lsmean, colour=Mode, group=Mode
                , ymin=lower.CL, ymax=upper.CL, width = 0.05)) + 
  labs(title="Interaction Plot of Time*Mode",y="Score") +
  geom_line() +
  geom_point() +
  geom_errorbar()


# Time*Class
lsm <- lsmeans(lm4, pairwise ~ Time | Class)
lsm <- summary(lsm$lsmeans)

ggplot(lsm, aes(x=Class, y=lsmean, fill=Time)) + 
  geom_bar(position="dodge",stat="identity",width=.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + 
  labs(title="Interaction Plot of Time*Class",y="Score")


# Mode*Class
lsm <- lsmeans(lm4, pairwise ~ Mode | Class)
lsm <- summary(lsm$lsmeans)

ggplot(lsm, aes(x=Class, y=lsmean, fill=Mode)) + 
  geom_bar(position="dodge",stat="identity",width=.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) + 
  labs(title="Interaction Plot of Mode*Class",y="Score")

# Line graph makes it easy to see which did better/worse
ggplot(lsm, aes(x=Class, y=lsmean, colour=Mode, group=Mode
                , ymin=lower.CL, ymax=upper.CL, width = 0.05)) + 
  labs(title="Interaction Plot of Mode*Class",y="Score") +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))


#### End Export Images ####
dev.off()


#Clear the console
cat("\014")
