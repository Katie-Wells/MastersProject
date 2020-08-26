## Trap rate analysis

## 
rm(list=ls())
dev.off()
getwd()

## Required packages:
require(MASS)
require(viridis)
require(ggplot2)
require(multcomp)

## Set wd:
setwd("C:/Users/kewel/OneDrive/Documents/ICL/Project/Data/Tracking_Hampstead_Heath_2018")

## Load in the bin data:
bin <- read.csv("Bin_Dens.csv")

## Read the hab_type as factors:
bin$hab_type <- as.factor(bin$hab_type)

## Plot the data to start with:
plot(count_hog~bin_dens_200m, data = bin)
plot(count_fox~bin_dens_200m, data = bin)

## the points are clustered towards the zero, as there are a lot of zeroes in the 
## data. 
## The data fits a poisson distribution, however there are lots zeros in the data, 
## so I checked for overdispersion:

## Hedgehog:
with(bin, tapply(count_hog, hab_type, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

##     Amenity Grassland   Broadleaved Woodland            Dense Scrub 
##"M (SD) = 3.53 (5.78)" "M (SD) = 2.68 (3.79)" "M (SD) = 4.46 (8.58)" 
##             Hedgerows        Human Structure             Tall Herbs 
##"M (SD) = 2.23 (3.53)" "M (SD) = 2.24 (4.76)" "M (SD) = 0.40 (0.74)" 

## Fox
with(bin, tapply(count_fox, hab_type, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

##     Amenity Grassland     Broadleaved Woodland              Dense Scrub 
##"M (SD) = 7.94 (6.32)" "M (SD) = 14.82 (18.09)" "M (SD) = 10.27 (11.72)" 
##               Hedgerows          Human Structure               Tall Herbs 
##"M (SD) = 17.68 (21.94)" "M (SD) = 19.24 (26.43)"   "M (SD) = 8.00 (9.53)" 

## SD is higher than the mean - suggesting over-dispersion. 
## Negative binomial regression is appropriate.

## This is from the MASS package:

## HEDGEHOG
## I ran four models to see which one fits the data the best. The survey effort
## is the logged offset term to get the trap rate:

## Null model:
model1 <- glm.nb(count_hog~1 + offset(log(bin$survey_effort)), data = bin)
summary(model1)

## Just bin:
model2 <- glm.nb(count_hog~bin_dens_200m + offset(log(survey_effort)), data = bin)
summary(model2)

## Just hab:
model3 <- glm.nb(count_hog~hab_type + offset(log(survey_effort)), data = bin)
summary(model3)

## Both:
model4 <- glm.nb(count_hog~bin_dens_200m + hab_type + offset(log(survey_effort)), data = bin)
summary(model4)

## Ran AIC() to see which model was best, and estimated the pseudo R squared:

## Model 1
AIC(model1)
## 557.4988

## Model 2
AIC(model2)
## 550.5042
## R2
(model2$null.deviance - model2$deviance)/model2$null.deviance
## 0.066

## Model 3
AIC(model3)
## 553.9255
## R2
(model3$null.deviance - model3$deviance)/model3$null.deviance
## 0.09

## Model 4
AIC(model4)
## 548.4105
## R2
(model4$null.deviance - model4$deviance)/model4$null.deviance
## 0.15 

## Model 4 is selected as the best model. 
## Plot residuals:
par(mfrow = c(1,2))
plot(model4, which = c(1,2))

## These are ok, although they are not perfect, I will keep this in mind. 
## Tukey's test to look at the differences in between all the habitat types
## (from the 'multcomp' package):
summary(glht(model4, mcp(hab_type="Tukey")))

## FOX
## Repeated this for the foxes:

## Null model:
model5 <- glm.nb(bin$count_fox~1 + offset(log(bin$survey_effort)))
summary(model5)

## Just bin:
model6 <- glm.nb(count_fox~bin_dens_200m + offset(log(survey_effort)), data = bin)
summary(model6)

## Just hab:
model7 <- glm.nb(count_fox~hab_type + offset(log(survey_effort)), data = bin)
summary(model7)

## Both:
model8 <- glm.nb(count_fox~bin_dens_200m +hab_type + offset(log(survey_effort)), data = bin)
summary(model8)

## AIC's for each model:
## Model 5
AIC(model5)
## 1012.903

## Model 6
AIC(model6)
## 1010.109
## R2
(model6$null.deviance - model6$deviance)/model6$null.deviance
## 0.02929309

## Model 7
AIC(model7)
## 1013.536
## R2
(model7$null.deviance - model7$deviance)/model7$null.deviance
## 0.05657965

## Model 8
AIC(model8)
## 1006.869
## R2
(model8$null.deviance - model8$deviance)/model8$null.deviance
## 0.1065437

## Model 8 is selected as the best model. 
## Plot residuals:
par(mfrow = c(1,2))
plot(model8, which = c(1,2))

## These are ok, although they are not perfect, I will keep this in mind too. 
## Tukey's test to look at the differences in between all the habitat types
## (from the 'multcomp' package):
summary(glht(model8, mcp(hab_type="Tukey")))

## Plotting using predict()

## Hedgehog trap rate ~ bin density:
## Create a new dataframe with all the variables of the same name within the model.
## Set the reference level - here I chose the same reference level as the model -
## 'Amenity Grassland':
df <- data.frame(
  survey_effort = mean(bin$survey_effort),
  bin_dens_200m = seq(0, 0.24978283, len = 1000),
  hab_type = as.factor(c("Amenity Grassland")))

## Combine the model and the new dataframe:
pred <- predict(model4, newdata = df, type = "response", se.fit=TRUE)

## Get confidence intervals
df$predicted <- pred$fit
df$lower <- pred$fit - 1.96*pred$se.fit
df$upper <- pred$fit + 1.96*pred$se.fit

## Plot:
ggplot() +
  geom_line(data=df, aes(x=bin_dens_200m, y=predicted), colour="red", size=.7) +
  geom_ribbon(data=df, aes(x=bin_dens_200m, 
                           ymin=lower, ymax=upper), 
              fill="darkred", alpha=0.3) +
  theme_bw() +
  xlab("Bin Density") +
  ylab("Hedgehog Trap Rate (Images/Night)") +
  theme_minimal()

## Hedgehog trap rate ~ habitat type:
## Create dataframe with all the habitats as factors:
df3 <- data.frame(
  survey_effort = mean(bin$survey_effort),
  bin_dens_200m = mean(bin$bin_dens_200m),
  hab_type = as.factor(c("Amenity Grassland", "Broadleaved Woodland", "Dense Scrub",
                         "Hedgerows", "Human Structure", "Tall Herbs")))

## Combine the model and the df:
pred3 <- predict(model4, newdata = df3, type = "response", se.fit=TRUE)

## Confidence intervals and predicted line:
df3$predicted <- pred3$fit
df3$lower <- pred3$fit - 1.96*pred3$se.fit
df3$upper <- pred3$fit + 1.96*pred3$se.fit

## Plot
ggplot(data=df3, aes(x=hab_type, y=predicted)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.9) +
  xlab("Habitat Type") +
  ylab("Hedgehog Trap Rate (Image/Night)") +
  theme_bw() +
  scale_x_discrete(breaks=c("Amenity Grassland","Broadleaved Woodland", "Dense Scrub",
                            "Hedgerows", "Human Structure", "Tall Herbs"),
                   labels=c("AG", "BW", "DS", "HR", "HS", "TH")) +
  geom_errorbar(aes(ymin=lower, ymax=upper, width=0.1)) +
  theme_minimal()

## Fox trap rate ~ bin density:
## Create a new dataframe with all the variables of the same name within the model.
## Set the reference level - here I chose the same reference level as the model -
## 'Amenity Grassland':
df2 <- data.frame(
  survey_effort = mean(bin$survey_effort),
  bin_dens_200m = seq(0, 0.24978283, len = 1000),
  hab_type = as.factor(c("Amenity Grassland")))

## Combine:
pred2 <- predict(model8, newdata = df2, type = "response", se.fit=TRUE)

## Confidence intervals and the predicted line:
df2$predicted <- pred2$fit
df2$lower <- pred2$fit - 1.96*pred2$se.fit
df2$upper <- pred2$fit + 1.96*pred2$se.fit

## Plot:
ggplot() +
  geom_ribbon(data=df2, aes(x=bin_dens_200m, 
                            ymin=lower, ymax=upper), 
              fill="skyblue4", alpha=0.3) +
  geom_line(data=df2, aes(x=bin_dens_200m, y=predicted), colour="blue", size=.7) +
  theme_bw() +
  xlab("Bin Density") +
  ylab("Fox Trap Rate (Image/Night)") +
  theme_minimal()

## Fox trap rate ~ habitat type:
## Create dataframe with all the habitats as factors:
df4 <- data.frame(
  survey_effort = mean(bin$survey_effort),
  bin_dens_200m = mean(bin$bin_dens_200m),
  hab_type = as.factor(c("Amenity Grassland", "Broadleaved Woodland", "Dense Scrub",
                         "Hedgerows", "Human Structure", "Tall Herbs")))

## Combine df and model:
pred4 <- predict(model8, newdata = df4, type = "response", se.fit=TRUE)

## Predicted line and CI's for error lines:
df4$predicted <- pred4$fit
df4$lower <- pred4$fit - 1.96*pred3$se.fit
df4$upper <- pred4$fit + 1.96*pred3$se.fit

## Plot:
ggplot(data=df4, aes(x=hab_type, y=predicted)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  xlab("Habitat Type") +
  ylab("Fox Trap Rate (Image/Night)") +
  theme_bw() +
  scale_x_discrete(breaks=c("Amenity Grassland","Broadleaved Woodland", "Dense Scrub",
                            "Hedgerows", "Human Structure", "Tall Herbs"),
                   labels=c("AG", "BW", "DS", "HR", "HS", "TH")) +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  geom_errorbar(aes(ymin=lower, ymax=upper, width=0.1)) +
  theme_minimal()

## Hedgehog & fox trap rate ~ bins (combined)
ggplot() + 
  geom_line(data=df, aes(x=bin_dens_200m, y=predicted), colour="darkred", size=1) +
  geom_ribbon(data=df, aes(x=bin_dens_200m, 
                           ymin=lower, ymax=upper), 
              fill="darkred", alpha=0.1) +
  geom_ribbon(data=df2, aes(x=bin_dens_200m, 
                            ymin=lower, ymax=upper), 
              fill="skyblue4", alpha=0.1) +
  geom_line(data=df2, aes(x=bin_dens_200m, y=predicted), colour="darkblue", size=1) +
  theme_bw() +
  ylab("Trap Rate (Images/Night)") +
  xlab("Bin Density") +
  theme_minimal()

## Hedgehog + fox trap rate ~ habitat type:

## Add the species to both dataframes:
df3$species <- "Hedgehog"
df4$species <- "Fox"

## Combine the dataframes:
combined <- rbind(df3, df4)

## Plot
ggplot(combined, aes(fill=species, y=predicted, x=hab_type)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Habitat Type") +
  ylab("Trap Rate (Image/Night)") +
  theme_bw() +
  scale_x_discrete(breaks=c("Amenity Grassland","Broadleaved Woodland", "Dense Scrub",
                            "Hedgerows", "Human Structure", "Tall Herbs"),
                   labels=c("AG", "BW", "DS", "HR", "HS", "TH")) +
  geom_errorbar(aes(ymin=lower, ymax=upper, width=0.1), width=.5,
                position = position_dodge(.9)) +
  theme_minimal() +
  scale_fill_manual("species",values = c('skyblue3', 'red')) +
  guides(fill=guide_legend(title="Species"))

