## Speed analysis
## Sequence data not available, so the first part of this code will not run.

rm(list=ls())
dev.off()
getwd()

## Install packages:
require(dplyr)
require(psych)
require(ggplot2)
require(viridis)

## Set wd:
setwd("C:/Project/Data/Tracking_Hampstead_Heath_2018")

## I got the harmonic means from the sequence data:
seqdat <- read.csv("seqdat.csv")

## I grouped the data by camera id (group_id) and species and summarised by harmonic
## mean - the harmonic.mean function is from the 'psych' package:
h_means <- seqdat %>%
  group_by(group_id, species) %>%
  summarise(h_mean = harmonic.mean(speed))

## One of the assumptions for a lm is that the response variable is normally 
## distributed. To check it is I did a histogram for each species:
## Hedgehog:
h <- h_means %>%
  filter(species == "hedgehog")

## This is not normally distibuted
hist(h$h_mean)

## Log the speed:
h$log_speed <- log(h$h_mean)

## This has made it more normally distributed:
hist(h$log_speed)

## Fox
f <- h_means %>%
  filter(species == "fox")

## This is not normally distibuted
hist(f$h_mean)

## Log the speed:
f$log_speed <- log(f$h_mean)

## This has made it normally distributed:
hist(f$log_speed)

## To get the log speed before saving, I did this for the whole dataset:
h_means$log_speed <- log(h_means$h_mean)

## I saved this data so I could create a dataframe in Excel of bin densities, habitat 
## types and mean speeds:
write.csv(h_means, "C:/Project/Data/Tracking_Hampstead_Heath_2018/h_means.csv")

## Load in the new datasets, created in Excel:
hedgehog <- read.csv("Hmean_hog.csv")
fox <- read.csv("Hmean_fox.csv")

## Ensure R reads the habitats as factors:
hedgehog$hab_type <- as.factor(hedgehog$hab_type)
fox$hab_type <- as.factor(fox$hab_type)

## Hedgehog:
## Outliers:
boxplot(speed~hab_type, data = hedgehog)

## homogenity of variances:
htest <- hedgehog%>%group_by(hab_type)%>%
  summarise(variance = var(speed))

## normal distribution:
hist(hedgehog$speed)
hist(hedgehog$log_speed)

## excessive zeros? 
## none

## visually inspect relationships:
plot(speed~bin_dens_200m, data = hedgehog)

## Fox:
## Outliers:
boxplot(speed~hab_type, data = fox)

## homogenity of variances:
htest <- fox%>%group_by(hab_type)%>%
  summarise(variance = var(speed))

## normal distribution:
hist(fox$speed)
hist(fox$log_speed)

## excessive zeros? 
## none

## visually inspect relationships:
plot(speed~bin_dens_200m, data = hedgehog)

## Run linear models:
## Hedgehogs:
## NULL:
model1 <- lm(log_speed~1, data = hedgehog)
summary(model1)

## Bin
model2 <- lm(log_speed~bin_dens_200m, data = hedgehog)
summary(model2)

## habitat
model3 <- lm(log_speed~hab_type, data = hedgehog)
summary(model3)

## both
model4 <- lm(log_speed~bin_dens_200m + hab_type, data= hedgehog)
summary(model4)

## AIC
AIC(model1)
## 60.89665

AIC(model2)
## 62.45326

AIC(model3)
## 64.07435

AIC(model4)
## 65.42898

## NULL model has lowest AIC, but Model 2 has less than 2 AIC points

## Fox
## NULL:
model5 <- lm(log_speed~1, data = fox)
summary(model5)

## Bin
model6 <- lm(log_speed~bin_dens_200m, data = fox)
summary(model6)

## habitat
model7 <- lm(log_speed~hab_type, data = fox)
summary(model7)

## both
model8 <- lm(log_speed~bin_dens_200m + hab_type, data= fox)
summary(model8)

## AIC
AIC(model5)
## 89.27178

AIC(model6)
## 89.98683

AIC(model7)
## 98.33712

AIC(model8)
## 98.77944

## NULL model has lowest AIC, but Model 6 has less than 2 AIC points

## Plotting from predicted values:

## Hedgehog speeds ~ bin density:
## Create a dataframe:
df <- data.frame(
  bin_dens_200m = seq(0, 0.24978283, len = 1000))

## Combine model and dataframe:
pred <- predict(model2, newdata = df, type = "response", se.fit=TRUE)

## Add confidence intervals and predicted lines (exp used to back transform the 
## log_speed):
df$predicted <- exp(pred$fit)
df$lower <- exp(pred$fit - 1.96*pred$se.fit)
df$upper <- exp(pred$fit + 1.96*pred$se.fit)

## Plot:
ggplot() +
  geom_line(data=df, aes(x=bin_dens_200m, y=predicted), colour="red", size=.7) +
  geom_ribbon(data=df, aes(x=bin_dens_200m, 
                            ymin=lower, ymax=upper), 
              fill="darkred", alpha=0.3) +
  theme_bw() +
  xlab("Bin Density") +
  ylab("Hedgehog Speed (m/s)") +
  theme_minimal()

## Fox speed ~ bin density:
## Create df for the predicted values:
df2 <- data.frame(
   bin_dens_200m = seq(0, 0.24978283, len = 1000))
pred2 <- predict(model6, newdata = df2, type = "response", se.fit=TRUE)

## Get CI and predicted lines (exp used to back transform the log_speed):
df2$predicted <- exp(pred2$fit)
df2$lower <- exp(pred2$fit - 1.96*pred2$se.fit)
df2$upper <- exp(pred2$fit + 1.96*pred2$se.fit)

## Plot
ggplot() +
  geom_line(data=df2, aes(x=bin_dens_200m, y=predicted), colour="blue", size=1) +
  geom_ribbon(data=df2, aes(x=bin_dens_200m, ymin=lower, ymax=upper), 
              fill="skyblue4", alpha=0.5) +
  theme_bw() +
  xlab("Bin Density") +
  ylab("Average Fox Speed (m/s)") +
  theme_minimal()

## Both on one graph:
ggplot() +
  geom_line(data=df2, aes(x=bin_dens_200m, y=predicted), colour="blue", size=1) +
  geom_ribbon(data=df2, aes(x=bin_dens_200m, ymin=lower, ymax=upper), 
              fill="skyblue4", alpha=0.5) +
  geom_line(data=df, aes(x=bin_dens_200m, y=predicted), colour="red", size=.7) +
  geom_ribbon(data=df, aes(x=bin_dens_200m, 
                           ymin=lower, ymax=upper), 
              fill="darkred", alpha=0.3) +
  theme_bw() +
  xlab("Bin Density") +
  ylab("Average Speed (m/s)") +
  theme_minimal()

## Although habitat type did not effect speeds, I wanted to include the graph to
## visually show what the models predicted:

## Hedgehog speed ~ habitat type:
df3 <- data.frame(
   hab_type = as.factor(c("Amenity Grassland", "Broadleaved Woodland", "Dense Scrub",
                         "Hedgerows", "Human Structure", "Tall Herbs")))

## Combine the model and the df:
pred3 <- predict(model3, newdata = df3, type = "response", se.fit=TRUE)

## Confidence intervals and predicted line (exp to back transform log speed):
df3$predicted <- exp(pred3$fit)
df3$lower <- exp(pred3$fit - 1.96*pred3$se.fit)
df3$upper <- exp(pred3$fit + 1.96*pred3$se.fit)

## Plot
ggplot(data=df3, aes(x=hab_type, y=predicted)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.9) +
  xlab("Habitat Type") +
  ylab("Hedgehog Speed (m/s)") +
  theme_bw() +
  scale_x_discrete(breaks=c("Amenity Grassland","Broadleaved Woodland", "Dense Scrub",
                            "Hedgerows", "Human Structure", "Tall Herbs"),
                   labels=c("AG", "BW", "DS", "HR", "HS", "TH")) +
  geom_errorbar(aes(ymin=lower, ymax=upper, width=0.1)) +
  theme_minimal()

## Fox speed ~ habitat type:
df4 <- data.frame(
  hab_type = as.factor(c("Amenity Grassland", "Broadleaved Woodland", "Dense Scrub",
                         "Hedgerows", "Human Structure", "Tall Herbs")))

## Combine the model and the df:
pred4 <- predict(model7, newdata = df4, type = "response", se.fit=TRUE)

## Confidence intervals and predicted line (exp to back transform log speed):
df4$predicted <- exp(pred4$fit)
df4$lower <- exp(pred4$fit - 1.96*pred4$se.fit)
df4$upper <- exp(pred4$fit + 1.96*pred4$se.fit)

## Plot
ggplot(data=df4, aes(x=hab_type, y=predicted)) +
  geom_bar(stat = "identity", fill = "skyblue4", alpha = 0.9) +
  xlab("Habitat Type") +
  ylab("Fox Speed (m/s)") +
  theme_bw() +
  scale_x_discrete(breaks=c("Amenity Grassland","Broadleaved Woodland", "Dense Scrub",
                            "Hedgerows", "Human Structure", "Tall Herbs"),
                   labels=c("AG", "BW", "DS", "HR", "HS", "TH")) +
  geom_errorbar(aes(ymin=lower, ymax=upper, width=0.1)) +
  theme_minimal()

## Both:
df3$species <- "Hedgehog"
df4$species <- "Fox"
combo_speed <- rbind(df3, df4)

ggplot(combo_speed, aes(fill=species, y=predicted, x=hab_type)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Habitat Type") +
  ylab("Speed (m/s)") +
  theme_bw() +
  scale_x_discrete(breaks=c("Amenity Grassland","Broadleaved Woodland", "Dense Scrub",
                            "Hedgerows", "Human Structure", "Tall Herbs"),
                   labels=c("AG", "BW", "DS", "HR", "HS", "TH")) +
  scale_fill_discrete(name = "Species", labels = c("Fox", "Hedgehog")) +
  geom_errorbar(aes(ymin=lower, ymax=upper, width=0.1), width=.5,
                position = position_dodge(.9)) +
  theme_minimal() +
  scale_fill_manual("species",values = c('skyblue3', 'red'))
