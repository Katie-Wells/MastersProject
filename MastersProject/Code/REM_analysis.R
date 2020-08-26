## REM analysis:
## Exifdata and position/speed data are private so cannot be provided for this
## analysis.

## Clear the WD:
rm(list=ls())
dev.off()
getwd()

## Set the WD:
setwd("C:/Users/kewel/OneDrive/Documents/ICL/Project/Data/Tracking_Hampstead_Heath_2018")

## I need 4 dataframes here:
## 1. The metadata for the animal images:
## 'Exifdat' here is for the animal images, not the deployment as in the CTtracking 
## code.
exifdat <- read.csv("animal_exifdata.csv")

## 2. The position data, created from the CTtracking code:
posdat <- read.csv("posdat.csv")


## 3. The sequence data, created from the CTtracking code:
seqdat <- read.csv("seqdat.csv")

## 4. The deployment table.
## This is another version of 'deptable' from the CTtracking analysis.
## This file does not have the camera model information on it as the other 'deptable'
## did from the CTtracking code:
depdat <- read.csv("deptable1.csv")

## Load the 'camtools' package:
source("camtools.R")

## Extract. tags() will extract the tag, time and date information from the animal 
## metadata in 'exifdat'
## This will give a dataframe with a column for each tagged species, as well as
## whether the image is the first contact or not:
tagdat <- extract.tags(exifdat)

## I saved this as a csv file as the formatting was incorrect, so I have to alter 
## the file in Excel. I had to  swap columns around and change their names:
write.csv(tagdat, "C:/Users/kewel/OneDrive/Documents/ICL/Project/Data/Tracking_Hampstead_Heath_2018/tagdat.csv",
          row.names = FALSE)

## I ran this after altering the csv file. This format meant the next line of code 
## could run properly:
tagdat <- read.csv("tagdat.csv")

## This 'first contact' images needed to be place into a subset to check that the
## deployment information matched with the times of the first contact image:
contactdat <- subset(tagdat, contact==1)

## Then I checked the data. Problematic data showed up as red points that didn't
## sit over the line (depicting the deployment time) for that site.
plot.deployments(contactdat, depdat)

## This line checks for the 'bad' deployments. If the contacts are outside of the
## deployment times, this needs investgating.
chk <- check.dates(contactdat, depdat)
chk$bad.data

## Event.count() creates a dataframe of trap rate data. It contains a row per 
## camera and columns for station (place) ID, survey effort in days and record counts 
## for each species:
trdat <- event.count(contactdat, depdat)
head(trdat)

## REM ANALYSIS
## as I am doing multiple species I will do one of these each
## List of required packages:
require(activity)

## These were downloaded from Github:
source("sbd.r")
source("distancedf.r")

## Species 1: Hedgehog:
sp <- "Hedgehog"

## ACTIVITY ESTIMATE
# Using the 'activity' package.

## The fitact() function fits a circular kernel model to the data. This is applied
## to the radian time of day for the first contact data, providing an an estimate
## of activity. 
## The model is fitted with bootstrapping and standard errors are provided
## by setting the 'sample' argument to either "data" or "model".
## The bootstrapping can be set with the 'reps' argument.
actmod <- fitact(subset(contactdat, species == sp)$time, 
                 bounds = c(18,8)*pi/12,
                 sample = "data",
                 reps = 100)

## Printing this contents as it contains the activity level estimate:
actmod@act 

##       act         se   lcl.2.5%  ucl.97.5% 
##0.55256363 0.02980483 0.45089337 0.55739654 

## This plots the activity pattern from the cam trap data. 
## This shows the data distribution and fitted circular kernel model, considering
## only nocturnal images.
plot(actmod, centre = "night", dline = list(col="grey"))

## SPEED ESTIMATE
## Done using the 'sbd.r' package.

## The first step is to check the distributions. If some are too slow or don't 
## move they may need to be removed. Alternatively, if some are too fast then 
## there bee errors:
speeds <- subset(seqdat, species=="hedgehog")$speed
hist(log10(speeds), main="", xlab="Speed (log10[m/s])")

## Subsetting can be done using R. This line gives the harmonic mean speeds for
## hedgehogs:
speeds <- subset(seqdat, species=="hedgehog" & speed > 0.001 & speed< 10)$speed 
(spdest <- hmean(speeds))

##      mean         se 
##0.13512388 0.01025501 

## DETECTION ZONE ESTIMATION
## This section uses the 'distancedf.r' package.

## Fitdf() fits detection functions to the raidal and angular distances at the 
## point of the first contact. This is used to estimate the detection zone.
## First, I subset the contact images for the hedgehog from the position data:
dzdat <- subset(posdat, frame_count==1 & species=="hedgehog")

## Then, I fit the detection functions to the radial distance.
## As there are no covariate columns, the first argument just includes the radial
## distances and 1. The second argument is the 'dzdat' data frame. As it is for
## radial distances, the third argument is a 'point' transect. The 'key' argument
## fits a hazard rate model (which is typically suitable). 'Order = 0' is used as
## flexability adjustment terms are not needed. If extreme values are detected, 
## then the truncation argument can be altered to change the max distance:
radmod <- fitdf(radius~1, dzdat, transect="point", key="hr", order=0, truncation=10)
radmod$edd

##   estimate        se
##p2 2.988004 0.1296855

## Plot the radial distances and fitted detection function:
plot(radmod$ddf, pdf=TRUE)

## All seems fine, so I will leave it.

## The final step for the REM parameters is to fit a model to the angle of the camera.
## This is similar to the radial detection function, however the transect argument
## is a line-type and key function is a half-normal detection function. 
## These are both default options, so they don't need to be included in the fitdf()
## function. As angles to the left of the centre of the camera are registered as 
## negative values they need to be to converted to absolute values before analysis:
dzdat$angle <- abs(dzdat$angle)
angmod <- fitdf(angle~1, dzdat, order = 0)
angmod$edd

##   estimate         se
##1 0.3555818 0.03491079

## This plot shows the distribution  of angluar distances and fitted detection 
## function:
plot(angmod$ddf)

## DENSITY ESTIMATION:
## The 'camtools' package is used to combine all the parameters for the REM. 

## The parameters should be turned into lists to pull togehter all the estimates from
## above. The letters are as follows:

## v = speed while active
## p = activity level
## r = detection zone radius
## theta = detection zone angle
## ensure units are all the same across parameters. 

## To ensure all the parameters are the same unit, the time is truncated to 14 hours
## for all of them, and converted to km rather than m. The angle is multiplied by
## 2 as this estimate is currently only for one half of the camera field of view.
## The lists are created for both the parameter estimates and the standard error:

param <- list(v = spdest["mean"] * 14*60^2 / 1000,
              p = actmod@act ["act"],
              r = radmod$edd$estimate / 1000, 
              theta = angmod$edd$estimate * 2) 

paramse <- list(v = spdest["se"] * 14*60^2 / 1000,
                p = actmod@act ["se"],
                r = radmod$edd$se /1000,
                theta = angmod$edd$se * 2)

## The bootTRD() function takes arguments for the number of records per station,
## the survey effort and the parameters estimated above as lists and estimates the
## density for the hedgehogs:
bootTRD(trdat[,sp], trdat$effort.days, param, paramse)

##      Density       SE
##[1,] 21.58471 4.845631

## Repeat the same code again for the fox this time:
sp <- "Fox"

## ACTIVITY ESTIMATE:
actmod <- fitact(subset(contactdat, species == sp)$time, 
                 bounds = c(18,8)*pi/12,
                 sample = "data",
                 reps = 100)
## Activity:
actmod@act 

##       act         se   lcl.2.5%  ucl.97.5% 
##0.55976900 0.02382574 0.51090306 0.59909165 

plot(actmod, centre = "night", dline = list(col="grey"))

## SPEED ESTIMATION:
speeds <- subset(seqdat, species=="fox")$speed
hist(log10(speeds), main="", xlab="Speed (log10[m/s])")
speeds <- subset(seqdat, species=="fox" & speed > 0.001 & speed< 10)$speed 

## Speed:
(spdest <- hmean(speeds))

##      mean         se 
##0.30694292 0.02619321 

## DETECTION ZONE ESTIMATION
dzdat <- subset(posdat, frame_count==1 & species=="fox")

## Radius:
radmod <- fitdf(radius~1, dzdat, transect="point", key="hr", order=0, truncation=10)
radmod$edd

##    estimate         se
## p2 3.457023 0.09491432

plot(radmod$ddf, pdf=TRUE)

## Angle
dzdat$angle <- abs(dzdat$angle)
angmod <- fitdf(angle~1, dzdat, order = 0)
angmod$edd

##    estimate          se
## 1 0.3042207 0.009329135

plot(angmod$ddf)

## DENSITY ESTIMATION:
param <- list(v = spdest["mean"] * 14*60^2 / 1000,
              p = actmod@act ["act"],
              r = radmod$edd$estimate / 1000, 
              theta = angmod$edd$estimate * 2) 

paramse <- list(v = spdest["se"] * 14*60^2 / 1000,
                p = actmod@act ["se"],
                r = radmod$edd$se /1000,
                theta = angmod$edd$se * 2)

## Density estimate:
bootTRD(trdat[,sp], trdat$effort.days, param, paramse)

##       Density       SE
## [1,] 45.17286 8.202934
