## CTtracking analysis: 
## Images and exifdata are private, so cannot be provided for this analysis.

## Clear R's brain:
rm(list=ls())
dev.off()
getwd()

## Set working directory to the location of the CTtracking package:
setwd("C:/Project/packages/CTtracking-master")

## Load package
source("CTtracking.R")

## Camera calibration:
## Set wd to the location of the camera images and data
setwd("C:/Project/Data/CamCal")

## Read.exif() reads the metadata of the images in the supplied file path and
## creates a dataframe.
## These were create for all camera trap models (Brownings and Reconyx).
exifdat.cam <- read.exif("./CamImages")

## Read.digidat() then creates a dataframe of digitisation data.
## This reads the csv files that match with the metadata. 
## The 'pole' argument tells R it's digitisation data, not animal.
camdat <- read.digidat("./CamData",
                       exifdat = exifdat.cam,
                       datatype = "pole")

## Cal.cam() fits the camera calibration models, with the above dataframe as the 
## sole argument
cmods <- cal.cam(camdat)

## Then plot to check there are no problems with the pole placements:
plot(cmods)

## All seems good with the models, so next I fit the deployment models.

## Deployment calibration:
## Set WD to the location of the deployment information.
setwd("C:/Project/Data/Tracking_Hampstead_Heath_2018")

## The steps are similar to the camera calibration steps.
## This is run if this is the first time the metadata is read!
## Create dataframe of metadata from the deployment images:
exifdat <- read.exif("./DeploymentImages")

## I saved this to make it easier to load when running this code again. The 
## read.exif() function can take a while to run with lots of images. 
## There are almost 3000 images to read.
write.csv(exifdat, "C:/Project/Data/Tracking_Hampstead_Heath_2018/exifdata.csv",
          row.names = FALSE)

## Load this if the above step has already been done! 
exifdat <- read.csv("exifdata.csv")

## Use this function again. This time to create a dataframe of deployment 
## digitisation. This matches the images to the corresponding csv files in the
## working directory:
depdat <- read.digidat(path="./DeploymentData",
                       exifdat = exifdat,
                       datatype = "pole")

## 'Pole' is used here again to read the deployment pole images. 

## The next step was to fit the deployment calibration models.
## This deployment table was created from a large dataframe, specifying when each
## camera was set up and taken down. It also specifies which type of camera was 
## used at each deployment. 
deptab <- read.csv("deptable.csv")
head(deptab)

## Cal.site fits the deployment calibration models. 
## Camera 45 had to few poles digitised, so it was removed from the analysis. 
## 'Depdat' is the deployment digitisation, 'cmods' are the camera calibration
## models and deptab tells "cal.site" which camera model to fit to each site.
smods <- cal.site(depdat, cmods, deptab)

## To check the placements, plot the models together:
plot(smods)

## Animal digitisation and positions:
## The first steps are the same as the previous steps, create a dataframe of the 
## metadata. 
## Ran this the first time, there are over 6500 images:
exifdat.anim <- read.exif("./AnimalImages")

## Saved this datframe to load it in easier later:
write.csv(exifdat.anim, "C:/Project/Data/Tracking_Hampstead_Heath_2018/animal_exifdata.csv",
          row.names = FALSE)

## Ran this after. whenever I came back to it:
exifdat.anim <- read.csv("animal_exifdata.csv")

## Again, this is to get a dataframe of the animal digitisation data. 
## Used "animal" this time as it is not a "pole" digitisation:
depdat1 <- read.digidat(path="./AnimalData",
                        exifdat = exifdat.anim,
                        datatype = "animal")

## Predict.pos() predicts the radius and angles from the digitisation data by using the
## calibrated deployment models and the deployment table:
posdat <- predict.pos(depdat1, smods)

## I saved this too, for the REM analysis later:
write.csv(posdat, "C:/Project/Data/Tracking_Hampstead_Heath_2018/posdat.csv",
          row.names = FALSE)

## seq.summary() predicts animal speeds using the position data created above:
seqdat <- seq.summary(posdat)

## I saved the sequence data too for the REM analysis:
write.csv(seqdat, "C:/Project/Data/Tracking_Hampstead_Heath_2018/seqdat.csv",
          row.names = FALSE)







