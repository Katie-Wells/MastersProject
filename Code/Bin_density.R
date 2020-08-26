## Bin Density:
rm(list=ls())
dev.off()
getwd()

## Load packages:
require(sf)

## Set the WD:
setwd("C:/Users/kewel/OneDrive/Documents/ICL/Project/Data")

## Load in the bin and camera locations
## St_read() can read kml files: 
bins <- st_read("Bin Co-ordinates.kml")
cameras <- st_read("Hampstead_Heath_camera_grid_2018.kml")

## Estimated Hampstead Heath coordinates (for visual inspection later):
HH <- st_polygon(list(rbind(c(-0.166182, 51.555284), c(-0.165782, 51.555422), c(-0.165811, 51.556391),
                            c(-0.165679, 51.556371), c(-0.164808, 51.557517), c(-0.164528, 51.558808),
                            c(-0.163359, 51.559428), c(-0.159068, 51.556313), c(-0.149706, 51.555512),
                            c(-0.150691, 51.557129), c(-0.153192, 51.558597), c(-0.151495, 51.559533),
                            c(-0.148930, 51.558343), c(-0.148611, 51.558517), c(-0.150779, 51.561280),
                            c(-0.153660, 51.561203), c(-0.153873, 51.562165), c(-0.156072, 51.564290),
                            c(-0.155932, 51.565163), c(-0.160352, 51.568171), c(-0.158475, 51.568958),
                            c(-0.158250, 51.569618), c(-0.158486, 51.570772), c(-0.161243, 51.570798),
                            c(-0.159623, 51.572519), c(-0.160501, 51.572803), c(-0.164347, 51.572557),
                            c(-0.167139, 51.573010), c(-0.167943, 51.572982), c(-0.168224, 51.574005),
                            c(-0.171281, 51.573552), c(-0.171266, 51.572472), c(-0.172927, 51.571529),
                            c(-0.174126, 51.569464), c(-0.178374, 51.570596), c(-0.181462, 51.572301),
                            c(-0.181517, 51.573209), c(-0.183303, 51.575519), c(-0.181986, 51.577897),
                            c(-0.182852, 51.578917), c(-0.189691, 51.576349), c(-0.184187, 51.571751),
                            c(-0.184277, 51.571325), c(-0.180903, 51.569329), c(-0.181733, 51.568432),
                            c(-0.180975, 51.568577), c(-0.180091, 51.567635), c(-0.180326, 51.567478),
                            c(-0.179550, 51.566559), c(-0.180470, 51.566211), c(-0.181042, 51.566891),
                            c(-0.182373, 51.566590), c(-0.183038, 51.567457), c(-0.184261, 51.568244),
                            c(-0.186632, 51.568578), c(-0.192598, 51.568391), c(-0.192705, 51.567244),
                            c(-0.191331, 51.565997), c(-0.189883, 51.564863), c(-0.190205, 51.562856),
                            c(-0.189068, 51.562635), c(-0.184819, 51.562355), c(-0.183402, 51.561270),
                            c(-0.182478, 51.559937), c(-0.181247, 51.560724), c(-0.180052, 51.561287),
                            c(-0.179394, 51.561193), c(-0.179014, 51.563273), c(-0.176346, 51.564375),
                            c(-0.175100, 51.563324), c(-0.176300, 51.562084), c(-0.176214, 51.561372),
                            c(-0.175576, 51.560906), c(-0.174402, 51.560910), c(-0.172321, 51.560063),
                            c(-0.170280, 51.558961), c(-0.171424, 51.558438), c(-0.172219, 51.557954),
                            c(-0.171672, 51.557639), c(-0.168184, 51.557242), c(-0.166852, 51.556798),
                            c(-0.166182, 51.555284))))

## Add the coordinate reference system (CRS) to HH:
HH <- st_sfc(HH, crs = 4326)

## The CRS needed to be changed from WGS84 to BNG for all objects:
bins <- st_transform(bins, 27700)
cameras <- st_transform(cameras, 27700)
HH <- st_transform(HH, 27700)

## Plot these together to check that it worked:
## plot these together:
plot(HH)
plot(cameras, add=TRUE, col='red')
plot(bins, add=TRUE, col='black', pch=3)

## This will count the bins within 200m of each camera
camera_regions <- st_buffer(cameras, dist= 200)
plot(camera_regions, add=TRUE, border='red', lty=2)

## this counts the number of bins in the camera regions, returns a logical response:
## TRUE if bin is present, FALSE if not. 
bins_in_camera_regions <- st_intersects(camera_regions, bins, sparse=FALSE)

## This sums all the rows which have TRUE in. This returns the bin density within
## each cirle. This is not weighted however. 
bin_counts <- rowSums(bins_in_camera_regions)
bin_counts

## As bins which are closer to the cameras will hold more importance than bins which
## are further away. This line of code takes the distance between all the bins and 
## all the cameras:
dist <- st_distance(cameras, bins)

## 'Dist' is the same shape as 'bins_in_camera_regions' so it can be used to set 
## the distance for cameras outside the 200m camera region as 'Inf'. 
dist[! bins_in_camera_regions] <- Inf
dist

## To calculate the inverse distances (and therefore get weighted bin density),
## use 1/dist. This means all the 'Inf' values will be read as 0. 
## Then I did  a sum of all the bins for each camera, and that number gives an
## indication of how close the bins are. 
inverse_distance_weighted <- rowSums(1/dist)

## Each bin that's within the region has a number for each camera. The number is 
## bigger depending on the closeness of the bin. The larger the number, the higher
## the bin density with in the camera. 

## Plot it all together
plot(inverse_distance_weighted ~ bin_counts)
inverse_distance_weighted

## Create a dataframe for bin density at every camera:
bin_200m <- data.frame(camera_ID = cameras$Name,
                       bin_density_200m = bin_counts,
                       inverse_bin_dens_200m = inverse_distance_weighted)
write.csv(bin_200m, "C:/Users/kewel/OneDrive/Documents/ICL/Project/Data/MAP STUFF/bins_within_200m.csv", row.names = FALSE)

## this csv file was used later to combine all the speed, count ahd habitat data
## manually, using Excel.