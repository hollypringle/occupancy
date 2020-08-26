# Load needed packages
library(raster)
library(rgdal)
library(dplyr)
library(maptools)
library(ggplot2)

# set working directory
setwd("G:/Landsat")

# import the centroid data and the vegetation structure data
# this means all strings of letter coming in will remain character
#options(stringsAsFactors=FALSE)

#Read in plot centroids
#sites <- read.csv("Camera_locations.csv")
#sites_df<-as.data.frame(sites)
#str(sites)
#head(sites)

#Read in buffers
buffer500<- shapefile("G:/Landsat/500m_buffer.shp")
dim(buffer500)
plot(buffer500)

#Load sat images
blue = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B2.TIF")
green = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B3.TIF")
red = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B4.TIF")
rgb = stack(red, green, blue)
plotRGB(rgb, stretch='hist')
infrared = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B5.TIF")

# Perform raster algebra to calculate a raster of NDVI values
ndvi = (infrared - red) / (infrared + red)
str(ndvi)
ndvi@crs

#Extract mean ndvi for each 500m buffer around CT site
ext_mean<-extract(ndvi,buffer500, fun=mean)
summary(ext_mean)
head(ext_mean)

#Extract min ndvi for each 500m buffer around CT site
ext_min<-extract(ndvi,buffer500, fun=min)
summary(ext_min)
head(ext_min)

#Extract max ndvi for each 500m buffer around CT site
ext_max<-extract(ndvi,buffer500, fun=max)
summary(ext_max)
head(ext_max)

#Create column with site names
sitenames<-buffer500$site
summary(sitenames)

#Combine site name and mean NDVI column
NDVI_mean_500mbuffer<-cbind(sitenames, ext_mean)
NDVI_mean_500mbuffer<-as.data.frame(NDVI_mean_500mbuffer)
colnames(NDVI_mean_500mbuffer) <- c("Site", "Mean NDVI")
head(NDVI_mean_500mbuffer)

write.csv(NDVI_mean_500mbuffer, "NDVI_mean_500m.csv")



















