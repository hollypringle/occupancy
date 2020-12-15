# Load needed packages
library(raster)
library(rgdal)
library(dplyr)
library(maptools)
library(ggplot2)

# set working directory
setwd("G:/Landsat")

#Read in site points
#options(stringsAsFactors=FALSE)
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

# Calculate a raster of NDVI values
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
NDVI_mean_500mbuffer<-cbind(sitenames, ext_mean, ext_min,ext_max)
NDVI_500mbuffer<-as.data.frame(NDVI_500mbuffer)
colnames(NDVI_500mbuffer) <- c("Site", "Mean NDVI", "Minimum NDVI", "Maximum NDVI")
head(NDVI_500mbuffer)

write.csv(NDVI_500mbuffer, "NDVI_500m.csv")



### scrap 

# Load needed packages
library(raster)
library(rgdal)
library(dplyr)

# Method 3:shapefiles
library(maptools)

# plotting
library(ggplot2)

dev.off()

# set working directory
setwd("G:/Landsat")

# import the centroid data and the vegetation structure data
# this means all strings of letter coming in will remain character
options(stringsAsFactors=FALSE)

# read in plot centroids
sites <- read.csv("Camera_locations.csv")
str(sites)
head(sites)

#Calculate NDVI
blue = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B2.TIF")
green = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B3.TIF")
red = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B4.TIF")
rgb = stack(red, green, blue)
plotRGB(rgb, stretch='hist')

# Load visible (red) and infrared bands
red = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B4.TIF")
infrared = raster("LC08_L1TP_169061_20190920_20190926_01_T1_B5.TIF")

# Perform raster algebra to calculate a raster of NDVI values
ndvi = (infrared - red) / (infrared + red)
str(ndvi)
ndvi@crs

#NDVI raster

writeRaster(x = ndvi,
            filename="G:/Landsat/ndvi_2019_GME_4.tif",
            format = "GTiff", # save as a tif
            datatype='INT2S', # save as a INTEGER rather than a float
            overwrite = TRUE)  # OPTIONAL - be careful. This will OVERWRITE previous files.

ndviraster<-raster("ndvi_2019_GME_4.tif")

sites_spdf <- SpatialPointsDataFrame(
  sites[,3:2], proj4string=ndviraster@crs, sites)
sites_spdf
plot(sites_spdf)
cent_max <- raster::extract(ndviraster,             # raster layer
                            coordinates,   # SPDF with centroids for buffer
                            buffer = 100,     # buffer size, units depend on CRS
                            fun=max,         # what to value to extract
                            df=TRUE)     
cent_max


#Extract NDVi for points
coordinates <- sites[, c("Longitude", "Latitude")]
coordinates(coordinates) <- ~Longitude + Latitude
proj4string(coordinates) <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84
+towgs84=0,0,0 ")


# Define shapefile's current CRS
projection(sites) <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
ndviraster@crs
coordinates@crs
plot(coordinates, pch=20, main="Full Dataset", axes=TRUE)















