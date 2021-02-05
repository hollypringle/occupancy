#Correct incorrect dates in metadata
meta2<-meta
meta2$Date<-as.Date(meta2$Date)
library(lubridate)
year(meta2$Date)

#MT48 mislabelled as 2017
MT48subset<-subset(meta2, CT_site == "MT48")
year(MT48subset$Date)
MT48subset$Date<-as.Date(MT48subset$Date)
MT48subset$DateTime<-as.Date(MT48subset$DateTime)
MT48subset$Date<-MT48subset$Date + lubridate::years(1)
MT48subset$DateTime<-MT48subset$DateTime + lubridate::years(1)
head(MT48subset)

#NB47 November subset mislabelled as October
NB47subset<-subset(meta2, CT_site == "NB47")
NB47subsetnovtest<-subset(NB47subsetest, month == "november")
year(NB47subsetnov$Date)
monthstest<-month(NB47subsetnovtest$Date)
head(monthstest)
monthstest<-as.data.frame(monthstest)
months11sub<-subset(monthstest, monthstest=="11")
months10sub<-subset(monthstest, monthstest=="10")
NB47subsetnov$Date<-as.Date(NB47subsetnov$Date)
NB47subsetnov$DateTime<-as.Date(NB47subsetnov$DateTime)
month(NB47subsetnov$Date) <- 11
month(NB47subsetnov$DateTime) <- 11
head(NB47subsetnov)
unique(NB47subsetnov$Date)

correcteddates<-rbind(MT48subset,NB47subsetnov)
correcteddates$DateTime<-as.POSIXct(paste(correcteddates$Date, correcteddates$Time), format="%Y-%m-%d %H:%M:%S")
removedcorrectedmeta2<- anti_join(meta2, correcteddates,by = "ImageID") 
N.img.removedcorrectedmeta2<- length(unique(removedcorrectedmeta2$ImageID)) 
N.img.correcteddates<- length(unique(correcteddates$ImageID)) 
N.img.meta2<- length(unique(meta2$ImageID)) 
correctedmeta2dates <- bind_rows(removedcorrectedmeta2,correcteddates) 
N.img.correctedmeta2dates<- length(unique(correctedmeta2dates$ImageID)) 
write.csv(correctedmeta2dates, "Kenya_CT2018_metadata_correcteddates.csv")

#Checking
head(removedcorrectedmeta2)
head(correcteddates)
removedcorrectedmeta2$Date<-as.Date(removedcorrectedmeta2$Date)
removedcorrectedmeta2$DateTime<-as.POSIXct(removedcorrectedmeta2$DateTime, format="%Y-%m-%d %H:%M:%S")
setdiff(correctedmeta2dates, meta2)
correctedmeta2dates$Date<-as.character(correctedmeta2dates$Date)
correctedmeta2dates$DateTime<-as.character(correctedmeta2dates$DateTime, format="%Y-%m-%d %H:%M:%S")

#2674 difference, 117 the same. Not all of NB subset was wrong

library(dplyr)

imagesize_MN_df<-rename(imagesize_MN_df, FilePath=imagesize__MN) #double underscore...
imagesize_NB_df<-rename(imagesize_NB_df, FilePath=imagesize_NB)
imagelist<-rbind(imagesize_MN_df,imagesize_OMC_df)
imagelist$ImageHeight<-1512 #add default image dimensions  - all that were checked manually were default size
imagelist$ImageWidth<-2688
MTNB_dims<-rename(MTNB_dims, FilePath=SourceFile)
imagelist_dimensions<-rbind(imagelist, MTNB_dims) #2357443 #14 empty images removed from NB and MT 
write.csv(imagelist, "imagelist_dimensions.csv")

totalimagelist<-rbind(imagesize_MN_df,imagesize_OMC_df, imagesize_MT_df, imagesize_NB_df) #2357457 images

imagelist_dimensions$ImageID<-sub(".*/2018/","",imagelist_dimensions$FilePath) #Get image ID
library(stringr)


imagelist_dimensions<-rename(imagelist_dimensions, FilePath_corrected=FilePath)

meta<-read.csv("Kenya_CT2018_metadata_correcteddates.csv") #2357374-

mismatchIMG<-anti_join(imagelist_dimensions,meta, by ="ImageID") # empty files are missing from meta- good
imagelist_dimensions$ImageID<-as.factor(imagelist_dimensions$ImageID)
meta_withdims<-merge(meta, imagelist_dimensions, by="ImageID")

meta_withdims$filepathcheck<- mapply(grepl, meta_withdims$ImageID,meta_withdims$FilePath_corrected)
meta_withdims<-rename(meta_withdims, FilePath_old=FilePath)
write.csv(meta_withdims, "Kenya_CT2018_metadata_withdims.csv")

