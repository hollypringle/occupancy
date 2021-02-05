### Part 1 : Final metadata checks and filling in the gaps ###

#Load required packages:
library(tidyr)
library(tidyverse)
library(dplyr)
library(beepr)
library(readbulk)

#Load in the complete metadata and check number of images, tags etc.
FullMetadata<-read.csv("FullMetadata_2018_VNov.csv") #List of tags from VOTT outputs with metadata
N.img.full <- length(unique(FullMetadata$image)) # number of images tagged
N.labels.full <- length(unique(FullMetadata$label)) # number of different tags
list.labels <- sort(unique(FullMetadata$label)) # back engineering the list of tags to double check

#In total, there are `r N.img.full` images from 2018 after including 5 minute intervals. 62411 were already tagged prior to this November round, so we should have 69716 in the new subset at the end of this. 
#The number of different tags is `r N.labels.full`

#Here are the labels:
`r list.labels`

#There are some typos and incorrect tags:
FullMetadata$label <- sub(pattern="ververt_monkey", replace="vervet_monkey",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="domestic_dog ", replace="domestic_dog",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="hyaena_spotted", replace="hyena_spotted",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="juvenille", replace="juvenile",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="other_rodent", replace="other_rodents",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="other_rodentss", replace="other_rodents",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="southern_groundhornbill", replace="southern_ground_hornbill",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="spring_hare", replace="springhare",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="unidentified ", replace="unidentified",FullMetadata$label) # correcting typo
FullMetadata$label <- sub(pattern="vechicle", replace="vehicle",FullMetadata$label) # correcting typo
FullMetadata$label<-gsub("\\s*\\([^\\)]+\\)","blue",as.character(FullMetadata$label))
FullMetadata$label <- sub(pattern="cblue", replace="elephant",FullMetadata$label)
list.labels.corrected<-sort(unique(FullMetadata$label))

#Now we have:
`r list.labels.corrected`

#OMC3 has no VOTT rectangle coordinates at the moment. We can extract these from the JSONs and add them to the metadata. This is only because the tags were extracted directly from the JSONs. If the CSV is exported from VOTT, we shouldn't need to do this.
library(dplyr) #bind_rows
library(here)
library(lubridate)
library(knitr)
library(stringr)
library(taxize)
library(profvis)
library(jsonlite)
library(tidyr)
library(stringr)
library(pbapply)

#OMC3listfiles <- list.files("D:/OMC3_output", recursive= TRUE, full.names = TRUE, pattern = "*asset.json") #directory where JSONs are stored

###JSON extraction function - this can be reused for each folder###
json_extract<-function(json_file){
  jsin<-jsonlite::fromJSON(json_file) 
  ImageID<-jsin$asset$name
  ImageWidth<-jsin$asset$size$width
  ImageHeight<-jsin$asset$size$height
  # TagID<-gsub("-asset.json", "",basename(json_file))
  
  json_loop_out<-NULL
  for (i in 1:nrow(jsin$regions)){  #length(jsin$regions)){
    
    CommonName<-as.character(jsin$regions$tags[[i]]) # (jsin$regions[[i]]$tags) sometimes works if this doesn't
    box_id<-jsin$regions$id[i]
    box_width <- jsin$regions$boundingBox$width[i]
    box_height<-jsin$regions$boundingBox$height[i]
    xmin<-min(jsin$regions$points[[i]]$x)
    ymin<-min(jsin$regions$points[[i]]$y)
    xmax<-max(jsin$regions$points[[i]]$x)
    ymax<-max(jsin$regions$points[[i]]$y)
    
    
    jlo_out<-data.frame(CommonName,box_id, box_width, box_height, xmin, ymin, xmax, ymax)
    json_loop_out<-rbind(json_loop_out, jlo_out)
  }
  
  #TagID<-paste(TagID, json_loop_out$box_id, sep = "_")
  
  json_out<-data.frame( ImageID = ImageID, ImageWidth = ImageWidth, ImageHeight = ImageHeight, json_loop_out, JSON_filepath = json_file)
  return(json_out)
}

json_OMC3 <- pblapply(OMC3listfiles,json_extract) 
df_OMC3<- do.call("rbind", json_OMC3)
write.csv(df_OMC3, "OMC3_coords.csv")

#add the coordinates to the metadata:
df_OMC3<-read.csv("OMC3_coords.csv")
df_OMC3 <- rename(df_OMC3, image = ImageID)
FullMetadata$xmin <- ifelse(is.na(FullMetadata$xmin), df_OMC3$xmin, FullMetadata$xmin)
FullMetadata$xmax <- ifelse(is.na(FullMetadata$xmax), df_OMC3$xmax, FullMetadata$xmax)
FullMetadata$ymin <- ifelse(is.na(FullMetadata$ymin), df_OMC3$ymin, FullMetadata$ymin)
FullMetadata$ymax <- ifelse(is.na(FullMetadata$ymax), df_OMC3$ymax, FullMetadata$ymax)
head(FullMetadata)

#Checking that OMC3 now has coordinates:
OMC3subset<-subset(FullMetadata, tagger =="liam"& stage=="nov"&ConservancyID=="OMC")
head(OMC3subset)

#To calculate the number of images to check per species per tagger, need to create a column which joins the label to the tagger:
FullMetadata <- unite(data=FullMetadata, # dataframe
                      col="label_tagger", #name of the new col
                      c("label", "tagger"), # cols to be joined
                      sep="_", remove=FALSE)
head(FullMetadata)
label.tagger <- as.data.frame(sort(table(FullMetadata$label_tagger))) # freq of unique label+tagger
tail(label.tagger)
N.labtag <- length(unique(FullMetadata$label_tagger))
label.tagger[label.tagger$Freq>100,]  ### just exploring data

#need to create a column with two distinct time periods (night and day) using proportional time:
str(FullMetadata)
FullMetadata$Period <- FullMetadata$time_prop 
FullMetadata$Period[FullMetadata$Period <0.25 | FullMetadata$Period >=0.75] <- "night" 
FullMetadata$Period[FullMetadata$Period >=0.25 & FullMetadata$Period <0.75] <- "day"
unique(FullMetadata$Period)

#There appear to be some NAs....
which(is.na(FullMetadata$Period), arr.ind=TRUE) 
# rows 153463:153465 are NA, need to understand why
check.na <- FullMetadata[c(153463:153465),]
# these are the files with NA. All 3 are images with humans without metadata, so they can be excluded from DF. NB: They all have the same image file name
FullMetadata <- FullMetadata[-c(153463:153465),] 
unique(FullMetadata$Period)
N.img.check <- length(unique(FullMetadata$image)) 


Now there are `r N.img.check` images from 2018 after including 5 minute intervals and removing these 3 rows. Only one in total has been removed since all 3 tags were in the same file.

#Create a column with unique ID with spp_tagger_period
FullMetadata <- unite(data=FullMetadata, # dataframe
                      col="label_tagger_period", #name of the new col
                      c("label_tagger", "Period"), # cols to be joined
                      sep="_", remove=FALSE) # keep original cols
head(FullMetadata)

### Part 2 ###
#Now, generate a list of image to be checked. We only want to check the most recent subset as the first batch of data has already been checked by Emily.
NewSubset<-subset(FullMetadata,stage=="nov")
N.img.NewSub <- length(unique(NewSubset$image)) # number of images tagged
write.csv(NewSubset, "2018Subset_NovemberStage.csv")

#The new subset contains `r N.img.NewSub` images. Add this to the 62410 images(after removing the image with NAs for dates)  that were already checked and we have the total of `r N.img.check` . Yay, it all adds up!
#Duplicates have to be removed at this stage to avoid clash later on between image sample size and tag sample size (causes replacement to be needed).
#Extra rows for images with more than one individual of a species will be removed, as we may end up checking the same image more than once. However, there will still be one row for each image left.
#We will remove ALL files with more than one species in, as we will not be able to tell which species has the label. For example, if an image has a Grant's and Thomson's gazelle in, we will be unable to tell which animal a tag for 'Grant's' is referring to.
removedduplicates<-NewSubset %>% distinct(image , label_tagger, .keep_all = TRUE) 
removedmultispecies<-removedduplicates[!(duplicated(removedduplicates$image) | rev(duplicated(rev(removedduplicates$image)))), ]

#Now calculate the sample sizes to be checked. 
#10% of images per tag per tagger are sub-setted with a max cap of 100, so for 5 taggers who all tagged 1000+ images of wildebeest a total of 500 wildebeest images are selected. Additionally, a lower cap of 10 is used for species where 10% of images of a species a tagger tagged was less than 10. If a tagger tagged less than 10 images of a species then all the images were selected. 
totalstable<-table(removedmultispecies$label,removedmultispecies$tagger)
totalsmatrix<-as.data.frame.matrix(totalstable)
totals10<-apply(totalsmatrix, 2, function(x) ifelse(x < 10, x, x*0.1)) #convert to 10% if larger than 10 #perhaps simpler: totals10<-totalsmatrix*0.1 #convert to 10%
totalsrounded<-ceiling(totals10) #round to whole number
maxcap<-apply(totalsrounded, 2, function(x) ifelse(x > 100, 100, x)) #add upper cap of 100
mincap<-apply(maxcap, 2, function(x) ifelse(x > 0 & x < 10, 10, x)) #add lower cap of 10
samplesizes <- ifelse(totalstable < 10, totalstable, mincap) #if total is less than 10, check all images and match to original metadata count
samplesizes <- cbind(rownames(samplesizes), data.frame(samplesizes, row.names=NULL))
head(samplesizes)
dfsample<-gather(samplesizes, key="tagger",value= "samplesize", emily:taras, na.rm = FALSE, convert = FALSE)
dfsample <- unite(data=dfsample, # dataframe #adding label_tagger column
                  col="label_tagger", #name of the new col
                  c("rownames(samplesizes)", "tagger"), # cols to be joined
                  sep="_", remove=FALSE) 
head(dfsample)

#The total number of images to check for accuracy is `r sum(dfsample$samplesize)`. To generate file list, we need to incorporate these sample sizes with the metadata. This will randomly select the correct number of files per tagger+species.
removedmultispecies$samplesize <- dfsample$samplesize[match(removedmultispecies$label_tagger, dfsample$label_tagger)] #add samplesize column to original metadata
head(removedmultispecies)
filestocheck <- removedmultispecies %>% group_by(label_tagger) %>% sample_n(samplesize, replace=FALSE) 
write.csv(filestocheck, "filestocheck.csv")
head(filestocheck)

#We now have a full sample of the images we need to check for accuracy. Next we need to create multiple folder for the different species images to be checked.
### Moving 'other' species ###
#Load in the November Subset metadata
NovSubset<-read.csv("2018Subset_NovemberStage.csv")
N.img.NovSubset<- length(unique(NovSubset$image))

#We need to copy over species such as 'other_mongoose' and 'other_bird' so we can identify them at a later stage.
head(NovSubset)
birdpath <- NovSubset[(NovSubset$label=="other_bird"), "FilePath"]  # file paths for all other birds for retagging
head(birdpath)

#Next, eliminate duplicated files - i.e. photos with more than 1 tag
birdpath <- unique(birdpath)

#We need to create a folder to check files. First a general folder to paste all files we will eventually check:
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/", 
                  "checking_tags"))

#And now a folder for birds only:
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "other_birds"))
file.copy(birdpath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/other_birds", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE) 

#Check number of images in folder is expected:
birdsubset<-subset(NovSubset, label =="other_bird")
N.img.birdsubset<- length(unique(birdsubset$image))

#There are `r N.img.birdsubset` in the metadata, and this matched with the number of files in the folder to check. Nice.
#Do the same for unidentified:
noidpath <- NovSubset[(NovSubset$label=="unidentified"), "FilePath"]  # file paths for all unidentified
head(noidpath)
# eliminating duplicated files - i.e. photos with more than 1 tag
noidpath <- unique(noidpath)
# creating folder to paste unidentified files
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "unidentified"))
file.copy(noidpath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/unidentified", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)
#Check number of images in folder is expected:
noidsubset<-subset(NovSubset, label =="unidentified")
N.img.noid<- length(unique(noidsubset$image))

#There are `r N.img.noid` in the metadata, and this matched with the number of files in the folder to check.

#And for mongoose:
mongoosepath <- NovSubset[(NovSubset$label=="mongoose_other"), "FilePath"]  # file paths for all mongoose
head(mongoosepath)
mongoosepath <- unique(mongoosepath)
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "mongoose_other"))
file.copy(mongoosepath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/mongoose_other", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

#And query
querypath <- NovSubset[(NovSubset$label=="query"), "FilePath"]  # file paths for all mongoose
head(querypath)
querypath <- unique(querypath)
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "query"))
file.copy(querypath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/query", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)
querysubset<-subset(NovSubset, label =="query")
N.img.query<- length(unique(querysubset$image))

#Next we need to create multiple folder for the different species images to be checked. Create subfolders in the directory according to label names:
foldername_list <- read.csv("tagging_check_foldernames_Nov.csv", header = TRUE)
subfolder_names<- (foldername_list$folder_name) 
for (j in 1:length(subfolder_names)){
  folder<-dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",subfolder_names[j]))
}

#Load in the "filestocheck" csv from the Checking Part 1. This is a spreadsheet which contains the list of random files we need to check to calculate accuracy.
file_list <- read.csv("filestocheck.csv", header = TRUE)
head(file_list)

#Make sure the list of tags look right:
list.labels.check <- sort(unique(filestocheck$label))
list.labels.check

#Add a column of the target diectory to the list of images to be checked.
file_list$targetpath <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/"
file_list$fulltargetpath <- paste0(file_list$targetpath,file_list$label)
head(file_list)

#Then move images into species folders 
#[i,21] is the current file pathway
#[i,28] is the target pathway
#[i,3] is the image name
for (i in 1:nrow(file_list)) {
  file.copy(from = paste0(file_list[i,21]), 
            to=paste0(file_list[i,28], "/", file_list[i,3]), overwrite = TRUE, recursive = FALSE, copy.date=TRUE) } 
beep(sound = 1, expr = NULL)
for (j in 1:length(subfolder_names)){
  folder<-dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/outputs/",subfolder_names[j],"_output"))
} #create output folders

#Now assess the checked tags.
#NB: Make sure to add both 'misidentified' and 'missed' labels. I forgot to add 'misdentified' labels to half of them and had to manually add in excel after :( not ideal :(
#Find labels only captured in multispecies images- these will need to be pulled out separately.
check = as.data.frame(setdiff(NovSubset$label_tagger, file_list$label_tagger)) #
colnames(check)[colnames(check) == "setdiff(NovSubset$label_tagger, file_list$label_tagger)"]<- "label_tagger"
multispeciescheck <-subset(NovSubset, label_tagger %in% check$label_tagger)
N.img.multi <- length(unique(multispeciescheck$image))
list.labels.multi <- sort(unique(multispeciescheck$label)) # back engineering the list of tags to double check

#At this point I realised 6 images were just tagged 'animal' (by me, oops)- a leftover from the trial machine learning tags . These need to be corrected.
animalremoved<-NovSubset[!(NovSubset$label=="animal"),]
N.img.animalremoved<- length(unique(animalremoved$image))
commonimage<-intersect(NovSubset$image,animalremoved$image)
animalremoved.retag<-NovSubset[!NovSubset$image %in% commonimage,]
animalremoved.retagpath <- animalremoved.retag[(animalremoved.retag$label=="animal"), "FilePath"]  # file paths for all unidentified
head(animalremoved.retagpath)

# eliminating duplicated files - i.e. photos with more than 1 tag
animalremoved.retagpath <- unique(animalremoved.retagpath)

# creating folder to paste unidentified files
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "animalremovedretag"))
file.copy(animalremoved.retagpath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/animalremovedretag", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

#Back to the multispecies images... I am removing 'juvenile'. It only appears with other species, but I am not going to check these now. I just want to check the species.
multijuvenileremoved<-multispeciescheck[!(multispeciescheck$label=="juvenile"),]
N.img.multijuvenileremoved <- length(unique(multijuvenileremoved$image))
list.labels.multi <- sort(unique(multispeciescheck$label)) # back engineering the list of tags to double check
list.labels.multijuvenileremoved <- sort(unique(multijuvenileremoved$label)) # back engineering the list of tags to double check
multipath <- multijuvenileremoved$FilePath #file paths for all multi species images
head(multipath)
multipath <- unique(multipath)
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "multijuvenileremoved"))
file.copy(multipath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/multijuvenileremoved", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

library(readbulk)
checkedtags2 <- read_bulk(directory = "D:/MaraData/Part2/checking_tags/outputs", subdirectories = TRUE, extension = "*.csv",data = NULL, verbose =TRUE, fun = utils::read.csv)
head(checkedtags2)
summary(checkedtags2)  # check overall proportion of "n" tags 
N.img.rcheckedtags2<- length(unique(checkedtags2$image)) #2284

#The total number of checked images is 2274. This is because I did not check birds, and I added the labels that were only found in multispecies images. Unidentified were pulled out separately and retagged, so not included in this check either. 
checkedtags_corrected<-read.csv("checked_tags_corrected.csv")
misidentified<-subset(checkedtags_corrected,new=="misidentified")
misidentified<-unique(misidentified)
misidentified[!duplicated(misidentified,[c('image')]),]

#Aardwolf, hartebeest and grants gazelle had low accuracy (more than 10% incorrect). Extract and check...
aardwolfpath <- NovSubset[(NovSubset$label=="aardwolf"), "FilePath"]  # file paths for all unidentified
head(aardwolfpath)

# eliminating duplicated files - i.e. photos with more than 1 tag
aardwolfpath <- unique(aardwolfpath)

# creating folder to paste unidentified files
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "aardwolf_retag"))
file.copy(aardwolfpath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/aardwolf_retag", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

gazelle_grantspath <- NovSubset[(NovSubset$label=="gazelle_grants"), "FilePath"]  # file paths for all unidentified
head(gazelle_grantspath)

# eliminating duplicated files - i.e. photos with more than 1 tag
gazelle_grantspath <- unique(gazelle_grantspath)

# creating folder to paste unidentified files
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "gazelle_grants_retag"))
file.copy(gazelle_grantspath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/gazelle_grants_retag", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

hartebeest_cokespath <- NovSubset[(NovSubset$label=="hartebeest_cokes"), "FilePath"] 
head(hartebeest_cokespath)

# eliminating duplicated files - i.e. photos with more than 1 tag
hartebeest_cokespath <- unique(hartebeest_cokespath)

# creating folder to paste unidentified files
dir.create(paste0("//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/",
                  "hartebeest_cokes_retag"))
file.copy(hartebeest_cokespath, 
          to= "//live.rd.ucl.ac.uk/ritd-ag-project-rd00lk-kejon62/biome_health_project_files/country_files/kenya/tagging_photos/5_minute_2018/NEW/checking_tags/hartebeest_cokes_retag", 
          overwrite= FALSE,  recursive = TRUE, copy.date=TRUE)

# have to do the next bit in 3 steps, since sometimes a retag could be in the same image as an unidentified or query, so they would be duplicated otherwise...

#1) Now put all unidentified into one csv:
library(readbulk)
unidentified <- read_bulk(directory = "D:/MaraData/Part2/unidentified", subdirectories = TRUE, extension = "*.csv",data = NULL, verbose =TRUE, fun = utils::read.csv)
head(unidentified)
summary(unidentified)  

#2) Now all query
query<- read_bulk(directory = "D:/MaraData/Part2/query", subdirectories = TRUE, extension = "*.csv",data = NULL, verbose =TRUE, fun = utils::read.csv)
head(query)
summary(query)  # check overall proportion of "n" tags 

#3) Now all retags. 
retag <- read_bulk(directory = "D:/MaraData/Part2/retag", subdirectories = TRUE, extension = "*.csv",data = NULL, verbose =TRUE, fun = utils::read.csv)
head(retag)
summary(retag)  # check overall proportion of "n" tags 

#Replace old tags with new unidentified
old<-NovSubset
newnoid<-unidentified
removednoid <- anti_join(old, newnoid, by = "image") #removing the old unidentified tags
N.img.removednoid<- length(unique(removednoid$image)) #no of images when unidentigied are removed #67700
N.img.newnoid<- length(unique(newnoid$image)) #2016
newnoid$stage <- c("novretag") #change stage name
meta <- read.csv("Kenya_CT2018_metadata.csv")
correctednewnoid <- merge(newnoid,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewnoid$X<-NULL
head(correctednewnoid)
correctednoid <- bind_rows(removednoid,correctednewnoid) #combine the old and new unidentified
N.img.correctednoid<- length(unique(correctednoid$image))

#Now there are `r N.img.correctednoid` , which matches the original subset. Now to add the corrected queries #to this new dataframe.
newquery<-query
removedquery <- anti_join(correctednoid, newquery, by = "image") #removing the old unidentified tags
N.img.removedquery<- length(unique(removedquery$image)) #no of images when unidentigied are removed #69659
N.img.newquery<- length(unique(newquery$image)) #57
newquery$stage <- c("novretag") #change stage name
correctednewquery<- merge(newquery,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewquery$X<-NULL
head(correctednewquery)
correctedquery <- bind_rows(removedquery,correctednewquery) #combine the old and new unidentified
N.img.correctedquery<- length(unique(correctedquery$image))

#Now there are `r N.img.correctedquery` , which matches the original subset. Now to add the retags to this new #dataframe.
newretag<-retag
removedretag <- anti_join(correctedquery, newretag, by = "image") #removing the old unidentified tags
N.img.removedretag<- length(unique(removedretag$image)) #no of images when unidentigied are removed #69536
N.img.newretag<- length(unique(newretag$image)) #180
newretag$stage <- c("novretag") #change stage name
correctednewretag<- merge(newretag,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewretag$X<-NULL
correctednewretag$Subdirectory<-NULL
correctednewretag$File<-NULL
head(correctednewretag)
correctedretag <- bind_rows(removedretag,correctednewretag) #combine the old and new unidentified
N.img.correctedretag<- length(unique(correctedretag$image)) #69716

#Now there are `r N.img.correctedretag` , which matches the original subset
#Labelling ones I've retagged and binding to original metadata:
correctedNovV3<-correctedretag
write.csv(correctedNovV3, "DecMetadataNovV3.csv")

#Make sure the list of tags look right:
list.labels.check.correctedNovV3 <- sort(unique(correctedNovV3$label))
list.labels.check.correctedNovV3

list.labels.check.Nov <- sort(unique(NovSubset$label))
list.labels.check.Nov

#Add to previously tagged metadata:
removedfullmeta3 <- anti_join(FullMetadata, correctedNovV3, by = "image")
N.img.removedfullmeta3<- length(unique(removedfullmeta3$image)) #62410
correctedfullmetaV3 <- bind_rows(removedfullmeta3,correctedNovV3) #combine the old and new
correctedfullmetaV3$X.1<-NULL
head(correctedfullmetaV3)

#Correct missing data from retag subset
retagsubset.correctedfullmetaV3<-subset(correctedfullmetaV3, stage =="novretag")
retagsubset.correctedfullmetaV3$tagger<-c("holly")
head(retagsubset.correctedfullmetaV3)
N.img.retagsubset.correctedfullmetaV3<- length(unique(retagsubset.correctedfullmetaV3$image)) #2220
retagsubset.correctedfullmetaV3$Period <- retagsubset.correctedfullmetaV3$time_prop 
retagsubset.correctedfullmetaV3$Period[retagsubset.correctedfullmetaV3$Period <0.25 | retagsubset.correctedfullmetaV3$Period >=0.75] <- "night" retagsubset.correctedfullmetaV3$Period[retagsubset.correctedfullmetaV3$Period >=0.25 & retagsubset.correctedfullmetaV3$Period <0.75] <- "day"
unique(retagsubset.correctedfullmetaV3$Period)

retagsubset.correctedfullmetaV3 <- unite(data=retagsubset.correctedfullmetaV3, # dataframe
                                         col="label_tagger", #name of the new col
                                         c("label", "tagger"), # cols to be joined
                                         sep="_", remove=FALSE)

retagsubset.correctedfullmetaV3 <- unite(data=retagsubset.correctedfullmetaV3, # dataframe
                                         col="label_tagger_period", #name of the new col
                                         c("label_tagger", "Period"), # cols to be joined
                                         sep="_", remove=FALSE) 
head(retagsubset.correctedfullmetaV3)
removedfullmetav5 <- anti_join(correctedfullmetaV3, retagsubset.correctedfullmetaV3, by = "image")
N.img.removedfullmetav5<- length(unique(removedfullmetav5$image)) #129906
correctedfullmetaV5 <- bind_rows(removedfullmetav5,retagsubset.correctedfullmetaV3) #combine the old and new
N.img.correctedfullmetaV5<-length(unique(correctedfullmetaV5$image)) #132126
head(correctedfullmetaV5) ##############################################
write.csv(correctedfullmetaV5, "DecFullMetadataCorrectedV5.csv")

### Final file is renamed as FullMetadata_2018_VDECEMBER_FINAL on server.###
metadata<-read.csv("FullMetadata_2018_VDECEMBER_FINAL.csv")

###DECEMBER CHECkED TAGS
checkedtagscorrect<-read.csv("checked_tagsDECEMBER.csv")

#Remove aardwolf, hartebeest, 'animalremoved' and grants gazelle, as we have already added the retagged ones of these,. 
wrongfile_detail <- anti_join(checkedtagscorrect, correctfiles, by = "image")
wrongfile_detail<-wrongfile_detail[!grepl("aardwolf", wrongfile_detail$Original),]
wrongfile_detail<-wrongfile_detail[!grepl("hartebeest_cokes", wrongfile_detail$Original),]
wrongfile_detail<-wrongfile_detail[!grepl("gazelle_grants", wrongfile_detail$Original),]
wrongfile_detail<-wrongfile_detail[!grepl("animalremoved", wrongfile_detail$Original),]

#Remove extra tags
wrongfile_detail<-wrongfile_detail[!grepl("n", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("missed", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("misidentified", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("other_bird", wrongfile_detail$new),]
wrongfile_detail<-wrongfile_detail[!grepl("misidentified", wrongfile_detail$new),]
N.img.wrongfile_detail<-length(unique(wrongfile_detail$image)) #14
newwrongfile<-wrongfile_detail
N.img.newwrongfile<- length(unique(newwrongfile$image)) #14
newwrongfile$stage <- c("novretag") #change stage name
newwrongfile$T.F<-NULL
newwrongfile$Original<-NULL
newwrongfile <- newwrongfile %>% rename(label = new) 
correctednewwrongfile<- merge(newwrongfile,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewwrongfile$X<-NULL
correctednewwrongfile$Subdirectory<-NULL
correctednewwrongfile$File<-NULL
correctednewwrongfile$count<-NULL
head(correctednewwrongfile)
N.img.correctednewwrongfile<- length(unique(correctednewwrongfile$image)) #14

correctednewwrongfile$tagger<-c("holly")
head(correctednewwrongfile)
N.img.correctednewwrongfile<- length(unique(correctednewwrongfile$image)) #2220
correctednewwrongfile$Period <- correctednewwrongfile$time_prop 
correctednewwrongfile$Period[correctednewwrongfile$Period <0.25 | correctednewwrongfile$Period >=0.75] <- "night" 
correctednewwrongfile$Period[correctednewwrongfile$Period >=0.25 & correctednewwrongfile$Period <0.75] <- "day"
unique(correctednewwrongfile$Period)

correctednewwrongfile <- unite(data=correctednewwrongfile, # dataframe
                               col="label_tagger", #name of the new col
                               c("label", "tagger"), # cols to be joined
                               sep="_", remove=FALSE)

correctednewwrongfile <- unite(data=correctednewwrongfile, # dataframe
                               col="label_tagger_period", #name of the new col
                               c("label_tagger", "Period"), # cols to be joined
                               sep="_", remove=FALSE) 
head(correctednewwrongfile)
```

###EMILY CHECkED TAGS
checkedtagsemily<-read.csv("checked_tags_EMILY.csv")

#Remove species we have already retagged . 
wrongfile_detail.e <- anti_join(checkedtagsemily, correctfiles.e, by = "image")
wrongfile_detail.e<-wrongfile_detail.e[!grepl("aardvark", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("aardwolf", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("bateared_fox", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("caracal", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("gazelle_grants", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("honey_badger", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("hyena_striped", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("leopard", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("mongoose_banded", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("mongoose_other", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("oribi", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("reedbuck", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("secretary_bird", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("springhare", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("unidentified", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("wildcat", wrongfile_detail.e$original),]
wrongfile_detail.e<-wrongfile_detail.e[!grepl("n", wrongfile_detail.e$new),]
N.img.wrongfile_detail.e<-length(unique(wrongfile_detail.e$image)) #42
newwrongfile.e<-wrongfile_detail.e
N.img.newwrongfile.e<- length(unique(newwrongfile.e$image)) #42
newwrongfile.e$stage <- c("novretag_emily") #change stage name
newwrongfile.e$T.F<-NULL
newwrongfile.e$original<-NULL
newwrongfile.e$count<-NULL
newwrongfile.e <- newwrongfile.e %>% rename(label = new) 
correctednewwrongfile.e<- merge(newwrongfile.e,meta, by.x="image", by.y="ImageID", all.x=T)
correctednewwrongfile.e$X<-NULL
correctednewwrongfile.e$Subdirectory<-NULL
correctednewwrongfile.e$File<-NULL
head(correctednewwrongfile.e)

correctednewwrongfile.e$tagger<-c("emily")
head(correctednewwrongfile.e)
N.img.correctednewwrongfile.e<- length(unique(correctednewwrongfile.e$image)) #2220
correctednewwrongfile.e$Period <- correctednewwrongfile.e$time_prop 
correctednewwrongfile.e$Period[correctednewwrongfile.e$Period <0.25 | correctednewwrongfile.e$Period >=0.75] <- "night" 
correctednewwrongfile.e$Period[correctednewwrongfile.e$Period >=0.25 & correctednewwrongfile.e$Period <0.75] <- "day"
unique(correctednewwrongfile.e$Period)

correctednewwrongfile.e <- unite(data=correctednewwrongfile.e, # dataframe
                                 col="label_tagger", #name of the new col
                                 c("label", "tagger"), # cols to be joined
                                 sep="_", remove=FALSE)

correctednewwrongfile.e <- unite(data=correctednewwrongfile.e, # dataframe
                                 col="label_tagger_period", #name of the new col
                                 c("label_tagger", "Period"), # cols to be joined
                                 sep="_", remove=FALSE) 
head(correctednewwrongfile.e)

###Combine and add to main metadata
allcorrectedchecks <- bind_rows(correctednewwrongfile.e,correctednewwrongfile)
write.csv(correctednewwrongfile, "correctedchecks_second.csv")
head(allcorrectedchecks)
allcorrectedchecks$X.x<-NULL
allcorrectedchecks$X.y<-NULL
N.img.allcorrectedchecks<- length(unique(allcorrectedchecks$image)) #56
N.img.metadata<- length(unique(metadata$image)) #132126
removedfullmetachecks <- anti_join(metadata, allcorrectedchecks, by = "image")
N.img.removedfullmetachecks<- length(unique(removedfullmetachecks$image)) #132126

head(metadata)
metadata$X.1<-NULL
metadata$X<-NULL
N.img.metadata<- length(unique(metadata$image))

correctedfullmeta_final <- bind_rows(removedfullmetachecks,allcorrectedchecks)
N.img.correctedfullmeta_final<-length(unique(correctedfullmeta_final$image)) #132126
head(correctedfullmeta_final)

write.csv(correctedfullmeta_final, "Full2018Metadata_VDECEMBER_FINAL_WITHCHECKS.csv")

#Add metadata
finalmetadatadec<-read.csv("Full2018Metadata_VDECEMBER_FINAL_WITHCHECKS.csv")

finalmetadatadec$ConservancyID<-NULL
head(finalmetadatadec)

write.csv(finalmetadatadec, "Full2018Metadata_VDECEMBER_TAGSONLY.csv")
N.img.finalmetadatadec<- length(unique(finalmetadatadec$image))

meta<-read.csv("Kenya_CT2018_metadata.csv")
tagswithmeta<- merge(finalmetadatadec,meta, by.x="image", by.y="ImageID", all.x=T)
head(tagswithmeta)

tagswithmeta$X.x<-NULL
write.csv(tagswithmeta, "Full2018Metadata_VDECEMBER_TAGWITHMETA.csv")

#Create tags only file
tagwithmetadata<-read.csv("Full2018Metadata_VDECEMBER_TAGWITHMETA.csv")
head(tagwithmetadata)
tagwithmetadata$X<-NULL

write.csv(tagwithmetadata,"Full2018_tagsonly.csv")
nrow(tagwithmetadata)
N.img.tagwithmetadata <-length(unique(tagwithmetadata$image))

#title: "CorrectingDuplicatedFilepath"
#Load in metadata and tagged data

metajan<-read.csv("Kenya_CT2018_metadata_correcteddates.csv")
tagsjan<-read.csv("tagswithmetajan_occupancy.csv")

#Flag images in metadata where filepath doesn't match Image ID
metajan$flag <- mapply(grepl, metajan$ImageID,metajan$FilePath)

# add flagged duplicated filepaths to tags:

tagsjan$dupfilepath <- metajan$flag[match(tagsjan$image, metajan$ImageID)]
#dupfilepath: FALSE means filepath and file dont match

duplicatedfilepathtags<-subset(tagsjan, dupfilepath=="FALSE")
n.img.duplicatedfilepathtags<-length(unique(duplicatedfilepathtags$image))

#Table of sites (with no. of tags) which have filepaths that do not match image ID:

duplicatedfilepathtags5table <- table(duplicatedfilepathtags$Site)
duplicatedfilepathtags5table

#Remove flagged tags:
tagsjan_corrected<-tagsjan[!grepl("FALSE", tagsjan$dupfilepath),]
n.img.tagsjan<-length(unique(tagsjan$image))
n.img.tagsjan_corrected<-length(unique(tagsjan_corrected$image))

write.csv(tagsjan_corrected, "tagsjan_removed_duppath.csv")

#Remove rows which have been exactly duplicated:

morethan1_tag_coords<-tagsjan_corrected  %>% 
  group_by(image, xmin, ymin,xmax, ymax, tagger, stage, Species) %>% 
  mutate(dupecoord = n()>1)
  
morethan1tag_dupecoord<-subset(morethan1_tag_coords, dupecoord=="TRUE")
nrow(morethan1tag_dupecoord)  
n.img.morethan1tag_dupecoord<-length(unique(morethan1tag_dupecoord$image))
dedupedmorethan1tag_dupecoord<-morethan1tag_dupecoord%>%distinct(image, xmin, ymin,xmax, ymax, tagger, stage, Species, .keep_all=TRUE) #x distinguishes them  
n.img.dedupedmorethan1tag_dupecoord<-length(unique(dedupedmorethan1tag_dupecoord$image))
  
dedupedtagsjan<-tagsjan_corrected%>%distinct(image, xmin, ymin,xmax, ymax, tagger, stage, Species, .keep_all=TRUE) #x distinguishes them
nrow(dedupedtagsjan)  
n.img.dedupedtagsjan<-length(unique(dedupedtagsjan$image))

#77 duplicate rows removed, no images removed

#Remove boxes with more than 1 species tag
tagsjancorrected_juvremov<-dedupedtagsjan[!grepl("juvenile", dedupedtagsjan$Species),] 
tagsjancorrected_juvremov<-tagsjancorrected_juvremov[!grepl("animal", tagsjancorrected_juvremov$Species),]

library(dplyr)

tagsjan_dupelabel<- tagsjancorrected_juvremov  %>% 
  group_by(image, xmin, ymin,xmax, ymax) %>% 
  mutate(dupelabel = n()>1)
  
n.img.tagsjan_dupelabel<-length(unique(tagsjan_dupelabel$image))
morethan1tag_dupelabel<-subset(tagsjan_dupelabel, dupelabel=="TRUE")

length(unique(morethan1tag_dupelabel$image))
nrow(morethan1tag_dupelabel)
FinalTags_duplicatesremoved<-dedupedtagsjan[!(dedupedtagsjan$X %in% morethan1tag_dupelabel$X),]
length(unique(FinalTags_duplicatesremoved$image))
nrow(FinalTags_duplicatesremoved)

#Tidy columns:

write.csv(FinalTags_duplicatesremoved, "FinalTags_duplicatesremoved.csv")
FinalTags <- FinalTags_duplicatesremoved[,c(1:9, 18)]
FinalTags <- FinalTags %>%   rename(FilePath = FilePath.y)

write.csv(FinalTags, "FinalTags_Jan.csv")

#Reasons for removing rows:
RemovedRows<-anti_join(tagsjan, FinalTags, by=c("X"))
nrow(RemovedRows)
head(RemovedRows)
RemovedRows$ReasonRemoved <- ifelse(RemovedRows$dupfilepath %in% c("FALSE"), "incorrect_filepath", NA)

RemovedRows<- RemovedRows %>% 
  group_by(image, xmin, ymin,xmax, ymax) %>% 
  mutate(Two_Labels= n()>1)
  
RemovedRows$ReasonRemoved <- ifelse(RemovedRows$Two_Labels %in% c("TRUE"), "two_labels_same_box", RemovedRows$ReasonRemoved)
RemovedRows$ReasonRemoved <- ifelse(RemovedRows$ReasonRemoved %in% c(NA), "duplicated_row", RemovedRows$ReasonRemoved)


