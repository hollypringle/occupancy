MTOMC<-read.csv("MTOMC_FIXED.csv")
library(unmarked)
library(here)
library(dplyr)
library(data.table)
library(chron)

#checking consistency in labels used
head(MTOMC)

MTOMC <- rename(MTOMC, Site = CT_site)
MTOMC <- rename(MTOMC, Species = label)

sort(unique(MTOMC$Species))
unique(MTOMC$Site)
MTOMC$Species <- sub(pattern="hyaena_spotted", replace="hyena_spotted",MTOMC$Species) # correcting typo
sort(unique(MTOMC$Species))
MTOMC$Species <- sub(pattern="vechicle", replace="vehicle",MTOMC$Species) # correcting typo
sort(unique(MTOMC$Species))
MTOMC$Species <- sub(pattern="ververt_monkey", replace="vervet_monkey",MTOMC$Species) # correcting typo
sort(unique(MTOMC$Species))

MTOMC$Species<-gsub("\\s*\\([^\\)]+\\)","blue",as.character(MTOMC$Species)) #Get rid of any unwanted symbols #"Blue" is completely arbitrary- it's not even my favourite colour
MTOMC$Species <- sub(pattern="cblue", replace="elephant",MTOMC$Species) # correcting typo
sort(unique(MTOMC$Species))

MTOMC$DateTime<-sub("-","/",as.character(MTOMC$DateTime))
MTOMC$DateTime<-sub("-","/",as.character(MTOMC$DateTime))
MTOMC$DateTime<-sub("2017","2018",as.character(MTOMC$DateTime))
tail(MTOMC)

MTOMC$xmin <- NULL
MTOMC$xmax <- NULL
MTOMC$ymin <- NULL
MTOMC$ymax <- NULL
MTOMC$CT_id<-NULL

# renaming some cols in Data to match the functions below
MTOMC$DateTime <- as.Date(MTOMC$DateTime,"%Y/%m/%d") # making sure date col is Date

# loading matrix indicating whether camera trap site was working (1) or not (NA)
all_cams <- read.csv("all_sites_effort_MTOMC.csv") 
head(all_cams)
tail(all_cams)
str(all_cams)
all_cams$Date <- as.Date(all_cams$Date, "%d/%m/%Y") # making sure date is Date


sort(unique(all_cams$Date))
str(MTOMC)



startDate <- as.Date("2018/10/05","%Y/%m/%d")
endDate <- as.Date("2018/11/29","%Y/%m/%d")
calcOcc <-
  function(species, # species name - in dataframe - that the function is to be run for
           d = d, # dataframe with species, site, and each date it was seen at that site - must have a columns called Species, Site and DateTime
           all_cams = all_cams, # matrix with all the survey dates, 1s for dates when a camera was working/deployed and NAs for when it wasn't
           startDate = startDate,#start date in date format
           endDate = endDate) {
    # Make a vector of breaks
    brks <-seq(startDate, endDate, by = "day")   #makes a sequence of all the days from start to end
    
    # Create an empty matrix of dim sites x time periods
    occ <-matrix(0, ncol = length(unique(d$Site)), nrow = length(brks))
    colnames(occ) <- sort(unique(d$Site))
    rownames(occ) <- strftime(brks, format = "%Y-%m-%d")
    
    for (s in unique(d$Site)) {
      #this loops through each site and inserts 1's on days which there were captures
      seen <- NA
      captures <-na.omit(d$DateTime[d$Species == species & d$Site == s])
      # Were animals seen at the site
      seen <- which(brks %in% captures)
      # If the species was seen, occ = 1
      col_i <- which(colnames(occ) == s)
      occ[seen, col_i] <- 1
    }
    
    occ <- occ * all_cams[, 2:ncol(all_cams)]
    print(paste0(species, " done!"))
    species_name <- gsub(" ", "", species)
    row.names(occ) <- brks
    write.csv(occ, paste0("1d_matrix_", species_name, ".csv"))
    return(occ)
    
    
  }
# applying function to each species (label)
lapply(
  X = unique(MTOMC$Species),
  FUN = calcOcc,
  d = MTOMC,
  all_cams=all_cams,
  startDate = startDate,
  endDate = endDate) # this will save CSVs of spp matrices in the working directory


####  aggregating days into survey occasions ####
##timestepper - creates matrices of a given timestep, can choose to include or exclude NAs
timestepper <- function(occ_in, timestep, na_mode = "include") {
  if (na_mode == "include") {
    occ_in[is.na(occ_in)] <-0   #replacing NAs with 0s if we want to include them in analysis.
  }
  
  if (timestep > nrow(occ_in) / 2) {
    print(paste(
      "Time step is too large! Please reduce to",
      nrow(occ_in) / 2 ,
      "or less."
    ))
  } else {
    start <- seq(1, nrow(occ_in), by = timestep)
    end <- seq(timestep, nrow(occ_in), by = timestep)
    
    if (length(start) > length(end)) {
      start <- start[-length(start)]
    }
    
    timesteps <- matrix(nrow = length(start), ncol = ncol(occ_in))
    colnames(timesteps) <- colnames(occ_in)
    rownames(timesteps) <-
      paste(rownames(occ_in)[start], rownames(occ_in)[end], sep = ":")
    
    for (i in 1:length(start)) {
      timestep_out <- colSums(occ_in[start[i]:end[i], ])
      timesteps[i, ] <- timestep_out
      timesteps[timesteps > 0] <- 1
    }
    
    timesteps <- t(timesteps)
    return(timesteps)
    
  }
  
}

# reading in all CSVs with spp presence absence
filenames <- list.files("D:/CSVs/MTOMCMN_Matrices_New", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
head(ldf)

label <- basename(filenames)
label <- sub(pattern=".csv", replace="",label)
label <- sub(pattern="1d_matrix_", replace="",label)
names(ldf) <- label

# eliminating col "X" with dates before applying timestepper func
new_list <- lapply(ldf, function(x) x%>% select(-X)) 
new_list[[1]]

matrix_7d <- lapply(X = new_list,
                    FUN = timestepper,
                    timestep = 7,
                    na_mode = "exclude")  # must be exclude otherwise will consider days when CT wasn't working as actual survey days

matrix_7d[[1]]
names(matrix_7d) <- label # adding species names

# keeping only native mammals
matrix_native_7d<-"matric_native_7d_MTOMCMN.csv"
#matrix_native_7d <- matrix_7d[c(1,2,4,6,8,9,10,16,19,22,23,30,31,32,33,34,36,37,39,41,42,43,45,47,48,49,50,51,53,56,57,59,62,65,68,71,72,73,74,75,76)]
#names(matrix_native_7d)
#write.csv(matrix_native_7d, "matric_native_7d_MTOMCMN.csv")
###Correlation
covs <- read.csv("my_site_covs.csv")
head(covs2)
names(covs2)
tail(covs2)
covs2<-covs[!grepl("NB", covs$area),]
#write.csv(covs2,"covariates_MTOMCMN.csv")
#### exploring site variables ####
# correlation
covs2<-read.csv("covariates_MTOMCMN.csv")
covs_numeric <- covs2[,c(5,9,13,14,16,17,18,19,20,21,29,34,35,36,37)]
covs_numeric <- as.data.frame(scale(covs_numeric))
head(covs_numeric)
install.packages("psych")
library(psych)
pairs.panels(covs_numeric, scale=FALSE)
cor.test(covs_numeric$waterdist_short,covs_numeric$cattle_occupancy)
covs_numeric$Management <- covs$Management
covs_numeric$CT_site <- covs2$site
covs_numeric <- covs_numeric[-1,]
names(covs_numeric)
covs_numeric
library(unmarked)
install.packages("rlist")
library(rlist)
buffalo<-list.extract(matrix_native_7d, 5)
head(buffalo)
umf.buffalo<- unmarkedFrameOccu(y = buffalo, siteCovs = covs_numeric)
summary(umf.buffalo)
umf.buffalo

# Fit model and extract estimates
# Detection covariates follow first tilde, then occupancy covariates

### BUFFALO ###

m1BF <- occu(~1 ~1, data=umf.buffalo)
m1BF
#detection doesn't vary with any covariate
#summary(m1WB)
m1BF.p<-backTransform(m1BF, type="det") #detection #chance of observation
m1BF.psi<-backTransform(m1BF, type="state") #occupancy #prop. of observation

m1WBCI.psi<-confint(m1WB1.psi)
m1WBCI.p<-confint(m1WB1.p)

m2WB <- occu(~1 ~livestock_occupancy, data=umf.buffalo)
m2WB

m3WB<-occu(~tree_shrub_density ~1,data=umf.buffalo)
m3WB
#summary(m2BD)
#m3BD <- occu(~1 ~DistRiver, data=umf.barkingdeer)
#summary(m3BD)
#m4BD <- occu(~1 ~DistRiver+DistVillage, data=umf.barkingdeer)
#summary(m4BD)
#m5WB <- occu(~tree_shrub_density ~livestock_occupancy+fencedist_short+waterdist_short, data=umf.wildebeest)
#summary(m5WB

mful1Wildebeest<-occu(~tree_shrub_density ~propopen500m, data=umf.wildebeest)
summary(mful1Wildebeest)
names(covs_numeric)


chisq <- function(fm) {
  umf <- getData(fm)
  y <- getY(umf)
  y[y>1] <- 1
  sr <- fm@sitesRemoved
  if(length(sr)>0)
    y <- y[-sr,,drop=FALSE]
  fv <- fitted(fm, na.rm=TRUE)
  69
  y[is.na(fv)] <- NA
  sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
} 

pb_mful1Wildebeest<-parboot(mful1Wildebeest, statistic=chisq, nsim=1000, parallel=FALSE)
summary(pb_mful1Wildebeest)
install.packages("MuMIn")
library(MuMIn)
help(dredge)
getAllTerms(mful1Wildebeest)

#Dredge present cattle occupancy
dredge_mful1Wildebeest<-dredge(mful1Wildebeest,beta=c("none"), evaluate=TRUE, fixed="psi(livestock_occupancy)")
mful1Wildebeest_avg<-model.avg(dredge_mful1Wildebeest, subset = delta<=2,fit=TRUE)
mful1Wildebeest_top<-get.models(dredge_mful1Wildebeest, subset =1)[[1]]
summary (mful1Wildebeest_avg)
summary (mful1Wildebeest_top)







# need to eliminate OBZ03 from covs, as it is not on the species matrix
tempcovs <- tempcovs[-103,]
unique(tempcovs$CT_site) 
covs <- tempcovs[,c("DistRoads", "DistRiver", "DistVillage",
                    "propForest50", "propVegNat500")]
covs <-as.data.frame(scale(covs))  
mean(covs$DistRiver)  
mean(covs$propVegNat500)  
covs$CT_site <- tempcovs$CT_site  
covs$Management <- tempcovs$Management  
covs$Mount <- tempcovs$Mount  
head(covs,10)
write.csv(covs, "scaledCovs_148site_Nepal2019.csv")

Management <- covs$Management # one PA type must be 0 to eliminate its term in the occurence equation
BZ <- Management
BZ[BZ=="BZ"] <- 1
BZ[BZ=="NP" | BZ=="OBZ"] <- 0

NP <- Management
NP[NP=="NP"] <- 1
NP[NP=="BZ" | NP=="OBZ"] <- 0

OBZ <- Management
OBZ[OBZ=="OBZ"] <- 1
OBZ[OBZ=="BZ" | OBZ=="NP"] <- 0
Mount <- covs$Mount 
# need to trasnform in number for analysis
# one category must be 0 - this will be intercept for detection
Mount[Mount =="T"] <- 0
Mount[Mount =="P"] <- 1
Mount <- as.numeric(Mount)
ForCov <- covs$propForest50


