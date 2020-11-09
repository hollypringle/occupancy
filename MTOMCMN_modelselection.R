setwd("D:/CSVs")

library(unmarked)
library(here)
library(dplyr)
library(data.table)
library(chron)

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
                    timestep = 7, #7 days
                    na_mode = "exclude")  # must be exclude otherwise will consider days when CT wasn't working as actual survey days

matrix_7d[[1]]
names(matrix_7d) <- label # adding species names

# keeping only native mammals

matrix_native_7d <- matrix_7d[c(1,2,4,6,8,9,10,16,19,22,23,30,31,32,33,34,36,37,39,41,42,43,45,47,48,49,50,51,53,56,57,59,62,65,68,71,72,73,74,75,76)]
names(matrix_native_7d)

###Correlation

#### exploring site variables ####
# correlation
covs<-read.csv("covariates_MTOMCMN.csv")
names(covs)
covs$X<-NULL
covs_numeric <- covs[,c(5,6,7,8,16,17,19,20,21,22,23,32,37,38,39,40)]
covs_numeric <- as.data.frame(scale(covs_numeric))
names(covs_numeric)
#install.packages("psych")
library(psych)
pairs.panels(covs_numeric, scale=FALSE)
#cor.test(covs_numeric$waterdist_short,covs_numeric$cattle_occupancy)

covs_numeric$CT_site <- covs$site
#covs_numeric <- covs_numeric[-1,]
names(covs_numeric)
covs_numeric
library(unmarked)
#install.packages("rlist")
library(rlist)

# preparing data for unmarked
aardvark<-list.extract(matrix_native_7d, 1)
umf.aardvark <- unmarkedFrameOccu(y = aardvark, siteCovs = covs_numeric) 
summary(umf.aardvark)

# null model
m1.AA <- occu(~1 ~1, data=umf.aardvark)
summary(m1.AA)
backTransform(m1.AA, type="state")
backTransform(m1.AA, type="det")
names(covs_numeric)
# series of models representing different hypothesis
m2.AA <- occu(~NDVI_mean ~1, data=umf.aardvark)
#m2.WB <- occu(~1 ~Management-1, data=umf.WildBoar)
summary(m2.AA)

m3.AA <- occu(~tree_shrub_density ~humdist_short, data=umf.aardvark)
summary(m3.AA)

m4.AA <- occu(~1 ~humdist_short + propopen500m + tree_shrub_density, data=umf.aardvark)
summary(m4.AA)


# global model that will be used for dredging
mfull.AA <- occu(~propopen500m ~humdist_short + NDVI_mean + livestock_occupancy , data=umf.aardvark)
summary(mfull.AA)

library(MuMIn)
# dredge function - use ir wisely! Think hard about variables included in the global model and don't let the computer do the thinking...
drg.AA <- dredge(mfull.AA, rank = AIC) #, fixed = ('psi(pa_type)')) #dredging models
drg.AA

#plot(drg.AA, labAsExpr = TRUE)
#model-average estimates considering models with delta AIC <2
avg1.AA <-model.avg(drg.AA, subset=delta<=2) #model-average estimates
summary(avg1.AA)
summary(covs_numeric)
#model-average estimates considering models up to cumulative AIC weight of .95
# I prefer this option
avg2.AA <-model.avg(drg.AA, subset = cumsum(weight) <= .95, fit=T) 
summary(avg2.AA)

#predicting occupancy based on a categorical variable
#names of the variables must be exactly the same as in the model used for prediction
#newData=data.frame(Management=c("BZ", "NP", "OBZ"), propVegNat500=0, DistRiver=0)
#pred.avg2.WB<-predict(avg2.WB, type="state", se.fit=TRUE, full=T, newData)
#prdOccu <- as.data.frame(pred.avg2.WB)
#prdOccu$Management <- newData$Management

#predicting occupancy based on continuous variable, and a fixed management regime
range(covs_numeric$humdist_short)
newDataAA=data.frame(humdist_short=seq(-2,3.1,by=0.1), propopen500m=0, livestock_occupancy=0)
pred.avg2.AA<-predict(avg2.AA, type="state", se.fit=TRUE, full=T, newData)
prdOccuAA <- as.data.frame(pred.avg2.AA)
prdOccuAA$humdist_short <- newDataAA$humdist_short
prdOccuAA
#newData=data.frame(Management=c("BZ", "NP", "OBZ"), propVegNat500=1, DistRiver=0)
#pred.avg2.WB<-predict(avg2.WB, type="state", se.fit=TRUE, full=T, newData)
#prdOccu <- as.data.frame(pred.avg2.WB)
#prdOccu$Management <- newData$Management

#plot prob occupancy at each management regime
library(ggplot2)

prdOccuAA%>%
  ggplot(aes(humdist_short,fit))+
  geom_smooth_ci()



#IMPALA
names(matrix_native_7d)
impala<-list.extract(matrix_native_7d, 22)
umf.impala <- unmarkedFrameOccu(y = impala, siteCovs = covs_numeric) 
summary(umf.impala)

# global model that will be used for dredging
mfull.IM <- occu(~propopen500m ~humdist_short + NDVI_mean + livestock_occupancy , data=umf.impala)
summary(mfull.IM)

library(MuMIn)
# dredge function - use ir wisely! Think hard about variables included in the global model and don't let the computer do the thinking...
drg.IM <- dredge(mfull.IM, rank = AIC) #, fixed = ('psi(pa_type)')) #dredging models
drg.IM

#model-average estimates considering models with delta AIC <2
avg1.IM <-model.avg(drg.IM, subset=delta<=2) #model-average estimates
summary(avg1.IM)

#model-average estimates considering models up to cumulative AIC weight of .95
# I prefer this option
avg2.IM <-model.avg(drg.IM, subset = cumsum(weight) <= .95, fit=T) 
summary(avg2.IM)

#predicting occupancy based on a categorical variable
#names of the variables must be exactly the same as in the model used for prediction
#newData=data.frame(Management=c("BZ", "NP", "OBZ"), propVegNat500=0, DistRiver=0)
#pred.avg2.WB<-predict(avg2.WB, type="state", se.fit=TRUE, full=T, newData)
#prdOccu <- as.data.frame(pred.avg2.WB)
#prdOccu$Management <- newData$Management

#predicting occupancy based on continuous variable, and a fixed management regime
range(covs_numeric$humdist_short)
newData=data.frame(humdist_short=seq(-2,3.1,by=0.1), NDVI_mean=0, livestock_occupancy=0)
pred.avg2.IM<-predict(avg2.IM, type="state", se.fit=TRUE, full=T, newData)
prdOccuIM <- as.data.frame(pred.avg2.IM)
prdOccuIM$humdist_short <- newData$humdist_short
prdOccuIM




