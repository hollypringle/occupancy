names(matrix_native_7d)

names(covs_numeric)
library(unmarked)
library(here)
library(dplyr)
library(data.table)
library(chron)
library(rlist)
#Assign species
buffalo<-list.extract(matrix_native_7d, 5)
bushbuck<-list.extract(matrix_native_7d, 7)
dikdik<-list.extract(matrix_native_7d, 9)
eland<-list.extract(matrix_native_7d, 10)
elephant<-list.extract(matrix_native_7d, 11)
gazelle_grants<-list.extract(matrix_native_7d, 12)
gazelle_thomsons<-list.extract(matrix_native_7d, 13)
giraffe<-list.extract(matrix_native_7d, 16)
hartebeest_cokes<-list.extract(matrix_native_7d, 18)
hippopotamus<-list.extract(matrix_native_7d, 19)
impala<-list.extract(matrix_native_7d, 22)
topi<-list.extract(matrix_native_7d, 35)
warthog<-list.extract(matrix_native_7d, 37)
waterbuck<-list.extract(matrix_native_7d, 38)
wildebeest<-list.extract(matrix_native_7d, 39)
zebra<-list.extract(matrix_native_7d, 40)
hare<-list.extract(matrix_native_7d, 17)
jackal<-list.extract(matrix_native_7d, 23)
hyena_spotted<-list.extract(matrix_native_7d, 20)
vervet_monkey<-list.extract(matrix_native_7d, 36)
baboon<-list.extract(matrix_native_7d, 3)


#TEMPLATE

umf.topi <- unmarkedFrameOccu(y = topi, siteCovs = covs_numeric) 
summary(umf.topi)

# global model that will be used for dredging
mfull.topi <- occu(~tree_shrub_density ~humdist_short + waterdist_short + propopen500m+ shoat_occupancy+ cattle_occupancy , data=umf.topi)
summary(mfull.topi)

library(MuMIn)
# dredge function - use ir wisely! Think hard about variables included in the global model and don't let the computer do the thinking...
drg.topi <- dredge(mfull.topi, rank = AIC) #, fixed = ('psi(pa_type)')) #dredging models
drg.topi

#model-average estimates considering models with delta AIC <2
#avg1.topi <-model.avg(drg.topi, subset=delta<=2) #model-average estimates
#summary(avg1.topi)

#model-average estimates considering models up to cumulative AIC weight of .95
# I prefer this option
avgfull.topi <-model.avg(drg.topi, subset = cumsum(weight) <= .95, fit=T) 
summary(avgfull.hippopotamus)

#predicting occupancy based on a categorical variable
#names of the variables must be exactly the same as in the model used for prediction
#newData=data.frame(Management=c("BZ", "NP", "OBZ"), propVegNat500=0, DistRiver=0)
#pred.avg2.WB<-predict(avg2.WB, type="state", se.fit=TRUE, full=T, newData)
#prdOccu <- as.data.frame(pred.avg2.WB)
#prdOccu$Management <- newData$Management

#predicting occupancy based on continuous variable, and a fixed management regime

#HUMAN
range(covs_numeric$humdist_short)
newDatahuman=data.frame(humdist_short=seq(-1.215112,3.002436,by=0.1), propopen500m=0, cattle_occupancy=0, shoat_occupancy=0, waterdist_short=0)
pred.avgfull.human.topi<-predict(avgfull.topi, type="state", se.fit=TRUE, full=T, newDatahuman)
prdOccutopihuman <- as.data.frame(pred.avgfull.human.topi)
prdOccutopihuman$humdist_short <- newDatahuman$humdist_short
prdOccutopihuman

#SHOAT
range(covs_numeric$shoat_occupancy)
newDatashoat=data.frame(shoat_occupancy=seq(-0.4362186,2.4401843,by=0.1), propopen500m=0, cattle_occupancy=0, humdist_short=0, waterdist_short=0)
pred.avgfull.shoat.topi<-predict(avgfull.topi, type="state", se.fit=TRUE, full=T, newDatashoat)
prdOccutopishoat <- as.data.frame(pred.avgfull.shoat.topi)
prdOccutopishoat$shoat_occupancy <- newDatashoat$shoat_occupancy
prdOccutopishoat

#CATTLE
range(covs_numeric$cattle_occupancy)
newDatacattle=data.frame(cattle_occupancy=seq(-0.8821702,1.3358040,by=0.1), propopen500m=0, humdist_short=0, shoat_occupancy=0, waterdist_short=0)
pred.avgfull.cattle.topi<-predict(avgfull.topi, type="state", se.fit=TRUE, full=T, newDatacattle)
prdOccutopicattle <- as.data.frame(pred.avgfull.cattle.topi)
prdOccutopicattle$cattle_occupancy <- newDatacattle$cattle_occupancy
prdOccutopicattle

library(ggplot2)
#PLOT
topihumanplot<-ggplot(prdOccutopihuman, aes(x=humdist_short, y=fit)) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin= fit-(se.fit*1.96), ymax=fit+(se.fit*1.96)), alpha=0.2)+
  geom_line(size=1,colour="blue")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8),
        axis.text.y  = element_text(size=8)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  #scale_fill_manual(values=c("yellow4", "forestgreen", "tomato3"))+
  theme(legend.position="none")+
  ylab("Probability of occupancy")+
  xlab("Shortest distance to human settlement")+
  ggtitle("topi")


topicattleplot<-ggplot(prdOccutopicattle, aes(x=cattle_occupancy, y=fit)) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin= fit-(se.fit*1.96), ymax=fit+(se.fit*1.96)), alpha=0.2)+
  geom_line(size=1,colour="blue")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8),
        axis.text.y  = element_text(size=8)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  #scale_fill_manual(values=c("yellow4", "forestgreen", "tomato3"))+
  theme(legend.position="none")+
  ylab("")+
  xlab("Cattle pressure")+
  ggtitle("")


topishoatplot<-ggplot(prdOccutopishoat, aes(x=shoat_occupancy, y=fit)) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin= fit-(se.fit*1.96), ymax=fit+(se.fit*1.96)), alpha=0.2)+
  geom_line(size=1,colour="blue")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8),
        axis.text.y  = element_text(size=8)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  #scale_fill_manual(values=c("yellow4", "forestgreen", "tomato3"))+
  theme(legend.position="none")+
  ylab("")+
  xlab("Shoat pressure")+
  ggtitle("")


zebraplot<-grid.arrange(zebrahumanplot, zebracattleplot, zebrashoatplot, ncol = 3)
wildebeestplot<-grid.arrange(wildebeesthumanplot, wildebeestcattleplot, wildebeestshoatplot, ncol = 3)
giraffeplot<-grid.arrange(giraffehumanplot, giraffecattleplot, giraffeshoatplot, ncol = 3)
topiplot<-grid.arrange(topihumanplot, topicattleplot, topishoatplot, ncol = 3)
jackalplot<-grid.arrange(jackalhumanplot, jackalcattleplot, jackalshoatplot, ncol = 3)
hareplot<-grid.arrange(harehumanplot, harecattleplot, hareshoatplot, ncol = 3)
elephantplot<-grid.arrange(elephanthumanplot, elephantcattleplot, elephantshoatplot, ncol = 3)
impalaplot<-grid.arrange(impalahumanplot, impalacattleplot, impalashoatplot, ncol = 3)
hippopotamusplot<-grid.arrange(hippopotamushumanplot, hippopotamuscattleplot, hippopotamusshoatplot, ncol = 3)
gazelle_thomsonsplot<-grid.arrange(gazelle_thomsonshumanplot, gazelle_thomsonscattleplot, gazelle_thomsonsshoatplot, ncol = 3)
gazelle_grantsplot<-grid.arrange(gazelle_grantshumanplot, gazelle_grantscattleplot, gazelle_grantsshoatplot, ncol = 3)
warthogplot<-grid.arrange(warthoghumanplot, warthogcattleplot, warthogshoatplot, ncol = 3)
hartebeest_cokesplot<-grid.arrange(hartebeest_cokeshumanplot, hartebeest_cokescattleplot, hartebeest_cokesshoatplot, ncol = 3)
hyena_spottedplot<-grid.arrange(hyena_spottedhumanplot, hyena_spottedcattleplot, hyena_spottedshoatplot, ncol = 3)
vervet_monkeyplot<-grid.arrange(vervet_monkeyhumanplot, vervet_monkeycattleplot, vervet_monkeyshoatplot, ncol = 3)
baboonplot<-grid.arrange(baboonhumanplot, babooncattleplot, baboonshoatplot, ncol = 3)
buffaloplot<-grid.arrange(buffalohumanplot, buffalocattleplot, buffaloshoatplot, ncol = 3)


grid.arrange(zebraplot,giraffeplot,elephantplot, hippopotamusplot, buffaloplot,ncol = 1)
grid.arrange(baboonplot,vervet_monkeyplot, ncol = 1)
grid.arrange(hyena_spottedplot,jackalplot, ncol = 1)
grid.arrange(wildebeestplot, topiplot, gazelle_grantsplot, impalaplot,ncol = 1)
grid.arrange(warthogplot, hareplot, gazelle_thomsonsplot, dikdikplot,ncol = 1)
