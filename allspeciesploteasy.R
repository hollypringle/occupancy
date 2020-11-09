#TEMPLATE

umf.zebra <- unmarkedFrameOccu(y = zebra, siteCovs = covs_numeric) 
summary(umf.zebra)

# global model that will be used for dredging
mfull.zebra <- occu(~tree_shrub_density ~humdist_short + waterdist_short + propopen500m+ shoat_occupancy+ cattle_occupancy , data=umf.zebra)
summary(mfull.zebra)

library(MuMIn)
# dredge function - use ir wisely! Think hard about variables included in the global model and don't let the computer do the thinking...
drg.zebra <- dredge(mfull.zebra, rank = AIC) #, fixed = ('psi(pa_type)')) #dredging models
drg.zebra

#model-average estimates considering models with delta AIC <2
#avg1.zebra <-model.avg(drg.zebra, subset=delta<=2) #model-average estimates
#summary(avg1.zebra)

#model-average estimates considering models up to cumulative AIC weight of .95
# I prefer this option
avgfull.zebra <-model.avg(drg.zebra, subset = cumsum(weight) <= .95, fit=T) 
summary(avgfull.zebra)

#predicting occupancy based on a categorical variable
#names of the variables must be exactly the same as in the model used for prediction
#newData=data.frame(Management=c("BZ", "NP", "OBZ"), propVegNat500=0, DistRiver=0)
#pred.avg2.WB<-predict(avg2.WB, type="state", se.fit=TRUE, full=T, newData)
#prdOccu <- as.data.frame(pred.avg2.WB)
#prdOccu$Management <- newData$Management

#predicting occupancy based on continuous variable, and a fixed management regime

#HUMAN
range(covs_numeric$humdist_short)
newDatahuman=data.frame(humdist_short=seq(-1.215112, 3.002436,by=0.1), propopen500m=0, cattle_occupancy=0, shoat_occupancy=0, waterdist_short=0)
pred.avgfull.human.hyena_spotted<-predict(avgfull.hyena_spotted, type="state", se.fit=TRUE, full=T, newDatahuman)
prdOccuhyena_spottedhuman <- as.data.frame(pred.avgfull.human.hyena_spotted)
prdOccuhyena_spottedhuman$humdist_short <- newDatahuman$humdist_short
prdOccuhyena_spottedhuman

#SHOAT
range(covs_numeric$shoat_occupancy)
newDatashoat=data.frame(shoat_occupancy=seq(-0.4362186, 2.4401843,by=0.1), propopen500m=0, cattle_occupancy=0, humdist_short=0, waterdist_short=0)
pred.avgfull.shoat.hyena_spotted<-predict(avgfull.hyena_spotted, type="state", se.fit=TRUE, full=T, newDatashoat)
prdOccuhyena_spottedshoat <- as.data.frame(pred.avgfull.shoat.hyena_spotted)
prdOccuhyena_spottedshoat$shoat_occupancy <- newDatashoat$shoat_occupancy
prdOccuhyena_spottedshoat

#CATTLE
range(covs_numeric$cattle_occupancy)
newDatacattle=data.frame(cattle_occupancy=seq(-0.8821702,1.3358040,by=0.1), propopen500m=0, humdist_short=0, shoat_occupancy=0, waterdist_short=0)
pred.avgfull.cattle.hyena_spotted<-predict(avgfull.hyena_spotted, type="state", se.fit=TRUE, full=T, newDatacattle)
prdOccuhyena_spottedcattle <- as.data.frame(pred.avgfull.cattle.hyena_spotted)
prdOccuhyena_spottedcattle$cattle_occupancy <- newDatacattle$cattle_occupancy
prdOccuhyena_spottedcattle

library(ggplot2)
#PLOT
hyena_spottedhumanplot<-ggplot(prdOccuhyena_spottedhuman, aes(x=humdist_short, y=fit)) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin= fit-(se.fit*1.96), ymax=fit+(se.fit*1.96)), alpha=0.2)+
  geom_line(size=1,colour="blue")+
  coord_cartesian(ylim = c(0, 1), xlim = c(-1.2, 3))+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8),
        axis.text.y  = element_text(size=8)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(legend.position="none")+
  ylab("Probability of Occupancy")+
  xlab("Distance from human settlement")+
  ggtitle("Spotted hyena")


hyena_spottedcattleplot<-ggplot(prdOccuhyena_spottedcattle, aes(x=cattle_occupancy, y=fit)) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin= fit-(se.fit*1.96), ymax=fit+(se.fit*1.96)), alpha=0.2)+
  coord_cartesian(ylim = c(0, 1), xlim = c(-0.8,1.3))+
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


hyena_spottedshoatplot<-ggplot(prdOccuhyena_spottedshoat, aes(x=shoat_occupancy, y=fit)) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin= fit-(se.fit*1.96), ymax=fit+(se.fit*1.96)), alpha=0.2)+
  coord_cartesian(ylim = c(0, 1), xlim = c(-0.4, 2.4))+
  geom_line(size=1,colour="blue")+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x  = element_text(size=8),
        axis.text.y  = element_text(size=8)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  #scale_fill_manual(values=c("yellow4", "forestgreen", "tomato3"))+
  theme(legend.position="none")+
  ylab("")+
  xlab("Sheep/goat pressure")+
  ggtitle("")

hyena_spottedplot<-grid.arrange(hyena_spottedhumanplot, hyena_spottedcattleplot, hyena_spottedshoatplot, ncol = 3)

smallgrazerplot<-grid.arrange(grid.arrange(gazelle_thomsonsplot, warthogplot, dikdikplot, ncol = 1))

largegrazerplot<-grid.arrange(zebraplot, giraffeplot, elephantplot, hippopotamusplot, buffaloplot, ncol = 1)

carnivoreplot<-grid.arrange(grid.arrange(jackalplot, hyena_spottedplot, ncol = 1))
