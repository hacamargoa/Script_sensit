#Source("Senescheck.R")
library(FactoMineR)
library(missMDA)
library(ca)
library(ncdf4)
library(raster)
library(rworldmap)
library(dplyr)
library(plyr)
library(ltm)
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)

#Areas
crop<-c("SPAMest_Wheat_","SPAMest_Maize_")
cropirr<-c("SPAMest_Wheatirr_","SPAMest_Maizeirr_")
ArNew<-list()
ArNewi<-list()
ArNewr<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/Area")
for (j in 1:length(crop)){
  ArNew[[j]]<-list.files(pattern=crop[j])
  ArNewi[[j]]<-list.files(pattern=cropirr[j])
  ArNew[[j]]<-brick(ArNew[[j]])
  ArNewi[[j]]<-brick(ArNewi[[j]])*(ArNew[[j]]/ArNew[[j]])
  ArNewr[[j]]<-ArNew[[j]]-ArNewi[[j]]
}

ArWheat05<-ArNew[[1]][[45]];ArWheat05i<-ArNewi[[1]][[45]];ArWheat05r<-ArNewr[[1]][[45]]
ArMaiz05<-ArNew[[2]][[45]];ArMaiz05i<-ArNewi[[2]][[45]];ArMaiz05r<-ArNewr[[2]][[45]]


#Rayyield
crop<-c("Wheat","Maize")
cropR<-c("Ray_Yield_Wheat","Ray_Yield_Maize_")
crop_rayR<-list()
#Prod_ray<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs")
for (j in 1:length(crop)){
  crop_rayR[[j]]<-list.files(pattern=cropR[j])
  crop_rayR[[j]]<-brick(crop_rayR[[j]])
  
}
Deraster<-function(x){
  df<-list()
  for(i in 1:nlayers(x)){
    df[[i]]<-as.data.frame(x[[i]],xy=TRUE)
    df[[i]]$year<-2000+i
  }
  df<-lapply(df, setNames, c("Lon","Lat","Yield","Years"))
  return(bind_rows(df, .id = "label"))
}
crop_rayR1<-list()
for (i in 1:2){
  crop_rayR1[[i]]<-crop_rayR[[i]][[32:41]]
}
setwd("C:/Users/hac809/Desktop/Sensitivity2/")
yield_Raydf<-lapply(crop_rayR1,FUN=Deraster)
yield_Raydf<-lapply(yield_Raydf,FUN=na.omit)
ObsC=read.csv("Obs_corn.csv",h=T)
ObsC1=ObsC
ObsC1$Lat[ObsC1$Lat==51.25]<-49.25
YiMa<-yield_Raydf[[2]]
yieldRayMaize<-YiMa[paste0(YiMa$Lon,YiMa$Lat) %in% paste0(ObsC1$Lon,ObsC1$Lat),]
temp1<-data.frame(label=seq(1,8),Lon=-0.75, Lat=41.75, Yield=10.75881, Years=seq(2001,2008))
yieldRayMaize<-rbind(yieldRayMaize,temp1)
yieldRayAvg<-aggregate(Yield~Lon+Lat,yieldRayMaize,mean);colnames(yieldRayAvg)[3]="YieldRay"
yieldRayAvg$Areai<-extract(ArMaiz05i,yieldRayAvg[,c(1,2)])
yieldRayAvg$Arear<-extract(ArMaiz05r,yieldRayAvg[,c(1,2)])
yieldRayAvg$Lat[yieldRayAvg$Lat==49.25]<-51.25
yieldRayAvg$YieldRay=yieldRayAvg$YieldRay/10

#Maize
FMai<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_MaizBC.csv", h=T)
FMai<-subset(FMai,year<2011&year>2000)
FMaiy<-aggregate(FYield~UN,FMai, FUN=mean)
countries<-as.data.frame(gridCountriesDegreesHalf)
names(countries)<-c("UN","Lon","Lat")
Yield<-join(yield_d2CO2,ObsC)
Yield<-join(Yield,yieldRayAvg)
Yield<-join(Yield,countries)
Yield$Num<-ifelse(Yield$Sret==1,Yield$Num+8640,Yield$Num)
Yield<-join(Yield,FMaiy);Yield$FYield[which(is.na(Yield$FYield))]=7.0
Yield$FYield=Yield$FYield/10
Yield$YTeCS=(Yield$TeCS*Yield$Arear+Yield$TeCSi*Yield$Areai)/(Yield$Areai+Yield$Arear)/(2*0.446*0.87)
Yield$DiffRay<-abs(Yield$YieldRay-Yield$YTeCS)/Yield$YieldRay
Yield$DiffCount<-abs(Yield$FYield-Yield$YTeCS)/Yield$FYield


HI<-join(HI_d2CO2,ObsC)
HI<-join(HI,yieldRayAvg)
HI$Num<-ifelse(HI$Sret==1,HI$Num+8640,HI$Num)
HI$HTeCS=(HI$TeCS*HI$Arear+HI$TeCSi*HI$Areai)/(HI$Areai+HI$Arear)

Senes_HI<-join(senes[,-c(12:15)],HI[,c(1:11,23)])
plot(Senes_HI$d_perc_rf,Senes_HI$HTeCS,ylab="HI",xlab="Harvest dead leaves (%) Maize rainfed",col=ifelse(Senes_HI$Sret==1,"black","red"))
plot(Senes_HI$d_perc_ir,Senes_HI$HTeCS,ylab="HI",xlab="Harvest dead leaves (%) Maize irrigated",col=ifelse(Senes_HI$Sret==1,"black","red"))

####Evaluation by location. 

Yi_HIm=join(Yield[,c(1:11,25,26,27)],HI[,-c(12:22)])
Yi_HIm=join(Yi_HIm,summ)

attach(Yi_HIm)
bestsmC<-list()
bestsmR<-list()
for (i in (unique(paste0(Lon,Lat)))){
  temp1=subset(Yi_HIm,paste0(Lon,Lat)==i&HTeCS>0.300&HTeCS<0.59)
  temp1=temp1[which(temp1$sen_loc_ir>11),]
  temp=temp1[order(temp1$DiffCount),][1,]
  temp_=temp1[order(temp1$DiffRay),][1,]
  bestsmC[[i]]=aggregate(.~Lon+Lat,temp,FUN=mean)
  bestsmR[[i]]=aggregate(.~Lon+Lat,temp_,FUN=mean)
  }
Cou_bestm=do.call("rbind", bestsmC)[,c(1:3,12,15)]
Ray_bestm=do.call("rbind", bestsmR)[,c(1:3,12,15)]

#Function to create graph of k_means
wssplot<-function(data, nc=15, seed=1234){
  wss<-nrow(data)-1*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=1)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of Squares")
}

wssplot(Ray_bestm[,c(4,5)])
km_clus<-kmeans(Ray_bestm[,c(4,5)],2)
autoplot(km_clus,Ray_bestm[,c(4,5)],frame=T)
Ray_bestm=cbind(Ray_bestm,Cluster=km_clus[[1]])

wssplot(Cou_bestm[,c(4,5)])
km_clusCou<-kmeans(Cou_bestm[,c(4,5)],2)
autoplot(km_clusCou,Cou_bestm[,c(4,5)],frame=T)
Cou_bestm=cbind(Cou_bestm,Cluster=km_clusCou[[1]])

###Distance of QAE for clusters using Country yield  
SSE_3<-data.frame("Num"=1)
SSE_3<-as.data.frame(seq(1:17280));names(SSE_3)[1]<-"Num"
Yi_HIm$Cluster=ifelse(abs(Yi_HIm$Lat)<35|Yi_HIm$Lon>105,2,1)
for (i in SSE_3$Num){
  SSE_3[i,"Clust1R"]=quantile(Yi_HIm$DiffRay[which(Yi_HIm$Cluster==1&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"Clust2R"]=quantile(Yi_HIm$DiffRay[which(Yi_HIm$Cluster==2&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"Clust1"]=quantile(Yi_HIm$DiffCount[which(Yi_HIm$Cluster==1&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"Clust2"]=quantile(Yi_HIm$DiffCount[which(Yi_HIm$Cluster==2&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"HIav_CL1"]=mean(Yi_HIm$HTeCS[which(Yi_HIm$Cluster==1&Yi_HIm$Num==i)])
  SSE_3[i,"HIav_CL2"]=mean(Yi_HIm$HTeCS[which(Yi_HIm$Cluster==2&Yi_HIm$Num==i)])
  }
SSE2m<-join(SSE_3,Yi_HIm[,c(1:9)],match='first')
SSE2m<-join(SSE2m,summ)


Temp<-subset(SSE2m,sen_loc_ir>11&HIav_CL1<0.59&HIav_CL2>0.3)
Temp<-Temp[,c(-6,-7)]
MaizAv<-data.frame(matrix(ncol=9))
MaizBest<-data.frame(matrix(ncol=9))
for (i in 1:4){
  Temp1<-Temp[order(Temp[,i+1]),][1,c(1,6:13)]
  Temp2<-Temp[order(Temp[,i+1]),][c(1:10),c(1,6:13)]
  Temp2<-colMeans(Temp2)
  MaizBest[i,]<-Temp1
  MaizAv[i,]<-Temp2
}
names(MaizAv)=names(Temp)[c(1,6:13)];MaizAv$var<-paste0(names(Temp)[2:5],"Av")
names(MaizBest)=names(Temp)[c(1,6:13)];MaizBest$var<-names(Temp)[2:5]
Maiz<-rbind(MaizBest,MaizAv)



#Wheat
FWhe<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_WheaBC.csv", h=T)
FWhe<-subset(FWhe,year<2011&year>2000)
FWhey<-aggregate(FYield~UN,FWhe, FUN=mean)
nc_winter<-"C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/winter_and_spring_wheat_areas_phase3.nc4"
WWdist<-stack(nc_winter,varname="wwh_mask")
SWdist<-stack(nc_winter,varname="swh_mask")

#Wheat

ObsW1$Lon[ObsW1$Lon==23.75]<-23.25
YiWh<-yield_Raydf[[1]]
yieldRayWheat<-YiWh[paste0(YiWh$Lon,YiWh$Lat) %in% paste0(ObsW1$Lon,ObsW1$Lat),]
yieldRayWAvg<-aggregate(Yield~Lon+Lat,yieldRayWheat,mean);colnames(yieldRayWAvg)[3]="YieldRay"
yieldRayWAvg$Lon[yieldRayWAvg$Lon==23.25]<-23.75
yieldRayWAvg$YieldRay=yieldRayWAvg$YieldRay/10
yieldRayWAvg$Areai<-extract(ArWheat05i,yieldRayWAvg[,c(1,2)])
yieldRayWAvg$Arear<-extract(ArWheat05r,yieldRayWAvg[,c(1,2)])
ObsW1=ObsW
ObsW1$Lon[ObsW1$Lon==23.25]<-23.75;ObsW1=ObsW1[,-5]
temp<-ObsW1[c(1,2)]
temp$Spring<-extract(SWdist,temp[,c(2,1)])
temp$Spring[is.na(temp$Spring)]=1
temp$wheat<-ifelse(temp$Spring==1,"Spring","Winter");
wCult=temp[-3];rm(temp)
YieldW<-join(yield_d2WW3[-17],wCult)
YieldW<-join(YieldW,yieldRayWAvg)
YieldW<-join(YieldW,countries)
YieldW<-join(YieldW,FWhey)
YieldW$FYield=YieldW$FYield/10
YieldW$TeWi<-ifelse(YieldW$wheat=="Spring",YieldW$TeSWi,YieldW$TeWWi)
YieldW$TeW<-ifelse(YieldW$wheat=="Spring",YieldW$TeSW,YieldW$TeWW)
YieldW$YTeW=(YieldW$TeW*YieldW$Arear+YieldW$TeWi*YieldW$Areai)/(YieldW$Areai+YieldW$Arear)/(2*0.446*0.88)
YieldW$Num<-ifelse(YieldW$Sret==1,YieldW$Num+8640,YieldW$Num)
YieldW$DiffRay<-abs(YieldW$YTeW-YieldW$YieldRay)/YieldW$YieldRay
YieldW$DiffCount<-abs(YieldW$YTeW-YieldW$FYield)/YieldW$FYield

HIw<-join(HI_d2WW3[-17],wCult)
HIw$TeWi<-ifelse(HIw$wheat=="Spring",HIw$TeSWi,HIw$TeWWi)
HIw$TeW<-ifelse(HIw$wheat=="Spring",HIw$TeSW,HIw$TeWW)
HIw$Num<-ifelse(HIw$Sret==1,HIw$Num+8640,HIw$Num)
HIw$Areai<-extract(ArWheat05i,HIw[,c(10,11)])
HIw$Arear<-extract(ArWheat05r,HIw[,c(10,11)])
HIw$HTeW=(HIw$TeW*HIw$Arear+HIw$TeWi*HIw$Areai)/(HIw$Areai+HIw$Arear)

Senes_HIw<-join(senesw[,-c(12:19)],HIw[,c(1:15,23)])
plot(Senes_HIw$d_perc_wwrf,Senes_HIw$TeWW,ylab="HI",xlab="Ratio_d/a_TeWW",col=ifelse(Senes_HIw$Sret==1,"black","red"))
plot(Senes_HIw$d_perc_wwir,Senes_HIw$TeWWi,ylab="HI",xlab="Ratio_d/a_TeWWi",col=ifelse(Senes_HIw$Sret==1,"black","red"))
plot(Senes_HIw$d_perc_swrf,Senes_HIw$TeSW,ylab="HI",xlab="Ratio_d/a_TeSW",col=ifelse(Senes_HIw$Sret==1,"black","red"))
plot(Senes_HIw$d_perc_swir,Senes_HIw$TeSWi,ylab="HI",xlab="Ratio_d/a_TeSWi",col=ifelse(Senes_HIw$Sret==1,"black","red"))


Yi_HIw=join(YieldW[,c(1:11,21,28:29)],HIw[,c(1:11,24)])
Yi_HIw=join(Yi_HIw,summw)

SSE_3w<-data.frame("Num"=1)
SSE_3w<-as.data.frame(seq(1:17280));names(SSE_3w)[1]<-"Num"
for (i in SSE_3w$Num){
  SSE_3w[i,"SpringC"]=quantile(Yi_HIw$DiffCount[which(Yi_HIw$wheat=="Spring"&Yi_HIw$Num==i)],0.8)
  SSE_3w[i,"WinterC"]=quantile(Yi_HIw$DiffCount[which(Yi_HIw$wheat=="Winter"&Yi_HIw$Num==i)],0.8)
  SSE_3w[i,"SpringR"]=quantile(Yi_HIw$DiffRay[which(Yi_HIw$wheat=="Spring"&Yi_HIw$Num==i)],0.8)
  SSE_3w[i,"WinterR"]=quantile(Yi_HIw$DiffRay[which(Yi_HIw$wheat=="Winter"&Yi_HIw$Num==i)],0.8)
  SSE_3w[i,"HIav_Sp"]=mean(Yi_HIw$HTeW[which(Yi_HIw$wheat=="Spring"&Yi_HIw$Num==i)])
  SSE_3w[i,"HIav_Wi"]=mean(Yi_HIw$HTeW[which(Yi_HIw$wheat=="Winter"&Yi_HIw$Num==i)])
}
SSE3w<-join(SSE_3w,Yi_HIw[,c(1:9)],match='first')
SSE3w<-join(SSE3w,summw)


Temp<-subset(SSE3w,PercWWi>10)
WheatAv<-data.frame(matrix(ncol=9))
wheatBest<-data.frame(matrix(ncol=9))
for (i in 1:4){
  x=ifelse(i==1|i==3,6,7)
  TempX=subset(Temp,Temp[,x]<0.45&Temp[,x]>0.35)
  Temp1<-TempX[order(TempX[,i+1]),][1,c(1,8:15)]
  Temp2<-TempX[order(TempX[,i+1]),][c(1:10),c(1,8:15)]
  Temp2<-colMeans(Temp2)
  WheatBest[i,]<-Temp1
  WheatAv[i,]<-Temp2
}
names(WheatAv)=names(TempX)[c(1,8:15)];WheatAv$var<-paste0(names(TempX)[2:5],"Av")
names(WheatBest)=names(TempX)[c(1,8:15)];WheatBest$var<-names(TempX)[2:5]
Wheat<-rbind(WheatBest,WheatAv)


#Europe Optimization
AlterW<-YieldW
AlterW<-subset(AlterW,Lat>50&Lat<61)
AlterW<-AlterW[-c(12:26,28,29)]

SSE<-data.frame("Num"=1)
SSE<-as.data.frame(seq(1:17280));names(SSE)[1]<-"Num"
for (i in SSE$Num){
  SSE[i,"MeanY"]=mean(AlterW$YTeW[which(AlterW$Num==i)])
}
SSE2<-join(SSE,AlterW[,c(1:9)],match='first')
SSE2<-join(SSE2,SSE3w[c(1,6,7)])
SSE2<-join(SSE2,summw)

WheatAvE<-data.frame(matrix(ncol=9))
WheatBestE<-data.frame(matrix(ncol=9))
for (i in 1:2){
  bestww<-subset(SSE3w,HIav_Wi<0.45&PercWWi>10)
  bestww<-bestww[order(bestww[,(i*2)+1]),][c(1:50),]
  bestww<-join(bestww,SSE2[c(1,2)])
  Temp2<-bestww[order(-bestww$MeanY),][c(1:8),c(1,8:15)]
  WheatBestE[i,]<-Temp2[1,]
  WheatAvE[i,]<-colMeans(Temp2)
}

names(WheatAvE)=names(bestww)[c(1,8:15)];WheatAvE$var<-paste0(names(bestww)[2:5],"Av")
names(WheatBestE)=names(bestww)[c(1,8:15)];WheatBestE$var<-names(bestww)[2:5]
WheatE<-rbind(WheatBestE,WheatAvE)

