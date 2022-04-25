source("Senescheck.R")
#library(FactoMineR)
#library(missMDA)
#library(ca)
#library(ncdf4)
#library(raster)
#library(rworldmap)
#library(ltm)
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)


#Maize
yieldAvg=read.csv("inputs/Maize/Obs_corn.csv",h=T)
Yield<-join(yield_d2CO2,yieldAvg)
Yield$Num<-ifelse(Yield$Sret==1,Yield$Num+8640,Yield$Num)
Yield$YTeCS=(Yield$TeCS*Yield$Arear+Yield$TeCSi*Yield$Areai)/(Yield$Areai+Yield$Arear)/(2*0.446*0.87)
Yield$DiffRay<-abs(Yield$YieldRay-Yield$YTeCS)/Yield$YieldRay
Yield$DiffCount<-abs(Yield$Fyield-Yield$YTeCS)/Yield$Fyield

HI<-join(HI_d2CO2,yieldAvg)
HI$Num<-ifelse(HI$Sret==1,HI$Num+8640,HI$Num)
HI$HTeCS=(HI$TeCS*HI$Arear+HI$TeCSi*HI$Areai)/(HI$Areai+HI$Arear)

####Evaluation by location. 
Yi_HIm=join(Yield[,c(1:11,22:24)],HI[,-c(12:21)])
Yi_HIm=join(Yi_HIm,summ)


###K_means function to identify two cultivars###

#Function to create graph of k_means
wssplot<-function(data, nc=15, seed=1234){
  wss<-nrow(data)-1*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=1)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of Squares")
}

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

wssplot(Ray_bestm[,c(4,5)])
km_clus<-kmeans(Ray_bestm[,c(4,5)],2)
autoplot(km_clus,Ray_bestm[,c(4,5)],frame=T)
Ray_bestm=cbind(Ray_bestm,Cluster=km_clus[[1]])

wssplot(Cou_bestm[,c(4,5)])
km_clusCou<-kmeans(Cou_bestm[,c(4,5)],2)
autoplot(km_clusCou,Cou_bestm[,c(4,5)],frame=T)
Cou_bestm=cbind(Cou_bestm,Cluster=km_clusCou[[1]])

###After results temperate regions above lat 35 degrees (N and S) where considered high yield (Cluster 1) except those in China 
###Quantile 80th of Distance for both reference yields and clusters 1 and 2 per setup
###Mean value of harvest index per cluster and setup

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

#Constraining setups to locations with more than 50% of locations with senescense and mean harvest index inside limits
Temp<-subset(SSE2m,sen_loc_ir>11&HIav_CL1<=0.59&HIav_CL2>=0.3)
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
YieldW<-join(yield_d2WW3[-17],ObsW)
YieldW$Fyield=YieldW$Fyield/10
YieldW$YTeW=(YieldW$TeW*YieldW$Arear+YieldW$TeWi*YieldW$Areai)/(YieldW$Areai+YieldW$Arear)/(2*0.446*0.88)
YieldW$Num<-ifelse(YieldW$Sret==1,YieldW$Num+8640,YieldW$Num)
YieldW$DiffRay<-abs(YieldW$YTeW-YieldW$YieldRay)/YieldW$YieldRay
YieldW$DiffCount<-abs(YieldW$YTeW-YieldW$Fyield)/YieldW$Fyield

HIw<-join(HI_d2WW3[-17],ObsW)
HIw$Num<-ifelse(HIw$Sret==1,HIw$Num+8640,HIw$Num)
HIw$HTeW=(HIw$TeW*HIw$Arear+HIw$TeWi*HIw$Areai)/(HIw$Areai+HIw$Arear)

Yi_HIw=join(YieldW[,c(1:11,25:28)],HIw[,c(1:11,26)])
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
WheatBest<-data.frame(matrix(ncol=9))
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
