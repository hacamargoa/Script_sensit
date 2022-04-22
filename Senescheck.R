#Source("ANOVA_HI.R")
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Bugfix_cond_sens/ret/CO")
Alc_reg=read.table("alcmass_d2.out", h=TRUE)
Alc_reg$Sret=1;Alc_reg$Num=Alc_reg$Num+8640
Alc_nort=read.table("../../nort/CO/alcmass_d2.out",h=TRUE);Alc_nort$Sret=0
Alc=rbind(Alc_reg,Alc_nort)
Alc<-Alc %>% relocate(Sret, .before = SLA);Alc=Alc[,-c(13,15,16)]
Dlc_reg=read.table("dlcmass_d2.out", h=TRUE)
Dlc_reg$Sret=1;Dlc_reg$Num=Dlc_reg$Num+8640
names(Dlc_reg)[c(11,13)]=c("TeCSd","TeCSid")
Dlc_nort=read.table("../../nort/CO/dlcmass_d2.out", h=TRUE);Dlc_nort$Sret=0
names(Dlc_nort)[c(11,13)]=c("TeCSd","TeCSid")
Dlc=rbind(Dlc_reg,Dlc_nort)
Dlc<-Dlc %>% relocate(Sret, .before = SLA);Dlc=Dlc[,-c(13,15,16)]
senes<-join(Alc,Dlc)

senes$d_perc_rf=ifelse(senes$TeCSd==0,0,senes$TeCSd/(senes$TeCS+senes$TeCSd))
senes$d_perc_ir=ifelse(senes$TeCSid==0,0,senes$TeCSid/(senes$TeCSi+senes$TeCSid))

dev.new()
hist(senes$d_perc_rf)
hist(senes$d_perc_ir)

summ<-data.frame("Num"=1)
summ<-as.data.frame(seq(1:17280));names(summ)[1]<-"Num"
for (i in summ$Num){
  summ[i,"sen_loc_rf"]=length(senes$d_perc_rf[which(senes$Num==i&senes$d_perc_rf>=0.7)])
  summ[i,"sen_loc_ir"]=length(senes$d_perc_ir[which(senes$Num==i&senes$d_perc_ir>=0.7)])
  
}

#Wheat
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Bugfix_cond_sens/ret/WW")
Alc_reg=read.table("alcmass_d2.out", h=TRUE)
Alc_reg$Sret=1;Alc_reg$Num=Alc_reg$Num+8640
Alc_nort=read.table("../../nort/WW/alcmass_d2.out",h=TRUE);Alc_nort$Sret=0
Alc=rbind(Alc_reg,Alc_nort);Alc=Alc[,-c(9:15)]
Alc<-Alc %>% relocate(Sret, .before = SLA);Alc=Alc[-16]
Dlc_reg=read.table("dlcmass_d2.out", h=TRUE)
Dlc_reg$Sret=1;Dlc_reg$Num=Dlc_reg$Num+8640
names(Dlc_reg)[c(18:21)]=c("TeWWd","TeSWd","TeWWid","TeSWid")
Dlc_nort=read.table("../../nort/WW/dlcmass_d2.out", h=TRUE);Dlc_nort$Sret=0
names(Dlc_nort)[c(18:21)]=c("TeWWd","TeSWd","TeWWid","TeSWid")
Dlc=rbind(Dlc_reg,Dlc_nort);Dlc=Dlc[,-c(9:15)]
Dlc<-Dlc %>% relocate(Sret, .before = SLA);Dlc=Dlc[-16]
senesw<-join(Alc,Dlc)

senesw$d_perc_wwrf=ifelse(senesw$TeWWd==0,0,senesw$TeWWd/(senesw$TeWWd+senesw$TeWW))
senesw$d_perc_swrf=ifelse(senesw$TeSWd==0,0,senesw$TeSWd/(senesw$TeSWd+senesw$TeSW))
senesw$d_perc_wwir=ifelse(senesw$TeWWid==0,0,senesw$TeWWid/(senesw$TeWWid+senesw$TeWWi))
senesw$d_perc_swir=ifelse(senesw$TeSWid==0,0,senesw$TeSWid/(senesw$TeSWid+senesw$TeSWi))

dev.new()
hist(senesw$d_perc_wwrf)
hist(senesw$d_perc_swrf)
hist(senesw$d_perc_wwir)
hist(senesw$d_perc_swir)


summw<-data.frame("Num"=1)
summw<-as.data.frame(seq(1:17280));names(summw)[1]<-"Num"
for (i in summ$Num){
  summw[i,"PercWW"]=length(senesw$d_perc_wwrf[which(senesw$Num==i&senesw$d_perc_wwrf>=0.5)])
  summw[i,"PercSW"]=length(senesw$d_perc_swrf[which(senesw$Num==i&senesw$d_perc_swrf>=0.5)])
  summw[i,"PercWWi"]=length(senesw$d_perc_wwir[which(senesw$Num==i&senesw$d_perc_wwir>=0.5)])
  summw[i,"PercSWi"]=length(senesw$d_perc_swir[which(senesw$Num==i&senesw$d_perc_swir>=0.5)])
  
}  

