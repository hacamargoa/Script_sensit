source("Ranges_YI.R")
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

HI_d2COa<-read.table("HICO.out",h=T);HI_d2COa$Sret=1
HI_d2COb<-read.table("HICO_nort.out",h=T);HI_d2COb$Sret=0
HI_d2CO<-rbind(HI_d2COa,HI_d2COb);
HI_d2CO<-HI_d2CO %>% relocate(Sret, .before = SLA)
HI_d2CO2<-join(HI_d2CO,yield_av[,c(1,2,4)])

for (j in c("Medium","High")){
#range bars Maize
plotf<-list()
HI.summary<-list()
HI_d2CO_2<-subset(HI_d2CO2,Group==j)
sdev=sd(HI_d2CO_2$TeCSi)
me=mean(HI_d2CO_2$TeCSi)
counter = 0
for (i in names(HI_d2CO_2)[2:9]){
  HI_d2CO_3<-HI_d2CO_2[,c(i,"Lon","Lat","TeCSi")];names(HI_d2CO_3)[1]<-"Var"
  HI.summary_all<- HI_d2CO_3 %>%
    group_by(Var) %>%
    summarise(
      sd_TeCSi = sd(TeCSi, na.rm = TRUE),
      mean_TeCSi = mean(TeCSi)
    )
    HI_sum2<-aggregate(TeCSi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,HI_d2CO_2,mean)
  HI_sum2<-HI_sum2[,c(i,"TeCSi")];names(HI_sum2)[1]<-"Var"
  HI.summary_loc <- HI_sum2 %>%
    group_by(Var) %>%
    summarise(
      sd_TeCSi_locat = sd(TeCSi, na.rm = TRUE),
      mean_TeCSi_locat= mean(TeCSi)
    )
  HI.summary[[i]]<-join(HI.summary_all,HI.summary_loc)
  counter=counter+1
  plotf[[i]]=ggplot(HI.summary[[i]]) +
    geom_errorbar(aes(x = mean_TeCSi, y = mean_TeCSi,
                      ymin = mean_TeCSi-sd_TeCSi,ymax = mean_TeCSi+sd_TeCSi,
                      color = as.character(Var)),width = 0.01,size = 2) +
    geom_errorbarh(aes(y = mean_TeCSi, xmin = mean_TeCSi_locat-sd_TeCSi_locat, xmax = mean_TeCSi_locat+sd_TeCSi_locat,
                       color = as.character(Var)),height = 0.01,size = 2,linetype = "longdash") + 
    geom_point(size = 4, aes(x = mean_TeCSi, y = mean_TeCSi,color = as.character(Var))) + theme_light() + ggtitle(paste(varnames[counter],"HI",j))+
    theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5), 
          legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+  
    xlab("Mean with one Std. without locations") + ylab("Mean with one Std. over all simulations")+xlim(me-sdev-0.10,me+sdev+0.05)+ylim(me-sdev-0.10,me+sdev+0.05)
}
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))

}

#wheat
HI_d2WWa<-read.table("HIWW.out",h=T);HI_d2WWa$Sret=1
HI_d2WWa<-HI_d2WWa[,-c(9:15)]
HI_d2WWb<-read.table("HIWW_nort.out",h=T);HI_d2WWb$Sret=0
HI_d2WWb<-HI_d2WWb[,-c(9:15)]
HI_d2WW<-rbind(HI_d2WWa,HI_d2WWb);
HI_d2WW<-HI_d2WW %>% relocate(Sret, .before = SLA)
HI_d2WW2<-join(HI_d2WW,coordw)
HI_d2WW2$TeWi<-ifelse(HI_d2WW2$wheat=="Spring",HI_d2WW2$TeSWi,HI_d2WW2$TeWWi)
HI_d2WW2$TeW<-ifelse(HI_d2WW2$wheat=="Spring",HI_d2WW2$TeSW,HI_d2WW2$TeWW)
HI_avw<-aggregate(TeWi~Lon+Lat,HI_d2WW2,mean);
HI_d2WW3<-join(HI_d2WW2,yield_avw[,c(1,2,4)])

for (j in c("Medium","High")){
  #range bars 
  plotf<-list()
  HI.summary<-list()
  HI_d2WW_2<-subset(HI_d2WW3,Group==j)
  sdev=sd(HI_d2WW_2$TeWi)
  me=mean(HI_d2WW_2$TeWi)
  counter = 0
  for (i in colnames(HI_d2WW_2)[2:9]){
    HI_d2WW_3<-HI_d2WW_2[,c(i,"Lon","Lat","TeWi")];names(HI_d2WW_3)[1]<-"Var"
    HI.summary_all<- HI_d2WW_3 %>%
      group_by(Var) %>%
      summarise(
        sd_TeWi = sd(TeWi, na.rm = TRUE),
        mean_TeWi = mean(TeWi)
      )
    
    HI_sum2<-aggregate(TeWi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,HI_d2WW_2,mean)
    HI_sum2<-HI_sum2[,c(i,"TeWi")];names(HI_sum2)[1]<-"Var"
    HI.summary_loc <- HI_sum2 %>%
      group_by(Var) %>%
      summarise(
        sd_TeWi_locat = sd(TeWi, na.rm = TRUE),
        mean_TeWi_locat= mean(TeWi)
      )
    HI.summary[[i]]<-join(HI.summary_all,HI.summary_loc)
    counter=counter+1
    plotf[[i]]=ggplot(HI.summary[[i]]) +
      geom_errorbar(aes(x = mean_TeWi, y = mean_TeWi,
                        ymin = mean_TeWi-sd_TeWi,ymax = mean_TeWi+sd_TeWi,
                        color = as.character(Var)),width = 0.02,size = 2) +
      geom_errorbarh(aes(y = mean_TeWi, xmin = mean_TeWi_locat-sd_TeWi_locat, xmax = mean_TeWi_locat+sd_TeWi_locat,
                         color = as.character(Var)),height = 0.02,size = 2,linetype = "longdash") +
      geom_point(size = 4, aes(x = mean_TeWi, y = mean_TeWi,color = as.character(Var))) + theme_light() + ggtitle(paste(varnames[counter],"HI",j))+
      theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+
      xlab("Mean with one Std. without locations") + ylab("Mean with one Std. over all simulations") + xlim(me-sdev-0.12,me+sdev+0.06)+ylim(me-sdev-0.12,me+sdev+0.06)
  }
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))
}
