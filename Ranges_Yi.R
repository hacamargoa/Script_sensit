library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

##Reading the yield outputs from all the parameter setups in LPJ-GUESS, Outputs are 
##separated by the absence/inclusion of stem retranslocation Sret=0 or 1)

##Maize
yield_d2COa<-read.table("inputs/Maize/yieldCO.out",h=T);yield_d2COa$Sret=1
yield_d2COb<-read.table("inputs/Maize/yieldCO_nort.out",h=T);yield_d2COb$Sret=0
yield_d2CO<-rbind(yield_d2COa,yield_d2COb);
yield_d2CO<-yield_d2CO %>% relocate(Sret, .before = SLA)
#Separating "Medium" and "High" locations
yield_av<-aggregate(TeCSi~Lon+Lat,yield_d2CO,mean);
tresh=quantile(yield_av$TeCSi,0.60)
yield_av$Group<-ifelse(yield_av$TeCSi<tresh,"Medium","High")
yield_d2CO2<-join(yield_d2CO,yield_av[,c(1,2,4)])
varnames=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","N_red")
#Loop to create range bars and for irrigated maize
for (j in c("Medium","High")){
  plotf<-list()
  yield.summary<-list()
  yield_d2CO_2<-subset(yield_d2CO2,Group==j)
  sdev=sd(yield_d2CO_2$TeCSi)
  me=mean(yield_d2CO_2$TeCSi)
  counter = 0
  #Loop for each parameter,sd_TeCSi includes all the variability while sd_TeCSi_locat does not include location variability
  for (i in colnames(yield_d2CO_2)[2:9]){
    yield_d2CO_3<-yield_d2CO_2[,c(i,"Lon","Lat","TeCSi")];names(yield_d2CO_3)[1]<-"Var"
    yield.summary_all<- yield_d2CO_3 %>%
      group_by(Var) %>%
      summarise(
        sd_TeCSi = sd(TeCSi, na.rm = TRUE),
        mean_TeCSi = mean(TeCSi)
      )
    yield_sum2<-aggregate(TeCSi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,yield_d2CO_2,mean)
    yield_sum2<-yield_sum2[,c(i,"TeCSi")];names(yield_sum2)[1]<-"Var"
    yield.summary_loc <- yield_sum2 %>%
      group_by(Var) %>%
      summarise(
        sd_TeCSi_locat = sd(TeCSi, na.rm = TRUE),
        mean_TeCSi_locat= mean(TeCSi)
      )
    yield.summary[[i]]<-join(yield.summary_all,yield.summary_loc)
    counter=counter+1
    plotf[[i]]=ggplot(yield.summary[[i]]) +
      geom_errorbar(aes(x = mean_TeCSi, y = mean_TeCSi,
                        ymin = mean_TeCSi-sd_TeCSi,ymax = mean_TeCSi+sd_TeCSi,
                        color = as.character(Var)),width = 0.02,size = 2) +
      geom_errorbarh(aes(y = mean_TeCSi, xmin = mean_TeCSi_locat-sd_TeCSi_locat, xmax = mean_TeCSi_locat+sd_TeCSi_locat,
                         color = as.character(Var)),height = 0.02,size = 2,linetype = "longdash") +
      geom_point(size = 4, aes(x = mean_TeCSi, y = mean_TeCSi,color = as.character(Var))) + theme_light() + ggtitle(paste(varnames[counter],"Yield",j))+
      theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+
      xlab("Mean with one Std. without locations") + ylab("Mean with one Std. over all simulations") + xlim(me-sdev-0.11,me+sdev+0.18)+ylim(me-sdev-0.11,me+sdev+0.18)
  }
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))
}

##Wheat
##Requires a correction from output removing some repeated columns
yield_d2WWa<-read.table("inputs/Wheat/yieldWW.out",h=T);yield_d2WWa$Sret=1
yield_d2WWa<-yield_d2WWa[,-c(9:15)]
yield_d2WWb<-read.table("inputs/Wheat/yieldWW_nort.out",h=T);yield_d2WWb$Sret=0
yield_d2WWb<-yield_d2WWb[,-c(9:15)]
yield_d2WW<-rbind(yield_d2WWa,yield_d2WWb);
yield_d2WW<-yield_d2WW %>% relocate(Sret, .before = SLA)
##Assigning wheat cultivar to each location accorsing to observed data
ObsW<-read.csv("inputs/Wheat/Obs_wheat.csv",h=T)
coordW<-ObsW[,c(1,2,7)]
coordW$loc<-seq(1:20)
yield_d2WW2<-join(yield_d2WW,coordW)
yield_d2WW2$TeWi<-ifelse(yield_d2WW2$wheat=="Spring",yield_d2WW2$TeSWi,yield_d2WW2$TeWWi)
yield_d2WW2$TeW<-ifelse(yield_d2WW2$wheat=="Spring",yield_d2WW2$TeSW,yield_d2WW2$TeWW)
yield_avw<-aggregate(TeWi~Lon+Lat,yield_d2WW2,mean);
#Separating "Medium" and "High" locations
tresh=quantile(yield_avw$TeWi,0.50)
yield_avw$Group<-ifelse(yield_avw$TeWi<tresh,"Medium","High")
yield_d2WW3<-join(yield_d2WW2,yield_avw[,c(1,2,4)])
#Loop to create range bars and for irrigated wheat
for (j in c("Medium","High")){
  #range bars 
  plotf<-list()
  yield.summary<-list()
  yield_d2WW_2<-subset(yield_d2WW3,Group==j)
  sdev=sd(yield_d2WW_2$TeWi)
  me=mean(yield_d2WW_2$TeWi)
  counter = 0
  #Loop for each parameter,sd_TeWi includes all the variability while sd_TeWi_locat does not include location variability
  for (i in colnames(yield_d2WW_2)[2:9]){
    yield_d2WW_3<-yield_d2WW_2[,c(i,"Lon","Lat","TeWi")];names(yield_d2WW_3)[1]<-"Var"
    yield.summary_all<- yield_d2WW_3 %>%
      group_by(Var) %>%
      summarise(
        sd_TeWi = sd(TeWi, na.rm = TRUE),
        mean_TeWi = mean(TeWi)
      )
    
    yield_sum2<-aggregate(TeWi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,yield_d2WW_2,mean)
    yield_sum2<-yield_sum2[,c(i,"TeWi")];names(yield_sum2)[1]<-"Var"
    yield.summary_loc <- yield_sum2 %>%
      group_by(Var) %>%
      summarise(
        sd_TeWi_locat = sd(TeWi, na.rm = TRUE),
        mean_TeWi_locat= mean(TeWi)
      )
    yield.summary[[i]]<-join(yield.summary_all,yield.summary_loc)
    counter=counter+1
    plotf[[i]]=ggplot(yield.summary[[i]]) +
      geom_errorbar(aes(x = mean_TeWi, y = mean_TeWi,
                        ymin = mean_TeWi-sd_TeWi,ymax = mean_TeWi+sd_TeWi,
                        color = as.character(Var)),width = 0.02,size = 2) +
      geom_errorbarh(aes(y = mean_TeWi, xmin = mean_TeWi_locat-sd_TeWi_locat, xmax = mean_TeWi_locat+sd_TeWi_locat,
                         color = as.character(Var)),height = 0.02,size = 2,linetype = "longdash") +
      geom_point(size = 4, aes(x = mean_TeWi, y = mean_TeWi,color = as.character(Var))) + theme_light() + ggtitle(paste(varnames[counter],"Yield",j))+
      theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+
      xlab("Mean with one Std. without locations") + ylab("Mean with one Std. over all simulations") + xlim(me-sdev-0.07,me+sdev+0.09)+ylim(me-sdev-0.07,me+sdev+0.09)
  }
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))
}


