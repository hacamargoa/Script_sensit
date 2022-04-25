source("Ranges_HI.R")
library(data.table)
library(DescTools)
library(stringi)

coord<-unique(HI_d2CO2[c("Lon","Lat")])
coord$loc<-seq(1:22)
#Maize
plotA<-list()
ANOVAy<-list()
##loop for each group to Run an ANOVA to evaluate the effect of the parameters on the variability
for (j in c("Medium","High")){
yield_d2CO_2<-subset(yield_d2CO2,Group==j)
yield_d2CO_2<-join(yield_d2CO_2,coord)
sisnmodel<-aov(TeCSi ~ loc+Sret*SLA*CN_min*CN_range*senNrate*senCrate*k_N*N_dem_re,yield_d2CO_2)
ANOVAy[[j]]<-summary (sisnmodel)
AOVdf<-as.data.frame(ANOVAy[[j]][[1]])
setDT(AOVdf,keep.rownames = TRUE);names(AOVdf)[1]<-"effects"
AOVdf$effects<-stri_replace_all_fixed(AOVdf$effects," ", "")
total<-sum(AOVdf$`Sum Sq`[-c(1,length(AOVdf$`Sum Sq`))])
SSperc<-data.frame(effects=names(yield_d2CO)[2:9])
#loop to obtain the sum of squares of single effect and the summatory of all the interactions for each parameter
for (i in names(yield_d2CO_2)[2:9]){
  temp<- subset(AOVdf,effects==i)
  temp2<-AOVdf[-c(1:9,length(AOVdf$`Sum Sq`)),]
  temp3<-temp2[temp2$effects %like% paste0("%",i,"%"),]
  SSperc[SSperc$effects==i,2]<-(temp$`Sum Sq`/total)*100
  SSperc[SSperc$effects==i,3]<-(sum(temp3$`Sum Sq`)/total)*100
}
SSperc$effects=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","Ndred")
test<- reshape2::melt(SSperc, id.vars='effects')
plotA[[j]]<-ggplot(test, aes(x=value, y=effects, fill=variable)) + geom_bar(stat = "identity", position="dodge")+scale_fill_hue(c = 40)+
  #theme(legend.title = element_blank(),legend.position= ifelse(j=="High","bottom","none"),legend.background = element_rect(fill = "white", color = "black"),
        #plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  xlab("%SS") + ylab("Parameter")+ ggtitle(paste("Maize Yield",j))+xlim(c(0,35))
}

dev.new()
do.call("grid.arrange",c(plotA, nrow = 2))


#Wheat
plotB<-list()
ANOVAY<-list()
for (j in c("Medium","High")){
  yield_d2WW_2<-subset(yield_d2WW3,Group==j)
  sisnmodel<-aov(TeWi ~ loc+Sret*SLA*CN_min*CN_range*senNrate*senCrate*k_N*N_dem_re,yield_d2WW_2)
  ANOVAY[[j]]<-summary (sisnmodel)
  AOVdf<-as.data.frame(ANOVAY[[j]][[1]])
  setDT(AOVdf,keep.rownames = TRUE);names(AOVdf)[1]<-"effects"
  AOVdf$effects<-stri_replace_all_fixed(AOVdf$effects," ", "")
  total<-sum(AOVdf$`Sum Sq`[-c(1,length(AOVdf$`Sum Sq`))])
  SSperc<-data.frame(effects=names(yield_d2WW)[2:9])
  for (i in names(yield_d2WW_2)[2:9]){
    temp<- subset(AOVdf,effects==i)
    temp2<-AOVdf[-c(1:9,length(AOVdf$`Sum Sq`)),]
    temp3<-temp2[temp2$effects %like% paste0("%",i,"%"), ]
    SSperc[SSperc$effects==i,2]<-(temp$`Sum Sq`/total)*100
    SSperc[SSperc$effects==i,3]<-(sum(temp3$`Sum Sq`)/total)*100
  }
  SSperc$effects=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","Ndred")
  test<- reshape2::melt(SSperc, id.vars='effects')
  plotB[[j]]<-ggplot(test, aes(x=value, y=effects, fill=variable)) + geom_bar(stat = "identity", position="dodge")+scale_fill_hue(c = 40)+
    #theme(legend.title = element_blank(),legend.position= ifelse(j=="High","bottom","none"),legend.background = element_rect(fill = "white", WWlor = "black"),
    #plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
    xlab("%SS") + ylab("Parameter")+ ggtitle(paste("Wheat Yield",j))+xlim(c(0,45))
}

dev.new()
do.call("grid.arrange",c(plotB, nrow = 2))

