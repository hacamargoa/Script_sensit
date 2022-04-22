source("C:/Users/hac809/Desktop/Sensitivity2/ANOVA.R")
library(data.table)
#library(DescTools)
library(stringi)
#library(gcookbook)
#library(plotly)

#Maize1
plotH<-list()
ANOVA<-list()
for (j in c("Medium","High")){
HI_d2CO_2<-subset(HI_d2CO2,Group==j)
HI_d2CO_2<-join(HI_d2CO_2,coord)
sisnmodel<-aov(TeCSi ~ loc+Sret*SLA*CN_min*CN_range*senNrate*senCrate*k_N*N_dem_re,HI_d2CO_2)
ANOVA[[j]]<-summary (sisnmodel)
AOVdf<-as.data.frame(ANOVA[[j]][[1]])
setDT(AOVdf,keep.rownames = TRUE);names(AOVdf)[1]<-"effects"
AOVdf$effects<-stri_replace_all_fixed(AOVdf$effects," ", "")
total<-sum(AOVdf$`Sum Sq`[-c(1,length(AOVdf$`Sum Sq`))])
SSperc<-data.frame(effects=names(yield_d2CO)[2:9])
for (i in names(yield_d2CO_2)[2:9]){
  temp<- subset(AOVdf,effects==i)
  temp2<-AOVdf[-c(1:9,length(AOVdf$`Sum Sq`)),]
  temp3<-temp2[temp2$effects %like% i, ]
  SSperc[SSperc$effects==i,2]<-(temp$`Sum Sq`/total)*100
  SSperc[SSperc$effects==i,3]<-(sum(temp3$`Sum Sq`)/total)*100
}
SSperc$effects=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","Ndred")
names(SSperc)[c(2,3)]<-c("Main","Interaction")
test<- reshape2::melt(SSperc, id.vars='effects')
plotH[[j]]<-ggplot(test, aes(x=value, y=effects, fill=variable)) + geom_bar(stat = "identity", position="dodge")+scale_fill_hue(c = 40)+
  #theme(legend.title = element_blank(),legend.position= ifelse(j=="High","bottom","none"),legend.background = element_rect(fill = "white", color = "black"),plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  xlab("%SS") + ylab("Parameter")+ggtitle(paste("Maize HI",j))+xlim(c(0,55))
}

dev.new()
do.call("grid.arrange",c(plotH, nrow = 2))

#Wheat
plotBh<-list()
ANOVAY<-list()
for (j in c("Medium","High")){
  HI_d2WW_2<-subset(HI_d2WW3,Group==j)
  sisnmodel<-aov(TeWi ~ loc+Sret*SLA*CN_min*CN_range*senNrate*senCrate*k_N*N_dem_re,HI_d2WW_2)
  ANOVAY[[j]]<-summary (sisnmodel)
  AOVdf<-as.data.frame(ANOVAY[[j]][[1]])
  setDT(AOVdf,keep.rownames = TRUE);names(AOVdf)[1]<-"effects"
  AOVdf$effects<-stri_replace_all_fixed(AOVdf$effects," ", "")
  total<-sum(AOVdf$`Sum Sq`[-c(1,length(AOVdf$`Sum Sq`))])
  SSperc<-data.frame(effects=names(HI_d2WW)[2:9])
  for (i in names(HI_d2WW_2)[2:9]){
    temp<- subset(AOVdf,effects==i)
    temp2<-AOVdf[-c(1:9,length(AOVdf$`Sum Sq`)),]
    temp3<-temp2[temp2$effects %like% i, ]
    SSperc[SSperc$effects==i,2]<-(temp$`Sum Sq`/total)*100
    SSperc[SSperc$effects==i,3]<-(sum(temp3$`Sum Sq`)/total)*100
  }
  SSperc$effects=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","Ndred")
  test<- reshape2::melt(SSperc, id.vars='effects')
  plotBh[[j]]<-ggplot(test, aes(x=value, y=effects, fill=variable)) + geom_bar(stat = "identity", position="dodge")+scale_fill_hue(c = 40)+
    #theme(legend.title = element_blank(),legend.position= ifelse(j=="High","bottom","none"),legend.background = element_rect(fill = "white", WWlor = "black"),
    #plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
    xlab("%SS") + ylab("Parameter")+ ggtitle(paste("Wheat HI",j))+xlim(c(0,55))
}

dev.new()
do.call("grid.arrange",c(plotBh, nrow = 2))


