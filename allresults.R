############## All classifications 1D
source('ExtraFunctions.R')
library(gridExtra)
library(cowplot)

### All combinations 
## Change depence degree in the state process
tpm.list=list()
tpm.list[[1]]=matrix(c(0.7,0.2,0.1,0.1,0.85,0.05,0.1,0.1,0.8),ncol=3,byrow=T) ## Low autocorrelation
tpm.list[[2]]=matrix(c(0.9,0.05,0.05,0.02,0.93,0.05,0.06,0.05,0.89),ncol=3,byrow=T) ## Medium autocorrelation
tpm.list[[3]]=matrix(c(0.98,0.01,0.01,0.005,0.99,0.005,0.005,0.015,0.98),ncol=3,byrow=T) # ## High autocorrelation


for (ind in 1:3)
{
  tpm=tpm.list[[ind]]

  source('simulations.R')
  source('classificationsML.R')
  source('hmmclassifications.R')
  
  ### combine results and plots
  levels(Res.HMMs$Model)=c('HMM-FB','HMM-Viterbi')
  
  Res.HMMs=Res.HMMs %>% relocate(names(Res.acc))
  All.res=rbind(Res.HMMs,Res.acc)
  
  write.csv(All.res,file=paste('ResSim1D_',as.character(ind),'.csv',sep=''))
  
  print(ind)
}
  

  
ResSim1D_1 <- read.csv("~/proyecto_doctoral/conceptualHMM/ResSim1D_1.csv")
ResSim1D_2 <- read.csv("~/proyecto_doctoral/conceptualHMM/ResSim1D_2.csv")
ResSim1D_3 <- read.csv("~/proyecto_doctoral/conceptualHMM/ResSim1D_3.csv")

# reorder levels
ResSim1D_1$DegreeC <- factor(ResSim1D_1$DegreeC, levels =c("Low_Ac","Medium_Ac","High_Ac"))
ResSim1D_2$DegreeC <- factor(ResSim1D_2$DegreeC, levels =c("Low_Ac","Medium_Ac","High_Ac"))
ResSim1D_3$DegreeC <- factor(ResSim1D_3$DegreeC, levels =c("Low_Ac","Medium_Ac","High_Ac"))

ResSim1D_1$Scenario <- factor(ResSim1D_1$Scenario, levels =c("Low_Overlap","Medium_Overlap","High_Overlap"))
ResSim1D_2$Scenario <- factor(ResSim1D_2$Scenario, levels =c("Low_Overlap","Medium_Overlap","High_Overlap"))
ResSim1D_3$Scenario <- factor(ResSim1D_3$Scenario, levels =c("Low_Overlap","Medium_Overlap","High_Overlap"))


DegreeC.state=c(rep('LOW ASP',dim(ResSim1D_1)[1]),rep('MED ASP',dim(ResSim1D_2)[1]),rep('HIGH ASP',dim(ResSim1D_3)[1]))


all=cbind(rbind(ResSim1D_1,ResSim1D_2,ResSim1D_3),DegreeC.state)

#Plots
## Accuracy

Acc1=ggplot(ResSim1D_1)+geom_boxplot(aes(Model,zero.one.loss,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme(legend.position = "none", axis.text.x=element_blank())
Acc2=ggplot(ResSim1D_2)+geom_boxplot(aes(Model,zero.one.loss,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme(legend.position = "none", axis.text.x=element_blank())
Acc3=ggplot(ResSim1D_3)+geom_boxplot(aes(Model,zero.one.loss,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme( axis.text.x=element_blank())
Acc1
Acc2
Acc3

CEplot1=ggplot(ResSim1D_1 %>% filter(Model!="KNN" & Model!='HMM-Viterbi'))+geom_boxplot(aes(Model,CE,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme(legend.position = "none", axis.text.x=element_blank())
CEplot2=ggplot(ResSim1D_2 %>% filter(Model!="KNN" & Model!='HMM-Viterbi'))+geom_boxplot(aes(Model,CE,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme(legend.position = "none", axis.text.x=element_blank())
CEplot3=ggplot(ResSim1D_3 %>% filter(Model!="KNN" & Model!='HMM-Viterbi'))+geom_boxplot(aes(Model,CE,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme( axis.text.x=element_blank())
CEplot1
CEplot2
CEplot3

CTplot1=ggplot(ResSim1D_1)+geom_boxplot(aes(Model,CT/500,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme(legend.position = "none", axis.text.x=element_blank())
CTplot2=ggplot(ResSim1D_2)+geom_boxplot(aes(Model,CT/500,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme(legend.position = "none", axis.text.x=element_blank())
CTplot3=ggplot(ResSim1D_3)+geom_boxplot(aes(Model,CT/500,col=Model))+facet_wrap(~Scenario+DegreeC)+theme_bw()+theme( axis.text.x=element_blank())

CTplot1
CTplot2
CTplot3


acc.plots=plot_grid(Acc1, Acc2, Acc3, labels=c("A", "B", "C"), ncol = 3, rel_widths =  c(1.5, 1.5,2))
#ggsave('all_AccIndex.pdf',acc.plots,width = 16,height = 9)

CE.plots=plot_grid(CEplot1,CEplot2, CEplot3, labels=c("A", "B", "C"), ncol = 3, rel_widths =  c(1.5, 1.5,2))
#ggsave('all_CEIndex.pdf',CE.plots,width = 16,height = 9)

CT.plots=plot_grid(CTplot1, CTplot2, CTplot3, labels=c("A", "B", "C"), ncol = 3, rel_widths =  c(1.5, 1.5,2))
#ggsave('all_CTIndex.pdf',CT.plots,width = 16,height = 9)
