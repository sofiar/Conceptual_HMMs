#### HMM predictions for univariate time series
library(ggplot2)
library(tidyverse)

source('ExtraFunctions.R')


scenario.it=c('Low_Overlap','Medium_Overlap', 'High_Overlap')
degreeC.it=c('Low_Ac','Medium_Ac','High_Ac')

Scenario=c()
DegreeC=c()
zero.oneViterbi=c()
zero.oneFB=c()
CE=c()
CT.viterbi=c()
CT.FB=c()

for (jj in 1:3)
{
  
  for (j in 1:3)
  {
    zoViterbi=numeric(Nrep)
    zoFB=numeric(Nrep)
    CEFB=numeric(Nrep)
    CTViterbi=numeric(Nrep)
    CTFB=numeric(Nrep)
    
    Data=SimData %>% dplyr::filter(scenario==scenario.it[j] & degreeC==degreeC.it[jj])
    
    for (i in 1:Nrep)
    {
      test=Data %>% filter(Nreps==i)
      train=Data %>% filter(Nreps!=i)
      
      ###supervised fit
      fitted.init=train %>% filter(Index==1) %>% group_by(States) %>% summarize(N=n()/Nrep)
      fitted.init=fitted.init$N
      fitted.obs.dist=train %>% group_by(States) %>% summarise(M=mean(Obs),SD=sd(Obs))
      
      allprobs <- matrix(NA, nrow=Nsims, m)
      
      for (k in 1:m)
      {
        allprobs[,k]=dnorm(x=test$Obs,mean=fitted.obs.dist$M[k],sd=fitted.obs.dist$SD[k])
      }
      
      
      # tpm
      tpm_est = diag(m)
      n <- Nsims
      smat <- matrix(0, nrow =m, ncol = m)
        for (k in c(1:Nrep)[-i]) {
          actual.ts=train %>% filter(Nreps==k)

          ws=list()
          ss=list()
          for (s in 1:m)
          {

            ws[[s]] <- which(actual.ts$States[1:(n - 1)] == s)
            ss[[s]] <-s-as.numeric(actual.ts$States[ws[[s]]+1])
          }


         for (mm in 1:m){
           for (l in 1:m)
           {
            smat[l, mm]=smat[l,mm]+length(which(ss[[l]]==l-mm))
           }
           }
        }
          wsm <- rowSums(smat)
          for (l in 1:m)
          {
            tpm_est[l,]=smat[l,]/wsm[l]

          }


    
    ### Prediction, Viterbi 
          
    Viterbi.pred=HMM.viterbi(x=test$Obs, m =m,  gamma = tpm_est, allprobs = allprobs,
                      delta = fitted.init)  
          
    zoViterbi[i]=mean(Viterbi.pred==test$States)
          
    CTViterbi[i]=consecutives.true(Viterbi.pred==test$States)
    
      
    ### Prediction FB 
    mod=list()
    mod$m=m
    mod$mus=fitted.obs.dist$M
    mod$sigmas=fitted.obs.dist$SD
    mod$delta=fitted.init
    mod$tpm=tpm_est
    FB.probs=norm.HMM.state_probs(x=test$Obs , mod ,allprobs)
    FB.pred=apply(FB.probs,2,which.max)    
    zoFB[i]=mean(FB.pred==test$States)
    
    quienes=which(FB.pred==test$States)
    CEFB[i]=cross.entropy(FB.probs,test$States,quienes)
    CTFB[i]=consecutives.true(FB.pred==test$States)
    
  }
    Scenario=c(Scenario,rep(scenario.it[j],Nrep))
    DegreeC=c(DegreeC,rep(degreeC.it[jj],Nrep))
    zero.oneViterbi=c(zero.oneViterbi,zoViterbi)
    zero.oneFB=c(zero.oneFB,zoFB)
    CE=c(CE,CEFB)
    CT.viterbi=c(CT.viterbi,CTViterbi)
    CT.FB=c(CT.FB,CTFB)
    
    
    }
}


## Summarize and plot results


Model=c(rep('FB',length(DegreeC)),rep('Viterbi',length(DegreeC)))
Res.HMMs=data.frame(DegreeC=rep(DegreeC,2),Scenario=rep(Scenario,2),zero.one.loss=c(zero.oneFB,zero.oneViterbi),CT=c(CT.FB,CT.viterbi),Model=Model,CE=c(CE,rep(NA,length(CE))))


Res.HMMs$Scenario=as.factor(Res.HMMs$Scenario)
Res.HMMs$Scenario= ordered(Res.HMMs$Scenario ,levels=c('Low_Overlap','Medium_Overlap','High_Overlap'))

Res.HMMs$DegreeC=as.factor(Res.HMMs$DegreeC)
Res.HMMs$DegreeC=ordered(Res.HMMs$DegreeC,levels=c('Low_Ac','Medium_Ac','High_Ac'))

ggplot(Res.HMMs)+geom_boxplot(aes(Model,zero.one.loss))+facet_wrap(~Scenario+DegreeC)+theme_bw()

ggplot(Res.HMMs)+geom_boxplot(aes(Model,CT))+facet_wrap(~Scenario+DegreeC)+theme_bw()



ggplot(Res.HMMs %>% filter(Model=='FB'))+geom_boxplot(aes(Model,CE))+facet_wrap(~Scenario+DegreeC)+theme_bw()





