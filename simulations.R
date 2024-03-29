### Simulation scheme 

# X1 ~ N(,)
# X2 ~ N(,)

# we should change overlap and autocorrelation degree between the observation distributions 
# then fit and predict with an HMM, random forest, SVM, LDA, and QDA (?)

####################
#### Scenario 1 ####
####################
# No overlap between X1 and X2
# mu1 and mu2 take distant values

##
# 1.a) No autocorrelation 
# X1_i ~ N(mu1,sigma1)
# X2_i ~ N(mu2,sigma2)
##

##
# 1.b) Medium autocorrelation (medium values for alpha)
# X1_i ~ N(mu_1+alpha*X1_{i-1},sigma1)
# X2_i ~ N(mu_2+alpha*X2_{i-1},sigma2)
##

##
# 1.c) High autocorrelation (high values for alpha)
# X1_i ~ N(mu_1+alpha*X1_{i-1},sigma1)
# X2_i ~ N(mu_2+alpha*X2_{i-1},sigma2)
##

####################
#### Scenario 2 ####
####################
# Medium overlap between X1 and X2
# mu1 and mu2 take ~close values

##
# 2.a) No autocorrelation 
# X1_i ~ N(mu1,sigma1)
# X2_i ~ N(mu2,sigma2)
##

##
# 2.b) Medium autocorrelation (medium values for alpha)
# X1_i ~ N(mu_1+alpha*X1_{i-1},sigma1)
# X2_i ~ N(mu_2+alpha*X2_{i-1},sigma2)
##

##
# 2.c) High autocorrelation (high values for alpha)
# X1_i ~ N(mu_1+alpha*X1_{i-1},sigma1)
# X2_i ~ N(mu_2+alpha*X2_{i-1},sigma2)
##

####################
#### Scenario 3 ####
####################
# No overlap between X1 and X2
# mu1 and mu2 take really close values

##
# 3.a) No autocorrelation 
# X1_i ~ N(mu1,sigma1)
# X2_i ~ N(mu2,sigma2)
##

##
# 3.b) Medium autocorrelation (medium values for alpha)
# X1_i ~ N(mu_1+alpha*X1_{i-1},sigma1)
# X2_i ~ N(mu_2+alpha*X2_{i-1},sigma2)
##

##
# 3.c) High autocorrelation (high values for alpha)
# X1_i ~ N(mu_1+alpha*X1_{i-1},sigma1)
# X2_i ~ N(mu_2+alpha*X2_{i-1},sigma2)
##



###############################################################################
library(tidyverse)
library(ggplot2)

# initial distribution
del <- c(1,1,1)/3
#tpm=matrix(c(0.98,0.01,0.01,0.005,0.99,0.005,0.005,0.015,0.98),ncol=3,byrow=T) # check this 
m=length(del)
# Set Observation parameters
# check if these values make sense: Do they give an stationary process

sigma1=1
sigma2=1
sigma3=1

# mu1=c(-2,0,0)
# mu2=c(2,1,0.5)
# mu3=c(4,2,1)

mu1=matrix(c(-2,-1,-0.5,-1,-0.5,-0.25,-0.4,-0.2,-0.1),ncol=3) # byrow Overlap degree, by column ar degree
mu2=matrix(rep(0,9),ncol=3)
mu3=matrix(c(2,1,0.5,1,0.5,0.25,0.4,0.2,0.1),ncol=3)

Mu=list(mu1,mu2,mu3)

alpha=c(0,0.5,.8) 

Nsims=500
Nrep=50

set.seed(99999)
# 1. simulate states
states <- matrix(NA, nrow = Nsims, ncol = Nrep)

for (j in 1:Nrep)
{
for (i in 1:Nsims)
{
  if (i == 1) {
    states[1, j] <- sample(x = 1:m, size = 1, prob = del) 
  }
  else {
    states[i, j] <- sample(x = 1:m, size = 1, prob = tpm[states[i -1, j], ])
      }
    }
  }

# 2. simulate observation process

AllS=list(Scenario1=list(),Scenario2=list(),Scenario3=list())

for (s in 1:3) # for each overlap scenario
{
  AllS[[s]]=list()
  for (j in 1:3) # for each autorregresive str scenario
  {
  AllS[[s]][[j]]=matrix(NA, nrow = Nsims,ncol=Nrep)
    
    
    for (i in 1:Nrep) # for each time series
    {
  
      cs=states[1,i]
      AllS[[s]][[j]][1,i]=rnorm(1,mean = Mu[[cs]][s,j],sd=1) # first obs
  
      for (k in 2:Nsims) # for each observation
      {
        cs=states[k,i]
        mm=Mu[[cs]][s,j]+AllS[[s]][[j]][k-1]*alpha[j]
        AllS[[s]][[j]][k,i]=mm+rnorm(1,mean = 0,sd=1)
  
      }
  
      }

    
      }
  }


# 3. Creation of the Data Frame with the simulated data
States=as.factor(rep(as.vector(states),9))
Nreps=as.factor(rep(rep(1:Nrep,each=Nsims),9))
scenario=as.factor(c(rep('Low_Overlap',3*Nrep*Nsims),rep('Medium_Overlap',3*Nrep*Nsims),rep('High_Overlap',3*Nrep*Nsims)))
degreeC=as.factor(rep(rep(c('Low_Ac','Medium_Ac','High_Ac'),each=Nrep*Nsims),3))
Index=rep(1:Nsims,Nrep*9)
Obs=c()
for (i in 1:3)
{
  for (j in 1:3)
  {
    Obs=c(Obs,as.numeric(AllS[[i]][[j]]))
  }
  
}

SimData=data.frame(States,Nreps,scenario,degreeC,Obs,Index)

#SimData %>% filter(scenario=='Low_Overlap' & degreeC=='Low_Ac') %>% group_by(States) %>% summarize(mo=mean(Obs))


## plot some examples

ggplot(SimData %>% filter(Nreps==30),aes(x=Index,y=Obs,color=States)) +
  geom_line(aes(group=1))+theme_classic()+
  facet_grid(vars(degreeC),vars(scenario))
 

