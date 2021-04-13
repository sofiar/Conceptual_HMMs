#### Classify simulations
source('simulations.R')


library(ggplot2)
library(tidyverse)
library(e1071)
library(randomForest)
library(MASS)
library(class)

Scenario=c()
DegreeC=c()
Accuracy=c()
Model=c()
CE=c()


scenario.it=c('Low_Overlap','Medium_Overlap', 'High_Overlap')
degreeC.it=c('Low_Ac','Medium_Ac','High_Ac')
for (jj in 1:3)
{
  
for (j in 1:3)
{
AccE.LDA=numeric(Nrep)
AccE.KNN=numeric(Nrep)
AccE.GLM=numeric(Nrep)

CE.LDA=numeric(Nrep)
CE.GLM=numeric(Nrep)
CE.KNN=rep(NA,Nrep)


Data=SimData %>% dplyr::filter(scenario==scenario.it[j] & degreeC==degreeC.it[jj])

for (i in 1:Nrep)
{
  test=Data %>% filter(Nreps==i)
  train=Data %>% filter(Nreps!=i)
  
  # fit lda model
  ldafit = lda(States~Obs, data = train)
  pred=predict(ldafit,test)
  predictions=pred$class
  post2= pred$posterior

  post=numeric(length(test$States))
  
  post[test$States==2]=post2[test$States==2,2]
  post[test$States==1]=post2[test$States==1,1]
  
  # quantify error
  AccE.LDA[i]=mean(test$States==predictions)
  CE.LDA[i]=-sum((predictions==test$States)*log(post))
  
  #Knn
  knn.preds=knn(as.matrix(train$Obs), as.matrix(test$Obs),train$States ,k=60)  ## select k with CV
  AccE.KNN[i]=mean(knn.preds==test$States)
  
  
  #glm
  glm.fit = glm(States~Obs, data = train, family = binomial)
  probs.glm=predict(glm.fit, test ,type ="response")
  
  post.glm=probs.glm
  post.glm[test$States==2]=1-post.glm[test$States==2]
  
  glm.pred=rep(1,dim(test)[1])
  glm.pred[probs.glm>.5]=2
  
  AccE.GLM[i]=mean(test$States==glm.pred)
  CE.GLM[i]=-sum((glm.pred==test$States)*log(post.glm))
  
  
  
  #fit SVM model
  # svmfit = svm(States∼Obs, data = train , kernel ="linear", cost =10, scale = FALSE )
  # predictions=predict(svmfit,test)
  
}


Scenario=c(Scenario,rep(scenario.it[j],Nrep*3))
DegreeC=c(DegreeC,rep(degreeC.it[jj],Nrep*3))
Accuracy=c(Accuracy,c(AccE.LDA,AccE.KNN,AccE.GLM))
CE=c(CE,c(CE.LDA,CE.KNN,CE.GLM))
Model=c(Model,c(rep("LDA",Nrep),rep('KNN',Nrep),rep('GLM',Nrep)))

if (is.na(sum(CE.GLM)))
{
  print (j)
  print (jj)
  
}

}
}

## Summarize and plot results

Res.acc=data.frame(DegreeC,Scenario,Accuracy,Model,CE)


Res.acc$Scenario=as.factor(Res.acc$Scenario)
Res.acc$Scenario= ordered(Res.acc$Scenario ,levels=c('Low_Overlap','Medium_Overlap','High_Overlap'))

Res.acc$Model=as.factor(Res.acc$Model)

Res.acc$DegreeC=as.factor(Res.acc$DegreeC)
Res.acc$DegreeC=ordered(Res.acc$DegreeC,levels=c('Low_Ac','Medium_Ac','High_Ac'))

ggplot(Res.acc)+geom_boxplot(aes(Model,Accuracy))+facet_wrap(~Scenario+DegreeC)+theme_bw()

ggplot(Res.acc %>% filter(Model!='KNN'))+geom_boxplot(aes(Model,CE))+facet_wrap(~Scenario+DegreeC)+theme_bw()


