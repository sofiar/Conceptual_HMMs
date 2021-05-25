#### Classify simulations
#source('simulations.R')


library(ggplot2)
library(tidyverse)
library(e1071)
library(randomForest)
library(MASS)
library(class)
library(nnet)


Scenario=c()
DegreeC=c()
zero.one.loss=c()
Model=c()
CE=c()
CT=c()


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

CT.LDA=numeric(Nrep)
CT.GLM=numeric(Nrep)
CT.KNN=numeric(Nrep)

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
  
  # post[test$States==2]=post2[test$States==2,2]
  # post[test$States==1]=post2[test$States==1,1]
  
  # quantify error
  AccE.LDA[i]=mean(test$States==predictions)
  quienes=which(test$States==predictions)
  
  CE.LDA[i]=cross.entropy(t(post2),test$States,quienes)
  CT.LDA[i]=consecutives.true(test$States==predictions)
  
  
  #Knn
  knn.preds=knn(as.matrix(train$Obs), as.matrix(test$Obs),train$States ,k=60)  ## select k with CV
  AccE.KNN[i]=mean(knn.preds==test$States)
  CT.KNN[i]=consecutives.true(knn.preds==test$States)
  
  #glm
  if (m==2)
  {
  glm.fit = glm(States~Obs, data = train, family = binomial)
  probs.glm=predict(glm.fit, test ,type ="response")
  
  post.glm=probs.glm
  post.glm[test$States==2]=1-post.glm[test$States==2]
  
  glm.pred=rep(1,dim(test)[1])
  glm.pred[probs.glm>.5]=2
  
  AccE.GLM[i]=mean(test$States==glm.pred)
  quienes=which(test$States==glm.pred)
  CE.GLM[i]=cross.entropy(rbind(1-probs.glm,probs.glm),test$States,quienes)
  }
  
  # glm, m>2
  
  model <- multinom(States~Obs, data = train,trace=FALSE)
  preds=predict(model,test,type='class')
  probs=predict(model,test,type='probs')
  
  AccE.GLM[i]=mean(test$States==preds)
  quienes=which(test$States==preds)
  CE.GLM[i]=cross.entropy(t(probs),test$States,quienes)
  
  CT.GLM[i]=consecutives.true(test$States==preds)
  
  #fit SVM model
  # svmfit = svm(States∼Obs, data = train , kernel ="linear", cost =10, scale = FALSE )
  # predictions=predict(svmfit,test)
  
}


Scenario=c(Scenario,rep(scenario.it[j],Nrep*3))
DegreeC=c(DegreeC,rep(degreeC.it[jj],Nrep*3))
zero.one.loss=c(zero.one.loss,c(AccE.LDA,AccE.KNN,AccE.GLM))
CE=c(CE,c(CE.LDA,CE.KNN,CE.GLM))
CT=c(CT,c(CT.LDA,CT.KNN,CT.GLM))

Model=c(Model,c(rep("LDA",Nrep),rep('KNN',Nrep),rep('GLM',Nrep)))

if (is.na(sum(CE.GLM)))
{
  print (j)
  print (jj)
  
}

}
}

## Summarize and plot results

Res.acc=data.frame(DegreeC,Scenario,zero.one.loss,Model,CE,CT=CT)


Res.acc$Scenario=as.factor(Res.acc$Scenario)
Res.acc$Scenario= ordered(Res.acc$Scenario ,levels=c('Low_Overlap','Medium_Overlap','High_Overlap'))

Res.acc$Model=as.factor(Res.acc$Model)

Res.acc$DegreeC=as.factor(Res.acc$DegreeC)
Res.acc$DegreeC=ordered(Res.acc$DegreeC,levels=c('Low_Ac','Medium_Ac','High_Ac'))

#AccuracyPLot=ggplot(Res.acc)+geom_boxplot(aes(Model,zero.one.loss))+facet_wrap(~Scenario+DegreeC)+theme_bw()
ggplot(Res.acc)+geom_boxplot(aes(Model,CT))+facet_wrap(~Scenario+DegreeC)+theme_bw()

#ggplot(Res.acc %>% filter(Model!='KNN'))+geom_boxplot(aes(Model,CE))+facet_wrap(~Scenario+DegreeC)+theme_bw()
#ggsave('MLclasRes.pdf',AccuracyPLot,width = 8,height = 8)

