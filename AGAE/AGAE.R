library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(ggplot2)
library(pROC)
library(kknn)
library(ranger)
library(e1071)
library(genalg)
setwd("D:\\新冠inhouse\\代码上传\\AGAE")#####please change this path to "AGAE" file manually
load(".\\AGAE.RData")

aucvalue=c()
setname=c("trainingset","internaltest","172114","155454","177477","157103")
for (ID in setname) {
  print(ID)
  data=read.table(paste0(".\\",ID,"\\",ID,"riskfile.csv"),sep = ",",row.names = 1,header = T)
  
  ######
  owndata=data
  owndata=as.data.frame(owndata)
  for (i in 1:ncol(owndata)) {
    owndata[,i]=as.numeric(owndata[,i])
  }
  task2 = TaskRegr$new("intersetprediction", owndata , target = "Severity")
  pred_logreg = learner_logreg$predict(task2)
  a=c()
  if(ID%in%c("trainingset","internaltest","152418"))
  {
    for (i in 1:length(owndata$Severity)) {
      if(owndata$Severity[i]%in%c(1,2))
      {
        a=c(a,0)
      }
      else
      {
        a=c(a,1)
      }
    }
  }
  
  if(ID%in%c("172114","177477","157103"))
  {
    for (i in 1:length(owndata$Severity)) {
      if(owndata$Severity[i]%in%c(0))
      {
        a=c(a,0)
      }
      else
      {
        a=c(a,1)
      }
    }
  }
  
  if(ID%in%c("155454"))
  {
    for (i in 1:length(owndata$Severity)) {
      if(owndata$Severity[i]%in%c(0,1))
      {
        a=c(a,0)
      }
      else
      {
        a=c(a,1)
      }
    }
  }
  
  rocobj1<-roc(a,pred_logreg$response)
  pdf(file=paste0("./ROC/",ID,"multiROC.pdf"),width=6,height=6)
  plot(rocobj1, print.auc=TRUE)
  dev.off()
  
  
  aucvalue=c(aucvalue,auc(rocobj1))
  
  
  write.table(cbind(owndata,pred_logreg$response),
              file=paste0("./score/",ID,"riskTest.txt"),
              sep="\t",
              quote=F,
              row.names=T)
  
  
}
outdata=cbind(setname,aucvalue)
write.table(outdata,paste0("./ensemble_result.csv"),row.names=T,col.names=T,sep=",")
