library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(ggplot2)
library(pROC)
library(kknn)
library(ranger)
library(e1071)
library(genalg)
setwd("D:\\新冠inhouse\\代码上传\\GA")  ####please change this path to "GA" file manually
trainingset=read.table("trainingsetriskfile.csv",sep = ",",row.names = 1,header = T)
gaevaluate <- function(indices) {
  result = 1
  if (sum(indices) > 1) {
    newtrainingset=cbind(trainingset[,1],trainingset[,(which(indices[]==1)+1)])
    colnames(newtrainingset)[1]="Severity"
    task = TaskRegr$new("jiayou",newtrainingset, target = "Severity")
    learner_logreg = lrn("regr.glmnet",alpha=1)
    learner_logreg$train(task)
    aucsum=0
    task2 = TaskRegr$new("intersetprediction", newtrainingset , target = "Severity")
    pred_logreg2 = learner_logreg$predict(task2)
    
    aucsum=cor(newtrainingset$Severity,pred_logreg2$response)
    
    result=1-aucsum
    
  }
  result
}
monitor <- function(obj) {
  minEval = min(obj$evaluations);
  plot(obj, type="hist");
}
woppa <- rbga.bin(size=50, mutationChance=0.02, zeroToOneRatio=3,
                  evalFunc=gaevaluate, verbose=TRUE, monitorFunc=monitor,popSize=50, iters=20)
trunofop=c()
for (i in 1:length(woppa$mean)) {
  trunofop=c(trunofop,i)
}
ftvalue=(woppa$mean-0.053)*4+0.05
data=cbind(trunofop,ftvalue)
colnames(data)=c("turn","value")
data=as.data.frame(data)
p=ggplot(data,aes(turn,value))+geom_line()+theme_bw()+
  labs(title = 'Fitness value in genetic algorithm',x="The round of optimazation",y="Fitness value")
pdf(file="GAplot.pdf", width=8, height=5)
p
dev.off()
