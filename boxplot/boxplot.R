library(plyr)
library(ggplot2)
library(ggpubr)
setwd("D:\\新冠inhouse\\代码上传\\boxplot") ###please change this path to "boxplot" file manually.
setname=c("trainingset","internaltest","172114","155454","177477","157103")
for (id in setname) {
  rt=read.table(paste0(id,"riskTest.txt"), header=T, sep="\t", check.names=F,row.names = 1)
  rt=rt[,c(1,ncol(rt))]
  colnames(rt)=c("group","Score")
  rt=rt[order(rt[,"group"]),]
  rt=as.data.frame(rt)
  rt[,1]=as.numeric(rt[,1])
  rt[,2]=as.character(rt[,2])
  trait="group"
  bioCol=c("#0066FF","#FF0000","#FF9900","#6E568C","#7CC767","#223D6C","#D20A13","#FFD121","#088247","#11AA4D")
  bioCol=bioCol[1:length(unique(rt[,trait]))]
  rt1=rt[,c(trait, "group")]
  colnames(rt1)=c("trait", "group")
  df=as.data.frame(table(rt1))
  df=ddply(df, .(group), transform, percent = Freq/sum(Freq) * 100)
  df=ddply(df, .(group), transform, pos = (cumsum(Freq) - 0.5 * Freq))
  df$label=paste0(sprintf("%.0f", df$percent), "%")
  df$group=factor(df$group, levels=c("Low", "High"))
  
  rt2=rt[,c(trait, "Score")]
  colnames(rt2)=c("trait", "Score")
  type=levels(factor(rt2[,"trait"]))
  comp=combn(type, 2)
  my_comparisons=list()
  for(i in 1:ncol(comp)){my_comparisons[[i]]<-comp[,i]}
  rt2[,1]=as.character(rt2[,1])
  rt2[,2]=as.numeric(rt2[,2])
  boxplot=ggboxplot(rt2, x="trait", y="Score", fill="trait",
                    xlab=trait,
                    ylab="BGE score",
                    legend.title=trait,
                    palette=bioCol
  )+  geom_jitter(width =0.2)+ 
    stat_compare_means(comparisons=my_comparisons,method= "t.test")
  pdf(file=paste0(id,"boxplot.pdf"),width=4,height=4.5)
  print(boxplot)
  dev.off()
  
}
