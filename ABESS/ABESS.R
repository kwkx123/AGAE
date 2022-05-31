#####perfrom ABESS algorithm on 5925 gene-pairs

library(abess)
setwd("D:\\新冠inhouse\\代码上传\\ABESS") ###please change this path to "ABESS" file manually. 
exp=read.table("5925gene_pairs.csv", header=T, sep=",", check.names=F, row.names=1)
exp=t(exp)
exp=as.matrix(exp)
labely=read.table("trainingset_cli.csv", header=T, sep=",", check.names=F, row.names=1)
labely=labely[,1]

abess_fit <- abess(exp, labely,support.size=0:100)
str(extract(abess_fit))
## helpful generic functions:
print(abess_fit)
coef(abess_fit, support.size = 5)
# predict(abess_fit,
#         newx = dataset[["x"]][51:100, ],
#         support.size = c(6)
# )
str(extract(abess_fit, 6))
deviance(abess_fit)
plot(abess_fit)
plot(abess_fit, type = "tune")
extract(abess_fit, 7)[4][1]
outlist=c("TSHZ1|BCAS2",  "U2AF2|PSMB9",  "RNF187|ITPKB", "RTF1|CD83",    "NCR3|TUBG1"  , "METTL9|RPL14", "TEX2|CASS4")
exp=exp[,outlist]
write.table(exp, file="7gene-pair.csv", sep=",", quote=F, col.names=T,row.names = T)
