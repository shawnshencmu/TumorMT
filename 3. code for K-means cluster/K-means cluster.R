#! /usr/bin/env Rscript

rm(list = ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 
set.seed(1234)
library(ConsensusClusterPlus)

pheno <-read.table("pheno.txt", row.names = 1, header = T, sep = "\t")
pheno1 <-t(pheno)
pheno2 <-c(pheno1)
express <-read.csv("genechoose.csv", row.names = 1, header = T, sep = ",")
typ <-read.table("cancer type.txt", header = F, sep = "\t")


express2 <-express[,0]
for(i in 1:nrow(typ))
  {
  
  a <-typ[i,]
  pheno3 <- ifelse(pheno2 ==a, TRUE, FALSE)
  express1 <-express[pheno3]
  express2 <-cbind(express2,express1)
}

dat1 <-t(express2)
dat2 <-as.data.frame(dat1)
b <-as.data.frame(t(as.data.frame(scale(dat2, scale = T))))
data2 <-data.matrix(b)
a <-"all"
title= a
results = ConsensusClusterPlus(data2,maxK=20,reps=100, pItem=0.8,pFeature=1,title=title,innerLinkage="average",
                               finalLinkage="average",clusterAlg="km",distance="euclidean",plot="png")
write.csv(results[[2]][["consensusClass"]], paste(a,"2.csv"))    
write.csv(results[[3]][["consensusClass"]], paste(a,"3.csv"))    
write.csv(results[[4]][["consensusClass"]], paste(a,"4.csv"))    
write.csv(results[[5]][["consensusClass"]], paste(a,"5.csv"))    
write.csv(results[[6]][["consensusClass"]], paste(a,"6.csv"))    
write.csv(results[[7]][["consensusClass"]], paste(a,"7.csv"))    
write.csv(results[[8]][["consensusClass"]], paste(a,"8.csv"))    

write.csv(results[[9]][["consensusClass"]], paste(a,"9.csv"))    
write.csv(results[[10]][["consensusClass"]], paste(a,"10.csv"))    
write.csv(results[[11]][["consensusClass"]], paste(a,"11.csv"))    
write.csv(results[[12]][["consensusClass"]], paste(a,"12.csv"))    
write.csv(results[[13]][["consensusClass"]], paste(a,"13.csv"))    
write.csv(results[[14]][["consensusClass"]], paste(a,"14.csv"))    
write.csv(results[[15]][["consensusClass"]], paste(a,"15.csv")) 


write.csv(results[[16]][["consensusClass"]], paste(a,"16.csv"))    
write.csv(results[[17]][["consensusClass"]], paste(a,"17.csv"))    
write.csv(results[[18]][["consensusClass"]], paste(a,"18.csv"))    
write.csv(results[[19]][["consensusClass"]], paste(a,"19.csv"))    
write.csv(results[[20]][["consensusClass"]], paste(a,"20.csv")) 

icl = calcICL(results,title=title,plot="png")



rm(list = ls())
