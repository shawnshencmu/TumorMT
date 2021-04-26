rm(list=ls())

options(stringsAsFactors = F)
# amplification and deletion should be calculated separately
mut <- read.csv(file.choose(),sep=",",header=T,row.names = 1)

chisp<-data.frame(data=NA)
#计算cluster内的频率
statf <-data.frame(data=NA)
#计算全局的频率
statt <-data.frame(data=NA)
#1
for (i in 5:ncol(mut)) {
   data1<-cbind(mut$Cluster.I,mut[,i])
   colnames(data1)[2] = "muti"
   #colnames(data1)[2] = colnames(mut)[i]
   colnames(data1)[1] = "Cluster"
   #colnames(data1)[1] = colnames(mut)[3]
   chi<- xtabs(~Cluster+muti,data =data1)
   Pi<-chisq.test(chi)
   chisp[i-4,1] <- Pi$p.value
   m <-as.data.frame(chi)
   statf[i-4,1] <-(m[4,3]/(m[2,3]+m[4,3]))
   statt[i-4,1] <-(m[4,3]/(m[2,3]+m[4,3]))/(m[3,3]/(m[3,3]+m[1,3]))
   
}

#2
for (i in 5:ncol(mut)) {
  data1<-cbind(mut$Cluster.II,mut[,i])
  colnames(data1)[2] = "muti"
  #colnames(data1)[2] = colnames(mut)[i]
  colnames(data1)[1] = "Cluster"
  #colnames(data1)[1] = colnames(mut)[3]
  chi<- xtabs(~Cluster+muti,data =data1)
  Pi<-chisq.test(chi)
  chisp[i-4,2] <- Pi$p.value
  m <-as.data.frame(chi)
  statf[i-4,2] <-(m[4,3]/(m[2,3]+m[4,3]))
  statt[i-4,2] <-(m[4,3]/(m[2,3]+m[4,3]))/(m[3,3]/(m[3,3]+m[1,3]))
}

#3
for (i in 5:ncol(mut)) {
  data1<-cbind(mut$Cluster.III,mut[,i])
  colnames(data1)[2] = "muti"
  #colnames(data1)[2] = colnames(mut)[i]
  colnames(data1)[1] = "Cluster"
  #colnames(data1)[1] = colnames(mut)[3]
  chi<- xtabs(~Cluster+muti,data =data1)
  Pi<-chisq.test(chi)
  chisp[i-4,3] <- Pi$p.value
  m <-as.data.frame(chi)
  statf[i-4,3] <-(m[4,3]/(m[2,3]+m[4,3]))
  statt[i-4,3] <-(m[4,3]/(m[2,3]+m[4,3]))/(m[3,3]/(m[3,3]+m[1,3]))
}

#4
for (i in 5:ncol(mut)) {
  data1<-cbind(mut$Cluster.IV,mut[,i])
  colnames(data1)[2] = "muti"
  #colnames(data1)[2] = colnames(mut)[i]
  colnames(data1)[1] = "Cluster"
  #colnames(data1)[1] = colnames(mut)[3]
  chi<- xtabs(~Cluster+muti,data =data1)
  Pi<-chisq.test(chi)
  chisp[i-4,4] <- Pi$p.value
  m <-as.data.frame(chi)
  statf[i-4,4] <-(m[4,3]/(m[2,3]+m[4,3]))
  statt[i-4,4] <-(m[4,3]/(m[2,3]+m[4,3]))/(m[3,3]/(m[3,3]+m[1,3]))
}

rownames(chisp)<- colnames(mut)[5:ncol(mut)]
colnames(chisp)<- colnames(mut)[1:4]
rownames(statf) <-rownames(chisp)
colnames(statf) <-colnames(chisp)
rownames(statt) <-rownames(chisp)
colnames(statt) <-colnames(chisp)

#fdr
fdrv <-chisp

fdrq <-matrix(data = NA, ncol = ncol(fdrv), nrow = nrow(fdrv))

for (i in 1:ncol(fdrv)) 
{
  fdr<-p.adjust(fdrv[,i],method="BH", n = nrow(fdrq))
  fdrq[,i] <-fdr
}
fdrq <-as.data.frame(fdrq)
rownames(fdrq) <-rownames(fdrv)
colnames(fdrq) <-colnames(fdrv)

write.csv(fdrq,file = "kafang.csv")
write.csv(statf, file = "incluster.csv")
write.csv(statt,file="totalcluster.csv")

