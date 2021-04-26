rm(list =ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 

data <-read.csv(file.choose(), header = T, row.names = 1)
cluster <-as.matrix(table(data[,1]))
type <-as.matrix(table(data[,2]))

result <-matrix(data=NA, nrow = 4, ncol = 4)

for (i in 1:4){
  for (j in 1:4){
 
    datsm <-data[data$Cluster==rownames(cluster)[i],]
    datsm <-datsm[datsm$Type==rownames(type)[j],]
  result[i,j] <-phyper(nrow(datsm)-1,type[j],sum(type)-type[j],cluster[i])
  }
}
rownames(result) <-rownames(cluster)
colnames(result) <-rownames(type)

fb <-1-result
fb <-log10(fb)*(-1)

write.csv(fb,"hypergeo.csv")
