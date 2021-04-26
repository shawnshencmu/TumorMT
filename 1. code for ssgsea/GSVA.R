rm(list = ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 
library(GSVA)
data=read.delim(file.choose(),row.names = 1,header = T, sep = ",") # curated expression profile could be downloaded at Zenodo
genelist=read.delim(file.choose(),row.names = 1,header=F)
nrow=nrow(genelist)
data=data.matrix(data)
genelist[genelist==""]=NA  
genesets=c()
for (i in 1:nrow)
  
{
  a=genelist[i,]
  a=a[!is.na(a)]  
  a=list(as.character(as.matrix(a)))  
  genesets=c(genesets,a)
}

result=gsva(data,genesets, method = c("ssgsea"), ssgsea.norm = F)
result=data.matrix(result)
rownames(result)=rownames(genelist)
colnames(result)=colnames(data)
write.csv(result,file="GSVA_result.csv", row.names = T)

