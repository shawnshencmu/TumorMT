rm(list =ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 
Args=commandArgs(T)
# Args =c(
#   "/root/emt/upload/glioma.txt",
#   "glioma",
#   "count",
#   "/root/emt/result"
# )
library(sva)
library(ggplot2)
library(patchwork)
library(pheatmap)
library(data.table)
#step1 data prepare
tcga <-fread("/root/emt/comparison/expression.csv", header = T, sep = ",", data.table = F) # 这个表就得1G
# tcga <-fread(file.choose(), header = T, sep = ",", data.table = F)
tcga <-cbind(tcga[,1], sample(tcga[,2:ncol(tcga)],1000,replace = F))
expr <-read.table(paste(Args[4],"/",Args[2],"-normalized.txt", sep = ""), header = T, sep = "\t")
# expr <-read.table(file.choose(), header = T, sep = "\t")
expr <-expr[!duplicated(expr[,1]),]
rownames(expr) <-expr[,1]
expr <-expr[,-1]
expr <-log(expr+1,2)
expr <-round(expr, digits = 2)
expr <-cbind(rownames(expr),expr)
names(expr)[1] <-c("Genes")
names(tcga)[1] <-c("Genes")
extc <-merge(expr,tcga, by = "Genes") #delete variate 这个表也很大
rownames(extc) <-extc[,1]
extc <-extc[,-1] 
exprgroup <-expr[1,2:ncol(expr)]
tcgagroup <-tcga[1,2:ncol(tcga)]
exprgroup[1,] <-c(1)
tcgagroup[1,] <-c(2)
batchgroup <-as.data.frame(t(cbind(exprgroup,tcgagroup)))
colnames(batchgroup) <-c("Group")

exprpcagroup <-expr[1,2:ncol(expr)]
tcgapcagroup <-tcga[1,2:ncol(tcga)]
exprpcagroup[1,] <-c("Sample")
tcgapcagroup[1,] <-c("TCGA")
pcagroup <-as.data.frame(t(cbind(exprpcagroup,tcgapcagroup)))
colnames(pcagroup) <-c("Group")
rm(tcga) #tcga这个可以删除了就

#step2 remove batcheffect
extc[is.na(extc)] <- 0
aver <-apply(extc, 1, mean)
aver <-round(aver, digits = 2)
extc[,ncol(extc)+1] <-aver
extc<-extc[!extc[,ncol(extc)] ==0,]
extc <-extc[,1:(ncol(extc)-1)]
extc <- as.matrix(extc)
batch = batchgroup$Group
combat <-ComBat(dat = extc, batch = batch, mod=NULL, par.prior=TRUE, prior.plots=FALSE)
combat <-round(combat,digits = 2)
geneset <-read.csv("/root/emt/comparison/geneset.csv", header = T, sep = ",")
# geneset <-read.csv(file.choose(), header = T, sep = ",")
combat <-as.data.frame(combat)
samplexpr <-combat[1:ncol(exprgroup)]
write.csv(samplexpr,paste(Args[4],"/",Args[2],"-expressionafterbatch.csv", sep = "")) 

extc <-as.data.frame(extc)
extc <-cbind(rownames(extc), extc)
names(extc)[1] <-c("Gene")
beforebatch <-c()
beforebatch <-merge(geneset, extc, by="Gene")
rownames(beforebatch) <-beforebatch[,1]
beforebatch <-beforebatch[,-1]
# # for (i in 1:nrow(geneset)) {
#   a <-extc[geneset[i,],]
#   
#   beforebatch <- rbind(beforebatch,a)}
beforebatch <-as.data.frame(t(beforebatch))
combat <-cbind(rownames(combat), combat)
names(combat)[1] <-c("Gene")
afterbatch <-c()
afterbatch <-merge(geneset, combat, by="Gene")
rownames(afterbatch) <-afterbatch[,1]
afterbatch <-afterbatch[,-1]
# # for (i in 1:nrow(geneset)) {
#   a <-combat[geneset[i,],]
#   
#   afterbatch <- rbind(afterbatch,a)}
afterbatch <-as.data.frame(t(afterbatch))

pcabefore <- prcomp(beforebatch)
pcsbefore <-data.frame(pcabefore$x, DATA = pcagroup$Group) 
m <-ggplot(pcsbefore,aes(x=PC1,y=PC2,color=DATA))+ geom_point(size =0.1, alpha=0.75)+stat_ellipse(level = 0.95, show.legend = F)+
  theme_minimal()+labs(title = "PCA analysis before batch")
pcaafter <- prcomp(afterbatch)
pcsafter <-data.frame(pcaafter$x, DATA = pcagroup$Group) 
n <-ggplot(pcsafter,aes(x=PC1,y=PC2,color=DATA))+ geom_point(size =0.1, alpha=0.75)+stat_ellipse(level = 0.95, show.legend = F)+
  theme_minimal()+labs(title = "PCA analysis after batch")
o <-m+n
ggsave(paste(Args[4],"/",Args[2],"-PCA.jpeg", sep = ""), o, height = 15, width = 30, units = c("cm"), dpi = 300)

rm(combat,expr,m,n,o,pcabefore,pcsbefore,pcaafter,pcsafter,a,extc)

#step3 scale and dist
sample <-afterbatch[1:ncol(exprgroup),]
tcga <-read.csv("/root/emt/comparison/tcga.csv", header = T, sep = ",",row.names = 1)
# tcga <-read.csv(file.choose(), header = T, sep = ",", row.names = 1)
sample <-as.data.frame(t(sample))
tcga <-as.data.frame(t(tcga))
sample <-cbind(rownames(sample), sample)
tcga <-cbind(rownames(tcga), tcga)
names(sample)[1] <-c("Gene")
names(tcga)[1] <-c("Gene")
a <-ncol(sample)-1
tc <-merge(sample, tcga, by="Gene")
rownames(tc) <-tc[,1]
tc <-tc[,-1]
sample <-tc[,1:a]
tcga <-tc[,(a+1):(ncol(tc))]
sample <-as.data.frame(t(sample))
tcga <-as.data.frame(t(tcga))
samplescale <-matrix(data = NA, nrow = nrow(sample), ncol = ncol(sample))
for (i in 1:nrow(sample))
 {

  tcscale <-rbind(sample[i,],tcga)
  tcscale <-scale(tcscale)
  samplescale[i,] <-tcscale[1,]


}
rownames(samplescale) <-rownames(sample)
colnames(samplescale) <-colnames(sample)
tcgascale <-scale(tcga)
tcgascale <-as.data.frame(tcgascale)
tcgascale <-as.data.frame(t(tcgascale))
dist <-c()
dist <-cbind(dist, tcgascale$TCGA.EJ.5521.01)
dist <-cbind(dist, tcgascale$TCGA.C5.A3HF.01)
dist <-cbind(dist, tcgascale$TCGA.C5.A1BJ.01)
dist <-cbind(dist, tcgascale$TCGA.EW.A1P7.01)
samplescale <-as.data.frame(t(samplescale))
dist <-cbind(dist,samplescale)
dist <-t(dist)
dist1 <-dist(dist)
dist1 <-as.matrix(dist1)
dist1 <-dist1[,1:4]
dist1 <-dist1[5:(ncol(exprgroup)+4),]
dist1 <-as.data.frame(dist1)
min <-apply(dist1, 1, min)
dist1 <-cbind(dist1, min)
for (i in 1:nrow(dist1))
{
  if (dist1[i,5] ==dist1[i,1]) {dist1[i,6] <-c(1)}
   else{
     if (dist1[i,5] ==dist1[i,2]) {dist1[i,6] <-c(2)}
     else{
       if (dist1[i,5] ==dist1[i,3]) {dist1[i,6] <-c(3)}
       else{dist1[i,6] <-c(4)}
     }
   }
  
  
}
dist1 <-dist1[,6]
dist1 <-as.data.frame(dist1)
rownames(dist1) <-colnames(exprpcagroup)
colnames(dist1) <-c("Cluster")
write.csv(dist1,paste(Args[4],"/",Args[2],"-cluster.csv", sep = ""))
#step4 pheatmap
mapdat <-as.data.frame(t(samplescale))
mapdat <-cbind(dist1, mapdat)
mapdat <-mapdat[order(mapdat[,1]),]
mapdat <-mapdat[,-1]
mapdat <-as.data.frame(t(mapdat))
bk=unique(c(seq(-2,2,length=100)))
mapdat[mapdat>2]=2 #限定上限，使表达量大于2的等于2
mapdat[mapdat< -2]= -2 #限定下限，使表达量小于-2的等于-2

#35gene 热图
jpeg(paste(Args[4],"/",Args[2],"-clusterresults.jpeg", sep = ""), width = 2000, height = 1400,units = "px", res = 300)
a <-pheatmap(mapdat,annotation_col=dist1,show_colnames =T,show_rownames =T,
         cluster_cols = FALSE,cluster_rows =T,scale="none",fontsize_row=6,fontsize_col=6,
         breaks=bk,color = colorRampPalette(c("#1B71AF", "white","#C35443"))(100),
         cellheight = 6, cellwidth = 10, main = "Cluster Results of Uploading Samples")
print(a)
dev.off()

q()
