rm(list =ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 
library(maftools)
library(data.table)
mutc <-fread(file.choose(), header = T, data.table = F) #mutation profile could be downloaded from Zenodo dataset
clin <-read.table(file.choose(), header = T, sep = "\t")
genelist <-read.csv(file.choose(), header = T)
names(genelist) <-c("Hugo_Symbol")
mutc <-merge(genelist, mutc, by="Hugo_Symbol")
mut <-read.maf(maf = mutc, clinicalData=clin)
colors <-c("Missense_Mutation"="#DE1E1A",
           "Frame_Shift_Del"="#1A6F34",
           "Nonsense_Mutation"="#173F90",
           "Frame_Shift_Ins"="#653487",
           "In_Frame_Del"="#95C882",
           "In_Frame_Ins"="#ECE535",
           "Nonstop_Mutation"="#CDB3CE",
           "Splice_Site"="#7A191E",
           "Translation_Start_Site"="#A9771B",
           "Multi_Hit"="#000000")
#先整体提一下，然后描绘一下特征
clin.clst1 <- subset(clin, Cluster=="Cluster I")$Tumor_Sample_Barcode
clst1 <-subsetMaf(maf=mut, tsb=clin.clst1, isTCGA=FALSE)
pdf("clusterI summary.pdf" )
plotmafSummary(maf=clst1, rmOutlier=TRUE, addStat="median", dashboard=TRUE, titvRaw = FALSE, color=colors)
dev.off()
clin.clst2 <- subset(clin, Cluster=="Cluster II")$Tumor_Sample_Barcode
clst2 <-subsetMaf(maf=mut, tsb=clin.clst2, isTCGA=FALSE)
pdf("clusterII summary.pdf" )
plotmafSummary(maf=clst2, rmOutlier=TRUE, addStat="median", dashboard=TRUE, titvRaw = FALSE, color=colors)
dev.off()
clin.clst3 <- subset(clin, Cluster=="Cluster III")$Tumor_Sample_Barcode
clst3 <-subsetMaf(maf=mut, tsb=clin.clst3, isTCGA=FALSE)
pdf("clusterIII summary.pdf" )
plotmafSummary(maf=clst3, rmOutlier=TRUE, addStat="median", dashboard=TRUE, titvRaw = FALSE, color=colors)
dev.off()
clin.clst4 <- subset(clin, Cluster=="Cluster IV")$Tumor_Sample_Barcode
clst4 <-subsetMaf(maf=mut, tsb=clin.clst4, isTCGA=FALSE)
pdf("clusterIV summary.pdf" )
plotmafSummary(maf=clst4, rmOutlier=TRUE, addStat="median", dashboard=TRUE, titvRaw = FALSE, color=colors)
dev.off()

#把非某一类的提取出来
clin.clst <- subset(clin, Cluster !="Other")$Tumor_Sample_Barcode
clst <-subsetMaf(maf=mut, tsb=clin.clst, isTCGA=FALSE)
clin.clstno1 <- subset(clin, Cluster !="Cluster I")$Tumor_Sample_Barcode
clstno1<-subsetMaf(maf=clst, tsb=clin.clstno1, isTCGA=FALSE)
clin.clstno2 <- subset(clin, Cluster !="Cluster II")$Tumor_Sample_Barcode
clstno2<-subsetMaf(maf=clst, tsb=clin.clstno2, isTCGA=FALSE)
clin.clstno3 <- subset(clin, Cluster !="Cluster III")$Tumor_Sample_Barcode
clstno3<-subsetMaf(maf=clst, tsb=clin.clstno3, isTCGA=FALSE)
clin.clstno4 <- subset(clin, Cluster !="Cluster IV")$Tumor_Sample_Barcode
clstno4<-subsetMaf(maf=clst, tsb=clin.clstno4, isTCGA=FALSE)
#进行计算比较
com1 <- mafCompare(m1=clst1, m2=clstno1, m1Name="Cluster I", m2Name="Other", minMut=5)
write.csv(com1$results, "Cluster I vs Others.csv",row.names = F)
com2 <- mafCompare(m1=clst2, m2=clstno2, m1Name="Cluster II", m2Name="Other", minMut=5)
write.csv(com2$results, "Cluster II vs Others.csv",row.names = F)
com3 <- mafCompare(m1=clst3, m2=clstno3, m1Name="Cluster III", m2Name="Other", minMut=5)
write.csv(com3$results, "Cluster III vs Others.csv",row.names = F)
com4 <- mafCompare(m1=clst4, m2=clstno4, m1Name="Cluster IV", m2Name="Other", minMut=5)
write.csv(com4$results, "Cluster IV vs Others.csv",row.names = F)


#!!!单独比较3和4
com34<-mafCompare(m1=clst4,m2=clst3, m1Name="Cluster IV",m2Name = "Cluster III", minMut = 5)
write.csv(com34$results, "Com34.csv",row.names = F)
pdf("com34.pdf" )
forestPlot(mafCompareRes=com34, pVal=0.000001, color=c("#926FA6","#468E5D"), geneFontSize=1)
dev.off()
#棒棒糖

pdf("bang1.pdf", width = 5, height = 5)
lollipopPlot2(m1=clst3,m2=clst4,m1_name="Cluster III", m2_name="Cluster IV", gene = "PTEN", colors = colors,domainLabelSize=0.0001,pointSize=0.5)
dev.off()
pdf("bang2.pdf", width = 5, height = 5)
lollipopPlot2(m1=clst3,m2=clst4,m1_name="Cluster III", m2_name="Cluster IV", gene = "PIK3CA", colors = colors,domainLabelSize=0.0001,pointSize=0.5)
dev.off()
pdf("bang3.pdf", width = 5, height = 5)
lollipopPlot2(m1=clst3,m2=clst4,m1_name="Cluster III", m2_name="Cluster IV", gene = "FAT1", colors = colors,domainLabelSize=0.0001,pointSize=0.5)
dev.off()
pdf("bang4.pdf", width = 5, height = 5)
lollipopPlot2(m1=clst3,m2=clst4,m1_name="Cluster III", m2_name="Cluster IV", gene = "CDKN2A", colors = colors,domainLabelSize=0.0001,pointSize=0.5)
dev.off()
#具体作图
gene <-read.csv(file.choose(), header = T, sep = ",")
gene <-gene[,1]
pdf("total.pdf")
oncoplot(maf=clst, genes =gene,borderCol=NULL,keepGeneOrder=T,fontSize =0.6, color=colors)
dev.off()

pdf("1.pdf")
oncoplot(maf=clst1, genes =gene,borderCol=NULL,keepGeneOrder=T,fontSize =0.6, color=colors)
dev.off()

pdf("2.pdf")
oncoplot(maf=clst2, genes =gene,borderCol=NULL,keepGeneOrder=T,fontSize =0.6, color=colors)
dev.off()

pdf("3.pdf")
oncoplot(maf=clst3, genes =gene,borderCol=NULL,keepGeneOrder=T,fontSize =0.6, color=colors)
dev.off()

pdf("4.pdf")
oncoplot(maf=clst4, genes =gene,borderCol=NULL,keepGeneOrder=T,fontSize =0.6, color=colors)
dev.off()



