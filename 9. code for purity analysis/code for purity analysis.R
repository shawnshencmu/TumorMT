##estimate文件包置于R-library文件夹下，输入文件TXT为第一行ID，第一列基因名，结果于文档下，.gct格式，Excel可打开##

data1=read.delim(file.choose(),row.names = 1,header = T) # expression profile could be downloaded at Zenodo dataset
library(estimate)
write.table(data1,file="data1.gct",sep="\t",quote=F)

filterCommonGenes("data1.gct","commongene.gct", id=c("GeneSymbol", "EntrezID"))##选取重叠基因

estimateScore("commongene.gct","ESTIMATE.gct",platform="illumina")###illumina/agilent/affymetrix

##purity = COS(0.6049872018+0.0001467884*estimatescore)##