rm(list = ls())
options(stringsAsFactors = F)

library(survival)
dat <-read.csv(file.choose(), header = T, row.names = 1, sep = ",")

cancertype <-read.csv(file.choose(), header = T, sep = ",")

result=c()

dat1 <-cbind(dat, scale(dat[,2]))

names(dat1)[5] <-c("score")

a=coxph(Surv(as.numeric(as.character(dat1$DSS.time)),as.numeric(as.character(dat1$DSS)))~as.numeric(as.character(dat1[,5])),data=dat1)
pvalue1=summary(a)
pvalue2=c(pvalue1$conf.int[1],pvalue1$conf.int[3],pvalue1$conf.int[4],pvalue1$waldtest[3],as.data.frame(summary(a)[7])[,4])
result=rbind(result,pvalue2)

for (i in (1:nrow(cancertype)))
{
  x=cancertype[i,1]
  dat2 <-dat[dat$Cancer.Type==x,]
  dat2 <-cbind(dat2, scale(dat2[,2]))
  a=coxph(Surv(as.numeric(as.character(dat2$DSS.time)),as.numeric(as.character(dat2$DSS)))~as.numeric(as.character(dat2[,5])),data=dat2)
  pvalue1=summary(a)
  pvalue2=c(pvalue1$conf.int[1],pvalue1$conf.int[3],pvalue1$conf.int[4],pvalue1$waldtest[3],as.data.frame(summary(a)[7])[,4])
  result=rbind(result,pvalue2)
  
  
}


colname=c("HR","low95","high95","p-value","z-score")
result3=rbind(colname,result)
write.csv(result3,"TCGA_UnivariateCOX_result.csv")
