rm(list = ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 
library("survminer")
library("survival")
dat <-read.csv(file.choose(), header = T, sep = ",")



fit <- survfit(Surv(OS.time, OS) ~ Cluster, data = dat)
jpeg(file="survival1.jpeg", width = 10,height = 15, units = "cm", res = 300)
p <-ggsurvplot(fit, data = dat, 
           title = "Survival Comarasion Among Different Clusters",
           pval = TRUE, pval.method = TRUE, 
           #surv.median.line = "hv", 
           legend.title = "Cluster",               # Change legend titles
           legend.labs = c("Cluster I", "Cluster II", "Cluster III", "Cluster IV"), 
           cumevents = F,
           xlab = "Time in Months",
           break.time.by = 60,
           palette = c("#226DA6","#BA5243","#468E5D","#926FA6"),
           risk.table = FALSE)
p
dev.off()

