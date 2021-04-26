rm(list =ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 
library(ggplot2)


express <-read.csv(file.choose(), header = T, row.names = 1, sep = ",")

express1 <-express[,4:ncol(express)]
express1 <-scale(express1)
express_pca <- prcomp(express1) 


express_pcs <-data.frame(express_pca$x, DATA = express$CancerType)  

CancerType=c(BLCA="#8D5C9F",BRCA="#CCA5B0", CESC="#BC99C3", COAD="#B87BAF", ESCA="#D187AE",
         GBM="#D0B37C", HNSC="#7AC18F", KIRC="#C24E7B", KIRP="#E9D6C9", LGG="#6B9E63",
         LIHC="#87C26A", LUAD="#CBE1DF", LUSC="#8E624F", OV="#D4C2DD", PAAD="#D4E0B2",
         PCPG="#BFDA8C", PRAD="#D6AE4F", READ="#DA9387", SKCM="#CBD95F", STAD="#764881",
         TGCT="#7194C8", THCA="#8ACDD4", THYM="#AAD5B3", UCEC="#88C054")


Cluster= c(`Cluster I`="#226DA6",`Cluster II` ="#BA5243",`Cluster III` ="#468E5D", `Cluster IV`="#926FA6")





p <-ggplot(express_pcs,aes(x=PC1,y=PC2,color=CancerType))+
  geom_point(size =0.5, alpha=1)+stat_ellipse(size=1.5,level = 0.50, show.legend = F)+
  scale_color_manual(values=CancerType)+labs(title = "PCA Analysis by Cancer Types")
mytheme<-theme_bw()+theme(panel.grid.major= element_line(color = "white"),panel.grid.minor =element_line(color= "white"),legend.title = element_blank())+theme(axis.text.x=element_text(size=12))+
theme(axis.text.y=element_text(size=12))+ theme(plot.title = element_text(hjust = 0.5))

p <-p+mytheme
p

ggsave("Cancer Type.jpeg",p,width = 23,height = 20,units = c("cm"),dpi = 300)

p <-ggplot(express_pcs,aes(x=PC1,y=PC2,color=Cluster))+
  geom_point(size =0.5, alpha=1)+stat_ellipse(size=1.5,level = 0.50, show.legend = F)+
  scale_color_manual(values=Cluster)+labs(title = "PCA Analysis by MT Type")
mytheme<-theme_bw()+theme(panel.grid.major= element_line(color = "white"),panel.grid.minor =element_line(color= "white"),legend.title = element_blank())+theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=12))+ theme(plot.title = element_text(hjust = 0.5))

p <-p+mytheme
p

ggsave("MT Type.jpeg",p,width = 23,height = 20,units = c("cm"),dpi = 300)


