rm(list = ls())
options(stringsAsFactors = F)
express <-read.csv(file.choose(), sep = "," ,header = T, row.names = 1)

library(ggplot2)
library(ggsignif)
compar <-list(c("Cluster III","Cluster IV"), c("Cluster II","Cluster III"),c("Cluster II","Cluster IV"), c("Cluster I","Cluster II"),c("Cluster I","Cluster III"),c("Cluster I","Cluster IV"))
x <-ggplot(express, aes(x=Cluster, y=Mesenchymal.like.score, fill = Cluster)) + 
  geom_violin(trim =T)+
  geom_boxplot(width =0.1, outlier.alpha =0, fill = "white")+scale_fill_manual(values = c("#226DA6","#BA5243","#468E5D","#926FA6"))+
  geom_signif(comparisons = compar,step_increase = 0.075,map_signif_level = T,test = t.test, size = 1,textsize = 5)+
  labs(title = "",y="")+theme_minimal()+theme(legend.title = element_blank())+theme(axis.text.x=element_text(size=20,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position = "none")+theme(axis.text.y=element_text(size=20,face = "bold",color = "black",family="Arial"))
  
x
ggsave("MT.jpeg", x, width = 10, height = 15, units = c("cm"), dpi = 300)

