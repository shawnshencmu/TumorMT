rm(list = ls())
options(stringsAsFactors = F)
express <-read.csv(file.choose(), sep = "," ,header = T, row.names = 1)

library(ggplot2)
library(ggsignif)
library(ggridges)

x <-ggplot(express,aes(x=IFNG, y=Cluster))+geom_density_ridges(aes(fill =Cluster ),alpha=0.8)+scale_fill_manual(values = c("#226DA6","#BA5243","#468E5D","#926FA6"))+
  labs(title = "")+theme_minimal()+theme(legend.title = element_blank())+theme(axis.text.x=element_text(size=20,angle = -45,hjust=1, face = "bold",color = "black",family="Arial"))+theme(axis.title.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position = "none")+theme(axis.text.y=element_text(size=20,face = "bold",color = "black",family="Arial"))


x
ggsave("ifng.jpeg", x, width = 10, height = 15, units = c("cm"), dpi = 300)




