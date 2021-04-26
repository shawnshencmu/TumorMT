rm(list = ls())
library(ggplot2)
library(RColorBrewer)
library(ggpointdensity)
options(stringsAsFactors = F)
x=colorRampPalette(rev(brewer.pal(n = 11, 
                                  name = "Spectral")))(256)
library(data.table)
express <-fread(file.choose(), header = T, sep = ",", data.table = F)
class <-fread(file.choose(), header = T, sep = ",", data.table = F)
express <-express[,1:37]
express[express <0] <-0
express <-cbind(express,class)

dat <-express

dat[dat$X176Lu_EQ7_PDL1 >3,36] <-3
datmk <-dat[dat$Tissue_Type=="SB_MK",]
datmk <-datmk[datmk$Patient_ID !="SB_MK2",]
datnc <-dat[dat$Tissue_Type=="SB_NC",]
datnc <-datnc[datnc$Patient_ID !="SB_NC2",]




# MK CD163

mkpdl1 <-ggplot()+geom_point(data = datmk[datmk$Macrophage!="Macrophage",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_point(data = datmk[datmk$Macrophage=="Macrophage",], aes(x=tsne_1,y=tsne_2,color=`X176Lu_EQ7_PDL1`),size=0.01)+scale_color_gradientn(colors=x)+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
mkpdl1
#  NC DC
ncpdl1 <-ggplot()+geom_point(data = datnc[datnc$Macrophage!="Macrophage",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_point(data = datnc[datnc$Macrophage=="Macrophage",], aes(x=tsne_1,y=tsne_2,color=`X176Lu_EQ7_PDL1`),size=0.01)+scale_color_gradientn(colors=x)+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
ncpdl1

ggsave("mkpdl1.jpeg",mkpdl1,width = 10,height = 10,units = c("cm"), dpi = 300) 
ggsave("ncpdl1.jpeg",ncpdl1,width = 10,height = 10,units = c("cm"), dpi = 300) 

