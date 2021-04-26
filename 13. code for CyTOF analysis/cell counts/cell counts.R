library(ggplot2)
library(RColorBrewer)
library(ggpointdensity)
options(stringsAsFactors = F)
x=colorRampPalette(rev(brewer.pal(n = 11, 
                                  name = "Spectral")))(256)


dat<-read.csv(file.choose(),header = T,check.names = F, sep = ",")
datmk <-dat[dat$Tissue_Type=="SB_MK",]
datmk <-datmk[datmk$Patient_ID !="SB_MK2",]
datnc <-dat[dat$Tissue_Type=="SB_NC",]
datnc <-datnc[datnc$Patient_ID !="SB_NC2",]
# MK T
mkt <-ggplot()+geom_point(data = datmk[datmk$`T cell` !="T-cell",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_pointdensity(data = datmk[datmk$`T cell` =="T-cell",], aes(x=tsne_1,y=tsne_2),size=0.01)+scale_color_gradientn(colors=x,limits=c(0,500))+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
mkt
#  NC T
nct <-ggplot()+geom_point(data = datnc[datnc$`T cell` !="T-cell",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_pointdensity(data = datnc[datnc$`T cell` =="T-cell",], aes(x=tsne_1,y=tsne_2),size=0.01)+scale_color_gradientn(colors=x,limits=c(0,500))+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))#+theme(legend.position = "none")
nct

ggsave("mkt.jpeg",mkt,width = 10,height = 10,units = c("cm"), dpi = 300) 
ggsave("nct.jpeg",nct,width = 10,height = 10,units = c("cm"), dpi = 300) 


# MK DC
mkdc <-ggplot()+geom_point(data = datmk[datmk$`DC cell` !="DC",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_pointdensity(data = datmk[datmk$`DC cell` =="DC",], aes(x=tsne_1,y=tsne_2),size=0.01)+scale_color_gradientn(colors=x,limits=c(0,600))+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
mkdc
#  NC DC
ncdc <-ggplot()+geom_point(data = datnc[datnc$DC!="DC",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_pointdensity(data = datnc[datnc$DC =="DC",], aes(x=tsne_1,y=tsne_2),size=0.01)+scale_color_gradientn(colors=x,limits=c(0,600))+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
ncdc

ggsave("mkdc.jpeg",mkdc,width = 10,height = 10,units = c("cm"), dpi = 300) 
ggsave("ncdc.jpeg",ncdc,width = 10,height = 10,units = c("cm"), dpi = 300) 

# MK-macroph
mkmacro <-ggplot()+geom_point(data = datmk[datmk$Macrophage !="Macrophage",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_pointdensity(data = datmk[datmk$Macrophage =="Macrophage",], aes(x=tsne_1,y=tsne_2),size=0.01)+scale_color_gradientn(colors=x,limits=c(0,600))+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
mkmacro
#  NC DC
ncmacro <-ggplot()+geom_point(data = datnc[datnc$Macrophage!="Macrophage",], aes(x=tsne_1,y=tsne_2),color="grey81",size=0.01)+
  geom_pointdensity(data = datnc[datnc$Macrophage =="Macrophage",], aes(x=tsne_1,y=tsne_2),size=0.01)+scale_color_gradientn(colors=x,limits=c(0,600))+
  theme_bw()+
  theme(axis.text.x=element_text(size=15,angle = 30,hjust=1,face = "bold",color = "black",family="Arial"))+
  theme(axis.text.y=element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(panel.grid =element_blank())+
  theme(axis.title.y = element_text(size=15,face = "bold",color = "black",family="Arial"))+
  theme(axis.title.x = element_text(size=15,face = "bold",color = "black",family="Arial"))+theme(legend.position = "none")
ncmacro

ggsave("mkmacro.jpeg",mkmacro,width = 10,height = 10,units = c("cm"), dpi = 300) 
ggsave("ncmacro.jpeg",ncmacro,width = 10,height = 10,units = c("cm"), dpi = 300) 
