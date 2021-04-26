rm(list =ls())
options(stringsAsFactors = F)
options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))) 

library(ggplot2)
library(ggrepel)
library(patchwork)

cnv <-read.csv(file.choose(), sep = ",", header = T)

#提取AMP，DEL
amp <-cnv[cnv$Status =="Amp",]
del <-cnv[cnv$Status =="Del",]

#1
amp1p <-amp[amp$P1 < 0.05,]
amp1f <-amp1p[amp1p$F1 >0.2,]
amp1fc <-amp1f[amp1f$FC1 >1,]
amp1n <-rbind(amp[amp$P1 >= 0.05,], amp1p[amp1p$F1 <= 0.2,], amp1f[amp1f$FC1 <=1,])

del1p <-del[del$P1 < 0.05,]
del1f <-del1p[del1p$F1 >0.2,]
del1fc <-del1f[del1f$FC1 >1,]
del1n <-rbind(del[del$P1 >= 0.05,], del1p[del1p$F1 <= 0.2,], del1f[del1f$FC1 <=1,])


a <-ggplot()+geom_bar(data=amp1fc, aes(Rank, F1), stat="identity",inherit.aes = F, fill="#C5533F") +
  geom_bar(data=amp1n, aes(Rank, F1), stat="identity",inherit.aes = F, fill="#b0b0b3")+
  geom_bar(data=del1fc, aes(Rank, -F1), stat="identity",inherit.aes = F, fill="#1272B4")+
  geom_bar(data=del1n, aes(Rank, -F1), stat="identity",inherit.aes = F,fill="#b0b0b3")+
  scale_y_continuous(limits = c(-1,1))+
  geom_text_repel(data=amp1fc, aes(Rank, F1+0.02, label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y = (0.6-(as.numeric(amp1fc$F1+0.02))),direction="y",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  geom_text_repel(data=del1fc, aes(Rank, -(F1+0.02), label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y = -(0.6-(as.numeric(del1fc$F1+0.02))),direction="y",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  coord_flip()+scale_x_reverse()+geom_rect(data = cnv, aes(ymin = -0.01, ymax = 0.01,
                                                               xmin = Rank-0.5, xmax = Rank+0.5, fill=Ch),
                                           inherit.aes = FALSE)+scale_fill_manual(values=c("grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey"))+
theme(legend.position = "none",
        line = element_line(size = 0.5),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(face = "bold",size = 11), 
        axis.text.x = element_text(face = "bold",size = 9),
        plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent"))+ 
        labs(title = c("Cluster I"), y=c("CNV Score"))



#2
amp2p <-amp[amp$P2 < 0.05,]
amp2f <-amp2p[amp2p$F2 >0.2,]
amp2fc <-amp2f[amp2f$FC2 >1,]
amp2n <-rbind(amp[amp$P2 >= 0.05,], amp2p[amp2p$F2 <= 0.2,], amp2f[amp2f$FC2 <=1,])

del2p <-del[del$P2 < 0.05,]
del2f <-del2p[del2p$F2 >0.2,]
del2fc <-del2f[del2f$FC2 >1,]
del2n <-rbind(del[del$P2 >= 0.05,], del2p[del2p$F2 <= 0.2,], del2f[del2f$FC2 <=1,])


b <-ggplot()+geom_bar(data=amp2fc, aes(Rank, F2), stat="identity",inherit.aes = F, fill="#C5533F") +
  geom_bar(data=amp2n, aes(Rank, F2), stat="identity",inherit.aes = F, fill="#b0b0b3")+
  geom_bar(data=del2fc, aes(Rank, -F2), stat="identity",inherit.aes = F, fill="#1272B4")+
  geom_bar(data=del2n, aes(Rank, -F2), stat="identity",inherit.aes = F,fill="#b0b0b3")+
  scale_y_continuous(limits = c(-1,1))+
  geom_text_repel(data=amp2fc, aes(x=Rank, y=F2+0.02, label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y = as.numeric(amp2fc$F2),direction="both",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  geom_text_repel(data=del2fc, aes(x=Rank, y=-(F2+0.02), label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y =-as.numeric(del2fc$F2),direction="both",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  coord_flip()+scale_x_reverse()+geom_rect(data = cnv, aes(ymin = -0.01, ymax = 0.01,
                                                           xmin = Rank-0.5, xmax = Rank+0.5, fill=Ch),
                                           inherit.aes = FALSE)+scale_fill_manual(values=c("grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey"))+
  theme(legend.position = "none",
        line = element_line(size = 0.5),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(face = "bold",size = 11), 
        axis.text.x = element_text(face = "bold",size = 9),
        plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent"))+ 
  labs(title = c("Cluster II"), y=c("CNV Score"))

b

#3
amp3p <-amp[amp$P3 < 0.05,]
amp3f <-amp3p[amp3p$F3 >0.2,]
amp3fc <-amp3f[amp3f$FC3 >1,]
amp3n <-rbind(amp[amp$P3 >= 0.05,], amp3p[amp3p$F3 <= 0.2,], amp3f[amp3f$FC3 <=1,])

del3p <-del[del$P3 < 0.05,]
del3f <-del3p[del3p$F3 >0.2,]
del3fc <-del3f[del3f$FC3 >1,]
del3n <-rbind(del[del$P3 >= 0.05,], del3p[del3p$F3 <= 0.2,], del3f[del3f$FC3 <=1,])


c <-ggplot()+geom_bar(data=amp3fc, aes(Rank, F3), stat="identity",inherit.aes = F, fill="#C5533F") +
  geom_bar(data=amp3n, aes(Rank, F3), stat="identity",inherit.aes = F, fill="#b0b0b3")+
  geom_bar(data=del3fc, aes(Rank, -F3), stat="identity",inherit.aes = F, fill="#1272B4")+
  geom_bar(data=del3n, aes(Rank, -F3), stat="identity",inherit.aes = F,fill="#b0b0b3")+
  scale_y_continuous(limits = c(-1,1))+
  geom_text_repel(data=amp3fc, aes(x=Rank, y=F3+0.02, label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y = as.numeric(amp3fc$F3),direction="both",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  geom_text_repel(data=del3fc, aes(x=Rank, y=-(F3+0.02), label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y =-as.numeric(del3fc$F3+0.05),direction="both",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  coord_flip()+scale_x_reverse()+geom_rect(data = cnv, aes(ymin = -0.01, ymax = 0.01,
                                                           xmin = Rank-0.5, xmax = Rank+0.5, fill=Ch),
                                           inherit.aes = FALSE)+scale_fill_manual(values=c("grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey"))+
  theme(legend.position = "none",
        line = element_line(size = 0.5),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(face = "bold",size = 11), 
        axis.text.x = element_text(face = "bold",size = 9),
        plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent"))+ 
  labs(title = c("Cluster III"), y=c("CNV Score"))

c
#4
amp4p <-amp[amp$P4 < 0.05,]
amp4f <-amp4p[amp4p$F4 >0.2,]
amp4fc <-amp4f[amp4f$FC4 >1,]
amp4n <-rbind(amp[amp$P4 >= 0.05,], amp4p[amp4p$F4 <= 0.2,], amp4f[amp4f$FC4 <=1,])

del4p <-del[del$P4 < 0.05,]
del4f <-del4p[del4p$F4 >0.2,]
del4fc <-del4f[del4f$FC4 >1,]
del4n <-rbind(del[del$P4 >= 0.05,], del4p[del4p$F4 <= 0.2,], del4f[del4f$FC4 <=1,])


d <-ggplot()+geom_bar(data=amp4fc, aes(Rank, F4), stat="identity",inherit.aes = F, fill="#C5533F") +
  geom_bar(data=amp4n, aes(Rank, F4), stat="identity",inherit.aes = F, fill="#b0b0b3")+
  geom_bar(data=del4fc, aes(Rank, -F4), stat="identity",inherit.aes = F, fill="#1272B4")+
  geom_bar(data=del4n, aes(Rank, -F4), stat="identity",inherit.aes = F,fill="#b0b0b3")+
  scale_y_continuous(limits = c(-1,1))+
  geom_text_repel(data=amp4fc, aes(x=Rank, y=F4+0.02, label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y = as.numeric(amp4fc$F4),direction="y",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  geom_text_repel(data=del4fc, aes(x=Rank, y=-(F4+0.02), label=Chr), inherit.aes = F, size=2,segment.size = 0.3,
                  nudge_y =-as.numeric(del4fc$F4),direction="y",arrow = arrow(angle=30,ends="last",length = unit(0.04, "inches")))+
  coord_flip()+scale_x_reverse()+geom_rect(data = cnv, aes(ymin = -0.01, ymax = 0.01,
                                                           xmin = Rank-0.5, xmax = Rank+0.5, fill=Ch),
                                           inherit.aes = FALSE)+scale_fill_manual(values=c("grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey", "black","grey", "black","grey", "black","grey", "black",
                                                                                           "grey", "black","grey"))+
  theme(legend.position = "none",
        line = element_line(size = 0.5),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(face = "bold",size = 11), 
        axis.text.x = element_text(face = "bold",size = 9),
        plot.title = element_text(hjust = 0.5))+
  theme( panel.background = element_rect(fill = "transparent"))+ 
  labs(title = c("Cluster IV"), y=c("CNV Score"))

a|b|c|d






