#AL <- data.frame(x=c(0,2,2,0),y=c(0,0,1,1),z=rep('A',4))
BL <- data.frame(x=c(4,7,7,4),y=c(0,0,1,1),z=rep('B',4))
CL <- data.frame(x=c(7,9,9,7),y=c(0,0,1,1),z=rep('C',4))
#DL <- data.frame(x=c(9,12,12,9),y=c(0,0,1,1),z=rep('D',4))
GL <- data.frame(x=c(2,4,4,2),y=c(0,0,1,1),z=rep('G',4))
#EL <- rbind(AL,GL,BL,CL,DL)
EL <- rbind(GL,BL,CL)
FL <- data.frame(x=c(2,4,7,9),y=c(1,1,1,1),z=rep('F',4),v=c('\nDeath','\nOviposition','\nEclosion','Finding of\nCadaver'))
RL <- data.frame(x=c(3,5.5,8),y=c(0.5,0.5,0.5),z=c('R','R','R'),v=c('PAI','DI','PEI'))
HL <- data.frame(x=c(5.5),y=c(-1),z=c('H'),v=c('PMI = PAI + DI + PEI'))
ggplot(EL,aes(x,y,fill=z))+geom_polygon()+theme_minimal()+theme(legend.position = "none")+scale_y_continuous(breaks=NULL,limits = c(-5,5))+scale_x_continuous(breaks = NULL,limits=c(0,12))+xlab('')+ylab('')+geom_point(data=FL,aes(x,y),shape=19)+geom_text(data=FL,aes(label=v),vjust=-0.25,size=7)+geom_text(data=RL,aes(label=v),vjust=0.17,size=7)+geom_text(data=HL,aes(label=v),vjust=0,size=7)

                                                                                                                                                                                                                                                                                                                                                                                                              