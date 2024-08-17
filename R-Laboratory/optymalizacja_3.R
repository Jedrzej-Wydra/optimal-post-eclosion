Start_time <- Sys.time()
K <- 434
t0 <- 11.3
c <- 7.3
b0 <- 8.735611
b1 <- -0.41162
m <- 13.3

library("plotly", lib.loc="D:/R/R-3.5.3/library")
library("dplyr", lib.loc="D:/R/R-3.5.3/library")
library(readxl)
temperatury <- read_excel("C:/Users/wwjed/OneDrive/V rok/sprawa pupariów/temperatury2.xlsx")

rownames(temperatury) <- c(-365:-1,1:365)

library(ggplot2)

ggplot(temperatury,aes(day,temp))+geom_line()+theme_minimal()

dt <- temperatury

dt$temp <- dt$temp - t0
dt$temp <- ifelse(dt$temp<0,0,dt$temp)

ggplot(dt,aes(day,temp))+geom_line()+theme_minimal()

Rx <- c(0)
for(i in 2:730)
{
  Rx[i] <- mean(c(dt$temp[i-1],dt$temp[i]))
}
Rx <- data.frame(day=dt$day,ADD=cumsum(Rx))

ggplot(Rx,aes(day,ADD))+geom_line()+theme_minimal()

#Funkcja R ju¿ pomniejszona o K

R <- function(FoC,PEI)
{
  f <- (FoC-PEI)==ceiling(Rx$day)
  return(Rx$ADD[f]-K)
}

Odwr_R <- function(z)
{
  f <- min(abs(Rx$ADD-z))==(abs(Rx$ADD-z))
  return(ifelse(ceiling(min((Rx$day[f])))<0,ceiling(max((Rx$day[f]))),ceiling(min((Rx$day[f])))))
}

T_14 <- mean(temperatury$temp[1:30])
for(i in 32:730)
{
  T_14[i-30] <- mean(temperatury$temp[(i-30):(i-1)])
}
T_14 <- c(rep(NA,30),T_14)
T_14 <- data.frame(day=Rx$day,T14=T_14)
M_T_14 <- function(z)
{
  f <- z==ceiling(T_14$day)
  return(T_14$T14[f])
}

PAI <- function(z)
{
  ifelse(z>=m,return(round(c+exp(b0+b1*z),digits = 0)),NA)
}

PAI2 <- function(z)
{
  ifelse(z>=9.9,return(round(c+exp(b0+b1*z),digits = 0)),113)
}

PAI_it <- function(d)
{
  i=30
  co=i
  cola <- 1
  while(co!=0&cola<1000)
  {
    x=match(d,ceiling(temperatury$day))
    y=mean(temperatury$temp[(x-i):x])
    a=PAI2((y))
    y=mean(temperatury$temp[(x-a):x])
    b=PAI2((y))
    co=a-b
    i=b
    cola <- cola+1
  }
  ifelse(co2>=1000,return(i),return(i))
}

#UWAGA! Zwróæ uwagê na funkcjê PAI! Pamiêtaj, ¿e PAI_it nie potrzebuje M_T_14, a PAI potrzebuje z³o¿enia.

PMI <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI))+(PAI_it((Odwr_R(R(FoC,PEI))))))
}

data_PMI <- data.frame(Death=rep(1,29),PEI=0:28)

for(i in 2:365)
{
  for(j in 0:28)
  {
    data_PMI <- rbind(data_PMI,c(i,j))
  }
}
time <- c()
for(i in 1:length(data_PMI$PEI))
{
  time <- c(time,PMI(data_PMI[i,1],data_PMI[i,2]))
}

data_PMI <- data.frame(data_PMI,TIME=time)

ggplot(data_PMI,aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(64,323))+theme_minimal()+labs(fill='PMI',x='Finding of Cadaver (Day of a Year)',y='PEI (Days)')+ggtitle('PMI-CHART AS HEATMAP')

ggplot(data_PMI[7222:8990,],aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(75,125))+theme_minimal()+labs(fill='PMI',x='Finding of Cadaver (Day of a Year)',y='PEI (Days)')+ggtitle('PMI-CHART AS HEATMAP (Second Part of a Year)')

ggplot(data_PMI[1:5278,],aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(163,325))+theme_minimal()+labs(fill='PMI',x='Finding of Cadaver (Day of a Year)',y='PEI (Days)')+ggtitle('PMI-CHART AS HEATMAP (First Part of a Year)')





#fig <-  plot_ly(data_PMI,x=~Death,y=~PEI,z=~TIME)
#fig <- fig %>% add_markers()
#fig <- fig %>% layout(scene=list(xaxis=list(title='Finding of Cadaver'),yaxis=list(title='PEI'),zaxis=list(title='PMI')))
#fig

#ggplot(data_PMI,aes(PEI,TIME))+geom_line(colour='#FF9900')+facet_wrap(~Death)+theme_minimal()+labs(fill='PMI',x='Findin of Cadaver',y='PEI')+ggtitle('PMI-CHART')

#data_PMI_2 <- subset(data_PMI,data_PMI$Death>=182&data_PMI$Death<=197)

#ggplot(data_PMI_2,aes(PEI,TIME))+geom_line(colour='#FF9900')+facet_wrap(~Death)+theme_minimal()+labs(fill='PMI',x='Findin of Cadaver',y='PEI')+ggtitle('PMI-CHART')

#PAI_CHART <- data.frame(day=1:365)
#PAI_Temp <- M_T_14(1)
#PAI_LENGTH <- PAI(M_T_14(1))
#for(i in 2:365)
#{
#  PAI_Temp <- c(PAI_Temp,M_T_14(i))
#  PAI_LENGTH <- c(PAI_LENGTH,PAI(M_T_14(i)))
#}
#PAI_CHART <- data.frame(day=1:365,temp=PAI_Temp,length=PAI_LENGTH)
#PAI_CHART$length <- ifelse(is.na(PAI_CHART$length),-1,PAI_CHART$length)
#ggplot(PAI_CHART,aes(day,length))+geom_point()+theme_minimal()

PMI2 <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI)))
}

Sys.time()-Start_time