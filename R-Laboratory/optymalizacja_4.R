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

TS <- function(z)
{
  x=ifelse(z>0,z,365+z)
  return(ifelse(x>=1&x<=31,NA,
  ifelse(x>=32&x<=59,NA,
  ifelse(x>=60&x<=90,NA,
  ifelse(x>=91&x<=120,10.8,
  ifelse(x>=121&x<=151,15.5,
  ifelse(x>=152&x<=181,19.1,
  ifelse(x>=182&x<=212,18.6,
  ifelse(x>=213&x<=243,18.8,
  ifelse(x>=244&x<=273,13.5,
  ifelse(x>=274&x<=304,8.3,
  ifelse(x>=305&x<=334,NA,
  ifelse(x>=335&x<=365,NA,NA)))))))))))))
}

PAI <- function(z)
{
  ifelse(z>=m,return(round(c+exp(b0+b1*z),digits = 0)),NA)
}

PMI <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI))+PAI(TS(Odwr_R(R(FoC,PEI)))))
}

###################################


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

ggplot(data_PMI,aes(Death,PEI,fill=TIME))+geom_tile()+scale_fill_gradient2(low="#FFFF00",mid="#FF9900",high = "#CC0000",midpoint=192.5,guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(64,321))+theme_minimal()+labs(fill='PMI',x='Findin of Cadaver',y='PEI')+ggtitle('PMI-CHART AS HEATMAP')
fig <-  plot_ly(data_PMI,x=~Death,y=~PEI,z=~TIME)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene=list(xaxis=list(title='Finding of Cadaver'),yaxis=list(title='PEI'),zaxis=list(title='PMI')))
fig

ggplot(data_PMI[7512:8700,],aes(Death,PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(77,115))+theme_minimal()+labs(fill='PMI',x='Finding of Cadaver (Day of a Year)',y='PEI (Days)')+ggtitle('PMI-CHART AS HEATMAP')


PMI2 <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI)))
}

PMI2(364,0)
