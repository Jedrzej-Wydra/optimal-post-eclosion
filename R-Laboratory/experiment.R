Start_time <- Sys.time()
K <- 240.2
t0 <- 8.9

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

PMI <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI)))
}

data_PMI <- data.frame(Death=rep(1,365),PEI=((-182):182))

time <- c()
for(i in 1:length(data_PMI$PEI))
{
  time <- c(time,PMI(data_PMI[i,1],data_PMI[i,2]))
}

data_PMI <- data.frame(data_PMI,TIME=time)

ggplot(data_PMI,aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(21,317))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('PMI-CHART AS HEATMAP')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(0,120,243,365))

wl <- data.frame(PEI=c(NA),PMI=c(NA),Date=c(NA))
for(i in c(183))
{
  for(j in ((-182):182))
  {
    wl <- rbind(wl,c(j,PMI(i,j),i))
  }
}
wl <- wl[-1,]

#wl$Date <- factor(recode(as.character(wl$Date),'16'="16. January",'45'="14. Febryary",'75'="16. March",'105'="15 April",'136'="16. May",'166'="15. June",'197'="16. July",'228'="16. August",'258'="15. September",'289'="16. October",'319'="15. November",'350'="16. December"),levels = c("16. January","14. Febryary","16. March","15 April","16. May","15. June",'197'="16. July","16. August","15. September","16. October","15. November","16. December"))

ggplot(wl,aes(PEI,PMI))+geom_line()+scale_x_continuous(breaks = c(-182,-91,0,91,182))+theme_minimal()+ggtitle("Protophormia terraenovae")


Sys.time()-Start_time