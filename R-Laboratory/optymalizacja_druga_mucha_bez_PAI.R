Start_time <- Sys.time()
K <- 240.2
t0 <- 8.9

library("plotly")
library("dplyr")
library(readxl)
temperatury <- read_excel("C:/Users/Blending_line/OneDrive/V rok/sprawa pupariów/temperatury2.xlsx")

rownames(temperatury) <- c(-365:-1,1:365)

library(ggplot2)

ggplot(temperatury,aes(day,temp))+geom_line()+theme_minimal()+scale_y_continuous(breaks = seq(from=-20, to=30, by=1))+ylab("Temperature [C]")+geom_abline(intercept = 0,slope=0)+ggtitle("Average Daily Temperatures (Interpolation)")+xlab("Day of the Year")+scale_x_continuous(limits = c(-365,365),breaks = seq(from=-360,to=360,by=30))+theme(text=element_text(size=20))

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

#Funkcja R ju? pomniejszona o K

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

data_PMI <- data.frame(Death=rep(1,91),PEI=0:90)

for(i in 2:365)
{
  for(j in 0:90)
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

ggplot(data_PMI,aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(title="minPMI\n [days]",ticks=FALSE,barheight=10),limits=c(21,375))+theme_minimal()+labs(fill='PMI',y='Finding of a cadaver [day of a year]',x='PEI [days]')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(31,59,90,120,151,181,212,243,273,304,334,365),labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))+theme_minimal()+theme(text=element_text(size=20))

ggplot(data_PMI[8100:12740,],aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(196,247))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('Protophormia terraenovae: PMI-CHART AS HEATMAP\n(First Part of a Year)')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(90,106,122,140))

ggplot(data_PMI[24480:29120,],aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(40,119))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('Protophormia terraenovae: PMI-CHART AS HEATMAP\n(Second Part of a Year)')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(270,286,302,320))

wl <- data.frame(PEI=c(NA),PMI=c(NA),Date=c(NA))
for(i in c(16,45,75,105,136,166,197,228,258,289,319,350))
{
  for(j in seq(from=0,to=90,by=5))
  {
    wl <- rbind(wl,c(j,PMI(i,j),i))
  }
}
wl <- wl[-1,]

wl$Date <- factor(recode(as.character(wl$Date),'16'="16. January",'45'="14. February",'75'="16. March",'105'="15. April",'136'="16. May",'166'="15. June",'197'="16. July",'228'="16. August",'258'="15. September",'289'="16. October",'319'="15. November",'350'="16. December"),
                  levels = c('16'="16. January",'45'="14. February",'75'="16. March",'105'="15. April",'136'="16. May",'166'="15. June",'197'="16. July",'228'="16. August",'258'="15. September",'289'="16. October",'319'="15. November",'350'="16. December"))

ggplot(wl,aes(PEI,PMI))+geom_point()+facet_wrap(~Date,nrow = 3)+scale_x_continuous(breaks = c(0,30,60,90))+theme_minimal()+ylab("minPMI [days]")+xlab("PEI [days]")+theme(text=element_text(size=20))


Sys.time()-Start_time