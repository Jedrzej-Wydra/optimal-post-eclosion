Start_time <- Sys.time()
K <- 434
t0 <- 6.4
c <- 7.3
b0 <- 8.735611
b1 <- -0.41162
m <- 6.4

library("plotly")
library("dplyr")
library(readxl)
temperatury <- read_excel("C:/Users/Blending_line/OneDrive/V rok/sprawa pupariów/temperatury2.xlsx")

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
  qwe=(round(c+exp(b0+b1*z),digits = 0))
  return(ifelse(qwe>=210,210,qwe))
}


PAI_it <- function(d)
{
  i=7
  co=i
  cola <- 1
  while(co!=0&cola<1000)
  {
    x=match(d,ceiling(temperatury$day))
    y=mean(temperatury$temp[(x-i):x])
    a=PAI((y))
    y=mean(temperatury$temp[(x-a):x])
    b=PAI((y))
    co=a-b
    i=b
    cola <- cola+1
  }
  return(i)
}

PAI_it_rev <- function(d)
{
  i=7
  co=i
  cola <- 1
  while(co!=0&cola<1000)
  {
    x=match(d,ceiling(temperatury$day))
    y=mean(temperatury$temp[x:(x+i)])
    a=PAI((y))
    y=mean(temperatury$temp[x:(x+a)])
    b=PAI((y))
    co=a-b
    i=b
    cola <- cola+1
  }
  return(i)
}

#UWAGA! Zwróæ uwagê na funkcjê PAI! Pamiêtaj, ¿e PAI_it nie potrzebuje M_T_14, a PAI potrzebuje z³o¿enia.

PMI <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI))+(PAI_it((Odwr_R(R(FoC,PEI))))))
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

ggplot(data_PMI,aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(42,375))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('Stearibia nigriceps: PMI-CHART AS HEATMAP')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(31,59,90,120,151,181,212,243,273,304,334,365),labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))+theme_minimal()

ggplot(data_PMI[8100:12740,],aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(209,264))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('Stearibia nigriceps: PMI-CHART AS HEATMAP\n(First Part of a Year)')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(90,106,122,140))

ggplot(data_PMI[24480:29120,],aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(56,138))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('Stearibia nigriceps: PMI-CHART AS HEATMAP\n(Second Part of a Year)')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(270,286,302,320))


wl <- data.frame(PEI=c(NA),PMI=c(NA),Date=c(NA))
for(i in c(16,45,75,105,136,166,197,228,258,289,319,350))
{
  for(j in seq(from=0,to=90,by=5))
  {
    wl <- rbind(wl,c(j,PMI(i,j),i))
  }
}
wl <- wl[-1,]

wl$Date <- factor(recode(as.character(wl$Date),
                         '16'="16. January",'45'="14. February",'75'="16. March",'105'="15. April",'136'="16. May",'166'="15. June",'197'="16. July",'228'="16. August",'258'="15. September",'289'="16. October",'319'="15. November",'350'="16. December"),
                  levels = c('16'="16. January",'45'="14. February",'75'="16. March",'105'="15. April",'136'="16. May",'166'="15. June",'197'="16. July",'228'="16. August",'258'="15. September",'289'="16. October",'319'="15. November",'350'="16. December"))

ggplot(wl,aes(PEI,PMI))+geom_point()+facet_wrap(~Date,nrow = 3)+scale_x_continuous(breaks = c(0,30,60,90))+theme_minimal()+ylab("minPMI")+geom_smooth(method='loess',se=F)



PMI2 <- function(FoC,PEI)
{
  return(FoC-Odwr_R(R(FoC,PEI)))
}

ggplot(data.frame(day=Rx$day,ADD=(Rx$ADD)),aes(day,ADD))+geom_line()
ggplot(temperatury,aes(day,temp))+geom_line()+geom_abline(slope=0,intercept = 7.3)

Sys.time()-Start_time
kontrola <- data.frame(x=c(0),y=c(PMI(0,0)))
for(i in 1:365)
{
  kontrola[i,] <- c(i,PMI(i,0))
}
ggplot(kontrola,aes(x,y))+geom_point()+geom_text(aes(label=x))+scale_x_continuous(limits = c(0,231))


check <- function(x)
{
  p1p=x-PMI(x,0)
  p2p=PAI_it_rev(p1p)
  p3p=p1p+p2p+PMI2(x,0)
  p3p>=x-5&p3p<=x+5
}

check2 <- function(x)
{
  p1p=x-PMI(x,0)
  p2p=PAI_it_rev(p1p)
  return(p1p+p2p+PMI2(x,0))
}

check3 <- function(x,Pei)
{
  p1p=x-PMI(x,Pei)
  p2p=PAI_it_rev(p1p)
  return(p1p+p2p+PMI2(x,Pei))
}

kontrola2 <- data.frame(x=c(0),y=c(0))
for(i in 1:231)
{
  kontrola2[i,] <- c(i,check(i))
}
ggplot(kontrola2,aes(x,y))+geom_point()

kontrola3 <- data.frame(x=c(0),y=c(PMI(0,0)),z=T)
for(i in 1:231)
{
  kontrola3[i,] <- c(i,PMI(i,0),(check(i)))
}
ggplot(kontrola3,aes(x,y,colour=z))+geom_point()+scale_x_continuous(limits = c(0,231))
