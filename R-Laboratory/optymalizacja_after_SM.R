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
  ifelse(z>=m,return(round(c+exp(b0+b1*z),digits = 0)),NA)
}

PAI2 <- function(z)
{
  ifelse(z>=9.9,return(round(c+exp(b0+b1*z),digits = 0)),113)
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

ggplot(data_PMI,aes(y=Death,x=PEI,fill=TIME))+geom_tile()+scale_fill_gradientn(colours=c("#CCFFCC","#FFFFCC","#FFFF99","#FFFF66","#FFFF33","#FFFF00","#FFCC33","#FFCC00","#FF9900","#FF6600","#FF3300","#CC3300","#CC3333","#990000","#660000","#330000","#000000"),guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(42,341))+theme_minimal()+labs(fill='PMI',y='Finding of Cadaver (Day of a Year)',x='PEI (Days)')+ggtitle('Stearibia nigriceps: PMI-CHART AS HEATMAP')+scale_x_continuous(breaks = c(0,30,60,90))+scale_y_continuous(breaks = c(0,120,243,365))

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

wl$Date <- factor(recode(as.character(wl$Date),'16'="16. January",'45'="14. February",'75'="16. March",'105'="15 April",'136'="16. May",'166'="15. June",'197'="16. July",'228'="16. August",'258'="15. September",'289'="16. October",'319'="15. November",'350'="16. December"),levels = c("16. January","14. February","16. March","15 April","16. May","15. June",'197'="16. July","16. August","15. September","16. October","15. November","16. December"))

ggplot(wl,aes(PEI,PMI))+geom_point()+facet_wrap(~Date,nrow = 3)+scale_x_continuous(breaks = c(0,30,60,90))+theme_minimal()+ylab("minPMI")+geom_smooth(method='loess')


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