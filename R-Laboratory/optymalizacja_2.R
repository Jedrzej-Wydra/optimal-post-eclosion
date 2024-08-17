start_time <- Sys.time()
library(readxl)
library("tidyr")
library("reshape2")
#importowanie danych
dtemp <- read_excel("C:/Users/Blending_line/OneDrive/V rok/sprawa pupariów/temperatury.xlsx")
dtemp$temp <- round(dtemp$temp,digits = 1)
View(dtemp)

#interpolacja

library(ggplot2)
library(ggthemes)

x<-dtemp$day
f<-dtemp$temp
fx <- data.frame(x,f)

#Wykres rozrzutu

ggplot(fx, aes(x,f))+geom_point(size=1)+theme_minimal()+scale_x_continuous(breaks=seq(from=0,to=365,by=30))+scale_y_continuous(breaks=seq(from=-26,to=37,by=1))+geom_hline(yintercept=0)+ggtitle('Average Daily Temperatures (Measurements)')+ylab('Temperature [C]')+xlab('Day of the Year')

ggplot(fx, aes(x,f))+geom_line(size=1)+theme_minimal()+scale_x_continuous(breaks=seq(from=0,to=365,by=30))+scale_y_continuous(breaks=seq(from=-26,to=37,by=1))+geom_hline(yintercept=0)+ggtitle('Average Daily Temperatures (Interpolation)')+ylab('Temperature [C]')+xlab('Day of the Year')

t0 <- 11.3
fx$f <- fx$f-t0
fx$f <- ifelse(fx$f<0,0,fx$f)

ggplot(fx, aes(x,f))+geom_line(size=1)+theme_minimal()+scale_x_continuous(breaks=seq(from=0,to=365,by=30))+scale_y_continuous(breaks=seq(from=-26,to=37,by=1))+geom_hline(yintercept=0)+ggtitle('Effective Temperatures')+ylab('Temperature')+xlab('Day of the Year')

Fx <- c(0)
for(i in 2:365)
{
  Fx[i] <- mean(c(fx$f[i-1],fx$f[i]))
}
Fx <- data.frame(x=x,FF=cumsum(Fx))

ggplot(Fx, aes(x,FF))+geom_line(size=1)+theme_minimal()+geom_hline(yintercept=0)+ggtitle('Cumulated Day-Degrees')+ylab('Cumulated Day-Degrees')+xlab('Day of the Year')

PEI <- c(0:30)
T <- seq(from=20,to=30,by=0.5)
PMI <- function(x,y)
{
  return((434/(x-11.3))+y)
}
hm <- outer(T,PEI,PMI)
hm <- as.data.frame(hm)
names(hm) <- as.character(PEI)
hm <- data.frame(T,hm)
names(hm) <- c('T',as.character(PEI))
View(hm)
hm %>% gather(a,b,'0':'30') -> hm
hm$b <- as.numeric(hm$b)
hm$a <- as.numeric(hm$a)
ggplot(hm,aes(a,T,fill=b))+geom_tile()+scale_fill_gradient2(low="#FFFF00",mid="#FF9900",high = "#CC0000",midpoint=50,guide=guide_colorbar(ticks=FALSE,barheight=10),limits=c(20,80))+theme_minimal()+labs(fill='PMI',x='PEI',y='Temperature')+ggtitle('PMI, ASSUMING PAI = 0')

Sys.time() - start_time