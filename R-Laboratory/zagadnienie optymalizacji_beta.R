#Ilorazy ró¿nicowe dla n wêz³ów

x<-c(1940, 1950, 1960, 1970, 1980, 2000, 2010)
f<-c(139, 150.7, 179.3, 203.3, 226.5, 249.6, 252.4)
N<-data.frame(x,f)
for(i in 1:(length(x)-1))
{
  for(j in 1:((length(x)-1)-i+1))
  {
    a<-i+j
    b<-i+2
    N[a,b]<-(N[a,b-1]-N[a-1,b-1])/(N[a,1]-N[a-i,1])
  }
  
}
N

#Wielomian Newtona dla n wêz³ów

L<-function(z)
{
  W=N[1,2]
  for(i in 1:(length(x)-1))
      {
        X=1
        for(j in 1:i)
        {
          X=X*(z-N[j,1])
        }
    W=W+N[1+i,2+i]*X
      }
  return(W)
}

#Stopieñ wielomianu
#Liczba wêz³ów minus jeden

length(x)-1

#Wspó³czynniki wielomianu



library(ggplot2)
library(ggthemes)
ggplot(data.frame(x=c(0:100),y=c(0:100)),aes(x,y))+stat_function(fun=L,colour='#FF33FF')+scale_y_continuous(limits=c(0,350))+scale_x_continuous(limits=c(1930,2019))+theme_minimal()+ggtitle('Wielomian interpolacyjny dla 5 wêz³ów')

View(N)

#Rysowanie tabel
#library(formattable)
#formattable(Wyniki)
#formattable(N)

