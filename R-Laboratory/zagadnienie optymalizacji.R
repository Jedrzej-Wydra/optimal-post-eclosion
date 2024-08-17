start_time <- Sys.time()

#library(ggplot2)
#library(ggthemes)

x<-c(0:12)+0.5
f<-c(-2.5,	-1.1,	3.2,	8.2,	13.4,	16.7,	18.2,	17.8,	14,	9.2,	3.8,	-0.3, -2.5)
fx <- data.frame(x,f)

#Wykres rozrzutu

ggplot(fx, aes(x,f))+geom_point()+theme_minimal()+scale_x_continuous(breaks=x)+scale_y_continuous(breaks=seq(from=-3,to=19,by=1))+geom_hline(yintercept=0)+ggtitle('Œrednie temperatury miesiêczne')

#R jest liczb¹ sklejanych funkcji

R <- 6
T <- length(x)
WielomianyNewtona<-list()
WielomianyCoef<-list()
Coefs<-list()
Granice<-list()
N<-list()
A<-list()
B<-list()
Y<-list()
SF<-ggplot(fx, aes(x,f))+geom_point()+theme_minimal()+ggtitle('Wielomian Newtona temperatur')
for(O_O in 1:R)
{
  #Dane
  x1<-x[(1+floor((T*(O_O-1)/R))):ceiling((T*(O_O)/R))]
  f1<-f[(1+floor((T*(O_O-1)/R))):ceiling((T*(O_O)/R))]
  
  fx1 <- data.frame(x1,f1)
  
  #Ilorazy ró¿nicowe dla n wêz³ów
  
  N[[O_O]]<-data.frame(x1,f1)
  for(i in 1:(length(x1)-1))
  {
    for(j in 1:((length(x1)-1)-i+1))
    {
      a<-i+j
      b<-i+2
      N[[O_O]][a,b]<-(N[[O_O]][a,b-1]-N[[O_O]][a-1,b-1])/(N[[O_O]][a,1]-N[[O_O]][a-i,1])
    }
    
  }
  
  #Wielomian Newtona dla n wêz³ów
  
  L1<-function(z,H=O_O)
  {
    #Wielomian Newtona
    W=N[[H]][1,2]
    for(i in 1:(length(x1)-1))
    {
      X=1
      for(j in 1:i)
      {
        X=X*(z-N[[H]][j,1])
      }
      W=W+N[[H]][1+i,2+i]*X
    }
    return(W)
  }
  
  #Wspó³czynniki wielomianu
  
  A[[O_O]] <- rep(1,length(x1))
  for(i in 2:length(x1))
  {
    A[[O_O]]=rbind(A[[O_O]],rep(i,length(x1)))
  }
  
  for(i in 1:dim(A[[O_O]])[2])
  {
    A[[O_O]][,i]<-(A[[O_O]][,i])^(i-1)
  }
  B[[O_O]] <- L1(A[[O_O]][,2])
  
  Y[[O_O]]=solve(A[[O_O]],B[[O_O]],tol=1e-40)
  
  L21 <- function(z,H=O_O)
  {
    #Wielomian po wspó³czynnikach
    D <- Y[[H]][1]
    for(i in 2:length(x1))
    {
      D <- D + Y[[H]][i]*(z^(i-1))
    }
    return(D)
  }
  WielomianyNewtona[[O_O]] <- L1
  WielomianyCoef[[O_O]] <- L21
  Coefs[[O_O]] <- Y[[O_O]]
  Granice[[O_O]] <- x1
  SF <- SF+stat_function(fun=WielomianyCoef[[O_O]],args=list(H=O_O),colour='#CC3399',xlim=c(min(x1),max(x1)))
}

#Wielomian Newtona (wspó³czynniki)

SF+scale_x_continuous(breaks=x)+scale_y_continuous(breaks=seq(from=-3,to=19,by=1))+geom_hline(yintercept=0)

#DD=(T-t0)d
#t0=8.9*C
Granice[[1]] <- c(0,Granice[[1]])
t0 <- 8.9
t <- seq(from=0,to=12,by=0.001)
tx <- list()
for(i in 1:length(Granice))
{
  tx[[i]] <- WielomianyCoef[[i]](subset(t,t>=min(Granice[[i]])&t<max(Granice[[i]])),H=i)-t0
}

data <- data.frame(x=t,f=unlist(tx))

data$f <- ifelse(data$f<0,0,data$f)

ggplot(data,aes(x,f))+geom_point(size=0.2,colour='#3333FF')+theme_minimal()+scale_x_continuous(breaks=x)+scale_y_continuous(breaks=seq(from=0,to=19,by=1))+ggtitle("Wykres temperatur powy¿ej zera fizjologicznego")

###################################################################
#Dalszy kod nie jest ogólny

#Dane
#x_2 <- c(3.55,5.05,6.55,8.05,9.55)
#f_2 <- c(0,6.5,9.55,6.5,0)

#Z mniejsz¹ liczb¹ wêz³ów chyba jest lepiej. Obliczenia bêd¹ zdecydowanie prostrze.
x_2 <- c(3.55,6.55,9.55)
f_2 <- c(0,9.55,0)


fx_2 <- data.frame(x_2,f_2)

#Ilorazy ró¿nicowe dla n wêz³ów

N_2<-data.frame(x_2,f_2)
for(i in 1:(length(x_2)-1))
{
  for(j in 1:((length(x_2)-1)-i+1))
  {
    a<-i+j
    b<-i+2
    N_2[a,b]<-(N_2[a,b-1]-N_2[a-1,b-1])/(N_2[a,1]-N_2[a-i,1])
  }
  
}

#Wielomian Newtona dla n wêz³ów

L_2<-function(z)
{
  #Wielomian Newtona
  W=N_2[1,2]
  for(i in 1:(length(x_2)-1))
  {
    X=1
    for(j in 1:i)
    {
      X=X*(z-N_2[j,1])
    }
    W=W+N_2[1+i,2+i]*X
  }
  return(W)
}

#Wspó³czynniki wielomianu

A_2 <- rep(1,length(x_2))
for(i in 2:length(x_2))
{
  A_2=rbind(A_2,rep(i,length(x_2)))
}

for(i in 1:dim(A_2)[2])
{
  A_2[,i]<-(A_2[,i])^(i-1)
}
B_2 <- L_2(A_2[,2])

Y_2 <- solve(A_2,B_2,tol=1e-40)

L2_2 <- function(z)
{
  #Wielomian po wspó³czynnikach
  D <- Y_2[1]
  for(i in 2:length(x_2))
  {
    D <- D + Y_2[i]*(z^(i-1))
  }
  return(D)
}
Zero <- function(z)
{
  return(0)
}
Y_2

ggplot(data,aes(x,f))+geom_point(size=0.2,colour='#3333FF')+theme_minimal()+scale_x_continuous(breaks=x)+scale_y_continuous(breaks=seq(from=0,to=19,by=1))+stat_function(fun=L2_2,colour='#CC3399',xlim=c(min(x_2),max(x_2)))+ggtitle('Na³o¿ony model funkcji temperatury powy¿ej zera fizjologicznego')

ggplot(data,aes(x,f))+theme_minimal()+scale_x_continuous(breaks=x)+scale_y_continuous(breaks=seq(from=0,to=19,by=1))+stat_function(fun=L2_2,colour='#CC3399',xlim=c(min(x_2),max(x_2)))+stat_function(fun=Zero,colour='#CC3399',xlim=c(0,min(x_2)))+stat_function(fun=Zero,colour='#CC3399',xlim=c(max(x_2),12))+ggtitle('Model funkcji temperatury powy¿ej zera fizjologicznego')

#L_2 jest moj¹ funkcj¹ R(s) (patrz: notatki)

#r'(x)=R(x)

Y_3 <- c(Y_2[1],0.5*Y_2[2],(1/3)*Y_2[3])

rr<-function(z)
{
  D <- Y_3[1]*z
  for(i in 2:length(Y_3))
  {
    D <- D + Y_3[i]*(z^(i))
  }
  return(D)
}
r<-function(z)
{
  return(rr(z)-rr(3.55))
}

ggplot(data,aes(x,f))+stat_function(fun=r,colour='#CC3399',xlim=c(min(x_2),max(x_2)))+theme_minimal()+scale_x_continuous(breaks=seq(from=-100,to=100,by=1))+scale_y_continuous(breaks=seq(from=-100,to=100,by=1))+ggtitle('Wykres ca³ki z f(x)')

ggplot(data,aes(x,f))+stat_function(fun=r,colour='#CC3399',xlim=c(min(x_2),max(x_2)))+theme_minimal()+scale_x_continuous(breaks=seq(from=-100,to=100,by=1))+scale_y_continuous(breaks=seq(from=-100,to=100,by=1))+coord_flip()+ggtitle('Wykres funkcji odwrotnej do ca³ki z f(x)')

Sys.time()-start_time