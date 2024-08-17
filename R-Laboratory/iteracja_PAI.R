test <- PAI(M_T_14(1))
for(i in 2:365)
{
  test <- c(test,PAI(M_T_14(i)))
}
test <- data.frame(x=1:365,y=test)
ggplot(test,aes(x,y))+geom_line()

PAI2 <- function(z)
{
  ifelse(z>=9.9,return(round(c+exp(b0+b1*z),digits = 0)),113)
}

PAI_it <- function(d)
{
i=30
co=i
cola <- 1
while(co!=0&cola<1500)
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
ifelse(co2>=1500,return(NA),return(i))
}

test2 <- PAI_it(1)
for(i in 2:365)
{
  test2 <- c(test2,PAI_it(i))
}
test2 <- data.frame(x=1:365,y=test2)
ggplot(test2,aes(x,y))+geom_line()+geom_point()
