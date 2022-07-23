#Session 5B:
rm(list=ls())

#Parameter choices:
n=100000
b0=4
b1=6
sig=7


# data generation;
x=seq(20,30,length.out=n)
eps=rnorm(n,0,sig)

y=b0+b1*x+eps

# Analysis:
plot(x,y,col='red',cex=1.5)
b1_hat=sum((x-mean(x))*(y-mean(y)))/sum(x-mean(x)^2)
b0_hat=mean(y)-b1_hat*mean(x)
sig2_hat=sum((y-b0_hat-b1_hat*x)^2)/(n-2);sig2_hat

plot(x,y)
lines(x,b0_hat+b1_hat*x,col='blue',lwd=3)






## Data Generation

rm(list = ls())
## Parameter choices
n = 100
b0 = 0
b1 = 1
sig =2

#initiation of vectors
b1_hat=0
b0_hat=0
itn=1000

## data generation 
x = seq(-1,1, length.out = n)
for(j in 1:itn)
{
  eps = rnorm(n, 0 ,sig)
  y = b0 + b1 * x + eps
  b1_hat[j] = sum((x - mean(x)) * (y - mean(y)))/sum((x - mean(x))^2)
  b0_hat[j] = mean(y) - b1_hat[j] * mean(x)
  
  sig2_hat[j] = sum((y - b0_hat[j] - b1_hat[j] * x)^2)/(n -2)
}
plot(b1_hat)
mean(b1_hat)
var(b1_hat)
hist((b1_hat=mean(b1_hat)/sqrt(var(b1_hat))),probability=TRUE)
u=seq(-3,3,0.01)
lines(u,dnorm(u,0,1),col=2)

hist(sig2_hat*(n-2)/sig^2,probability = TRUE)
u=seq(60,160,1)
lines(u,dchisq(u,n-2),col=2,lwd=2)

plot(x,y)
lines(x,b0_hat+b1_hat*x,col=2)

##summery of m
