#setup1
#data genration 
rm(list =ls())
#parameter choices
n =100
b0 = 0
b1 = 1
sig =2
#initialition of vector
b1_hat =0
b0_hat =0
sig2_hat=0
itn=100
#data geneation
x=seq(-1,1, length.out =n)
for (j64 in vector) {
  
}
eps =rnorm(n,0,sig)
y= b0+b1*x+eps
#Analysis
plot(x,y)
b1_hat[j]=sum((x-mean(x))*(y-mean(x)))/sum((x-mean(x))^2)
b1_hat
b0_hat[j] = mean(y)-b1_hat*mean(x)
sig2_hat[j] = sum((y-b0_hat-b1_hat*x)^2)/(n-2)
b0_hat
sig2_hat
plot(x,y)
lines(x,b0_hat+b1_hat*x, col=5)
sig^2/sum((x-mean(x))^2)
