n = 100
b0 = 3
b1 = 5
sig = 8


b1_hat = 0
b0_hat = 0
sig2_hat = 0
itn =1000
#data
x = seq(-1,1, length.out = n)
for (j in 1:itn) {
  eps = rnorm(n, 0 , sig)
  y = b0+b1*x+eps
  b1_hat[j] = sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) 
  b0_hat[j] = mean(y) - b1_hat[j]*mean(x)
  sig2_hat[j] = sum((y - b0_hat[j]-b1_hat[j]*x)^2)/(n-2)
}
hist(sig2_hat*(n-2)/sig^2, probability = TRUE)
u = seq(60,160,1)
lines(u, dchisq(u, n-2), col = 2, lwd =2)
m = lm(y~x)
summary(m)


## manual analysis
b1_hat = sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) 
b0_hat = mean(y) - b1_hat*mean(x)
sig2_hat = sum((y - b0_hat-b1_hat*x)^2)/(n-2)
rss= sum((y-b0_hat-b1_hat*x)^2)

#one line analysis
x1=x
x2=x^2
m=lm(y~x1+x2)
lines(x, m$fitted.values,col=2)
summary(m)
plot(m$residuals)
#for gaussianity
hist(m$residuals)
summary(m)
