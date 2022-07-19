
a = 0
b = 1

f= function(x){
  3*x^2
}
curve(f(x),a,b, col = "red", lwd = 2)
integrate(f,lower = a, upper = b)
 ### that is PDF..........

g = function(x){
  2*abs(sin(4*x))
}
curve(g(x),a,b,col="red",lwd =2)
out= integrate(g,a,b)
out$value


new_g = function(x){
  g(x)/out$value
}


curve(new_g(x),col="blue", lwd = 2, lty = 2, add =TRUE)
legend("topleft",legend = c("g","new_g"),col= c("red","blue"),lty = c(1,2),
       lwd =c(2,2))
integrate(new_g, a, b)

#The letter d
# d stand for  density 
# uniform(a,b)
a = 0
b = 1
curve(dunif(x, min = a, max =b)
      -1,2,col = "red", lwd =2)

a = -1
b = 2
curve(dunif(x, min = a,max = b),col = "blue", lty = 2, lwd = 2, add = TRUE)

#exponential density function
curve(dexp(x, rate = 1), -1,4, col = "red", lwd =2, ylab = "f(x)",main="exponential(l)")

curve(dnorm(x, mean = 0, sd= 1),-4,4, col = "red", lwd=2)

abline(v = -1, col = "green", lwd=2, lty = 2)
abline(v = 1, col = "green", lwd=2, lty = 2)

out = integrate(dnorm, -1,1)
out$value
text(0,0.2, "0.68 ", cex =2, col = "magenta")

#binomial distributon


n = 6
p = 0.3
x = 0:n
p_x = choose(n, x)*p^x*(1-p)^(n-x)
print(p_x)
plot(x,p_x, pch = 19, cex =1.5)

plot(x,p_x,type = "h",col ="red" , lwd = 2)
points(x, p_x, cex = 1.5, col = "blue",pch =19)
title("Bin(n=6 , p=0.3)")

#disscussion with bell curve
mu = 0
sigma = 1
curve(dnorm(x, mean =  mu, sd= sigma), 
      col = "red", lwd = 2 , -4, 4)
n= 400
set.seed(12)
x= rnorm(n = n, mean = mu, sd = sigma)
x
hist(x, probability = TRUE,
     main = paste("n=",n))
curve(dnorm(x, mean = mu, sd = sigma), add =TRUE , col = "red" , lwd = 2)

par(mfrow =c(2,2))
n_vals = c(10,50,100,400)
for(n in n_vals){x= rnorm(n = n, mean = mu, sd = sigma)
x
hist(x, probability = TRUE,
     main = paste("n=",n))
curve(dnorm(x, mean = mu, sd = sigma), add =TRUE , col = "red" , lwd = 2)

  
}

#
p=0.3
n=200000
x=rbinom(n=n, size = 1, prob = p)
print(x)
prop = sum(x)/n
print(prop)


n_vals = 1:100
prop = numeric(length = length(n_vals))
for (n in n_vals){
  x=rbinom(n=n, size = 1, prob = p)
  prop[n] = sum(x)/n
  
}

par(mfrow= c(1,1))
print(prop)
plot(n_vals, prop, type = "l", col = "red")
abline(h = p , col = "blue" , lty = 2, lwd=3)

lines(n_vals, prop , col = "green")
abline(v=20,  col = "magenta",lwd = 3)



rep = 100
n_vals =1:100
prop = matrix(data = NA , nrow = rep, ncol = length(n_vals))


for (i in 1:rep) {
  for(n in n_vals){
    x= rbinom(n = n, size = 1, prob = p)
    prop[i,n] =sum(x)/n
  }
  
}
par(mfrow = c(2,3))
hist(prop[,3], probability = TRUE, xlim=c(0,1))
hist(prop[,6], probability = TRUE, xlim=c(0,1))
hist(prop[,9], probability = TRUE, xlim=c(0,1))
hist(prop[,12], probability = TRUE, xlim=c(0,1))
hist(prop[,15], probability = TRUE, xlim=c(0,1))


View(prop)
par(mfrow = c(2,3))
hist(prop[,1], probability = TRUE,
     main = "n=1", xlab = c(0,1))

hist(prop[,2], probability = TRUE,
     main = "n=2", xlab = c(0,1))

hist(prop[,3], probability = TRUE,
     main = "n=3", xlab = c(0,1))

hist(prop[,4], probability = TRUE,
     main = "n=4", xlab = c(0,1))
