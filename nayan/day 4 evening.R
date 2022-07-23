n =5
x = rnorm(n = n , mean = 0, sd =1)
hist(x, probability = TRUE, col= "gray60")
curve(dnorm(x), add = TRUE, col = "red", lwd =2)
x_bar = mean(x)
print(x_bar)

n=5
rep=100
x_bar = numeric(length = rep)
print(x_bar)
for (i in 1:rep) {
  x = rnorm(n=n , mean=0, sd= 1)
  x_bar[i] = mean(x)
}
print(x_bar)

hist(x_bar, probability = TRUE,
     main = paste("n=",n),
     xlab = expression(bar(X[n])),
     xlim = c(-3,3), breaks = 10)
curve(dnorm(x), add=TRUE, col ="red",
      lwd =2)

curve(dnorm(x,mean = 0,sd= 1/sqrt(n)),
      add = TRUE, col = "blue",lwd =3, lty =2)

###uniform
par(mfrow=c(1,1))
n=30
rep=100
x_bar = numeric(length = rep)
print(x_bar)
for (i in 1:rep) {
  x = runif(n=n)
  x_bar[i] = mean(x)
}
print(x_bar)

hist(x_bar, probability = TRUE,
     main = paste("n=",n),
     xlab = expression(bar(X[n])), breaks = 10)
curve(dunif(x), add=TRUE, col ="red",
      lwd =2)

curve (dnorm(x,mean = 0.5, sd = 1/sqrt(12*n)),
       add= TRUE, col ="blue",
       lwd =2, lty=2 )



    ###do the above the coin tossing experiment
curve(dnorm(x), col ="red", lwd = 2,
      -3,3)
abline(v= 1, col = "blue", lwd=3, lty =2)
pnorm(1)
x = rnorm(1)
points(x, 0 , pch = 19, cex =1.5)

n_vals =1:1000
prop = numeric(length = length(n_vals))
for(n in n_vals){
  x= rnorm(n = n )
  prop[n]= sum(x<1)/n
} 
 plot(n_vals, prop, type = "l",col ="red", lwd = 2)
abline(h= pnorm(1), lwd =3, col ="blue", lty = 2)


f = function(x){
  (1/pi)*(1/(1+x^2))
}
curve(f(x), -5,5, col ="red", lwd=2)
integrate(f, -Inf, Inf)
curve(dnorm(x), add = TRUE, col ="blue",
      lwd =2)
n_vals = 1:10000
x_bar = numeric(length = length(n_vals))
for(n in n_vals){
  x = rcauchy(n = n)
  x_bar[n] = mean(x)
}
plot(n_vals, x_bar, type = "l",
     lwd =2, col = "red")


x = rnorm(n=100)
y= rnorm(n=100)
plot(x,y, pch =19, col ="red")
cor(x,y)            ##########coreation

x= rnorm(n =100)
y = 0.1 + 1*x +
                   bv ,
