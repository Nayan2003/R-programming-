curve(exp(x) , -2,2, col = "red", lwd = 2)
curve(1+x, col = "blue", lwd = 2, lty = 2,
      add = TRUE)
curve(1+x+x^2/ factorial(2), col= "magenta", 
      lwd = 2, lty =2, add = TRUE)
curve(1+x+x^2/factorial(2) + x^3/ factorial(3),
      add= TRUE, col = "green",lty =2,
      lwd = 2)
legend("topleft", legend =c("n=1","n =2","n=3"),
       col = c("blue","magenta","green"),
       lwd = c(2,2,2), lty = c(2,2,2), bty = "n")
points(0,0,pch = 19, col = "red", cex = 1.5)


g =function(x){
  exp(x)
}

mu = 1
par(mfrow=c(2,2))
sigma = 1
n=10
approx_mu =g(mu)

approx_var =g(mu)^2 * sigma^2 / n



x= rnorm(n = n, mean =mu, sd = sigma)
x_bar = mean(x)
g(x_bar)


sim= 1000
g_vals = numeric(length = sim)
for (i in 1:sim) {
  x= rnorm(n = n, mean = mu, sd = sigma)
  x_bar = mean(x)
  g_vals[i] = g(x_bar)
}
print(g_vals)
hist(g_vals, probability = TRUE, main = paste("n=", n) )
curve(dnorm(x, mean = approx_mu,
            sd= sqrt(approx_var)),
      add = TRUE, col ="red", lwd=2)