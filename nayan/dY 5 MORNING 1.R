mu_0 = 0   # null hypothesis
sigma = 1  # population standard deviation
n = 25     # sample size
alpha = 0.05

qnorm(1-alpha/2)
x = rnorm(n = n, mean = mu_0,
          sd = sigma)
x_bar = mean(x)
T = (x_bar - mu_0)/(sigma/sqrt(n))
abs(T)>qnorm(1-alpha/2)

mu_1 = 1
n_vals = 1:10000
counter = numeric(length = length(n_vals))
for (i in 1:length(n_vals)) {
  x = rnorm(n=n, mean = mu_1,
            sd = sigma)
  x_bar = mean(x)
  T = (x_bar - mu_0)/(sigma/sqrt(n))
  counter[i] = abs(T)>qnorm(1-alpha/2)
}


mu_vals = seq(-1, 1, by = 0.01)
prob_reject = numeric(length = length(mu_vals))
n = 20
mu_0 = 0
sim = 1000
for (j in 1:length(mu_vals)) {
  mu = mu_vals[j]
  reject = rep(FALSE, sim)
  for (i in 1:sim) {
    x = rnorm(n = n, mean = mu, sd = sigma)
    x_bar = mean(x)
    T = (x_bar - mu_0)/(sigma/sqrt(n))
    reject[i] = abs(T)>qnorm(1-alpha/2)
  }
  prob_reject[j] = mean(reject)
}
prob_reject
plot(mu_vals, prob_reject,
     type = "l",col = "red",
     lwd = 2,
     xlab = expression(mu), 
     cex.lab = 1.5,
     ylab = expression(P(Reject~H[0])))
abline(h = alpha, col = "blue",
       lwd = 2, lty = 2)
text(0.8, 0.09, expression(alpha),cex = 1.5)
abline(v = 0.5, lwd = 2, lty = 2,
       col = "magenta")
points(0.5, 0.63, pch = 19, cex=2)
title("Power function")
