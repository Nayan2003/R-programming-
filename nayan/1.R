n =100
x= seq(-1,1, length.out=n)
x1=matrix(c(rep(1,n),x), nrow = n, ncol =2)
view(x1)
t(x1)%*%x1
solve(t(x1)%*%x1)*det(t(x1)%*%x1)
