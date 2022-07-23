f=function(x){sin(x)+sin(2*x)+cos(3*x)}
df = function(x){cos(x)+2*cos(2*x)-3*sin(3*x)}
curve(df, from = 0, to =4 * pi, col = "red")
curve(f,from = 0, to =4 * pi, col = "blue", add = TRUE)
abline(h =0, v= 0, col ="gray60")


curve(f, from = 0, to =4 , col = "red")
optimize(f,c(0,4), maximum = TRUE)


curve(f, from = 2, to =3 , col = "red")
optimize(f,c(2,3), maximum = TRUE)

curve(f, from = 0, to =5 , col = "red")
optimize(f,c(0,5), maximum = FALSE)


curve(f, from = 3, to =4.5 , col = "red")
optimize(f,c(3,4), maximum = TRUE)




curve(f, from = 2, to =3.5 , col = "blue", add = TRUE)
abline(h=0,v=0, col= "gray60")
uniroot(f,c(2,3.5))

optim(5, f, method = "SANN")

optim(5, f, method = "ga")


######## linear programming problem
library(lpSolve)

f.obj <- c(200, 6000, 3000, 200)
fat <- c(800, 6000, 1000, 400)
vitx<- c(50, 3, 150, 100)
vity <- c(10, 10, 75, 100)
vitz <- c(150,35,75,5)
f.cons=rbind(fat, vitx, vity, vitz)
f.dir =c("<=","<=",">=",">=")
f.rhs <- c(13800,600,300,550)
lp("max", f.obj, f.cons, f.dir, f.rhs)
lp("max", f.obj, f.cons, f.dir, f.rhs)$solution

###All integer problem
lp("max", f.obj, f.cons, f.dir, f.rhs, all.int = TRUE)$solution

#1st and 3rd integer and 2nd and 4th continuos vars
lp("max", f.obj, f.cons, f.dir, f.rhs,int.vec = c(1,3))$solution



f.obj <- c(3, 1, 2,1)
a <- c(-1, 3, 1, -2)
b<- c(7, 3, 1,0)
c <- c(1, 2,0,0)
d <- c(1,3,0,0)
f.dir =c(">=",">=",">=",">=")
f.rhs <- c(17,23,11,11)
f.cons=rbind(a,b,c,d)

lp("max", f.obj, f.cons, f.dir, f.rhs)
lp("max", f.obj, f.cons, f.dir, f.rhs)$solution
lp("max", f.obj, f.cons, f.dir, f.rhs, all.int = TRUE)$solution

##knapsack problem

library(adagio)
p<-c(15,100,90,60,40,15,10,1)
w<-c(2,20,20,10,40,30,60,10)
cap<-80
(is<-knapsack(w,p,cap))





############
library(TSP)
etsp<-ETSP(data.frame(x = runif(100),
                      y = runif(100)))
plot(etsp)

tour <- solve_TSP(etsp)
tour
plot(etsp, tour)

######least squre problem
data()
View(trees)
x1<- trees$Girth
x2<- trees$Height
y = trees$Volume

plot(x1,y)
help(lm)
lm(y~x1)
###using normal equation
n = length(x1)
ones = rep(1,n)
A = cbind(x1,ones)
Y= as.matrix(y)
solve(t(A)%*%A)%*%t(A)%*%Y

##using generalizatiiiiioon inverse
library(MASS)
ginv(A)%*%Y


###
lm(y~x1+x2)

A = cbind(x1,x2,ones)
solve(t(A)%*%A)%*%t(A)%*%Y
ginv(A)%*%Y

