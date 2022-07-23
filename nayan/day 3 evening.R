# plot the graf of f(x) = x^3-3x+5
f= function(x){6*x^4+2*x^3-x^2-3*x+5}
  curve(f,-3,3)
abline(h = 0, v = 0, col ="gray60")


polyroot(c(5,-3,-1,2,6))

## task
f= function(x){2*x^3-x^2-3*x+5}
curve(f,-3,3)
abline(h = 0, v = 0, col ="gray60")


polyroot(c(5,-3,-1,2))

##roots of f(x)=1+(3+2i)x+(3-7i)x^2=0

f= function(x){x^3-3*x-1}
curve(f,1.85,1.95, col = "red")
abline(h = 0, v = 0, col ="gray60")

polyroot(c(1,(3-7i),(3+2i)))

##visection method to find a root

f= function(x){x^3-3*x-1}
a =1
b=3
f(a)*f(b) #since f(a)*f(b)<0, f has a real root between a and b
mid = (a+b)/2; mid
f(mid)
if(f(a)*f(mid)<0){
  b = mid
  
}else{
  a = mid
}
print(c(a,b))


######in  for loop
for(k in 1:10){
  mid = (a+b)/2; mid
  f(mid)
  if(f(a)*f(mid)<0){
    b = mid
    
  }else{
    a = mid
  }
  cat("iteration", k, "a=", a, "b=", b, "\n")
}


## user defined function for bisection method

bisect= function(f, a,b , maxit = 100){
  for(k in 1:maxit){
    mid = (a+b)/2
    if (f(a)*f(mid)<0){
      b=mid
      
    }else{
      a = mid
    }
  root = (a+b)/2
  return(root)
  }
}
f= function(x){x^3-3*x-1}
bisect(f,1,3,200)
polyroot(c(-1,-3,0,1))



##########roots using newton raphson method
f= function(x){x^3-5*x^2+2*x+6}
curve(f,-2,5,col ="red")
abline(h= 0 , v=0, col ="gray60")

df= function(x){3*x^2-10*x+2}


x0 = 0

###iteration 1
x1 = x0-f(x0)/df(x0)
print(x1)
####iteration 2
x2 = x1-f(x1)/df(x1)
print(x2)
###iteration 3
x3 = x2-f(x2)/df(x2)
print(x3)


###using loop run 10 times
x0 =-1
maxit = 10  # maximum number oif itteration
for(k in 1:maxit){
  x1 =x0-f(x0)/df(x0)
  cat("Iteration :",k,"xk",x1,"\n")
  x0 = x1
}
####user define function for newton - raphson
newton = function(f, df, x0, maxit=100){
  for(k in 1:maxit){
    x0=x0-f(x0)/df(x0)
  }
  return(x0)
}
f = function(x){x^3-5*x^2+2*x+6}
df = function(x){3*x^2-10*x+2}
newton(f, df,0.5,300)


## solving system of linear equation iteratively
A= matrix(c(4,-2,1,2,-7,3,1,5,-20), nrow = 3, byrow = TRUE)
A
b = matrix(c(7.5,8,10), nrow = 3)
b
D = diag(diag(A))
D
solve(A,b)
L<-U<-A
L[upper.tri(A, diag = TRUE)] <-0
U[lower.tri(A, diag = TRUE)] <-0
x0 = matrix(c(1,1,1),3);x0
D1= solve(D)  # to find invers
H = -D1%*%(L+U)
C= D1%*%b
x1= H%*%x0+C
print(x1)


## find X2 

x2 = H%*%x1+C
print(x2)

##find x3
x3 =H%*%x2+C
print(x3)

##find x3
x4 =H%*%x3+C
print(x4)

##find x4
x5 = H%*%x4+C
print(x5)


### to for loop use loop
x0 = matrix(c(0,0,0),3)
maxit = 10
for (k in 1:maxit){
  x0 = H%*%x0+C
  cat("Iteration :", k, "xk =",x0,"\n")
}



### Trapezoidal Rule

trap<-function(f, a, b,m = 100){
  x= seq(a,b, length.out = m +1)
  y= f(x)
  
  p.area =sum((y[2:(m+1)+y[1:m]]))
  p.area = p.area*abs(b-a)/(2*m)
  return(p.area)
}

phi<- function(x) exp(-x^2/2)/ sqrt(2*pi)
curve(phi,-3,3)
trap(phi,-3,3,100)