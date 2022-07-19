u<-c(1,-1,2) #u<- assignening a values
v<-c(3,-1,1)
2*u-3*v
u*v #element wise multiplication
sum(u*v) #dot product
sqrt(sum(u*u)) #length of the vector u
vec_length<-function(a){
  return(sqrt(sum(a*a)))
}
vec_length(u)

p<-sum(u*v)/vec_length(u)^2*u ##Orthogonal projection
p
sum((v-u)*u)
##user define function
orth_proj <- function(a,b){
  p<-sum(a*b)/vec_length(a)^2*a
  return(p)
}

orth_proj(u,v)

library(matlib)
help(matlib)

Proj(v,u)

##################### Working with Matrices#####################
A<-c(3,2,5)
B<-c(8,9,5)
C<-c(5,8,2)

A= matrix(c(2,9,5,-3,2,10,29,9,4), nrow=3, byrow = TRUE)
B=matrix(c(6,5,2,8,541,6,2,4,8), nrow=3, byrow = TRUE)
A
B
t(A) #returns transpose of A
2*A+3*B #linear combination
A*B #element wise multiplication
A%*%B #Matrix multiplication
det(A) #diterminant of A
det(B)
A1=solve(A) #invers of A
A%*%A1
round(A%*%A1)
mpower(A,2)
help(mpower)




###########matrix power function
matrix_power<-function(A,p){
  R =A
  for(i in 2:p){
    R = R%*%A
  }
  return(R)
}

P = matrix(c(0.2,0.3,0.4,0.1,
             0.3,0.1,0.5,0.1,
             0.25,0.25,0.25,0.25,
             0.25,0.25,0.25,0.15), nrow=4)
p
x0 = matrix(c(2500,2500,2500,2500),nrow = 4)
x0
matrix_power(P,5000)%*%x0


################################ Reduce row echlon form RRE
library(pracma)
rref(A)

A =matrix(c(2,5,8,6,
            5,6,6,2,
            3,5,6,3,
            8,8,2,6),nrow=4)
A
B=matrix(c(10,-2,8,4))
Aug = cbind(A,B)
Aug
rref(Aug)
solve(A,B)
Aug1= rref(Aug)
s=Aug[,5]
s
A%*%s





I4=diag(4) #4 x 4 identity matrix
cbind(A,I4)
rref(cbind(A,I4))
Ainv = rref(cbind(A,I4))[,5:8]
Ainv




v1=c(1,2,-3)
v2=c(3,-2,1)
v3=c(5,3,2)
A = cbind(v1,v2,v3);A
det(A)
A=matrix(c(v1,v2,v3),nrow=3,bycol)




v=c(1,1,1)
A1=cbind(A,v);A1
rref(A1)
alfa =rref(A1)[,4] ##Coordinates of v w.r.t. the basis {v1,v2,v3}
alfa




##Extending a linearly independent vectors to a basis 

v1= c(1,2,-3,4,2)
v2= c(2,2,-3,4,2)
v3= c(2,2,-2,4,2)
rref(rbind(v1,v2,v3))
M = cbind(v1,v2,v3,diag(5))
rref(M)
T = function(x){c(2*x[1]-3*x[2]+x[3],x[1]+2*x[2]-3*x[3],x[1]+x[2]+x[3])}
##########basis of B={u1,u2,u3} of the domain
u1=c(1,-1,2)
u2=c(3,1,-5)
u3=c(-2,4,7)

#########bais of C={v1,v2,v3} of the co domain
v1=c(2,0,1)
v2=c(3,1,-5)
v3=c(1,0,3)

#### M colomn matrix whose column are v1,v2,v3 and T(u1),T(u2),T(u3)
M<- cbind(v1,v2,v3,T(u1),T(u2),T(u3))
####Apply rref to M
rref(M)
## the last 3 columns of rref(M) is the requied matrix
MA =rref(M)[,4:6]
MA  


##########
A = matrix(c(4,-1,2,3,
             -1,7,2,-1,
             2,2,-1,0,
             3,-1,0,7),nrow=4, byrow=TRUE)
A  
eig(A)  
eigen(A)$values
eigen(A)$vector
P = eigen(A)$vectors
D=solve(P)%*%A%*%P
print(D)
