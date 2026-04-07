1+2*(3+4)
sqrt((4+3)*(2+1))
cos(4*pi)
log(0)
factorial(6)
choose(52,5)
(1+2i)*(1-2i)
sqrt(-1)

x<-c(2,5,4,10,8)
sqrt(x)
x-6
(x/10)**2

sale<-c(1,1,3,4,7,11)
length(sale)
sum(sale)
cumsum(sale)
sum(sale)*3

c<-c(3,6,14,90,54,2,8,65,28,45,7)
c>30
c[c<10]
length(c[c>=10])
c[1:5]<-c[1:5]+5
c

x<-matrix(c(4,6,8,2,3,1,9,3,5,7,6,4,2,7,1),ncol=3)
x
dim(x)
x+3
y<-t(x)
x%*%y

a<-matrix(c(1,2,4,2,1,7,3,6,2,2,4,5),nrow = 3)
b<-matrix(c(1,0,2,1,3,1,4,5,5,3,7,1,2,4,3,2),nrow = 4)
a%*%solve(b)
b%*%t(a)

r<-matrix(c(56,27,43,120,68,34,47,32,110),nrow = 3)
r<50
