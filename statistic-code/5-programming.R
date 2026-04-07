# decision
x<-1
if (x>0){
  print('x is positive')
}else if(x==0){
  print('x equals to 0')
}else{
  print('x is negative')}

ifelse(x>0,'x is positive','x is negative')

a<-c(1,4,2)
sapply(a,function(x) switch(x,'red','blue','yellow','green'))

# loop
for (i in 1:10){
  a=i^2
  print(a)
}

while (x<20){
  x<-x+2
  print(x)
}

for (i in 1:8){
  if (i==7){
    break
  }else if (i==3){
    next
  }
  print(i^3)
}

joker<-function(x){
  print('error!.............')
}
joker(2)

fun<-function(x,y,s){
  multi<-x%*%y
  final<-multi^s
  return (final)
}
fun(cbind(c(1,2,4),c(3,2,5)),c(4,2),4)

# exercise
## 1
cv<-function(x){
  cv<-(sd(x)/mean(x))*100
  cat(cv,'%')
  hist(x)
}
cv(c(5,3,2,4,1,4,1,7,3,5))

## 2
dis<-function(x,y,x1,y1){
  d<-sqrt((x-x1)^2+(y-y1)^2)
  return(d)
}
md<-function(df){
  j=0.00001
  for (i in length(df)){
    x<-df[i,'x']
    y<-df[i,'y']
    x1<-df[i,'x1']
    y1<-df[i,'y1']
    dis<-dis(x,y,x1,y1)
    if(dis>=j){
      j<-dis
    }else{
      j<-j
    }
    return(j)
  }
}
df<-data.frame(
  x=c(3,1,4,6,1),
  y=c(2,1,4,2,1),
  x1=c(4,1,3,2,4),
  y1=c(1,3,5,1,2)
)
md(df)
plot(df$x,df$y,col='red')
points(df$x1,df$y1,col='blue')

### the distance between no.i and others
m<-function(x,y,i){
  d<-sqrt((x[-i]-x[i])^2+(y[-i]-y[i])^2)
  D<-max(d)
  return(D)
}
x<-c(3,3,8,6,6)
y<-c(2,1,4,2,1)
m(df$x,df$y,5)

## 3
sign<-function(j,i){
  if(j>i){
    return(1)
  }else if(i==j){
    return(0)
  }else{
    return(-1)
  }
}
MK<-function(j,i){
  n=length(i)
  for (i in 1:n){
    for (j in 1:n){
      return(sum(sign(j,i)))
    }
  }
}
MK(x,y)

df<-function(i,x){
  n<-length(x)
  j<-i+1:n
  df<-x[j]-x[i]
  return(df)
}
smk<-function(x){
  n<-length(x)
  i<-1:(n-1)
  s<-do.call(c,lapply(i,function(i) df(i,x)))
  s[s>0]<-1
  s[s=0]<-0
  s[s<0]<--1
  s<-na.omit(s)
  smk<-sum(s)
  return(smk)
}
smk(y)
zmk<-function(x){
  n<-length(x)
  v<-n*(n-1)*(2*n+5)/18
  s<-smk(x)
  if(s>0){
    z<-(s-1)/(sqrt(v))
  }else if(s==0){
    z<-0
  }else{
    z<-(s+1)/(sqrt(v))
  }
  return(z)
}
zmk(y)
