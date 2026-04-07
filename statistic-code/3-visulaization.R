#argument in plot
plot(trees$Girth,trees$Height,main = 'blace charry trees',
     sub='height of cheery trees',xlab = 'grith',ylab = 'height',
     cex=2,cex.main=3,cex.sub=0.7,cex.axis=1.2,cex.lab=1.7,  #front size
     col=1,col.main='2',col.sub=3,col.axis='cyan',col.lab='magenta',  #color
     ylim=c(60,90),xlim=c(5,25))

#class of plot
par(mfrow=c(3,2))
plot(trees$Height,type='p')#points
plot(trees$Height,type='l')#lines
plot(trees$Height,type='b')#both-not overlapping
plot(trees$Height,type='o')#both-overlapping
plot(trees$Height,type='h')#vertical lines
plot(trees$Height,type='s')#step plot

#pch
par(mfrow=c(1,1))
plot(trees$Height,pch=11)

#lty
plot(trees$Height,type='l',lty=2)

#text
tplot<-c(rep(c('a','b','c'),10),'end')
plot(trees$Height,type='s')
text(trees$Height,tplot,pos=2)#pos means position

#points and lines
car<-c(1,3,6,4,9)
truck<-c(2,5,4,5,12)
suv<-c(4,4,6,6,15)
plot(car,type='o',ylim=c(0,15),pch=1)
points(truck,pch=4,col='darkcyan')
lines(suv,lty=2,col='magenta',pch=2)

#legend
legend(2,15,col=c(1,2),pch=1:2,lty = 1:2,c('car','suv'),cex=0.8)

#curve
curve(x*log(x+1)+sin(x^3),from=0,to=5,
      col='hotpink',lty=2,type='o',pch=19,cex=0.7)

#layout
layout(matrix(c(1,1,2,3),nrow=2,byrow = F))

#barplot
barplot(car,main='cars',col=rainbow(5),
  names.arg=c('mon','tue','wed','thu','fri'),xlim=c(0,7),
  legend=c('mon','tue','wed','thu','fri'))
barplot(5,col='brown',add=T,space=6.3)

#hist
veh<-cbind(car,truck,suv)
barplot(veh,col = rainbow(5))
barplot(veh,col = rainbow(5),beside=T)
par(mfrow=c(2,1))
hist(mtcars$mpg,breaks = 10,prob=T)#density
hist(mtcars$mpg,breaks = 10,prob=F)#frequeancy

#boxplot
boxplot(mtcars$mpg)
boxplot(mtcars$mpg~mtcars$cyl,horizontal = T,col=rainbow(3))
boxplot(veh)
boxplot()

#pie
pie(car,labels = c('mon','tue','wed','thu','fri'))

#stripchart
stripchart(mtcars$mpg)
stripchart(mtcars$mpg~mtcars$cyl)
stripchart(data.frame(veh),vertical = T)

#exerciese
InsectSprays
boxplot(InsectSprays$count~InsectSprays$spray,col=rainbow(6),xlab = 'spray',ylab = 'count')
stripchart(InsectSprays$count~InsectSprays$spray,pch=19,col='slategray',add=T,vertical=T)

#big exercise
data<-data.frame(
  row.names=c('A','B','C','D','E','F','G','H','I','J'),
  'population under 65 years'=c(75,50,60,63,80,72,58,65,40,50),
  'poor under 65 years'=c(35,80,45,76,70,95,15,30,43,27),
  'population above 65 years'=c(25,50,40,37,20,28,42,35,60,50),
  'poor above 65 years'=c(65,20,55,24,30,5,85,70,57,34))

plot(1:nrow(data),data$population.under.65.years,pch=21,col=1,
     xlab='country',ylab='percentage')
points(1:nrow(data),data$poor.under.65.years,pch=22,col=2)
points(1:nrow(data),data$population.above.65.years,pch=23,col=2)
points(1:nrow(data),data$poor.above.65.years,pch=24,col=2)
lines(rep(1,4),data[1,],col=5)
lines(rep(2,4),data[2,],col=6)
lines(rep(3,4),data[3,],col=7)
lines(rep(4,4),data[4,],col=8)
lines(rep(5,4),data[5,],col=9)
lines(rep(6,4),data[6,],col=10)
lines(rep(7,4),data[7,],col=11)
lines(rep(8,4),data[8,],col=12)
lines(rep(9,4),data[9,],col=13)
lines(rep(10,4),data[10,],col=14)
legend("topright",
       legend = c("Pop. under 65", "Poor under 65", "Pop. above 65", "Poor above 65"),
       col = 1:4,
       pch = 21:24,
       cex = 0.7)
text(1:10,50,c('A','B','C','D','E','F','G','H','I','J'))

male<-data.frame(
  row.names=c('black','brown','red','blond'),
  'brown'=c(32,53,10,3),
  'blue'=c(11,50,10,30),
  'hazel'=c(10,25,7,5),
  'green'=c(3,15,7,8)
)
female<-data.frame(
  row.names=c('black','brown','red','blond'),
  'brown'=c(36,66,16,4),
  'blue'=c(9,34,7,64),
  'hazel'=c(5,29,7,5),
  'green'=c(2,14,7,8)
)
male_eye<-apply(male,MARGIN =2,FUN =sum )
female_eye<-apply(female,MARGIN =2,FUN =sum )
male_hair<-apply(male,MARGIN =1,FUN =sum )
female_hair<-apply(female,MARGIN =1,FUN =sum )
layout(matrix(c(1,1,2,3,4,5),nrow=2))
pie(c(sum(male),sum(female)),labels = c('male','female'),col=c('darkblue','hotpink'))
pie(male_hair,labels = c('black','brown','red','blond'),
    col=c('black','brown','red','yellow'),main='male_hair')
pie(female_hair,labels = c('black','brown','red','blond'),
    col=c('black','brown','red','yellow'),main='female_hair')
pie(male_eye,labels = c('brown','blue','hazel','green'),
    col=c('brown','blue','bisque','green'),main='male_hair')
pie(female_eye,labels = c('brown','blue','hazel','green'),
    col=c('brown','blue','bisque','green'),main='female_hair')

date<-c('1/10','2/10','3/10','4/10','5/10','6/10','7/10')
Ge<-c(300,700,600,400,400,800,400)
Cv<-c(50,100,75,50,100,35,50)
Se<-c(850,400,100,400,300,550,600)
O<-c(100,200,200,300,200,100,250)
data<-cbind(Ge,Cv,Se,O)
tmail<-apply(data,MARGIN = 1,FUN = sum)
names(tmail)=date
barplot(tmail,col='darkblue',space=c(3,rep(2,6)),xlim=c(2,30))
barplot(t(data),col=rainbow(4),xlab='date',ylab='email',space=rep(2,7),
        legend=c("Genuine","Virus","Spam","Others"),add=T)


a<-c(1,2,4,6,8,6,4,2,1)
b<-c(1,2,4,6,8,6,4,2,1)
barplot(a,space=rep(2,8),col='darkgreen',ylim=c(0,9),main = "Merry Christmas!")
barplot(b,space=c(3,rep(2,8)),col='darkgreen',add=T)
points(15,8,pch=17,col='yellow',cex=4)
points(15,8,pch=18,col='yellow',cex=5)
# Add some "ornaments" (randomly placed circles)
num_ornaments <- 30
ornament_x <- runif(num_ornaments, 0, 25)
ornament_y <- runif(num_ornaments, 0, 9)
points(ornament_x, ornament_y, pch = 16, col = sample(c("red", "yellow", "blue"), num_ornaments, replace = TRUE), cex = 0.8)

