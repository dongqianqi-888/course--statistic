#call the file
##read.csv()
##read.table()
##read from excel
library(readxl)
excel_sheets('class.xlsx')
data<-read_excel(file.choose(),sheet='order')
data
##read data from clipboard
myclip<-read.table(file='clipboard',header = F)
myclip

#save the file
text<-'i wanna a holiday with my friends'
write.table(text,file='text.txt',col.names = F,row.names = F)
##save in excel
library(writexl)
write_xlsx(myclip,path='rclass.xlsx')
##save in clipboard
write.table(myclip,file = 'clipboard',col.names = F,row.names = F)

#describing numerical data
##central tendency
data("trees")
rowMeans(trees)
colMeans(trees)
median(trees$Girth)
x<-c(1,2,2,2,3,4,5,6)
temp<-table(x)
names(temp)[temp==max(temp)]
library(DescTools)
Mode(x)
##quantile
quantile(trees$Girth,0.33)
summary(trees$Girth)
##variation
range(trees$Girth)
max(trees$Girth)-min(trees$Girth)
IQR(trees$Girth)
var(trees$Girth)
sd(trees$Girth)
CV<-sd(trees$Girth)/mean(trees$Girth)*100
##shape
library(e1071)
skewness(trees$Girth)
kurtosis(trees$Girth)


###exerciese
data<-read.table(file='clipboard',header=T)
head(data)
data$avg<-(data$PA+data$AB+data$H)/3
library(dplyr)
df<-data%>%select(Year,Tms,N.Bat,BatAge,avg)
head(df)
write.csv(df,file='BATTING.csv',row.names = F)

#function
##head()
##tail()
mtcars[which(mtcars$cyl==6),]
subset(mtcars,subset = hp<100,select = c(mpg,disp))
sort(mtcars$mpg,decreasing=T)
mtcars[order(mtcars$mpg,-mtcars$cyl),]
##merge(df1,df2,by='var')
num<-factor(x,labels = c('a','b','c','d','e','f'))
mons <- c("March","April","January","November","January","September",
          "October","September", "November","August","January",
          "November", "November", "February","May","August","July",
          "December","August","August","September","November",
          "February","April")
table(mons)
mons <- factor(mons,levels=c("January","February","March",
                             "April","May","June","July", "August","September", "October", 
                             "November", "December"), ordered=TRUE)
table(mons)
names(table(mons))
mf<-cut(women$weight,breaks = 3,labels = c('thin','medium','fat'))
table(mf)
mf<-cut(women$weight,breaks = c(100,120,140,160,180,200))
table(mf)
new<-interaction(CO2$Plant,CO2$Type,drop=T,sep=';')
table(new)
data(rock)
lapply(rock,FUN=mean)
sapply(rock,FUN=mean)
apply(trees,MARGIN = 2,mean)
apply(trees,MARGIN = 1,median)
do.call(cbind,lapply(rock,mean))
y<-c(12,34,12,567,54,2345,54,NA,3,45)
is.na(y)
sum(y,na.rm = T)
y<-na.omit(y)
m<-c('a','c','b')
paste(m,'?')
paste(m,1:3)
paste0(m,'!')
paste(m,1:3,sep = ',')
paste(m,1:3,collapse = ',')
library(dplyr)
trees%>%as_tibble()%>%select(Girth)
trees%>%as_tibble()%>%arrange(Girth)
mtcars%>%as_tibble()%>%filter(cyl==6)
mtcars%>%as_tibble()%>%mutate(cyln=cyl+1)
mtcars%>%as_tibble()%>%rename(new=cyl)
mtcars%>%as_tibble()%>%pull(cyl)

##exercise
factor<-factor(mtcars$cyl)
print(factor)
prop<-prop.table(table(mtcars$cyl))
print(prop)
mean(mtcars$mpg)
median(mtcars$mpg)
quantile(mtcars$mpg,c(0.25,0.75))
IQR(mtcars$mpg)
sapply(mtcars[1:7], IQR)         
mo<-cut(mtcars$mpg,breaks=4,labels=c('low','medium','high','very high'))
do<-cut(mtcars$disp,breaks=4,labels=c('low','medium','high','very high'))
interaction(mo,do,drop = T)%>%table()
table(mo,do)

data <- data.frame(
  Farm = c("MO", "MO", "MO", "MO", "LN", "SE", "QM"),
  Month = c(11, 7, 7, "NA", 9, 9, 11),
  Year = c(0, 0, 1, "NA", 3, 3, 2),
  Sex = c(1, 2, 2, 2, 1, 2, 2),
  LengthClass = c(1, 1, 1, 1, 1, 1, 1),
  LengthCT = c(75, 85, 91.6, 95, "NA", 105.5, 106),
  Ecervi = c(0, 0, 0, "NA", 0, 0, 0),
  Tb = c(0, 0, 1, "NA", 0, 0, 0)
)
str(data)
mean(as.numeric(data$LengthCT),na.rm = T)
count(data[data$Sex==1,])
count(data[data$Tb==1,])
data%>%as_tibble() %>%mutate(sqrt_length=sqrt(as.numeric(data$LengthCT)))
mean(abs(as.numeric(data$LengthCT)-mean(as.numeric(data$LengthCT),na.rm = T)),na.rm = T)

     