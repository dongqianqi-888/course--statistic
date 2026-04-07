#hypothsis testing
##testing about mean
library(TeachingDemos)
###sd of sample is known and normal distribution
x<-rnorm(10,mean=2.1,sd=0.3)
z.test(x,mu=2,stdev = 0.3,alternative = 'two.side')

###sd of sample is unknown but normal distribution
x<-rnorm(20,mean=1,sd=0.5)
t.test(x,mu=2,alternative = 'less')

###sd of two samples is unknown
x<-c(1.3,1.5,1.2,1.7,1.3)
y<-c(1.6,1.7,1.8,1.6,1.5)
###two independent normal distribution
t.test(x,y,mu=0,alternative = 'less',var.equal = T)
###paried sample with difference normal distribution
t.test(x,y,mu=0,alternative = 'less',paired = T)

#--------------------------------------------------------------------------

##anova(testing about value in different group)
library(lattice)
barley
###does the yield is different in different variety? 
one.way<-aov(yield~variety,data=barley)
summary(one.way)
###does the yield is different in different variety and different site?
###the site and the variety is independece.
two.way <- aov(yield ~ variety+site, data = barley)
summary(two.way)
###the site and the variety is effective.
interaction <- aov(yield ~ variety*site, data = barley)
summary(interaction)

###exercise
SNP_type<-rep(c("AA","AG","GG"),c(3,3,3))
SNP_value<-c(82,83,97,83,78,68,38,59,55)
SNP<-data.frame(SNP_type,SNP_value)
SNP.aov<-aov(SNP_value~SNP_type,data=SNP)
summary(SNP.aov)

#------------------------------------------------------------------

##testing about propotion
###this propotion is equal 0.4 or not?
prop.test(52,100,p=0.4,alternative="greater")
###these propotions is equal or not?
prop.test(c(132,135),c(400,390),alternative="two.sided",correct = F)

#-------------------------------------------------------------------

##chi-square test(testing about variance)
###variance is equal 2.25 or not?
library(EnvStats)
data<-c(12.43, 11.71, 14.41, 11.05, 9.53, 11.66, 
        9.33,11.71,14.35,13.81)
varTest(data,alternative="greater",sigma.squared = 2.25)
###the ratio of varx and vary is equal default value or not?
var.test(x,y,alternative="two.sided")

#------------------------------------------------------------------

##exercise
x<-c(1.1,1.2,1.3,1.2,1.4)
t.test(x,mu=1.2,alternative = 'greater')
###accept the H0
prop.test(c(30,20),c(80,100),alternative="greater",correct = F)
###reject the H0
PlantGrowth
A<-aov(weight~group,data=PlantGrowth)
summary(A)
###vary is not very significant

#===================================================================

#linear regression
##simple lm
eruption.lm <- lm(eruptions~waiting, data=faithful)
summary(eruption.lm)
plot(eruptions~waiting, data=faithful,main= "Plot of Y vs X")
abline(eruption.lm, col="red")

H20<-c(0.99,1.02,1.15,0.95)
O2<-c(90.01,89.05,91.43,87.33)
ex<-data.frame(H20,O2)
names(ex)<-c("Hydrogen_Level","Oxygen_Purity")
ex.lm<-lm(Oxygen_Purity~Hydrogen_Level,data=ex)
summary(ex.lm)
plot(Oxygen_Purity~Hydrogen_Level,data=ex,pch=19)
abline(ex.lm)

##multiple lm
pairs(~stack.loss + Air.Flow + Water.Temp + Acid.Conc., 
      data=stackloss)
stackloss.lm <- lm(stack.loss~ Air.Flow + Water.Temp + 
                       Acid.Conc., data=stackloss)
summary(stackloss.lm)

##dummy-variable lm
class(mtcars$cyl)
mtcars$CYL<-as.factor(mtcars$cyl)
mt.lm<-lm(mpg~CYL,data=mtcars)
summary(mt.lm)

##residuals analysis
residuals(stackloss.lm)
fitted(stackloss.lm)

##detect non-linearity and non-constant variance
##curve pattern means non-linearily
##funnel shape means unequal variance
##random scatter around 0 is good
plot(fitted(stackloss.lm),residuals(stackloss.lm))
abline(h=0,col='red')
##check normality of error
hist(residuals(stackloss.lm))
qqnorm(residuals(stackloss.lm))
qqline(residuals(stackloss.lm))

###standardized and studentized residuals
###>2 means possible outlier; >3 means strong outlier
rstandard(stackloss.lm)
rstudent(stackloss.lm)

###detect high leverage point
###>2(p/n) means this point is high leverage point(p=number of beta)
hatvalues(stackloss.lm)

###detact influential elements
###>2sqrt(p/n) means.....
dffits(stackloss.lm)
###>1 means potentially influential; >4/n means worth investigating
cooks.distance(stackloss.lm)

##exercise
library(car)
Salaries
l1<-lm(Salaries$salary~Salaries$yrs.service)
l2<-lm(Salaries$salary~Salaries$yrs.service+Salaries$yrs.since.phd)
l3<-lm(Salaries$salary~Salaries$yrs.service+Salaries$rank)
