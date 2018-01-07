data(cars)
mydata<-cars
str(mydata)
speed<-mydata$speed
distance<-mydata$dist
mean(speed)
mean(distance)
plot(speed,distance)
getwd()
setwd('C:/Software/Data/Cummins-Madhavi/MDA Projects/MSPA/Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/RPractice/Rpractice_scripts')
ls()
rm(list=ls())
write.table(mydata, "mydata.csv")

###Working with vectors
seq(1,11,by=2)
seq(1,11,length=6)
rep(0,11)
rep(1:3,6)
rep(1:3,each = 6)
rep(1:3,rep(6,3))
rep(1:3, c(2, 6, 6))
x <- c(1, 2, 3)
y <- c(1.4, 1.7, 3.1)
z <- c(x, y)
x <- c(1, 2, 3)
y <- c(1.4, 1.7)
z <- 3.1
x <- append(x, y, after = 1)
x <- append(x, z, after = 5)
append(x, c("A", "B"), after = 3 )
z <- c(16, 4)
sqrt(z)


###excercise
x <- seq(1, 6)
y <- rep(1:3, 2)
x
y
z<-c(x,y)
length(z)
sum(z)
x+y
x*y
x-1
x**2

seq(2,9)
seq(2, 9, length = 8)
seq(9, 2, -1)
rep(c(1, 2), 4)
rep(c(1, 2), c(4, 4))
rep(1:4, rep(3, 4))


##(a) 6, 6, 6, 6, 6,6

rep(6,6)


#(b) 5, 8, 5, 8, 5, 8, 5, 8

rep(c(5,8),4)

#(c) 5, 5, 5, 5, 8, 8, 8, 8

rep(c(5,8),rep(4,2))

###Descriptive stats
x <- seq(11,1)
min(x)
max(x)
which.min(x)
which.max(x)
quantile(x, probs = c(0.25, 0.5, 0.75))
summary(x)
x <- c(8.75, 9.45, 4.35, 6.85, 9.45, 10.55, 7.75, 8.25, 10.55, 2.45, 15.75, 7.45 )
mean(x)
round(mean(x), digits = 3)
signif(mean(x), digits = 3)
ceiling(mean(x))
floor(mean(x))
x[1:4]
summary(x[1:4])
x[5:8]
summary(x[5:8])
x[9:12]
summary(x[9:12])


###Excercises
x <- c(8.75, 9.45, 4.35, 6.85, 9.45, 10.55, 7.75, 8.25, 10.55, 2.45, 15.75, 7.45 )
x[6] + x[7]

x[c(5, 6, 7, 8)]
x[5:8]
x[c(1:4, 9:12)]
c(x[5:8],x[c(1:4, 9:12)])

x <- c(8.75, 9.45, 9.35, 9.85, 9.45, 10.55, 9.75, 8.25, 10.55, 9.45, 9.75, 8.45 )
summary(x[1:6])
summary(x[7:12])

###Logical comparisons

x <- -1
if (x > 0) signal <- "FAIL" else
 if (x <= 0) signal <- "SUCCESS"

if (x > 0) signal <- "FAIL" else
 if (x <= 0 & x > -2) signal <- "SUCCESS" else
 signal <- "FAIL"

x <- c(7.5, 8.2, 3.1, 5.6, 8.2, 9.3, 6.5, 7.0, 9.3, 1.2, 14.5)
location <- x == 8.2
location

sum(location)
x[location]

which(x == 8.2)

x <- rep(c("A", "B", "C"), 3)
location <- x == "B"
location
location <- x == "B"
x1 <- c(1, 4, 3, NA, 7)
x2 <- c("a", "B", NA, "NA")
is.na(x1)
is.na(x2)

##Excercise
y <- c(33, 44, 29, 16, 25, 45, 33, 19, 54, 22, 21, 49, 11, 24, 56)
min(y)
max(y)
which(y==min(y))
which(y==max(y))

med<-median(y)
y[which(y<med)]
med
y[which(y>med)]
sort(y)


###Matrices

x <- c(1, 2, 3)
y <- c(1.4, 1.7, 4)
z <- rbind(x, y)
dim(z)
z<-cbind(x,y)
dim(z)
cbind(z,z)
rbind(z,z)

x <- c(8.75, 9.45, 4.35, 6.85, 9.45, 10.55, 7.75, 8.25, 10.55, 2.45, 15.75, 7.45 )
matrix(x, nrow = 3)
matrix(x, ncol = 4)
matrix(x, nr = 3, byrow = T)

x <- c(8.75, 9.45, 4.35, 6.85)
x <- matrix(x, nrow = 2)
x
y<-matrix(seq(-1,2),nrow=2)
x+y
x-y
x*y
x%*%y
round(x%*%y,digits=1)

y <- c(1, 3)
x <- matrix(c(1, -1, 1, 1), nr = 2, byrow = T)
solve(x,y)
x%*%(solve(x,y))
x[1,1]
x[c(1,2),2]
x[,2]

#Excercises

x <- c(4, 3, 2, 1)
x <- matrix(x, nrow = 2)
y <- c(4, 9, 1, 16)
y <- matrix(y, nrow = 2)
x
y

2*x
sqrt(y)
x*x
x%*%x
solve(y)
round(solve(y)%*%y,digits=1)
y[1,]
y[,2]

z<-c(2,1)
z%*%y
y%*%z


###Accessing exazmple datasets

data()
data(cars)
str(cars)
head(cars,n=5L)
summary(cars)

median(cars[,2])
mean(cars[,1])
mean(cars$speed)
speed<-cars$speed
mean(speed)

##Excercise

data(women)
help(women)
mean(women$height)
mean(women$weight)
summary(women)



###The apply() function
data(women)
str(women)
women
apply(women,2,mean)

#Excercies
data("Loblolly")
str(Loblolly)
head(Loblolly)
l1<-Loblolly[,c(1,2)]
str(l1)
apply(l1,2,mean)
apply(cars,2,mean)
summary(cars)
summary(l1)

### The aggregate() and table() Functions

data("ToothGrowth")
ToothGrowth$dose<-factor(ToothGrowth$dose)
str(ToothGrowth)
aggregate(ToothGrowth$len, by = list(ToothGrowth$supp, ToothGrowth$dose), mean)
result <- with(ToothGrowth, aggregate(len, by = list(supp, dose), mean)) 
colnames(result) <- c("supp", "dose", "mean")
result

addmargins(table(ToothGrowth$supp, ToothGrowth$dose)) 


###Execercise
data("ToothGrowth")
aggregate(len ~ supp+dose, data=ToothGrowth,FUN=median)
with(ToothGrowth,addmargins(table(supp,dose)))


##Loops

sum <- 0 
for (k in 1:10){
    sum <- sum + k 
    } 
sum

sum<-0
k<-1
while(k<=10){
  sum<-sum+k
  k<-k+1
}
sum

###Excercise
for (k in 1:5){
  print(factorial(k))
}

k<-1
while(k<=5){
  print(factorial(k))
  k<-k+1
}


###Writing functions

cv<-function(x){
  object<-100*sd(x)/mean(x)
  return(object)
}

data(trees)
str(trees)
head(trees)
girth<-trees[,1]
cv(girth)

cv(trees[,1])
cv(trees$Girth)
apply(trees,2,cv)

###Excercise

variance<-function(x){
  object<-var(x)
  return(object)
}
apply(trees,2,variance)

##Statistical computation, simulation and random sampling

dnorm(5,mean=5, sd=3)
x<-seq(-5,10,by=0.1)
results<-dnorm(x,mean=3,sd=2)
head(results,n=5L)

data(trees)
set.seed(123)
index<-sample(1:nrow(trees),5,replace=FALSE)
index<-seq(1,nrow(trees),by=10)
trees[index,]

##Excercise

dnorm(0.5,mean=2,sd=sqrt(0.25))
pnorm(2.0,mean=2,sd=sqrt(0.25))
qnorm(0.95,mean=2,sd=sqrt(0.25))
pnorm(3.0,mean=2.0,sd=sqrt(0.25)) - pnorm(1.0,mean=2.0,sd=sqrt(0.25))

?dt()
dt(0.5,5)
pt(2.0,5)
qt(0.95,5)
pt(3,5)-pt(1,5)

###Graphics

data(cars)
speed<-cars$speed
distance<-cars$dist
par(mfrow=c(2,2))
hist(speed)
boxplot(speed)
hist(distance)
boxplot(distance)
par(mfrow = c(1,1))
summary(cars)
plot(speed,distance)

#excercise

data("faithful")
str(faithful)
plot(faithful$waiting, faithful$eruptions)

x<-rnorm(250)
#par(mfrow=c(1,2))
hist(x)
stem(x)
#par(mfrow=c(1,1))


#Color applications

color<- colors()
str(color)
color[1:5]
color<-palette("default")
color
color<-palette(c("red","purple","blue","green3","yellow","orange"))
color
pie(rep(1,6),labels=c(color[1:6]),col=c(color[1:6]),main="Color Wheel")

library(gplots)

col2hex(c("red", "purple", "blue", "green3", "yellow", "orange")) 
col2hex(color[1:6]) 
col2hex(seq(1, 6)) 

par(mfrow=c(4,1))
new_orange = rgb(255,127,0,maxColorValue = 255)
barplot(1,axes=FALSE,col=new_orange)
new_orange = rgb(1.0,0.5,0,maxColorValue = 1.0)
barplot(1,axes=FALSE,col=new_orange)
new_orange = rgb(255,127,0,127,maxColorValue = 255)
barplot(1,axes=FALSE,col=new_orange)
new_orange = rgb(1.0,0.5,0,0.5,maxColorValue = 1.0)
barplot(1,axes=FALSE,col=new_orange)
par(mfrow=c(1,1))


n<-6
rand.data<-replicate(n,rnorm(100,100,sd=1.5))
sq<-seq(0,n-1,1)
col.list<-rgb(1-sq/(n-1),0,sq/(n-1))
boxplot(rand.data,col=col.list)
col2rgb(rgb(1-sq/(n-1),0,sq/(n-1)))


if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
  }
display.brewer.all() 


require("RColorBrewer")
set.seed(123)
sample<-table(rbinom(10000,10,0.5))/10000
successes <- (seq(0,10))
barplot(sample,main="Binomial sample proportions",ylab="frequency",ylim=c(0,0.3),xlab="Number of successes",
        names.arg=c(factor(seq(0,10))),col=c(brewer.pal(11,"Spectral")))


set.seed(123)
variable<-rnorm(500,mean=0,sd=1)
par(mfrow=c(1,3))
hist(variable,ylim=c(0,120),col=rgb(1,0,0,0.25))
hist(variable,ylim = c(0,120),col=rgb(1,0,0,0.5))
hist(variable,ylim = c(0,120),col=rgb(1,0,0))
par(mfrow=c(1,1))


sales <- c(0.70, 0.74, 0.64, 0.39, 0.70, 2.20, 1.98, 0.64, 1.22, 0.20, 1.64, 1.02, 2.95,     0.90, 1.76, 1.00, 1.05, 0.10, 3.45, 1.56, 1.62, 1.83, 0.99, 1.56, 0.40, 1.28,     0.83, 1.24, 0.54, 1.44, 0.92, 1.00, 0.79, 0.79, 1.54, 1.00, 2.24, 2.50, 1.79,1.25, 1.49, 0.84, 1.42, 1.00, 1.25, 1.42, 1.15, 0.93, 0.40, 1.39, 0.30, 0.35)
sales <- jitter(sort(sales, decreasing = FALSE),75) 
week <- seq(1:length(sales)) 
c <- week/length(week)  # This is a use-defined function that specifies a proportion. 
coly <- rgb(c,0,0)      # This assigns a color to each week. 
plot(week, sales, col=coly, pch=19, cex=1, main = "Sales per Week") 


data(mtcars)
col.list<-numeric(0)
cylinders<-factor(mtcars$cyl)
hp<-mtcars$hp
mpg<-mtcars$mpg
color<-c("red","blue","green")
col.list[cylinders == "4"] <- 1 # Denotes "red" from the palette. 
col.list[cylinders == "6"] <- 2# Denotes "red" from the palette. 
col.list[cylinders == "8"] <- 3 # Denotes "red" from the palette. 
plot(hp, mpg, pch = 16, col = c(color[col.list]), main = "MPG vs HP") 
legend(x = 250, y = 30,legend = paste("Cylinders", seq(4, 8, 2)), col = c(color), pch = 16) 
lines(lowess(mtcars$hp, mtcars$mpg, 0.5), lwd = 2) 
