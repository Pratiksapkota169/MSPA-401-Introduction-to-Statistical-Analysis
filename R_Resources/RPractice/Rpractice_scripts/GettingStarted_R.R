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



