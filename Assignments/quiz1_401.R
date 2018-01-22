##problem 1
df<-read.csv2("tests_data.csv", header = TRUE)
str(df)


# Find mode

y <- factor(df$var1)
summary(y)
names(summary(y))

# Supplemental function for finding the mode
# 1) transform numeric values into factor
# 2) use summary() to gain the frequency table
# 3) return mode the index whose frequency is the largest
# 3) transform factor back to numeric even there are more than 1 mode

mode <- function(x){
  y <- as.factor(x)
  freq <- summary(y)
  mode <- names(freq)[freq[] == max(freq)]
  as.numeric(mode)
}
mode(df$var1)

##problem 3
df <- c(31,  47,  29,  31,  16,  48,  41,  50,  54,  37,  22)
max(df)-min(df)
range(df)

##problem 4
df<- c(18,  18,  18,  9,  15,  5,  10,  5,  15,  17,  7)
round(sd(df),1)
sqrt((sum((df - mean(df))^2))/(length(df)-1))


##problem 5
df<-read.csv("tests_data.csv", header = FALSE)
str(df)
x<-df$V1
median(x)
summary(x)
sort(x)

##Problem 7
67.1+2*3.5
1-(1/4)


##Problem 8

df<-read.csv("tests_data.csv", header = FALSE)
str(df)
x<-df$V1
x<-sort(x)
mean(x)
mean(x, trim = 0.1)
round(length(x)*0.1)
mean(x[3:18])


##Problem 9
mag<-c(5,7,9,10,9)
#mag<-c(6,22,35,29,16,8,4,2)
cells <- seq(from = 50, to = 100, by = 10)
center <- seq(from = 54.5, to = 94.5, by =  10)
class <- cut(mag, cells, include.lowest=FALSE, right = FALSE)

Table <- data.frame(table(class), center)
Table$Freq<-mag 
Table

# Sample mean and standard deviation method Section 3.3 Black.
fx <- Table$Freq*center
fx2 <- Table$Freq*(Table$center^2)
Table <- data.frame(Table, fx, fx2)

Table

average <- sum(fx)/sum(Table$Freq)
average
s2 <- (sum(Table$Freq)*sum(fx2) - (sum(fx))^2)/(sum(Table$Freq)*(sum(Table$Freq)-1))
s <- sqrt(s2)
s2
s

##Or
s2 <- sum(Table$Freq)*sum(fx2) - (sum(fx))^2
s <- sqrt(s2/(sum(Table$Freq)*(sum(Table$Freq)-1)))
s

##Problem 10

df<-read.csv("tests_data.csv", header = TRUE)
sapply(df,FUN=sd)
mean(df$RestA)

###Extra credit #1


##Use *set.seed(1237)* and *rnorm(n, mean = 0, sd = 1)* with *n* = 10, *n* = 30, *n* = 100 and *n* = 300 
##to draw four different random samples from the standard normal distribution. Reset *set.seed(1237)* prior 
##to drawing each of the four samples.

##For each sample, calculate the first, second and third quartile using *quantile()*. Use "type = 2" 
##(method used in Business Statistics) and "type = 7" (R default) and generate quartiles for each.

set.seed(1237)
n<-30000
x<-rnorm(n,mean=0,sd=1)
x
p <- c(0.25, 0.5, 0.75)

quantile(x, probs = p, na.rm = FALSE, names = TRUE, type = 2)
quantile(x, probs = p, na.rm = FALSE, names = TRUE, type = 7)

x.quantile<-function(n){
  set.seed(1237)
  p <- c(0.25, 0.5, 0.75)
  x<-rnorm(n,mean=0,sd=1)
  cat("\n Quartiles using Type2: For n =",n, ",",quantile(x, probs = p, na.rm = FALSE, names = TRUE, type = 2))
  cat("\n Quartiles using Type7: For n= ",n,",", quantile(x, probs = p, na.rm = FALSE, names = TRUE, type = 7))
}


s<-c(10,30,100,300)



for (n in s){
x.quantile(n)
}
