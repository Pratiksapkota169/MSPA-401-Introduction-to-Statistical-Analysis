home_prices <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/home_prices.csv")
###Lesson1
str(home_prices)

##Types of measurements
##1. PRICE = Selling price ($hundreds) - Ratio
##2. SQFT = Square feet of living space - Ratio
##3. YEAR = Year of construction (year) - Interval
##4. BATHS = Number of bathrooms - Ordinal
##5. FEATS - Ratio = Number out of 11 features (dishwasher, refrigerator, microwave, disposal, washer, intercom, skylight(s), compactor, dryer, handicap ﬁt, cable TV access) 
##6. NBR = Located in northeast sector of city (YES) or not (NO) - Nominal
##7. CORNER = Corner location (YES) or not (NO)  - Nominal
##8. TAX = Annual taxes ($) - Ratio

price<-home_prices$PRICE
set.seed(9999)
srs<-sample(price,12)
srs
mean(srs)

index<-seq(7,117,10)
ss<-price[index]
ss
mean(ss)
mean(price)
summary(srs)
summary(ss)

par(mfrow=c(2,1))

hist(srs)
hist(ss)
stem(srs)
stem(ss)
par(mfrow=c(1,1))

###Lesson2
tax<- home_prices$TAX

hist(price)
hist(tax)
plot(price,tax)
stem(tax)

par(mfrow=c(2,1))
hist(price)
hist(tax)
par(mfrow=c(1,1))

summary(price)
summary(tax)
with(home_prices,plot(price,tax))

with(home_prices,hist(price,breaks=seq(1300,5500,600)))

with(home_prices,hist(tax,breaks=seq(500,4500,500)))

##Lesson3
mileage <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/mileage.csv")
str(mileage)
head(mileage)
tapply(mileage$MPG,mileage$CLASS,mean)
tapply(mileage$MPG,mileage$CLASS,sd)
tapply(mileage$HP,mileage$CLASS,mean)
tapply(mileage$HP,mileage$CLASS,sd)
hist(mileage$MPG)
hist(mileage$HP)
summary(mileage)

mpg_class<-aggregate(MPG~CLASS,mileage,mean)
mpg_class$sd<-aggregate(MPG~CLASS,mileage,sd)[,2]
mpg_class

hp_class<-aggregate(HP~CLASS,mileage,mean)
hp_class$sd<-aggregate(HP~CLASS,mileage,sd)[,2]
hp_class

shoppers <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/shoppers.csv")

summary(shoppers)
hist(shoppers$Spending)
library("psych")
describe(shoppers)
quantile(shoppers$Spending,0.1)

range <- function(x) {max(x, na.rm = TRUE) - min(x, na.rm = TRUE)}
summary_stats <- function(x) { 
  stats <- data.frame(rbind(mean(x, na.rm = TRUE), 
                            median(x, na.rm = TRUE), 
                            range(x), 
                            sd(x, na.rm = TRUE), 
                            var(x, na.rm = TRUE), 
                            quantile(x, probs = c(0.25), na.rm = TRUE), 
                            quantile(x, probs = c(0.75), na.rm = TRUE), 
                            quantile(x, probs = c(0.10), na.rm = TRUE)), 
                      row.names = c("Mean", "Median", "Range", "StdDev", "Var", "Q1", "Q3", "P10")) 
  colnames(stats) <- "Value" 
  return(stats)
  }
summary_stats(shoppers$Spending)


pontus <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/pontus.csv")
str(pontus)
summary_stats(pontus$Age)
summary_stats(pontus$Ht)
summary_stats(pontus$HtOpp)
apply(pontus[,5:6],2,summary_stats)
diff.p<-(pontus$Ht-pontus$HtOpp)
hist(diff.p)
boxplot(diff.p)
with(pontus, boxplot(Ht, HtOpp, names = c("President's Height", "Opponent's Height")))
mean(diff.p,na.rm=TRUE)
mean(pontus$Ht,na.rm=TRUE)-mean(pontus$HtOpp,na.rm=TRUE)

geyser <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/geyser.csv")
str(geyser)

apply(geyser,2,summary)
with(geyser,boxplot(WEEK1, WEEK2,names=c("W1","W2")))
par(mfrow=c(1,2))
hist(geyser$WEEK1)
hist(geyser$WEEK2)
par(mfrow=c(1,1))
