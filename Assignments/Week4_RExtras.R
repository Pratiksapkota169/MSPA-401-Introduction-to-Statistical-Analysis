###Lesson 4
##Problem1
shoppers <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/shoppers.csv")
str(shoppers)
table(shoppers$Spending>=40)
sum(shoppers$Spending>=40)/nrow(shoppers)
sum(shoppers$Spending<10)/nrow(shoppers)


n1<-sum(shoppers$Spending>=40)
n2<-sum(shoppers$Spending<10)
n<-nrow(shoppers)
n1*n2/(n*(n-1)/2)



m<-sum(shoppers$Spending>=10 & shoppers$Spending<=40)
m*(m-1)/(n*(n-1))


successful_combinations <- n1*n2*m*(m-1)/2 
total_combinations <- n*(n-1)*(n-2)*(n-3)/(4*3*2*1)
successful_combinations/total_combinations


shoppers_more_than_30 <- subset(shoppers, subset = Spending>30)
sum((shoppers_more_than_30$Spending>40)==TRUE)/nrow(shoppers_more_than_30)


##Problem2

set.seed(1234)
count_duplicates<-0
for (i in 1:100) {
  this_sample<- sample(1:365, size =22, replace = TRUE)
  if(length(this_sample) != length(unique(this_sample)))
    count_duplicates <- count_duplicates+ 1
}

prob_any_duplicates <- count_duplicates/100
prob_any_duplicates

set.seed(1234)
mean(replicate(100,any(duplicated((sample(1:365,22,replace = TRUE))))))

set.seed(1234)
mean(replicate(10000,any(duplicated((sample(1:365,22,replace = TRUE))))))



count<- function(N,p){
  x<-runif(n=N)
  count<- x<=p
  m<-sum(count)
  }

set.seed(1234)
result = NULL
for (i in 1:50) {
  result <- c(result, count(20,0.6))
}

result <- result >=11
sum(result)/50


set.seed(1234)
result = NULL

for (i in 1:10000) {
  result <- c(result, count(20,0.6))
}

outcome<-table(result)
outcome<-as.data.frame(outcome)
Count_Frequency <- result
hist(Count_Frequency)

sum(result)/10000

result <- result >=11
sum(result)/10000


trials<- c(0:20)
probabilities <- dbinom(trials,size=20,prob=0.6)

successes <- trials[5:20]
binomial_probs <- probabilities[5:20]
successes<-factor(successes)
barplot(binomial_probs, names.arg = successes, xlab = "Successes", ylab = "binom probs")

str(outcome)
relative_frequency <- outcome[,2]/sum(outcome[,2]) 
successes <- outcome[,1] 
successes <- factor(successes) 
barplot(relative_frequency, names.arg = successes, xlab = "successes", ylab = "relative frequency")

par(mfrow=c(1,2))
barplot(binomial_probs, names.arg = successes, xlab = "successes", ylab = "binomial probabilities") 
barplot(relative_frequency, names.arg = successes, xlab = "successes", ylab = "relative frequency")
par(mfrow=c(1,1))


###Lesson 5
#Problem 1
prob_win_four <- dbinom(x = 4, size = 4, prob = 1/6) 
sprintf("%.4f", prob_win_four) # result printed with four places behind decimal



prob_loses_four <- dbinom(x = 0, size = 4, prob = 1/6) 
sprintf("%.4f", prob_loses_four)


prob_wins_exactly_one <- dbinom(x = 1, size = 4, prob = 1/6) 
sprintf("%.4f", prob_wins_exactly_one)


prob_wins_at_least_one <- 1 - dbinom(x = 0, size = 4, prob = 1/6)
sprintf("%.4f", prob_wins_at_least_one)


##Problem 2

prob_15_of_20 <- pbinom(q = 15, size = 20, prob = 0.5, lower.tail = FALSE) 
sprintf("%.4f", prob_15_of_20)

target_p_value <- 0.05 
current_p_value <- 1.00 
n <- 0

while (current_p_value > target_p_value) { 
  n <- n + 2 
  current_p_value <- pbinom(q = n-1, size = n, prob = 0.5, lower.tail = FALSE)
  cat("\n Number of Consecutive Cups Correctly Identified:", n, "p_value: ", sprintf("%.4f", current_p_value)) 
  } 
cat("\n\nLady Tasting Tea Solution: ", n, "Consecutive Cups Correctly Labeled", "\n p-value: ",sprintf("%.4f", current_p_value),"<= 0.05 critical value")


##Problem 3

for (x in 0:20) cat("\n x:", x, "prob:", sprintf("%.4f", dpois(x, lambda = 4.6)))

x <- 0:20 
prob_x <- dpois(x, lambda = 4.6)
plot(x, prob_x, las = 1, type = "h") 
title("Poisson Probabilities (lambda = 4.6)")

#Problem 4
sprintf("%.4f", dbinom(x = 0, size = 100, prob = 0.001))
sprintf("%.4f", dpois(0, lambda = 0.1))


###Lesson 6

##Problem1
sprintf("%.4f", pnorm(75, mean = 81.14, sd = 20.71, lower.tail = TRUE))

sprintf("%.4f", pnorm(100, mean = 81.14, sd = 20.71, lower.tail = FALSE))

prob_greater_than_50 <- pnorm(50, mean = 81.14, sd = 20.71, lower.tail = FALSE)
prob_greater_than_100 <-pnorm(100, mean = 81.14, sd = 20.71, lower.tail = FALSE) 
sprintf("%.4f", prob_greater_than_50 - prob_greater_than_100)


##Problem2
sprintf("%.4f", qnorm(0.90, mean = 97.11, sd = 39.46, lower.tail = TRUE))

sprintf("%.4f", qnorm(0.50, mean = 97.11, sd = 39.46, lower.tail = TRUE))

##Problem3

set.seed(1234) # seed the random number generator for reproducibility 
my_first_sample <- rnorm(n = 50, mean = 100, sd = 4) 
std_error1 <- 4/sqrt(50) 
cat("\nmy_first_sample mean: ", mean(my_first_sample), " sample_std_dev: ", sd(my_first_sample), " std_error: ", std_error1, "\n")

my_second_sample <- rnorm(n = 50, mean = 100, sd = 4) 
std_error2 <- 4/sqrt(50) 
cat("\nmy_second_sample mean: ", mean(my_second_sample), " sample_std_dev: ", sd(my_second_sample), " std_error: ", std_error2, "\n")

my_third_sample <- rnorm(n = 5000, mean = 100, sd = 4) 
std_error3 <- 4/sqrt(5000) 
cat("\nmy_third_sample mean: ", mean(my_third_sample), " sample_std_dev: ", sd(my_third_sample), " std_error: ", std_error3, "\n")


##Problem 4
n <- 600
p <- 1/3 
x <- 250 
sprintf("%.6f", pbinom(q = x, size = n, prob = p, lower.tail = FALSE))
xCorrect <- 250 - 0.5 
z <- (xCorrect - n*p)/sqrt(n * p * (1-p))
sprintf("%.6f", pnorm(z, mean = 0, sd = 1, lower.tail = FALSE))


##Problem5

par(mfrow=c(1,3), oma=c(0,0,2,0)) 
hist(runif(25, min = 0, max = 1), main = "") 
hist(runif(100, min = 0, max = 1), main = "") 
hist(runif(400, min = 0, max = 1), main = "")
mtext("Histograms of uniform distribution (n = 25, 100 and 400)", side = 3, outer = T, line = -1)
par(mfrow=c(1,1))

##Problem 6

salaries <- read.csv("C:/NorthWestern_Courses/MSPA-401-Introduction-to-Statistical-Analysis/R_Resources/Lessons in R_Complete/Lessons in R_Data Files/salaries.csv")
str(salaries)
with(salaries, hist(AGE))
with(salaries, plot(density(AGE)))     
qqnorm(salaries$AGE, main = "AGE QQ",xlab = "Normal Quantiles",ylab = "Age Quantiles") 
qqline(salaries$AGE, distribution =qnorm, probs = c(0.25, 0.75), qtype = 7)


###Lesson 7

##PRoblem1 
set.seed(1234)
x <- c() # creates empty vector 
y <- c() # creates empty vector

for (i in 1:100) {
  z <- runif(10) 
  x <- append(x, mean(z)) # vector "x" will contain our 100 means 
  y <- append(y, var(z)) # vector "y" will contain our 100 variances 
  }

hist(x)
