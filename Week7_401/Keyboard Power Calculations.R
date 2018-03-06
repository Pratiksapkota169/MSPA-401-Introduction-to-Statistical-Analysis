# Power Calculation Program Using the Keyboard Experiment Data

keyboard <- read.csv(file.path("c:/R401/", "keyboard.csv"), sep=",")
str(keyboard)

difference <- keyboard$STYLE_B - keyboard$STYLE_A
summary(difference)

plot(keyboard$STYLE_B,keyboard$STYLE_A,xlab="B",ylab="A",col="blue",pch=16,
     main="Plot of Typing Speeds for Two Different Keyboards")

hist(difference, col = "green", main = "Histogram of Differences in Typing Speed", 
     xlab = "Keyboard Style B minus Keyboard Style A")
abline(v=mean(difference))

sigma <- sd(difference)
sigma

# Assume a standard deviation for the differences in typing speed. sigma = 1.62

sigma <- 1.62
N <- c(10, 25, 50, 75)
values <- seq(0, 2, 0.1)

t <- numeric(0)
power <- numeric(0)
result <- numeric(0)
index <- as.integer(0)

for (n in N){
  index <- index + 1
  critical <- qt(0.95, n-1, ncp = 0, lower.tail = TRUE)
  count <- as.integer(0)
  for (D in values){
    count <- count +1
    for (k in 1:10000){
      trial <- rnorm(n, mean = D, sd = 1.62)
      t[k] <- mean(trial)*sqrt(n-1)/sd(trial)}
    power[count] <- sum(t > critical)/10000}
  result <- c(result,power)
}

L <- length(values)
L1 <- L+1
L2 <- 2*L
L3 <- L2 + 1
L4 <- 3*L
L5 <- 3*L + 1
L6 <- 4*L
plot(values,result[1:L],type = "b", col = "green", lwd = 2, ylab = "Power",
     xlab = "True Difference in Typing Speeds (average words per minute)",
     main = "Power for Four Sample Sizes", ylim = c(0.0,1.0))
lines(values, result[L1:L2], type = "b", col = "blue", lwd = 2)
lines(values, result[L3:L4], type = "b", col = "red", lwd = 2)
lines(values, result[L5:L6], type = "b", col = "orange", lwd = 2)
legend("topleft", legend = c("n=10 green", "n=25 blue", "n=50 red", "n=75 orange"))
legend("bottomright", legend = c("Power Calculations for Keyboard Comparison", 
                                 "One-sided Student's t Test Used", "Standard Deviation Assumed is 1.62",
                                 "Type I Error Rate is 5%"))

par(mfrow = c(2,1))
t <- numeric(0)
power <- numeric(0)
result <- numeric(0)
index <- as.integer(0)

for (n in N){
  index <- index + 1
  critical <- qt(0.99, n-1, ncp = 0, lower.tail = TRUE)
  count <- as.integer(0)
  for (D in values){
    count <- count +1
    for (k in 1:10000){
      trial <- rnorm(n, mean = D, sd = 1.62)
      t[k] <- mean(trial)*sqrt(n-1)/sd(trial)}
    power[count] <- sum(t > critical)/10000}
  result <- c(result,power)
}

L <- length(values)
L1 <- L+1
L2 <- 2*L
L3 <- L2 + 1
L4 <- 3*L
L5 <- 3*L + 1
L6 <- 4*L
plot(values,result[1:L],type = "b", col = "green", lwd = 2, ylab = "Power",
     xlab = "True Difference in Typing Speeds (average words per minute)",
     main = "Power for Four Sample Sizes Type I Error Rate is 1%", ylim = c(0.0,1.0))
lines(values, result[L1:L2], type = "b", col = "blue", lwd = 2)
lines(values, result[L3:L4], type = "b", col = "red", lwd = 2)
lines(values, result[L5:L6], type = "b", col = "orange", lwd = 2)
legend("topleft", legend = c("n=10 green", "n=25 blue", "n=50 red", "n=75 orange"))
legend("bottomright", legend = c("One-sided Student's t Test Used", 
                                 "Standard Deviation Assumed is 1.62"))
                                
t <- numeric(0)
power <- numeric(0)
result <- numeric(0)
index <- as.integer(0)

for (n in N){
  index <- index + 1
  critical <- qt(0.90, n-1, ncp = 0, lower.tail = TRUE)
  count <- as.integer(0)
  for (D in values){
    count <- count +1
    for (k in 1:10000){
      trial <- rnorm(n, mean = D, sd = 1.62)
      t[k] <- mean(trial)*sqrt(n-1)/sd(trial)}
    power[count] <- sum(t > critical)/10000}
  result <- c(result,power)
}

L <- length(values)
L1 <- L+1
L2 <- 2*L
L3 <- L2 + 1
L4 <- 3*L
L5 <- 3*L + 1
L6 <- 4*L
plot(values,result[1:L],type = "b", col = "green", lwd = 2, ylab = "Power",
     xlab = "True Difference in Typing Speeds (average words per minute)",
     main = "Power for Four Sample Sizes Type I Error Rate is 10%", ylim = c(0.0,1.0))
lines(values, result[L1:L2], type = "b", col = "blue", lwd = 2)
lines(values, result[L3:L4], type = "b", col = "red", lwd = 2)
lines(values, result[L5:L6], type = "b", col = "orange", lwd = 2)
legend("topleft", legend = c("n=10 green", "n=25 blue", "n=50 red", "n=75 orange"))
legend("bottomright", legend = c("One-sided Student's t Test Used", 
                                 "Standard Deviation Assumed is 1.62"))

par(mfrow=c(1,1))