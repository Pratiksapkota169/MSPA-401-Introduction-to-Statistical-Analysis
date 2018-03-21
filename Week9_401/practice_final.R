##1
choose(30,4)
choose(30,4)*factorial(4)

##2
p.x<-c(0.37, 0.12, 0.06, 0.15, 0.30)
x<-c(0,1,2,3,4)
k<-x*p.x
mean.x<-sum(x*p.x)
mean.x
var.x<-sum((x-mean.x)^2 * p.x)
var.x
sqrt(var.x)
#mode 0, median 2
plot(x,cumsum(p.x), main = "cdf for P.x", type = "h", ylim = c(0, 1.0))
abline(h = 0.5, col = "red")
k
median(x)


##3
pnorm(-2, 0, 1,lower.tail = TRUE) + pnorm(3, 0, 1, lower.tail = FALSE)

##4
(0.31*18/31)/((0.25*8/25)+(0.31*18/31)+(0.44*7/44))

##5
approve<-c(8,18,7)
not.approve<-c(17,13,37)
chisq.test(cbind(approve,not.approve),correct = FALSE)
vote<-cbind(approve,not.approve)

# Alternative calculation
m <- addmargins(vote)
m
expected.vote <- cbind(0.33*m[1:3,3],0.67*m[1:3,3])
expected.vote
X <- ((vote - expected.vote)**2)/expected.vote
X <- sum(X[,1]+X[,2])
X                                   # 14.63252
pchisq(X,2,lower.tail = FALSE) 


###6

p.x<-c(0.46,0.41,0.09,0.04)
x<-c(0,1,2,3)
k<-x*p.x
mean.x<-sum(x*p.x)
mean.x
var.x<-sum((x-mean.x)^2 * p.x)
var.x
sqrt(var.x)
#mode 0, median 2
plot(x,cumsum(p.x), main = "cdf for P.x", type = "h", ylim = c(0, 1.0))
abline(h = 0.5, col = "red")
k
median(x)

###7
(10-8.5)/(12-6)
(12-10)/(12-6)


###8
qnorm(0.0901,lower.tail = TRUE)


##9
x <- seq(0,15)
dist_A <- round(dpois(x,5), digits = 4)
dist_A[16] <- 0.0003
dist_B <- round(dbinom(x,15,1/3), digits = 4)
plot(x,dist_A, type = "b", ylim = c(0,0.25), col = "red", ylab = "probability",
     lwd = "2", main = "Plot of Two Discrete Probability Distributions")
lines(x,dist_B, col = "green", lwd = "2", type = "b")
legend("topright", legend = c("dist_A is red", "dist_B is green"))

# Solution
mean_A <- round(sum(x*dist_A), digits = 2)   # [1] 5.0003
variance_A <- round(sum(dist_A*(x-mean_A)^2), digits = 2)  #  [1] 5.0019
mean_B <- round(sum(x*dist_B), digits = 2)  #  [1] 4.9995
variance_B <- round(sum(dist_B*(x-mean_B)^2), digits = 2)   #  [1] 3.332
mean_A
variance_A
mean_B
variance_B


##10

n<-pnorm(0+0.5, 15*0.4, sqrt(15*0.4*0.6), lower.tail = TRUE)
n
n<-pnorm(6+0.5, 15*0.4, sqrt(15*0.4*0.6), lower.tail = TRUE)-pnorm(6-0.5, 15*0.4, sqrt(15*0.4*0.6), lower.tail = TRUE)
n
1-pnorm(9+0.5, 15*0.4, sqrt(15*0.4*0.6), lower.tail = TRUE)

##16
# Calculate z-score:
stdDev <- sqrt((100 * 0.53) * (1 - 0.53))

z <- (45 - (100 * 0.53))/stdDev
z # [1] -1.602888

# Estimate p-value:
2 * pnorm(-1.60) # [1] 0.1095986

# Calculate critical z score:
qnorm(1 - 0.10/2, lower.tail = F) 

###19
z <- (78-68.2)/10.4
p <- 1-pnorm(z,0,1,lower.tail=TRUE)
p <- pnorm(z,0,1,lower.tail = F)
p
x <- seq(20,75,1)
sum(dbinom(x,75,p))
b <- (19.5-p*75)/sqrt(75*p*(1-p))
pnorm(b , 0, 1, lower.tail=FALSE)

###20
x<-c(62,53,64,52,52,54,58)
y<-c(158,176,151,164,164,174,162)
cor(x,y)
cor.test(x,y)


###21
z <- qnorm(0.975, 0, 1, lower.tail = TRUE)
n <- z^2*0.5*0.5/0.05^2   #  [1] 384.1459
n

###22
# critical z = 2.33

# Calculate pooled sample proportion:
p <- (((38/85) * 85) + ((23/90) * 90)) / (85 + 90)

# Calculate standard error:
stdErr <- sqrt(p * (1 - p) * ((1/85) + (1/90)))

z <- ((38/85) - (23/90)) / stdErr
z # [1] 2.657104

# Calculate the critical z-score:
qnorm(1 - 0.01) # [1] 2.326348


##22
x1 <- 12.5
s1 <- 3.9
n1 <- 14

x2 <- 13.8
s2 <- 5.2
n2 <- 17

# Calculate our t:
t <- (x1 - x2) / sqrt(s1^2/n1 + s2^2/n2)
t # [1] -0.7945437

s1n1 <- s1^2/n1
s2n2 <- s2^2/n2

df.calculated <- ((s1n1 + s2n2)^2)/(s1n1^2/(n1-1)+s2n2^2/(n2-1))
df.calculated
n1 + n2 -2

# Calculate the critical t:
qt(1 - 0.05, df = df.calculated, lower.tail = F) # [1] -1.699535



obs<-c(28,30,45,48,38,39)
ec<-rep(sum(obs)/6,times=6)
ec
diff<-sum((obs-ec)^2/ec)
pchisq(diff,df=5,lower.tail=FALSE)
qchisq(0.95,df=5,lower.tail = TRUE)
