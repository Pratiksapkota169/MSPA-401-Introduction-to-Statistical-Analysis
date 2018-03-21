##pROBLEM 8
x<-c(1.2,1.4,1.6,1.8,2.0)
y<-c(54,53,55,54,56)
lm(y~x)

##Problem 7
cost<-c(9,2,3,4,2,5,9,10)
number<-c(85,52,55,68,67,86,83,73)
cor(cost,number)
cor.test(cost,number)

##problem 9
c1<-c(3.33,3.42,3.39,3.30,3.46,3.39,3.36,3.44,3.37,3.38)
c2<-c(3.43,3.40,3.39,3.32,3.39,3.38,3.34,3.38,3.38,3.28)
sA <- sqrt(var(c1))
sB <- sqrt(var(c2))
nA <- 10
nB <- 10
dfA <- nA-1
dfB <- nB-1
F <- (sA/sB)^2  
F
qf(0.99, dfA, dfB,lower.tail = TRUE)   # 1.993819
pf(F, dfA, dfB, lower.tail = FALSE) 




###problem 10

n<-24
chi<-(n-1)*(81/65)^2
chi
pchisq(chi,n-1,lower.tail = FALSE)
qchisq(0.95,n-1,lower.tail = T)

##problem 6
# Source		 df	   SS		  MS = SS/df		F-statistic
# Treatment		3			75		25	              
# Error			  8    50		 6.25
# Total       12    125

F<-25/6.25
F
pf(F, 3, 8, lower.tail = FALSE)  # p-value = 0.0001607934
qf(0.9, 3, 8, lower.tail = TRUE)    # critical value = 4.938


###problem 6
n<-36
z<-(72.1-70)/(9/sqrt(n))
z
pnorm(z, mean = 0, sd = 1,lower.tail = FALSE)

##problem 4
before<-c(117,119,128,120,121,129,122,114,117,118)
after<-c(122,126,121,127,123,119,131,129,122,130)

delta <- after-before  #  null hypothesis is that the difference is zero.
#  alternative hypothesis is the difference is > 0.
sd(delta)
qt(0.90, 9, lower.tail = TRUE)

t.test(delta, alternative = c("greater"), mu = 0, paired = FALSE, conf.level = 0.9)


###problem 3
visits <- c(30,35,18,10,7)
visits1<-c(0.37,0.29,0.15,0.13,0.06)
expVisits<-sum(visits)*visits1
expVisits
# Calculate our chi-square statistic:
chiSq <- sum((visits - expVisits)^2 / expVisits)
chiSq
pchisq(chiSq, df = 5 - 1, lower.tail = F)

chisq.test(visits, p = visits1)


q.values <- qchisq(0.05, 4, lower.tail = FALSE)
q.values

pchisq(q.values, 4, lower.tail = TRUE)


###problem 2
P.x <- c(0.200,0.239,0.245,0.182,0.130,0.003,0.001)
x <- c(0, 1, 2, 3, 4,5,6)
sum(P.x)
mean.x <- sum(x*P.x)   
mean.x               # 1.89
var.x <- sum(P.x*(x - mean.x)^2)
var.x                # 2.9379
sqrt(var.x) 

plot(x,cumsum(P.x), main = "cdf for P.x", type = "h", ylim = c(0, 1.0))
abline(h = 0.5, col = "red")
pp<-x*P.x
pp
pnorm(2, mean.x, sqrt(var.x), lower.tail = TRUE)
ceiling(log(0.5)/log(0.5))

##problem1
(0.3*0.05)/(0.3*0.05+0.7*0.02)
