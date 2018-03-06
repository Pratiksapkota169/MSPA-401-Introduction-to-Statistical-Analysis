###Problem 2
n1<-100
n2<-100
x1<-42
x2<-45
p1<-0.42
p2<-0.45

p.b<-(n1*p1+n2*p2)/(n1+n2)
q.b<-1-p.b
z<- (p2-p1)/sqrt((p.b*q.b*((1/n1)+(1/n2))))
z
pnorm(z, mean = 0, sd = 1, lower.tail = FALSE)

x <- matrix(c(42,45,58,55), nrow = 2, ncol = 2, byrow = FALSE, dimnames = list(c("new_drug", "control"),c("success", "fail"))) 
print(x)
prop.test(x, alternative = "greater", conf.level = 0.95, correct = FALSE)


#Problem 3

n1<-35
m1<-19.4
v1<-1.4*1.4
n2<-40
m2<-15.1
v2<-1.3*1.3

d.a <- sqrt((1/n1)+(1/n2))
d.b <- sqrt((v1*(n1-1)+v2*(n2-1))/(n1+n2-2))
t<- (m1-m2)/(d.a*d.b)
t
df<-n1+n2-2

delta <- m1-m2
critical.value <- abs((qt(0.025, df, lower.tail = TRUE)))
critical.value
interval <- c( delta - critical.value*d.a*d.b,  delta + critical.value*d.a*d.b)
interval

##problem 4

d<-3
sd<-2.911
n<-8

t<-d*sqrt(n)/sd
df<-n-1

critical.value <- abs((qt(0.025, df, lower.tail = TRUE)))
critical.value
delta<-d
interval <- c( delta - critical.value*sd/sqrt(n),  delta + critical.value*sd/sqrt(n))
interval

##problem 6
qf(0.05,3,16,lower.tail = FALSE)


###Problem 7
v1 <- 2^2
v2 <- 1.1^2
n1<-13
n2<-16
F<-v1/v2
df.v1<-n1-1
df.v2<-n2-1
qf(0.05,df.v1,df.v2,lower.tail = FALSE)
pf(F,df.v1,df.v2,lower.tail = FALSE)

###problem 9
x<-c(2,4,5,6)
y<-c(7,11,13,20)
#x<-c(23,29,29,35,42,46,50,54,64,66,76,78)
#y<- c(69,95,102,118,126,125,138,178,156,184,176,225)
lm(y~x)

##Problem 10
x<-c(47.0,46.6,27.4,33.2,40.9)
y<-c(8,10,10,5,10)
cor(x,y)



###Extra credit 4
set.seed(123)

N <- 10000
beta.boot <- numeric(N)
alpha.boot <- numeric(N)
n <- 49 

for (i in 1:N)
{
  index <- sample.int(n,n, replace = TRUE) 
  test1 <- test[index, ] # resampled data
  #recalculate linear model estimates
  tBoot.lm <- lm(testB ~ testA, data = test1)
  alpha.boot[i] <- coef(tBoot.lm)[1] # new intercept
  beta.boot[i] <- coef(tBoot.lm)[2] # new slope
  
}

print("Bootstrapped Intercept confidence Interval:")
quantile(alpha.boot, c(.025,.975))

print("Bootstrapped Slope confidence Interval:")
quantile(beta.boot, c(.025,.975))

model <- lm(testB~testA, data = test)
lm.coef1 <- round(coef(model)[1],4)
lm.coef2 <- round(coef(model)[2],4)

ll<-round(quantile(alpha.boot,probs = 0.025),4)
ul<-round(quantile(alpha.boot,probs = 0.975),4)
hist(alpha.boot, main = "Sampling Distribution of intercept",
     freq  = FALSE, col = "lightblue", xlab = "Intercept Values")
lines(density(alpha.boot), lwd=2.5, col = "red")
abline(v= ll, col = "blue", lwd = 2, lty = 2)
abline(v= ul, col = "blue", lwd = 2, lty = 2)
abline(v = lm.coef1, col = "darkgreen",lty = 2, lwd=2)
text(ll-0.015,0.2,ll, col = 2)
text(ul+0.015,0.2,ul, col = 2)
legend ( "topright",legend = c("Est. Intercept:", lm.coef1))

ll<-round(quantile(beta.boot,probs = 0.025),4)
ul<-round(quantile(beta.boot,probs = 0.975),4)
hist(beta.boot, main = "Sampling Distribution of Slope",
     freq  = FALSE, col = "lightblue", xlab = "Slope Values")
lines(density(beta.boot), lwd=2.5, col = "red")
abline(v= ll, col = "blue", lwd = 2, lty = 2)
abline(v= ul, col = "blue", lwd = 2, lty = 2)
abline(v = lm.coef2, col = "darkgreen",lty = 2, lwd=2)
text(ll-0.015,6,ll, col = 2)
text(ul+0.015,6,ul, col = 2)
legend ( "topright",legend = c("Est. Slope:", lm.coef2))

df<-as.data.frame(cbind(alpha.boot,beta.boot))
plot(df$alpha.boot,df$beta.boot, main = "Scatter plot of Bootstrapped Slope vs Intercept", col = "darkgreen", xlab = "Intercepts", ylab = "Slopes")
abline(v = lm.coef1, col = "red", lw=2, lty=2)
abline(h = lm.coef2, col = "darkblue", lw=2,lty = 2)
legend ( "topright",legend = c(lm.coef1, lm.coef2), col = c("darkblue", "red"),lty=c(2,2),lw=2)


