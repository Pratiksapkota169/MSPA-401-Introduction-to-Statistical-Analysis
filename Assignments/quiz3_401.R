###Problem 2

critical.value <- qnorm(0.005, mean = 0, sd = 1, lower.tail = TRUE)
critical.value



##Problem 3
p<-30/56
q<-1-p
n<-56
z.values <- qnorm(c(0.025, 0.975), mean = 0, sd = 1, lower.tail = TRUE)
z.values

std <- sqrt(p*q/n)
std

c(signif(p+z.values[1]*std, digits=3), signif(p+z.values[2]*std, digits=3))

# These results match what is given in Business Statistics.  Another method is to
# use the one sample prop.test().  No correction is needed given the sample size.

prop.test(30, 56, alternative = c("two.sided"), conf.level = 0.95, correct = FALSE)
prop.test(30, 56, alternative = c("two.sided"), conf.level = 0.95, correct = TRUE)
binom.test(30, 56, p=0.5, alternative = c("two.sided"), conf.level = 0.95)

##Problem4

E<-0.005
z<-2.05
p<-0.5
q<-1-p

n<-(z**2)*p*q/(E**2)
n

###Problem 6

t.values <- qt(c(0.025, 0.975), 9, lower.tail = TRUE)
std <- 4.8/sqrt(10)
mu <- 8.1
c(signif(mu+t.values[1]*std, digits=3), signif(mu+t.values[2]*std, digits=4))


###problem 7
z.values <- qnorm(c(0.025, 0.975), mean = 0, sd = 1, lower.tail = TRUE)
z.values
(z.values[1]*500/135)**2


##Problem 8


critical.value <- qt(0.005, 143, lower.tail = TRUE)
critical.value

critical.value <- qt(0.025, 143, lower.tail = TRUE)
critical.value

1.6*6/2.611
(3.68*2.611/12)+65.7
66.5-1.97*3.68/12
66.5+1.97*3.68/12

##Problem 10
z.values <- qnorm(c(0.05, 0.95), mean = 0, sd = 1, lower.tail = TRUE)
z.values

# Note that this can be worked in reverse.
pnorm(z.values,mean = 0, sd = 1, lower.tail = TRUE)

pnorm(-1.63,mean = 0, sd = 1, lower.tail = TRUE)
z.values <- qnorm(c(0.05, 0.95), mean = 0, sd = 1, lower.tail = TRUE)
z.values
