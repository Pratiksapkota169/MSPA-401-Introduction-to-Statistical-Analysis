# This program gives bootstrapping illustrations in support of the third extra
# credit problem.  The vector z contains a random sample of 40 observations based 
# upon the distribution presented in the publication by Lumley, Diehr, Emerson and Chen.

# For the parent population, the mean is 1.458 and the standard deviation is 2.379.
# The parent population distribution is highly right skewed with extreme outliers.
# The objective of bootstrapping is to estimate a confidence interval for the
# population parameters based on a sample.  In this demo we will use the random 
# sample z to determine an interval estimate for the mean value.

z <- c(0.089, 0.359, 1.738, 0.086, 0.398, 9.800, 0.081, 0.177, 0.925, 1.890, 0.139,
       9.767, 1.141, 9.881, 0.209, 0.176, 0.335, 0.196, 0.030, 1.674, 0.079, 4.612,
       1.196, 0.508, 0.656, 0.167, 0.018, 0.176, 4.448, 0.329, 0.450, 0.282, 0.62,
       0.003, 3.026, 0.071, 3.252, 9.773, 0.008, 0.015)

mx <-  mean(z)
mx 
sdx <- sd(z)
sdx
median(z)

# We first provide a histogram of the sample data.

cells <- seq(from = 0, to = 12.0, by= 0.75)
hist(z, breaks = cells, main = "Histogram of Random Sample n = 40", xlab = "Sample Values",
     col = "red")
abline(v = mx, col = "green", lwd = 2, lty = 2)
abline(v = median(z), col = "green", lwd = 2, lty = 2)
legend("topright", legend = c("mean =  1.720","median = 0.347"))

# These data are used for bootstrapping.  10,000 random samples with replacement are
# drawn.  The mean, standard deviation and t statistic are calculated for each.  The
# approach shown below illustrates the basic concept behind bootstrapping.  There
# have been a number of enhancements developed over the years to improve upon this
# process.  The "boot" package provides some of these.

set.seed(1237)
m <-numeric(0)
t.m <- numeric(0)
N <- 10000

n <- 40
for (i in 1:N)
{
  w <- sample(z, n, replace = TRUE)
  m[i] <- mean(w)
  stder <- sd(w)/sqrt(n)
  t.m[i] <- (mean(w)-mx)/(stder)
}
summary(m)  # The vector m provides a sampling distribution for the sample mean.

# The percentile bootstrap method determines quantiles from the distribution of
# sample averages generated above.  These quantiles form a confidence interval
# for the population mean.

par(mfrow = c(1,2))
cells <- seq(0.0, 4.25, by = 0.25)
hist(m, main = "Mean Resampling Distribution n = 40", breaks = cells,
     freq  = FALSE, col = "red", xlab = "mean values", ylim = c(0, 1.0))
lines(density(m), lwd=2.5, col = "darkblue")
abline(v= quantile(m, probs = 0.025), col = "blue", lwd = 2, lty = 2)
abline(v= quantile(m, probs = 0.975), col = "blue", lwd = 2, lty = 2)
abline(v= mx, col = "green", lwd = 2, lty = 2)
qqnorm(m, col = "red", pch = 16)
qqline(m, col = "green", lwd = 2, lty = 2)
par(mfrow = c(1,1))

# The blue dotted lines show the endpoints of the 95% confidence interval determined
# using the quantile() function shown below.  The normal QQ plot reveals the
# distribution departs from normality due to the skewing in the data.  The parent
# population is skewed which is the reason for this.  Bootstrapping provides more
# realistic quantile estimates for confidence interval construction.

round(c(quantile(m, probs = c(0.025)), quantile(m, probs = c(0.975))), digits = 2)

# Note that this confidence interval includes the population mean value.

# Another bootstrap method uses the t-statistic.  Similarly to the above, quantiles
# are determined from the simulated sampling distribution for the t-statistic.  
# These are used to form a confidence interval using the t-statistic formula.  
# Skewing in the sampling distribution is a consequence of the skewing in the sample. 
# Bootstrapping provides a way to arrive at realistic quantiles for the t statistic
# which will differ from traditional t statistic quantiles that are determined
# based upon the assumption of normality.

summary(t.m)

x <- seq(-4, 4,0.1)
par(mfrow = c(1,2))
hist(t.m, main = "Resampling Distribution of t-statistic n=40", breaks = "Sturges",
     ylim = c(0, 0.45), prob = TRUE, col = "green", xlab = "t-statistic values")
curve(dt(x, df=39),add=TRUE, col= "darkred", lwd = 2)
abline(v= quantile(t.m, probs = 0.025), col = "blue", lwd = 2, lty = 2)
abline(v= quantile(t.m, probs = 0.975), col = "blue", lwd = 2, lty = 2)

qqplot(rt(1000, df = 39),t.m, xlab = "Theoretical t Quantiles", ylab = "Sample Quantiles",
       main = "Quantile-Quantile Plot t Statistic n = 40", 
       col = "green", pch = 16)
abline(0,1,col="red", lty = 2, lwd = 2)
par(mfrow = c(1,1))

# Quantiles for a 95% confidence interval are determined, and these are used with 
# the t-statistic formula to determine confidence interval endpoints.  Typically the
# confidence interval from this method is wider than the percentile bootstrap
# method.  The t-statistic approach compensates better for skewing involved in the
# original distribution.  Intervals determined this way have been shown in practice
# to be superior to the traditional t-statistic which assumes normality.  The
# confidence interval so determined includes the population mean value.

Q1 <- quantile(t.m, prob = c(0.025), names = FALSE)
Q2 <- quantile(t.m, prob = c(0.975), names = FALSE)
c(Q1, Q2)
round(c(mx - Q2*(sdx/sqrt(n)), mx - Q1*(sdx/sqrt(n))), digits = 2)
      
# Confidence intervals using the classical t-statistic reveal how the normality
# assumption affects the resulting confidence interval.  Bootstrapping is a better
# choice when faced with asymmetric non-normality.

t.test(z, alternative = c("two.sided"), mu = 0, conf.level = 0.95)

# All three confidence intervals contain the population mean value, however they
# differ.  Note that the bootstrap results are shifted to the right compared to the
# traditional t statistic results.  When this happens, due to the skewing of the
# sample data (and the underlying parent population distribution), the bootstrap
# results are preferred.  From another perspective, if all three methods are in 
# agreement, then the traditional t statistic method results have been confirmed.

##The "boot" Package

# The "boot" package has broad applicability and will be used here.  It is 
# discussed in Kabacoff Section 12.6 Pages 292-298 with examples.  Using it
# requires a little study.  

# For this example, the "boot" package requires a function to return the sample
# mean value for each individual resample drawn.  Depending on the parameter being
# estimated, this function would change.  If the variance is of interest, the 
# function would return that value.  Be sure to install the boot package.

library(boot)

set.seed(1237)

f <- function(data, i){
  d <- data[i]
  return(mean(d))
}

# The user-defined function above is substituted in boot() with z and a number  
# of samples are drawn with replacement.  The confidence bounds follow. The boot
# package has a variety of options for determining confidence intervals.  See
# boot.ci() for example shown below with the percentile option.  The different
# computational options may produce slightly different results depending on 
# the number of samples drawn during bootstrapping.  Confidence intervals are 
# determined in two ways using the results from boot().


results <- boot(data=z, statistic=f, R=10000)
round(quantile(results$t,c(0.025, 0.975)), digits=2) 

interval <- boot.ci(results, conf=0.95, type="perc")
round(c(interval$perc[4],interval$perc[5]), digits=2)

# If the bootstrap-t procedure is of interest, "boot" may be used to determine
# the quantiles needed for the t-statistic.  The function is more involved. Note below.

library(boot)
set.seed(1237)

mx <- mean(z)
n <- length(z)

f <- function(data, i){
  mx <- mean(data)
  n <- length(data)
  d <- data[i]
  return((mean(d)-mx)/(sd(d)/sqrt(n)))
}

results <- boot(data=z, statistic=f, R=10000)
round(quantile(results$t,c(0.025, 0.975)), digits=2) 

# The "boot" package provides considerable capability for bootstrapping. Using 
# it requires study.  Other discussions are given at 
# http://www.ats.ucla.edu/stat/r/faq/boot.htm and
# http://www.statmethods.net/advstats/bootstrapping.html.


Q1 <- quantile(results$t, prob = c(0.025), names = FALSE)
Q2 <- quantile(results$t, prob = c(0.975), names = FALSE)
c(Q1, Q2)
round(c(mx - Q2*(sdx/sqrt(n)), mx - Q1*(sdx/sqrt(n))), digits = 2)

Q1 <- quantile(t.m, prob = c(0.025), names = FALSE)
Q2 <- quantile(t.m, prob = c(0.975), names = FALSE)
c(Q1, Q2)
round(c(mx - Q2*(sdx/sqrt(n)), mx - Q1*(sdx/sqrt(n))), digits = 2)
