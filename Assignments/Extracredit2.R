example <- function(n){
  set.seed(123)
  result <- matrix(0, nrow = 1000, ncol = n)
  for (k in 1:1000) {
    result[k, ] <- rnorm(n, mean = 0, sd = 1)
  }
  return(result)
}

# We can define our 1000 x n matrices by passing our desired n to example(). For example, for
# n = 10:

sample10 <- example(10)


# You will need to add code here for creating the same objects with n = 40 and n = 100.
sample40 <- example(40)
sample100 <- example(100)



library(moments)  # install.packages("moments")

skewness10 <- apply(sample10, MARGIN = 1, FUN = skewness)
quantile10 <- quantile(skewness10,c(0.025,0.25,0.75,0.975))
cat("(0.025,0.25,0.75,0.975) quantiles of skewness statistic computed from 1000 normal random samples of size n=10 respectively: ", quantile10)

skewness40 <- apply(sample40, MARGIN = 1, FUN = skewness)
quantile40 <- quantile(skewness40,c(0.025,0.25,0.75,0.975))
cat("\n\n(0.025,0.25,0.75,0.975) quantiles of skewness statistic computed from 1000 normal random samples of size n=40 respectively: ", quantile40)

skewness100 <- apply(sample100, MARGIN = 1, FUN = skewness)
quantile100 <- quantile(skewness100,c(0.025,0.25,0.75,0.975))
cat("\n\n(0.025,0.25,0.75,0.975) quantiles of skewness statistic computed from 1000 normal random samples of size n=100 respectively: ", quantile100)


##Basic histograms
par(mfrow = c(3, 1))

hist(skewness10, col = "lightblue", main = "Histogram of samples skewness with n=10")
abline(v = quantile10, col = "red",lty=2, lwd=2)
text(quantile10,rep(200,length=4),c("2.5% \nquantile","1st \nquartile","3rd \nquartile","97.5% \nquantile"), adj=c(-0.1,1),col="red")


hist(skewness40,col = rgb(1,0,0,0.5),  main = "Histogram of samples skewness with n=40")
abline(v = quantile40, col = "darkgreen",lty=2, lwd=2)
text(quantile40,rep(200,length=4),c("2.5% \nquantile","1st \nquartile","3rd \nquartile","97.5% \nquantile"), adj=c(-0.1,1),col="darkgreen")


hist(skewness100,col = rgb(0,1,0,0.5),  main = "Histogram of samples skewness with n=100")
abline(v = quantile100, col = "blue",lty=2, lwd=2)
text(quantile100,rep(100,length=4),c("2.5% \nquantile","1st \nquartile","3rd \nquartile","97.5% \nquantile"), adj=c(-0.1,1),col="blue")


par(mfrow = c(1, 1))


###Histograms with density overlaid
par(mfrow = c(3, 1))


cells <- seq(-2,2,0.2)
x <- seq(-2.0,2.0,0.1)

m<-mean(skewness10);std<-sqrt(var(skewness10))
hist(skewness10,col = "lightblue", probability = T, breaks = cells, main = "Histogram of samples skewness with n=10")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)
abline(v = quantile10, col = "red",lty=2, lwd=2)
text(quantile10,rep(0.6,length=4),c("2.5% \nquantile","1st \nquartile","3rd \nquartile","97.5% \nquantile"), adj=c(-0.1,1),col="red")


m<-mean(skewness40);std<-sqrt(var(skewness40))
hist(skewness40,col = rgb(1,0,0,0.5), probability = T, breaks = cells, main = "Histogram of samples skewness with n=40")
curve(dnorm(x, mean=m, sd=std), col="red", lwd=2, add=TRUE)
abline(v = quantile40, col = "green",lty=2, lwd=2)
text(quantile40,rep(0.8,length=4),c("2.5% \nquantile","1st \nquartile","3rd \nquartile","97.5% \nquantile"), adj=c(-0.1,1),col="green")

m<-mean(skewness100);std<-sqrt(var(skewness100))
hist(skewness100,col = rgb(0,1,0,0.5), probability = T, breaks = cells,ylim = c(0.0,2.0), main = "Histogram of samples skewness with n=100")
curve(dnorm(x, mean=m, sd=std), col="darkgreen", lwd=2, add=TRUE)
abline(v = quantile100, col = "blue",lty=2, lwd=2)
text(quantile100,rep(1,length=4),c("2.5% \nquantile","1st \nquartile","3rd \nquartile","97.5% \nquantile"), adj=c(-0.1,1),col="blue")


par(mfrow = c(1, 1))


