set.seed(127)
quant <- numeric(0)
for (k in seq(from = 25, to = 5000, by = 25)){
  x <- rnorm(k, mean = 0, sd = 1)
  quant <- rbind(quant, quantile(x, probs = 0.5, type = 7))
}

size <- seq(from = 25, to = 5000, by = 25)

c1 <- qnorm(0.975,0,1,lower.tail = T)
c2 <- qnorm(0.025,0,1,lower.tail = T)

plot(size, quant, main = "Median Estimates versus Sample Size", xlab = "Sample Size", ylab = "Median", ylim = c(-0.2,0.2))
abline(h = 0.0)
lines(size, c1*sqrt(0.5*0.5/(size*dnorm(0,0,1)**2)), col = "red")
lines(size, c2*sqrt(0.5*0.5/(size*dnorm(0,0,1)**2)), col = "red")
