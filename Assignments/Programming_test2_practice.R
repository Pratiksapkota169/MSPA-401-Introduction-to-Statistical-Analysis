require(graphics)
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)
## extreme points are omitted since dbinom gives 0.
mtext("dbinom(k, log=TRUE)", adj = 0)
mtext("extended range", adj = 0, line = -1, font = 4)
mtext("log(dbinom(k))", col = "red", adj = 1)

matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
       dimnames = list(c("row1", "row2"),
                       c("C.1", "C.2", "C.3")))

t.test(pre,post, mu=0, var.equal=TRUE, paired=TRUE)


#t.test(pre, alternative = c("two.sided"), mu = mean(post), conf.level = 0.95)

wt.diff <- pre-post

t <- (mean(wt.diff)-0)/(sd(wt.diff)/(sqrt(10)))
t

critical.value <- qt(0.95, 9, lower.tail = TRUE)
critical.value

qt(c(0.025, 0.975), 9, lower.tail = TRUE)

# The critical value is less than the t-value so the null hypothesis can be
# rejected.  The difference in result is a consequence of a smaller sample variance.
# A p-value may be calculated as well.  It is smaller than 5%.

2*(pt(abs(t), 9, lower.tail = FALSE))

2*pt(-abs(t),9)

2*pt(t,9, lower.tail = TRUE)

2*pt(-t,9, lower.tail = FALSE)

# Since the entire data set is available, t.test() can be used.

t.test(wt.diff, alternative = c("two.sided"), mu = 0, conf.level = 0.95)

t.test(pre,post,alternative = c("two.sided"),paired = TRUE, var.equal = TRUE)


t.values <- qt(c(0.025, 0.975), 9, lower.tail = TRUE)
std <- sd(wt.diff)/sqrt(10)
mu <- mean(wt.diff)
c(signif(mu+t.values[1]*std, digits=5), signif(mu+t.values[2]*std, digits=5))