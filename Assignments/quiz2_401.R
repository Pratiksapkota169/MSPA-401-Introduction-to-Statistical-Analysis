##prob 1
4*(5/6)*(1/6)**3


###prob 2

(0.058*0.939)/((0.058*0.939)+(0.942*0.041))

##prob 3

L<-4
x<-5

(L**x)*(exp(-L))/factorial(x)
dpois(x,lambda = L)

##Prob 4

x<-c(1.3,2.2,2.7,3.1,3.3,3.7)
sort(x)
quantile(x,0.63,type = 2)
y<-seq(0,1,by=0.10)

##Prob 5

pnorm(8.7,mean=8.4,sd=1.8/6,lower.tail = FALSE)

##Prob 6

676*0.7


##Prob 8

round(qnorm(0.45,mean=1050,sd=218),1)

#Prob 9

number.of.possible.outcomes <- 5*5*5*5 
number.of.outcomes.with.agreement <- 5
number.of.outcomes.with.agreement/number.of.possible.outcomes

##Prob 10 - hypergeometric


(choose(7,0)*choose(42,3))/choose(49,3)
