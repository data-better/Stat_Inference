library(stats4)
library(manipulate)
library(arm)

dist_mle = function(n, lambda1, dist){

if (dist=="exp") { 
y <- rexp(n, rate=lambda1)
nLL <- function(lambda) -sum(dexp(y, lambda, log = TRUE))
main1="지수분포"
} else {
y <- rpois(n, lambda=lambda1)
nLL <- function(lambda) -sum(dpois(y, lambda, log = TRUE))
main1="포아송분포"
}
  
fit <- mle(nLL, method = "Brent", start = list(lambda = 1), 
             lower=0, upper=20, nobs=NROW(y))
print(summary(fit))
mle_y <- fit@coef

n = length(y)
lambda = seq(0, (lambda1+3), length=100)

t1 =paste0(main1, "의 히스토그램")
t2 =paste0(main1, "의 가능도함수")

par(mfrow=c(2,1))
if (dist == "exp") {
  LL  <- lambda^n*exp(-lambda * sum(y))
  hist(y, main=t1, freq=FALSE, xlab="", col="steelblue", xlim=c(0,10),border=TRUE, nclass=10)
  curve(dexp(x,lambda1),  0, 10, lwd=3, col=2, type="l", add=TRUE)
  
} else {
  LL  <- lambda^sum(y)*exp(-lambda*n)/prod(factorial(y))  
  discrete.histogram(y, main=t1, freq=FALSE, xlab="", col="steelblue", xlim=c(-0.5,10.5))
    curve(dpois(x,lambda1), 0, 10, lwd=3, col=2, type="h", add=TRUE)
}  

plot(lambda, LL, type="l",  xlab=expression(lambda), 
     ylab=expression(paste("L(", lambda, ")")), main=t2)
abline(v=mle_y,  lty=2, lwd=2, col=2)
abline(v=lambda1,lty=2, lwd=2, col=4)
legend("topright", c(expression(lambda[MLE]), "True"),
       lty=c(2,2), col=c(2,4), lwd=2, , bty="n")    
}

manipulate(
 dist = picker("exp", "pois"),  
 lambda = slider(0, 3, step=0.05, initial=1),
 n = slider(1, 100, step=1, initial=30),
 dist_mle(n,lambda, dist))

    
