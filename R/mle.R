library(stats)

exp_mle = function(n, lambda1){

y <- rexp(n, rate=lambda1)

nLL <- function(lambda) -sum(dexp(y, lambda, log = TRUE))
fit <- mle(nLL, method = "Brent", start = list(lambda = 1), 
             lower=0, upper=20, nobs=NROW(y))
print(summary(fit))
mle_y <- fit@coef

n = length(y)
lambda = seq(0, (lambda1+5), length=100)

LL = lambda^n*exp(-lambda * sum(y))
plot(lambda, LL, type="l",  xlab=expression(lambda), 
     ylab=expression(paste("L(", lambda, ")")) )
    abline(v=mle_y,  lty=2, lwd=2, col=2)
    abline(v=lambda1,  lty=2, lwd=1, col=4)
}

manipulate(
  lambda = slider(0, 5, step=0.1, initial=0.5),
  n = slider(1, 50, step=1, initial=10),
exp_mle(n,lambda)
)

    
1/mean(y) 
