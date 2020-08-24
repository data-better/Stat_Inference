########################################################################
# 1장
########################################################################

install.packages(“mosaic”); install.packages("mosaicCalc")
library(“mosaic”); library("mosaicCalc")
D(exp(-1*x^2/2)~x)  # 미분
F=antiD(exp(-1*x^2/2)~x) # 적분
F(Inf) - F(-Inf)           # 정적분
pnorm(1)-pnorm(-1)      # 표준정규분포 P(-1<X<1)
# 표준정규분포 그리기
plotFun(dnorm(x)~x,  xlim=range(-4,4)) 

########################################################################
# 2장
########################################################################

# 예 2.3
library(mosaic)
n_num =100000
r_dice = do(n_num) * sample(1:6,1)  
taa = tally(~sample, data=r_dice)
taa/n_num 
sample
sum(taa[c(1,3,5)])/n_num             
barchart(taa/n_num*100, xlab="prop(%)")

# 예 2.4
library(mosaic)
n_num =100000
r_rflip = do(n_num) * rflip(2) 
taa = tally(~heads, data=r_rflip)
taa/n_num
heads
histogram(~ heads, data=r_rflip, width=1)
(taa[2]+taa[3])/n_num; 1-taa[1]/n_num

# 예 2.5
library(mosaic)
n_num =100000
r_dice = do(n_num) * sample(1:6,1) 
sample = r_dice[r_dice$sample %%2 ==1,] 
taa = tally(~sample)
taa/n_num
sample
(taa[1]+taa[2])/(taa[1]+taa[2]+taa[3])

# 예 2.7 
n_num =100000
# 1:불량품, 0: 정상제품
r_box = do(n_num) * sample(c(0,0,1,1),2)  
r_box$X = apply(r_box,1,sum)
taa = tally(~X, data=r_box)
taa/n_num

# 예 2.10, 2.12 
n_num =100000
r_rflip = do(n_num) * rflip(2)  
# 기댓값과 분산
mean(r_rflip$heads)
var(r_rflip$heads)

# 예 2.16 
n_num =100000
r_rflip_1 = do(n_num) * rflip(1)  
r_rflip_2 = do(n_num) * rflip(1)  
r_rflip   = r_rflip_1 + r_rflip_2
r_rflip$X = r_rflip_1$heads
r_rflip$Y = r_rflip$heads
# 결합확률질량함수
ta_XY = tally(Y~X, data=r_rflip)
ta_XY/n_num
# 주변부확률질량함수
ta_X = tally(~X, data=r_rflip)
ta_X/n_num
ta_Y = tally(~Y, data=r_rflip)
ta_Y/n_num

########################################################################
# 3장
########################################################################

# 이항분포의 특성
library(distrEx)
Binom5_1 = Binom(5,0.5)
Binom5_2 = Binom(5,0.25)
Binom5_3 = Binom(5,0.75)
par(mfrow=c(3,1))
plot(Binom5_1,mfColRow = FALSE,to.draw.arg="d", ylab="")
plot(Binom5_2,mfColRow = FALSE,to.draw.arg="d", ylab="")
plot(Binom5_3,mfColRow = FALSE,to.draw.arg="d", ylab="")
E(Binom5_1); E(Binom5_2); E(Binom5_3)   
var(Binom5_1); var(Binom5_2); var(Binom5_3) 

# 이항분포와 포아송분포의 관계
library(distrEx)
Poisson_2 = Pois(2)
Binom_21  = Binom(10, 0.2)
Binom_22  = Binom(100, 0.02)

par(mfrow=c(3,1))
plot(Poisson_2, mfColRow = FALSE, to.draw.arg="d", ylab="")
plot(Binom_21, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(0, 12))
plot(Binom_22, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(0, 12))
b1 = d(Poisson_2)(0:12)
b2 = d(Binom_21)(0:12)
b3 = d(Binom_22)(0:12)
mean(abs(b2-b1))
mean(abs(b3-b1))

# 이항분포의 정규분포 근사
library(mosaic)
plotDist("binom", size=100, prob=.30, col=2, lwd=2) + plotDist("norm", mean=30, sd=sqrt(100 * .3 * .7), add=TRUE)
# 정규분포의 비교
plotDist("norm", mean=1, sd=1/2, lwd=2, xlim=c(-6,6) ) + plotDist("norm", col=2, lwd=2, add=TRUE) + plotDist("norm", mean=-1, sd=2, lwd=2, col=1, add=TRUE)

########################################################################
# 4장
########################################################################

# 예 4.8
n=100000
X1 = rnorm(n,1,1);X2 = rnorm(n,2,1) 
SX = X1+X2
plot(density(SX, bw=0.8), xlim=c(-4,9),ylim=c(0,0.35),
    main="", xlab="")
lines(density(X1, bw=0.8),lty=2, col=4)
lines(density(X2, bw=0.8),lty=2, col=2)
legend("topright", c(expression(X[1]+X[2]), 
   expression(X[1]), expression(X[2])), lty=c(1,2,2), col=c(1,4,2))

# 합의 확률분포
library(distrEx)
Bi_sum = Binom(5,0.3) + Binom(2, 0.3) + Binom(3, 0.3)
Bi_sum
Po_sum = Pois(5) + Pois(2) + Pois(3)
Po_sum
Norm_sum = Norm(5,2) + Norm(2,4) + Norm(3,3)
Norm_sum 
Norm_mean = (Norm(2,2) + Norm(2,2) + Norm(2,2))/3
Norm_mean 
 
# t분포
library(distrEx)
Ta = Norm()/sqrt(Chisq(5)/5)
par(mfrow=c(2,1))
plot(Td(5), mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(-6, 6))
plot(Ta, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(-6, 6))
p(Ta)(2);p(Td(5))(2)

# 대수의 법칙
library(distrTeach)
illustrateLLN(Distr = Pois(2), main="Posssion(2)")

# 중심극한정리
library(TeachingDemos)
clt.examp(1)
clt.examp(30)

########################################################################
# 5장
########################################################################

# 예제 5.8
curve(x^5*(1-x)^1 , 0, 1, xlab="p", ylab="L(p)", col="red")
curve(x^4*(1-x)^2 , 0, 1, col="blue", add=TRUE)
legend("topleft", bty="n", c(expression(paste(sum(x[i], i=1,n),"=5")), 
   expression(paste(sum(x[i], i=1,n),"=4"))), lty=1, col=c("red", "blue") )
abline(v=5/6, lty=2, col="red") ; abline(v=4/6, lty=2, col="blue")   

# 예 5.10 지수분포
library(stats4)
x = c(1.2, 1.3, 1.5, 2.1, 2.5,  2.7, 2.8)
minuslogL = function(lambda){
      -sum(dexp(x, lambda, log=TRUE)) }
MaxLikeEst = mle(minuslogL, start=list(lambda=1), 
                 method = "L-BFGS-B",  lower = 0, upper = Inf)
summary(MaxLikeEst)

mle = MaxLikeEst@coef
1/mean(x) 

n = length(x)
lambda = seq(0, 3, length=100)
likelihood = lambda^n*exp(-lambda * sum(x))
plot(lambda, likelihood, type="l",  xlab=expression(lambda), 
      ylab=expression(paste("L(",lambda,")")),col="red")
abline(v=mle,  lty=2, col="blue")
like_mle<-mle^n*exp(-mle * sum(x)) ; like_mle
lambda 

# 예 5.12 정규분포
library(stats4)
x = c(160, 155, 161, 170, 153, 158, 159, 165, 157, 165, 
        173, 165, 171, 177, 188, 168, 179, 172, 166, 171)
minuslogL = function(mu,sigma2){
     -sum(dnorm(x, mean=mu, sd=sqrt(sigma2), log=TRUE)) }
MaxLikeEst = mle(minuslogL, start=list(mu=160, sigma2=70),  
                  method = "L-BFGS-B", lower = c(-Inf, 0),  
                  upper = c(Inf, Inf))
summary(MaxLikeEst)
mle = MaxLikeEst@coef
mle_mu = mle[1]; mle_sigma2 = mle[2]
n = length(x)
mean(x) 
sum((x-mle_m)^2)/n
mu<-seq(150, 180, length=100)
likelihood<-rep(NA, length(mu))
for( i in 1: length(mu) ){
     mle_s2<-sum((x-mu[i])^2)/n
     likelihood[i]<-(2*pi)^(-n/2)* (mle_s2)^(-n/2) * exp(-n/2) }

plot(mu, likelihood, type="l",  xlab=expression(mu), 
      ylab=expression(paste("L(",mu,")")), col="red")
abline(v=mle_mu,  lty=2, col="blue")
like_mle=(2*pi)^(-n/2)*mle_sigma2^(-n/2)*exp(-n/2); like_mle

# 예 5.15 감마분포
library(stats4) 
x = c(3.6, 5,4, 4.5, 3,3, 6.5, 4.6, 5.9, 12.8, 7.2, 10.3) 

# 적률추정법 Gamma(alpha, 1/beta)
m1 = mean(x) ; m2 = mean(x^2)
mme_alpha = m1^2/(m2-m1^2)   # alpha shape mme
mme_beta  = (m2-m1^2)/m1     # beta scale mme
mme_alpha                      # r = alpha
mme_beta                       # lambda = 1/beta
minuslogL = function(alpha, beta){ 
     - sum(dgamma(x, shape=alpha, scale=beta,log=TRUE) )}
MaxLikeEst = mle(minuslogL, method = "L-BFGS-B",   
                  start=list(alpha=mme_alpha, beta =mme_beta), 
                  lower = c(1, 0), upper = c(Inf, Inf))
summary(MaxLikeEst)
mle = MaxLikeEst@coef
mle_alpha = mle[1]
mle_beta  = mle[2]

# 그림 5.4
hist(x, main="", freq=FALSE, xlab="", col="gray")
curve(dgamma(x, shape=mle_alpha, scale=mle_beta), 
  add=TRUE, col=2, lwd=2)
curve(dgamma(x, shape=mme_alpha, scale=mme_beta), 
   add=TRUE, col=4, lwd=2)
legend("topright", bty="n", c("mle","mme"), lty=1, lwd=2, 
  col=c("red", "blue") )

# 예 5.20 회귀모형
x = c(4,6,6,8,8,8,9,9,10,12)
y = c(9,10,18,20,15,17,20,22,25,30)
minuslogL = function(beta0, beta1, sigma2) {
 -sum(dnorm(y - x * beta1 - beta0, 0, sqrt(sigma2), log=TRUE)) }
MaxLikeEst = mle(minuslogL, method = "L-BFGS-B", start=list(beta0=1, beta1 = 1, sigma2 = 1), 
       lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
summary(MaxLikeEst)
        
mle = MaxLikeEst@coef
mle_beta0 = mle[1]; mle_beta1 = mle[2];
mle_sigma2 = mle[3]
n = length(y)
lse = lm(y~x)
summary(lse)
            
lse_beta0 = coef(a)[[1]]; lse_beta1 = coef(a)[[2]]
lse_sigma2 = sum( (y-beta0-beta1*x)^2 )/(n-2)
plot(x,y, ylim=c(0,30), xlim=c(0,12))
abline(lse, col=2)
beta0.grid<-seq(-6, 6, length=1000)
beta1.grid<-seq(-6, 6, length=1000) 
Q0<-rep(NA, 1000) ; Q1<-rep(NA, 1000)

for( i in 1:1000) 
Q0[i]=-sum((y-beta0.grid[i]-mle_beta1*x)^2)/2/mle_sigma2
      -n/2*log(2*pi*mle_sigma2)
for( i in 1:1000) 
Q1[i]=-sum((y-mle_beta0-beta1.grid[i]*x)^2)/2/mle_sigma2
       -n/2*log(2*pi*mle_sigma2)
par(mfrow=c(1,2))
plot(beta0.grid, Q0, type="l", col=2,
       xlab=expression(paste(beta,0)),
       ylab=expression(paste("L(",beta ,"0)"))) 
abline(v=mle_beta0, lty=2, col=4)
plot(beta1.grid, Q1, type="l", col=2,
   xlab=expression(paste(beta,1)),
   ylab=expression(paste("L(",beta,"1)")))
abline(v=mle_beta1, lty=2, col=4)


########################################################################
# 6장
########################################################################

# 예 6.5 정규분포의 표본평균과 표본분산의 불편성
set.seed(1808)
REP =1000
n = 20
sm.n   = bias_m.n = rep(NA,REP)
svar.n  = bias_v.n = rep(NA,REP)
mu=5      
sigma2=1  
for( i in 1:REP){
     x = rnorm(n, mean=mu, sd=sqrt(sigma2))
     sm.n[i]  = mean(x)
     bias_m.n[i] = mu - sm.n[i]
     svar.n[i]  = var(x)
     bias_v.n[i] = sigma2 - svar.n[i]
 }
mean(sm.n);   mean(bias_m.n)
mean(svar.n); mean(bias_v.n)

par(mfrow=c(1,2))
hist(sm.n, col="gray", xlab = expression(bar(X)), 
   freq=FALSE, main="")
abline(v=5, lty=2, lwd=2, col=2)
hist(svar.n, col="gray", xlab = expression(S^2), 
  freq=FALSE, main="")
abline(v=1, lty=2, lwd=2, col=2)

# 예 6.6 MSE 비교
sigma = 1
n.seq = seq(2, 100, by=1)
mse.s1 = 2*(sigma^4)/(n.seq-1)
mse.s2 = (sigma^4)*(2*n.seq-1)/n.seq^2 
mse.s = cbind(mse.s1, mse.s2)
matplot(n.seq, mse.s, type="ll", lty=1:2, 
   col=c("black", "red"), xlab="sample size n", ylab="MSE"  )
legend("topright", c(expression(MSE(S^2 )), 
   expression(MSE(hat(sigma)^2 ))),  lty=1:2, 
   col=c("black", "red"))     # 그림 6.4

########################################################################   
# 7장
########################################################################

# 예 7.6
library(mosaic)
library(TeachingDemos)
n_num =10000
z_test = do(n_num)*z.test(rnorm(10),stdev=1)$statistic
tally(~(abs(z)>qnorm(0.975)), data=z_test)/n_num
histogram(~z, data=z_test, width=1, groups = abs(z) qnorm(0.975), fit="normal", xlim=c(-5,5), breaks=c(-20:20)/2.5, type = "density")

# 예 7.7
library(mosaic)
library(TeachingDemos)
n_num =10000
n_num=10000
t_test = do(n_num)*t.test(rnorm(10))$statistic
tally(~(abs(t)>qt(0.975,9)), data=t_test)/n_num
tally(~(abs(t)>qnorm(0.975)), data=t_test)/n_num
histogram(~t,data=t_test, width=1, groups = abs(t)>qt(0.975,9), fit="t", xlim=c(-5,5), breaks=c(-20:20)/2, type = "density")

# 검정력의 비교
library(TeachingDemos)
power.examp(n = 1, stdev = 1, diff = 1, alpha = 0.05)
power.examp(n = 10, stdev = 1, diff = 1, alpha = 0.05)

# 검정력의 비교
library(TeachingDemos)
run.power.examp()

########################################################################
# 8장
########################################################################
n_num = 100
n = 10
data = matrix(rnorm(n_num * n), n)
ci   = apply(data, 2, function(x) t.test(x)$conf.int)
sum(ci[1, ] <= 0 & ci[2, ] >= 0)/n_num*100

plot(range(ci), c(0, n_num+1), type = "n", xlab = "신뢰구간",ylab = "시행")
for (i in 1:n_num) {
     if (ci[1,i]*ci[2,i] < 0) {lines(ci[, i], rep(i, 2), lwd = 1, col=2) 
     } else lines(ci[, i], rep(i, 2), lwd = 2, col=4)
 }
abline(v = 0, lwd = 2, lty = 2)

library(TeachingDemos)
ci.examp(mean.sim = 0, sd = 1, n = 10, reps = 100, conf.level = 0.95, method = "t")

# 9장

# 예 9.2
theta = seq(from=0,to=1,length=100)
prior.den = dbeta(theta, alpha, beta)
post.den = dbeta(theta, alpha+x, beta+n-x)
ymax = max(prior.den, post.den)
plot(theta, post.den, type="l", ylab="density", ylim=c(0,ymax))
 lines(theta, prior.den, col="magenta")
 legend(0.6,2.8,  c("posterior","prior"), lty=c(1,1),
           lwd=c(2.5,2.5),col=c("black","magenta"))

# 예 9.4
post.mean = (alpha+x)/(alpha+beta+n)
map = (alpha+x-1)/(alpha+beta+n-2)
post.med = qbeta(0.5,alpha+x,beta + n-x)
est = c(post.mean = post.mean, map=map, post.med = post.med)
est

#예 9.7.
ci = c(lower = qbeta(0.025,alpha+x,beta + n-x), upper = qbeta(0.975,alpha+x,beta + n-x))
ci

#예 9.15. 
B10 =dbinom(8,size=n, prob=0.75)/dbinom(8,size=n,prob=0.5)
logB10=log10(B10)
bf = c(logB10=logB10, B10= B10)



   



  




