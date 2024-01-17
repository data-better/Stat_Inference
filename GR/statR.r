### 제0장 패키지의 인스톨 ####

install = function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

required.packages = c("mosaic", "mosaicCalc", "distrEx", "distrTeach", "TeachingDemos", "stats4", "tkrplot")
install(required.packages)

### 1. 동전 던지기의 분포 ####
#library(mosaic)
 n_num =10000
 r_rflip = do(n_num) * rflip(2) 
 
# 앞면의 수의 분포 근삿값
 taa = tally(~heads, data=r_rflip)
  taa/n_num
 histogram(~ heads, data=r_rflip, width=1) 

### 2. 기댓값 구하기 ####
#library(mosaicCalc)
 
f  = makeFun(2*exp(-2*x) ~ x)  
F  = antiD(x*f(x) ~ x)
Ex = F(Inf) - F(0)

r_exp = rexp(10000,2)
mean(r_exp)

### 3. 이항분포 ####
#library(distrEX)

# 3-1 이항분포 B(n,p) 관련 기본 함수
n = 10
p = 0.5
dbinom(5, n, p)   # P(X=5)
pbinom(5, n, p)   # P(X<=5)
qbinom(0.5, n, p) # 분위수 산출
rbinom(10 , n, p) # 난수 생성

# 3-2. 이항분포의 기댓값과 분산 distr, distrEx 패키지 이용
distroptions("WarningArith" = FALSE) 
X = Binom(10, 0.5)

d(X)(5)     # P(X=5)
p(X)(5)     # P(X<=5)
q.l(X)(0.5) # 분위수 산출
r(X)(10)    # 난수 생성

E(X)        # 기댓값
var(X)      # 분산  
sd(X)       # 표준편차
E(2*X+5)    # 기댓값 연산
var(2*X+5)  # 분산 연산

# 3-3. 이항분포의 합의 분포
X1 = Binom(1, 0.3)
X2 = Binom(1, 0.3)
Y = X1 + X2

Y      # 분포
E(Y)   # 기댓값
var(Y) # 분산

### 4. 정규분포 ####

# 4-1. 정규분포의 함수 #
N= Norm(10, 2)  # 평균 10, 표준편차 2인 정규분포
plot(N)

Z= Norm()       # 평균 0, 표준편차 1인 정규분포
pnorm(1.96)
p(Z)(1.96)

# 4-2. 합의 분포 #

n=100000
X1 = rnorm(n,1,1)
X2 = rnorm(n,2,1) 
SX = X1+X2
plot(density(SX, bw=0.8), xlim=c(-4,9),ylim=c(0,0.35), main="", xlab="")
 lines(density(X1, bw=0.8),lty=2, col=4)
 lines(density(X2, bw=0.8),lty=2, col=2)
legend("topright", c(expression(X[1]+X[2]), 
                     expression(X[1]), expression(X[2])), lty=c(1,2,2), col=c(1,4,2))

N1= Norm(10, 2)
N2= Norm(10, 2)
NN = (N1 + N2)/2
NN
plot(NN)

### 5. t분포 ####
Z    = Norm()
Chi2 = Chisq(5) 
T_5  = Z/sqrt(Chi2/5)
T    = Td(5)

p(T_5)(-2:2)
p(T)(-2:2)

par(mfrow=c(2,1))
plot(T,   to.draw.arg="d", xlim=c(-6,6))
plot(T_5, to.draw.arg="d", xlim=c(-6,6))

### 6. 분포의 근사  ###

# 6-1. 이항분포의 포아송분포 근사 # 
 Poisson_2 = Pois(2)
 Binom_21  = Binom(10, 0.2)
 Binom_22  = Binom(100, 0.02)
 
 par(mfrow=c(3,1))
 plot(Poisson_2, mfColRow = FALSE, to.draw.arg="d", ylab="")
 plot(Binom_21, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(0, 12))
 plot(Binom_22, mfColRow = FALSE, to.draw.arg="d", ylab="", xlim=c(0, 12))
 par(mfrow=c(1,1))
 
# 6-2. 대수의 법칙 #
illustrateLLN(Distr = Pois(2), main="Posssion(2)")
 
# 6-3. 중심극한정리 #
clt.examp(1)
clt.examp(5)
clt.examp(10)
clt.examp(30)	
  
### 7. 가설검정 ####

# 7-1. 제1종의 오류와 검정력 #
run.power.examp()

# 7-2. P값 #
run.Pvalue.norm.sim()

### 8. 신뢰구간의 비교 ####
run.ci.examp()

