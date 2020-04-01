library(distrEx)
library(manipulate)

dist1 = function(distr,n, type1, type2){
Y = X_1 = distr
for (i in 1:(n-1)) Y = Y + distr
if (type2=="mean") {X = Y/n
} else {X = Y}

ty   = ifelse(type1=="pdf", "d", "p")
norm1 = ifelse(type1=="pdf", dnorm, pnorm)
sty1 = ifelse(type1=="pdf", expression(paste(X[i], " 의 확률밀도함수 : %C(%P)")), 
                          expression(paste(X[i], " 의 누적분포함수 : %C(%P)")))
if(type2=="sum"){
 sty2 = ifelse(type1=="pdf", expression(paste(sum(X[i], i==1, n), " 의 확률밀도함수 : %C(%P)")), 
               expression(paste(sum(X[i], i==1, n), " 의 누적분포함수 : %C(%P)")))
} else {
 sty2 = ifelse(type1=="pdf", expression(paste(bar(X), " 의 확률밀도함수 : %C(%P)")), 
                expression(paste(bar(X), " 의 누적분포함수 : %C(%P)")))
} 

sty3 = ifelse(type1=="sum", "개 %C(%P) 확률변수 합의 분포", "개 %C(%P) 확률변수 평균의 분포")

max1 = max(distr::q(X_1)(0.99), distr::q(X)(0.99))
min1 = min(distr::q(X_1)(0.01), distr::q(X)(0.01))

par(mfrow=c(2,1))
plot(X_1,mfColRow = FALSE,to.draw.arg=c(ty), ylab="", xlab="", xlim=c(min1, max1),
     cex.points = 1.0, main = paste(n, sty3),  inner = list(sty1))
 legend("bottom", c(paste0("기댓값 :", format(distrEx::E(X_1), nsmall=2)), 
                   paste0("분산 :",  format(distrEx::var(X_1)), nsmall=2), 
                   paste0("왜도 :",  format(distrEx::skewness(X_1), nsmall=2)), 
                   paste0("첨도 :",  format(distrEx::kurtosis(X_1), nsmall=2))), 
       col="white", bty="n", xpd = TRUE, horiz = TRUE, inset = c(-0.4,  -0.4))  

plot(X,  mfColRow = FALSE,to.draw.arg=c(ty), ylab="", xlab="", main="",  
     cex.points = 1.0,  xlim=c(min1, max1), inner = list(sty2))
# curve(norm1(x, distrEx::E(X), distrEx::sd(X)), col=2, add=TRUE, lwd=2)
legend("bottom", c(paste0("기댓값 :", format(distrEx::E(X), nsmall=2)), 
                   paste0("분산 :",  format(distrEx::var(X)), nsmall=2), 
                   paste0("왜도 :",  format(distrEx::skewness(X), nsmall=2)), 
                   paste0("첨도 :",  format(distrEx::kurtosis(X), nsmall=2))), 
       col="white", bty="n", xpd = TRUE, horiz = TRUE, inset = c(-0.4,  -0.4))  

}

manipulate(
stat = picker("sum", "mean"),         
distr = picker("Norm", "Exp", "Chisq", "Binom", "Pois"), 
n = slider(2, 100, step=1, initial=16, label="sample size"),
type = picker("pdf", "cdf"), 
if (distr=="Norm") {dist1(Norm(1,1),n, type, stat)
} else if(distr=="Exp") {dist1(Exp(1),n, type, stat)
} else if(distr=="Chisq") {dist1(Chisq(1),n, type, stat)
} else if(distr=="Binom") {dist1(Binom(1,0.5),n, type, stat)
} else {dist1(Pois(1),n, type, stat)
})
