library(manipulate)
options(warn = -1) 

a_dist = function(ap,ty,n,p){
  dist1 = ifelse(ty=="density", dbinom, pbinom) 
  ty1 = ifelse(ty=="density", "h", "s") 
  if (ap=="Poisson Approximation"){
  dist2 = ifelse(ty=="density", dpois, ppois)
  distl = function(x,n,p) dist2(x,n*p)
  ty2 = ty1; main1 = "포아송분포"
   } else {
  distl = ifelse(ty=="density", dnorm, pnorm)
  distl = function(x,n,p) dist2(x,n*p, sqrt(n*p*(1-p)))
  ty2 = "l" ;  main1 ="정규분포"
   }
    x = 0:n
  plot(x, dist1(x,n, p), type=ty1, ylab=ty, xlab="", xlim=c(-1, (n+1)), 
       lwd=2, col=4, main=paste("이항분포의 ", main1, " 근사")) 
  curve(distl(x,n,p), 0, n, lwd=2, col=2, type=ty2, add=TRUE)
  legend("bottom", c("이항분포", main1), col=c(4,2), lwd=2, bty="n", xpd = TRUE, 
         horiz = TRUE, inset = c(-0.4,  -0.4))  
}

par(mfrow=c(1,1))

manipulate(
  approx = picker("Poisson Approximation", "Normal Approximation"), 
  ty = picker("density","cdf"),
  n = slider(0, 100, initial=10,  step=10),
  p = slider(0,   1, initial=0.05, step=0.01),
  a_dist(approx, ty, n, p)
  )

manipulate(
  approx = picker("Poisson Approximation", "Normal Approximation","both"), 
  ty = picker("density","cdf"),
  n = slider(0, 100, initial=10,  step=10),
  p = slider(0,   1, initial=0.05, step=0.01),
  if (approx=="Poisson Approximation"){
    a_dist(approx, "density",n,p)
    a_dist(approx, "cdf",n,p)
  } else if (approx=="Normal Approximation") {
    a_dist(approx,"density",n,p)
    a_dist(approx,"cdf",n,p)
  } else {
    a_dist("Poisson Approximation",ty,n,p)
    a_dist("Normal Approximation", ty,n,p)
  })
