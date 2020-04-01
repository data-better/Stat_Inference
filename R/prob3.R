if (require(manipulate) & require(mosaic)) {
  manipulate(
    dist   = 
    approx = picker("Normal Approximation","Poisson Approximation" ), 
    kind = picker("density","cdf"),
    p = slider(0,    1, initial=0.25, step=0.01),
    n = slider(0, 1000, initial=10,  step=10),
    if (approx =="Normal Approximation"){
        plotDist("binom", size=n, prob=p,kind = kind, main="이항분포의 정규근사") + plotDist("norm", n*p, sqrt(n*p*(1-p)), col=2, kind =kind)
    } else { 
      plotDist("binom", size=n, prob=p,kind = kind, main="이항분포의 포아송분포 근사")   + plotDist("pois", lambda=n*p, col=2, kind = kind) 
      } 
  )
}  
  
##################################################################  
    qdist(distribution, seq(.1, .9, by = 0.10), 
          title = "Normal distribution", show.legend = FALSE,
          pattern = "rings")
    qdist("binom", seq(.1, .9, by = 0.10), size=100, prob=0.5,
          title = "Binomial distribution", show.legend = FALSE,
          pattern = "rings")
    qdist("pois", seq(.1, .9, by = 0.10), lambda=100*0.5,
          title = "Poisson distribution", show.legend = FALSE,
          pattern = "rings")
    
  )
}


qdist("norm", seq(.1, .9, by = 0.10), 
      title = "Deciles of a normal distribution", show.legend = FALSE,
      pattern = "rings")
qdist("binom", seq(.1, .9, by = 0.10), size=100, prob=0.5,
      title = "binomial distribution", show.legend = FALSE,
      pattern = "rings")
qdist("pois", seq(.1, .9, by = 0.10), lambda=100*0.5,
      title = "binomial distribution", show.legend = FALSE,
      pattern = "rings")

