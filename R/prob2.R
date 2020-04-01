if (require(manipulate) & require(mosaic)) {
  manipulate(
    distribution = picker("binom", "pois", "norm"), 
    kind = picker("density","cdf"),
    p = slider(0,    1, initial=0.5, step=0.05),
    n = slider(1, 1000, initial=10,  step=10),
    normal = checkbox(TRUE, "정규분포 추가"),
    poisson = checkbox(TRUE, "포아송분포 추가"),
    if (distribution=="binom"){
        plotDist("binom", size=n, prob=p,kind = kind)
      } else if (distribution=="pois"){ 
        plotDist("pois", lambda=n*p,col=1, kind = kind, add=poisson)
      } else {
        plotDist("norm", n*p, sqrt(n*p*(1-p)), col=2, kind =kind, add=normal)
      }
  )
}  
  
  
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

