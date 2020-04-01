if (require(manipulate) & require(distrEx)) {
  manipulate(
    distr = picker("N","Td", "Chisq"),
    size = slider(1, 100, initial=10, step=5),
    prob = slider(0, 1, initial=0.5, step=0.1),
    {  
  plotDist(distr, mean = size1*prob1, sd = sqrt(size1*prob1*prob1), lwd=2)
  plotDist("binom", size = size1, prob=prob1, col=2, add=TRUE)}
  )  
}

CLT_f = function(dist, n, an){
  illustrateCLT(Distr = dist, len = n, sleep=an)
}

if (require(manipulate) & require(distrEx)) {
  manipulate(
    distr1 = picker(Norm(0,1),Td(1), Chisq(1)),
    n  = slider(1, 100, initial=10, step=1),
    an  = slider(0, 0.5, initial=0.1, step=0.05),
   plot(distr1)
   )  
}
