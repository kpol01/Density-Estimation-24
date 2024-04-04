library(ggplot2)

J=function(h,c){
  fhat=Vectorize(function(x) density(X,from=x, to=x,n=1,bw=h, kernel=c)$y)
  fhati=Vectorize(function(i) density(X[-i],from=X[i],to=X[i],n=1,bw=h, kernel=c)$y)
  F=fhati(1:length(X))
  return(integrate(function(x) fhat(x)^2,-Inf,Inf)$value-2*mean(F)) 
} #returns Moh

                   
crossval <- function(n,c){
  set.seed(1)
  X=rnorm(n)
  optimize(J,interval=c(0.1,1),c)$minimum
}

crossval(500,"gaussian")
