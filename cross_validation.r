library(ggplot2)
crossval <- function(n,c){ #takes sample size and kernel as input
  set.seed(1)
  X=rnorm(n)
  J=function(h,c){
    fhat=Vectorize(function(x) density(X,from=x, to=x,n=1,bw=h, kernel=c)$y)
    fhati=Vectorize(function(i) density(X[-i],from=X[i],to=X[i],n=1,bw=h, kernel=c)$y)
    F=fhati(1:length(X))
    return(integrate(function(x) fhat(x)^2,-Inf,Inf)$value-2*mean(F)) #This is Mo(h)
  }
  optimize(J,interval=c(0.1,5),c)$minimum  #gives optimal hn as output by minimising Mo(h)w.r.t. h for a fixed n
}

hn<- c()
for(i in c(20,50,100,250)){
  hn <- c(hn,round(crossval(i,"gaussian"),2))
}
nhn<- matrix(c(c(20,50,100,250),hn), ncol=2,byrow=FALSE)
