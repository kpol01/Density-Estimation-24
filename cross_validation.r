library(ggplot2)

set.seed(1)
X=rnorm(50)

J=function(h,c){
  fhat=Vectorize(function(x) density(X,from=x, to=x,n=1,bw=h, kernel=c)$y)
  fhati=Vectorize(function(i) density(X[-i],from=X[i],to=X[i],n=1,bw=h, kernel=c)$y)
  F=fhati(1:length(X))
  return(integrate(function(x) fhat(x)^2,-Inf,Inf)$value-2*mean(F)) 
}

h=seq(0.1,1,by=.01) #values of bandwidths
Moh=Vectorize(J)(h, "gaussian")

qplot(h,Moh,geom="line",data=data.frame(h,Moh))

h_opt <- optimize(J,interval=c(0.1,1),"gaussian")$minimum

y <- density(X, bw=h_opt, kernel="gaussian")

s <- seq(min(X)-0.1, max(X)+0.1,0.1)
ds <- dnorm(s)
ps <- pnorm(s)
ggplot() + geom_histogram(data = data.frame(X), aes(x = X, y = ),center=-0.1/2, binwidth=0.1, color="black", fill="grey")  + 
  geom_line(data = data.frame(s, ds), aes(x = s, y = ds))

ggplot()+ geom_line(data= data.frame(x,ecdf(x)(x)), aes(x=x, y= ecdf(x)(x)), color="red")+
  geom_line(data = data.frame(s, ps), aes(x = s, y = ps))+ geom_line(data= data.frame(s), aes(x=s, y=0.99), colour="green")


qnorm(0.99)
x[ecdf(x)(x)==0.99]
