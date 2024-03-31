
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(cowplot)




hist4 <- function(n,h, rpdf, dens, alpha)      #if the simulating distribution has single argument
{
  set.seed(seed = 1)
  x <- rpdf(n, alpha)
  s<- seq(min(h),max(h),0.001)
  ds <- dens(s,alpha)
  fig <- ggplot() + geom_histogram(data = data.frame(x), aes(x = x, y = after_stat(density)),breaks=h, color="black", fill="grey")  + 
    geom_line(data = data.frame(s, ds), aes(x = s, y = ds), color="red")
  return (fig)
}

hist5 <- function(n, h, rpdf, dens, alpha, beta)      #if the simulating distribution has two arguments
{
  set.seed(seed = 1)
  x <- rpdf(n, alpha, beta)
  s <- seq(min(h),max(h),0.001)
  ds <- dens(s,alpha,beta)
  
  fig <- ggplot() + geom_histogram(data = data.frame(x), aes(x = x, y = after_stat(density)),breaks=h, color="black", fill="grey")  + 
    geom_line(data = data.frame(s, ds), aes(x = s, y = ds), color="red")
  return (fig)
}

hist_mix6 <- function(n,h,m1,s1,m2,s2,p){
  set.seed(seed=1)
  y <- rnorm(n*p,m1,s1)
  z <- rnorm(n*(1-p),m2,s2)
  x <- c(y,z[z!=y])
  s<- seq(min(h),max(h),0.1)
  ds <- p*dnorm(s,m1,s1)+(1-p)*dnorm(s,m2,s2)
  ggplot() + geom_histogram(data = data.frame(x), aes(x = x, y = after_stat(density)),breaks=h, binwidth=h, color="black", fill="grey")  + 
    geom_line(data = data.frame(s, ds), aes(x = s, y = ds),color="red")
}

#For varying width (binwidth are unequal)
hist_comp_width4 <- function(n, width, rpdf, dens, alpha)
{
  plots <- list()
  for(i in (1:length(width)))
  {
    plots[[i]] <- hist4(n, width[[i]], rpdf, dens, alpha)+labs(title = paste(c("h = ", round(width[[i]],2)), collapse="",sep=","))
  }
  grid.arrange(grobs = plots, ncol = round(length(width)/2))
}


hist_comp_width5 <- function(n, width, rpdf, dens, alpha, beta)
{
  plots <- list()
  for(i in (1:length(width)))
  {
    plots[[i]] <- hist5(n, width[[i]], rpdf, dens, alpha, beta)+labs(title = paste(c("h = ", round(width[[i]],2)), collapse="",sep=","))
  }
  grid.arrange(grobs = plots, ncol = round(length(width)/2))
}

hist_comp_width6 <- function(n, width, m1,s1,m2,s2,p)
{
  plots <- list()
  for(i in (1:length(width)))
  {
    plots[[i]] <- hist_mix6(n, width[[i]], m1,s1,m2,s2,p)+labs(title = paste(c("h = ", round(width[[i]],2)), collapse="",sep=","))
  }
  grid.arrange(grobs = plots, ncol = round(length(width)/2))
}




#Exponential
v<-c(0)
for(i in 1:7){
  v <- sort(c(v, 4/sqrt(prod(1:i))))
}
breaks <- list(v,v*2,v[-c(7:8)]*3,v[-c(7:8)]*4)
exp_width <- hist_comp_width4(500, breaks, rexp, dexp, 1)
title <- ggdraw() + draw_label("exp(1) with n = 500,x_0 = 0",fontface = 'bold')
plot_grid(title, exp_width,ncol = 1,rel_heights = c(0.1, 1))

#Normal
v<-c()
for(i in 3:8){
  v <- sort(c(v, 5/sqrt(prod(3:i))))
}
v<- sort(c(v,-v,0))
breaks <- list(v,v*2,v[-c(1,13)]*3,v[-c(1:2,12:13)]*8)
norm_width <- hist_comp_width5(500, breaks, rnorm, dnorm, 0, 1)
title <- ggdraw() + draw_label("N(0,1) with n=500, x0=0",fontface = 'bold')
plot_grid(title, norm_width,ncol = 1,rel_heights = c(0.1, 1))


#Beta


#mixture normal
v<-c()
for(i in 3:7){
  v <- sort(c(v, 1/sqrt(prod(3:i))))
}
v<- sort(c(v,-v,0))
breaks <- list(c(-5+8*v, 4+6*v),c(-5+6*v, 4+4*v),c(-5+12*v, 4+9*v),c(-5+10*v, 4+7*v))
normmix_width <- hist_comp_width6(1000,breaks,-5,1,4,3,0.3)
title <- ggdraw() + draw_label("0.3*N(-5,1)+0.7*N(4,3) with n=1000, x0=0",fontface = 'bold')
plot_grid(title, normmix_width,ncol = 1,rel_heights = c(0.1, 1))


#Geometric
v<- c(0.5,1.5,3,5,7.5,10.5)
breaks <- list(v,v*2,v*0.5,v*0.75)
geo_width <- hist_comp_width4(1000,breaks, rgeom, dgeom,0.3)
title <- ggdraw() + draw_label("Geometric(0.3) with n = 1000, x_0 = 5.5",fontface = 'bold')
plot_grid(title, geo_width,ncol = 1,rel_heights = c(0.1, 1))

