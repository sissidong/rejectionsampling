#' Single variable Rejection Sampling
#'
#' This function 'oneDsample(f,N,lb,up,method)' implements single variable rejection sampling for continuous rvs with or without bounded support.
#'
#' Additionally, the function 'oneDsampleplot()' and 'oneDsamplehist()' shows the plot of original function and simulation density function.


#' @param f       The pdf that we are sampling from
#' @param N       The number of attempted samples.  Default value is 50000.
#' @param lb      lower bound of support of f.      Default value is Inf.
#' @param ub      upper bound of support of f.      Default value is Inf.
#' @param method  There are three method you can choose, 'norm', 't' or 'unif'.
#'                Defalt method will be the best method we select for your pdf

#' @return A vector containing samples from pdf (including NA)
#'         ggplot of original function(red) and simulation density function
#'         histogram of simulation density function
#'
#' @export
#'
#' @example
#'
#' f<- function(x) {ifelse(-1< x & x < 0, 2*(x+1), 0)}
#'oneDsampleplot(oneDsample(f,20000,-1,0,method='unif'))
#'
#'f<-function(x) {ifelse(0 <= x & x <=1, 2*(1-x), 0)}
#'oneDsampleplot(oneDsample(f,50000))
#'
#'f = function(x) {ifelse(0 <= x & x <= 2*pi ,1/2/pi *(sin(x) + 1),0)}
#'oneDsampleplot(oneDsample(f,50000,0,2*pi))
#'
#'f<- function(x) dlnorm(x,mean=0,sdlog=1)
#'oneDsampleplot(oneDsample(f,50000,'norm'))
#'
#'f<- function(x) dnorm(x,-10,2)
#'oneDsampleplot(oneDsample(f,method='t'))
#'
#'f<- function(x) 1/(pi*(1+x^2))
#'oneDsampleplot(oneDsample(f))
#'
#'f<-function(x) {dunif(x,-500,500)}
#'oneDsampleplot(oneDsample(f,2000))
#'

unif<-function(f, N=50000, lb, ub){
  maxf<-optimize(f,c(lb,ub),maximum = TRUE)
  maxf<-maxf$objective
  data.frame(x = replicate(N, {sx <- runif(1, lb, ub);ifelse(runif(1,0,maxf) < f(sx), sx, NA)}))
}
norm<-function(f, N=50000){
  x<-runif(100000,-1000,1000)
  maxf<-max(f(x))
  a=x[which( f(x) == maxf )]
  a=mean(a)
  sd = 2/maxf
  c= 2*maxf/dnorm(a,a,sd)
  data.frame(x = replicate(N, {sx <- rnorm(1,a,sd); ifelse( runif(1,0,c*dnorm(sx,a,sd)) < f(sx), sx, NA)}))
}

t<-function(f,N=50000){
  x<-runif(100000,-1000,1000)
  maxf<-max(f(x))
  a=x[which( f(x) == maxf )]
  a=mean(a)
  sd=2/maxf
  c=2*maxf/dt.scaled(a,df=1,mean=a,sd)
  data.frame(x = replicate(N, {sx <- rt.scaled(1,1,mean=a,sd);
  ifelse( runif(1,0,c*dt.scaled(sx,df=1,mean=a,sd)) < f(sx), sx, NA)}))
}

oneDsample <- function(f, N=50000, lb=Inf, ub=Inf,method='best') {
  if (abs(integrate(f,lb,ub)$val-1)>0.001){
    stop("Error: not a pdf.The area under the function you given should be 1")
  }
  else{
    if(method=='best'){
      if(lb!=Inf & ub!=Inf){unif(f,N,lb,ub)}
      else{norm(f,N)}
    }
    else if(method=='t'){t(f,N)}
    else if(method=='norm'){norm(f,N)}
    else if(method=='unif'){
      if(lb!=Inf & ub!=Inf){unif(f,N,lb,ub)}
      else{stop("Error: if your pdf without bound, you cannot use uniform method.")}
    }
    else{stop("Error: there are three method you can choose, 'norm', 't' or 'unif'.") }
  }
}


oneDsampleplot<-function(oneDsample){
  w<-data.frame(oneDsample)
  ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
}



