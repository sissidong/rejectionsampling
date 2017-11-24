#' Single variable Rejection Sampling
#'
#' This function 'oneDsample(f,N,lb,up)' implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' Additionaly, the function 'oneDsampleplot()' and 'oneDsamplehist()' shows the plot of original function and simulation density function.

ImportS: ggplot2

#' @param f       The pdf that we are sampling from
#' @param N       The number of attempted samples.  Default value is 50000.
#' @param lb      lower bound of support of f.      Default value is Inf.
#' @param ub      upper bound of support of f.      Default value is Inf.
#' @param method  There are two method you can choose, 'norm' or 'unif'.
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
#'oneDsampleplot(oneDsample(f,20000,-1,0,'unif'))
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
#'oneDsampleplot(oneDsample(f))
#'
#'f<- function(x) 1/(pi*(1+x^2))
#'oneDsampleplot(oneDsample(f))
#'
#'
#'
#'

unif<-function(f, N=50000, lb, ub){
  maxf<-max(f(runif(10000,lb,ub)))+1
  data.frame(x = replicate(N, {sx <- runif(1, lb, ub);ifelse(runif(1,0,maxf) < f(sx), sx, NA)}))
}
norm<-function(f, N=50000){
  x<-runif(100000,-1000,1000)
  maxf<-max(f(x))
  a=x[which( f(x) == maxf )]
  minf<-min(f(x))
  b=x[which( f(x) == minf )]
  b=min((abs(b-a)))
  sd1 = (2/maxf)*3                       # 2/maxf according to Area of triangle is 1, *3 for safty
  #sd2 = 2/(sqrt(2*pi)*maxf)             #  for safety, make sd little large
  sd3 = (abs(b-a))/3
  sd=mean(sd1,sd3)
  c= maxf/dnorm(a,a,sd)
  data.frame(x = replicate(N, {sx <- rnorm(1,a,sd); ifelse( runif(1,0,c*dnorm(1,a,sd)) < f(sx), sx, NA)}))
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
    else if(method=='norm'){norm(f,N)}
    else if(method=='unif'){
      if(lb!=Inf & ub!=Inf){unif(f,N,lb,ub)}
      else{stop("Error: if your pdf without bound, you cannot use uniform method.")}
    }
    else{stop("Error: there are two method you can choose, 'norm' or 'unif'.") }
  }
}


oneDsampleplot<-function(oneDsample){
  w<-data.frame(oneDsample)
  ggplot(w,aes(x)) + geom_density() + stat_function(fun = f, color = "red")
}

oneDsamplehist<-function(oneDsample){
  a<-data.frame(na.omit(oneDsample))
  a<-unlist(a)
  hist(a)
}

