#' Single variable Rejection Sampling
#'
#' This function 'oneDsample(f,N,lb,up)' implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' Additionaly, the function 'oneDsampleplot()' and 'oneDsamplehist()' shows the plot of original function and simulation density function.

ImportS: ggplot2

#' @param f the pdf that we are sampling from
#' @param N the nimber of attempted samples.
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f

#' @return A vector containing samples from pdf (including NA)
#'         ggplot of original function(red) and simulation density function
#'         histogram of simulation density function
#'
#' @export
#'
#' @example
#'
#' f<- function(x) {ifelse(-1< x & x < 0, 2*(x+1), 0)}
#' a<-oneDsample(f,10000, -1, 0)
#' oneDsampleplot(a)
#'
#' f<-function(x) {ifelse(0 <= x & x <=1, 2*(1-x), 0)}
#' w<-oneDsample(f,10000, 0, 1)
#' oneDsamplehist(w)
#'
#' f = function(x) {ifelse(0 <= x & x <= 2*pi ,1/2/pi *(sin(x) + 1),0)
#' oneDsampleplot(oneDsample(f,1000,0,2*pi))
#'
#' f<- function(x) {ifelse(0<=x, dlnorm(x,mean=0,sdlog=1),0)}
#' a<-oneDsample(f,10000, 0, Inf)
#' oneDsampleplot(a)
#' oneDsamplehist(a)
#'
#' f<- function(x) dnorm(x,-10,2)
#' a<-oneDsample(f,10000, Inf, 10)
#' oneDsampleplot(a)
#' oneDsamplehist(a)
#'
#' f<- function(x) dnorm(x,-10,2)
#' a<-oneDsample(f,10000, Inf, Inf)
#' oneDsampleplot(a)
#'
#'
oneDsample <- function(f, N, lb, ub) {
  if (abs(integrate(f,lb,ub)$val-1)>0.001){
    stop("Error: not a pdf.The area under the function you given should be 1")
  }
  else{
    if(lb!=Inf & ub!=Inf){
      maxf<-max(f(runif(10000,lb,ub)))+1
      data.frame(x = replicate(N, {sx <- runif(1, lb, ub);ifelse(runif(1,0,maxf) < f(sx), sx, NA)}))
    }
    else{
      if(lb==Inf & ub!=Inf){
        x<-rnorm(10000,ub,100)
        maxf<-max(f(x))
        a=x[which(f(x)==maxf)]
        if(maxf>0.5){sx <- runif(N, a-20 , ub)}
        else{sx <- rnorm(N*100, a, 100)}
      }
      else if(lb!=Inf & ub==Inf){
        x<-rnorm(10000,lb,100)
        maxf<-max(f(x))
        a=x[which(f(x)==maxf)]
        if(maxf>0.5){sx <- runif(N, lb , a+20)}
        else{sx <- rnorm(N*100, a, 100)}
      }
      else{
        x<-rnorm(10000,0,100)
        maxf<-max(f(x))
        a=x[which(f(x)==maxf)]
        if(maxf>0.5){sx <- runif(N, a-20 , a+20)}
        else{sx <- rnorm(N*100, a, 1000)}
      }
      data.frame(x = {ifelse(runif(N*100,0,maxf+1) < f(sx), sx, NA)})
    }
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

