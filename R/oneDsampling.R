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
#' a<-oneDsample(f,1000, -1, 0)
#' oneDsampleplot(a)
#'
#' f<-function(x) {ifelse(0 <= x & x <=1, 2*(1-x), 0)}
#' w<-oneDsample(f,1000, 0, 1)
#' oneDsamplehist(w)
#'
#' f = function(x) {ifelse(0 <= x & x <= 2*pi ,1/2/pi *(sin(x) + 1),0)
#' oneDsampleplot(oneDsample(f,1000,0,2*pi))
#'
#'
#'
oneDsample <- function(f, N, lb, ub) {
  if (abs(integrate(f,lb,ub)$val-1)>0.001){
    stop("Error: not a pdf.The area under the function you given should be 1")
  }
  else{
    if(lb!=Inf & ub!=Inf){
      maxf<-max(f(runif(10000,lb,ub)))+1
      data.frame(x = replicate(N, {sx <- runif(1, lb, ub);
      ifelse(runif(1,0,maxf) < f(sx), sx, NA)}))
      }
    else if(lb>0 & ub==Inf){
      maxf<-max(f(rexp(10000,rate=0.001)))+1
      data.frame(x = replicate(N*10, {sx <- rexp(1, rate=0.001);
      ifelse(runif(1,0,maxf+1000*dexp(sx, rate=0.001)) < f(sx), sx, NA)}))
      }
    else if(lb==Inf & ub!=Inf){
      maxf<-max(f(rnorm(10000,ub,1000)))+1
      data.frame(x = replicate(N*100, {sx <- rnorm(1, ub, 10000^2);
      ifelse(runif(1,0,maxf+2.5*1000*dnorm(sx, ub, 1000)) < f(sx), sx, NA)}))
      }
    else if(lb!=Inf & ub==Inf){
      maxf<-max(f(rnorm(10000,lb,100)))+1
      data.frame(x = replicate(N*1000, {sx <- rnorm(1, lb, 1000);
      ifelse(runif(1,0,maxf+2.5*1000*dnorm(sx, lb, 1000)) < f(sx), sx, NA)}))
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



