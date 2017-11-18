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
#'         ggplot of original function and simulation density function
#'         histogram of simulation density function
#'
#' @export
#'
#' @example
#'
#' f<- function(x) {ifelse(0 < x & x < 1, 2*x, 0)}
#' a<-oneDsample(f,1000, 0, 1)
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
    ones <- runif(N, lb, ub)
    maxf<-max(f(ones))+1
    unis <- runif(N, 0, maxf)
    data.frame(x = replicate(20000, {potentialSample <- runif(1, lb, ub);
    ifelse(runif(1, 0, maxf) < f(potentialSample), potentialSample, NA)}))
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



