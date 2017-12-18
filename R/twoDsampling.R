#' Two variable Rejection Sampling
#'
#' This function "twoDsample(f, N, lbx, ubx, lby, uby)" implements two variabel rejection sampling for rvs with bounded support and which have bounded pdf.
#'
#' Additionaly, the function 'twoDsampleplot()' shows the plot of simulation joint density function.


#' @param f the pdf that we are sampling from
#' @param N the nimber of attempted samples.
#' @param lbx lower bound of x support of f
#' @param ubx upper bound of x support of f
#' @param lby lower bound of y support of f
#' @param uby upper bound of y support of f
#'
#' @return A vector containing samples from pdf
#'         ggplot of simulation joint density function
#'
#' @export
#'
#' @example
#' f <- function(x, y) x + y
#' A<-twoDsample(f,50000,0,1,0,1)
#' twoDsampleplot(A)
#'
#'
#' f<- function(x,y){ifelse(0<x & x <1 & 0<y & y<1 & 0<x+y & x+y<1, 24*x*y, 0)}
#' A<-twoDsample(f, N=10000, lbx=0, ubx=1, lby=0, uby=1-x)
#' twoDsampleplot(A)
#'
#' f<- function(x,y){ifelse(0<x & 0<y & x+y<=1, 2, 0)}
#' A<-twoDsample(f, N=5000, lbx=0, ubx=1, lby=0, uby=1-x)
#' twoDsampleplot(A)

#' f<- function(x,y){ifelse(0<x & x<1 & 0<y & y<=x, 2, 0)}
#' A<-twoDsample(f, N=5000, lbx=0, ubx=1, lby=0, uby=x)
#' twoDsampleplot(A)



twoDsample<- function(f, N=10000, lbx=-Inf, ubx=Inf, lby=-Inf, uby=Inf) {
  if (abs(int2(f,c(lbx,lby),c(ubx,uby))-1)>0.001){
    stop("Error: not a pdf. The area under the function you given should be 1")
  }
  else{
    if(lbx!=Inf & ubx!=Inf & lby!=Inf & uby!=Inf){
      x=runif(10000,lbx,ubx)
      maxf<-max(replicate(10000,f(x,y=runif(1,lby,uby))))
      data.frame(t(replicate(N, {pSX <- runif(1,lbx,ubx); pSY <- runif(1,lby,uby);
                            if(runif(1,0,maxf) < f(pSX, pSY)) {c(pSX, pSY)} else c(NA,NA)})))
    }
    else{stop}
  }
}


twoDsampleplot<-function(twoDsample){
  ggplot(twoDsample,aes(x=X1,y=X2)) + geom_density_2d()
}




twoDsample<- function(f, N=5000, lbx=-Inf, ubx=Inf, lby=-Inf, uby=Inf) {
  x=runif(5000,lbx,ubx)
  maxf<-max(replicate(5000,f(x,y=runif(1,lby,uby))))+0.5
  data.frame(t(replicate(N, {pSX <- runif(1,lbx,ubx); pSY <- runif(1,lby,uby);
  if(runif(1,0,maxf) < f(pSX, pSY)) {c(pSX, pSY)} else c(NA,NA)})))
}










