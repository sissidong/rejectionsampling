#' Two variable Rejection Sampling
#'
#' This function "twoDsample(f, N, lbx, ubx, lby, uby)" implements two variabel rejection sampling for rvs with bounded support and which have bounded pdf.
#'
#' Additionaly, the function 'twoDsampleplot()' shows the plot of simulation joint density function.

Imports: rmutil
         ggplot

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
#' twoDsampleplot(a)
#'
#'

twoDsample<- function(f, N, lbx, ubx, lby, uby) {
  if(abs(int2(f,c(lbx,lby),c(ubx,uby))-1)>0.001){
    stop("Error: not a pdf. The area under the function you given should be 1")
  }
  else{
    data.frame(t(replicate(N, {pSX <- runif(1,lbx,ubx); pSY <- runif(1,lby,uby);
                            if(runif(1,f(lbx,lby),f(ubx,uby)) < f(pSX, pSY)) {c(pSX, pSY)}
                            else c(NA,NA)})))
  }
}

twoDsampleplot<-function(twoDsample){
  ggplot(twoDsample,aes(x=X1,y=X2)) + geom_density_2d()
}









