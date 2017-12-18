#' Two variable Rejection Sampling
#'
#' This function "twoDsample(f, N, lbx, ubx, lby, uby)" implements two variabel rejection sampling for rvs with bounded support and which have bounded pdf.
#'
#' Additionaly, the function 'twoDsampleplot()' shows the plot of simulation joint density function.
#'
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
#'f <- function(x){
#'#' x1 = x[1]
#' x2 = x[2]
#' ifelse(0<x1 & x1<1 & 0<x2 & x2<1 & 0<x1+x2 & x1+x2<1, 24*x1*x2, 0)}
#'a <- twoDsample(f , N=1000, lbx = 0, ubx = 1, lby = 0, uby = 1)
#¡¯ggplot(a, aes(x, y)) +  geom_density_2d()






twoDsample <- function(f, N, lbx=-5000, ubx=5000, lby=-5000, uby=5000) {
  if (abs(adaptIntegrate(f, c(lbx, lby), c(ubx, uby), maxEval=10000)$integral - 1) > 0.001) {
    stop("Error: Bound is missing/wrong or the function is not a pdf. The area under the function you given should be 1")
  }
  if (lbx != -5000 & ubx != 5000 & lby != -5000 & uby != 5000){
    maxf <- max(replicate(100000,f(c(runif(1,lbx,ubx),runif(1,lby,uby)))))
    twos = c()
    n = 0
    while (n < N) {
      two <- c(runif(1,lbx,ubx),runif(1,lby,uby))
      if (runif(1, 0, maxf) < f(two)){
        twos = c(twos, two)
        n = n+1
      }
    }
    data.frame(x=twos[c(seq(1,length(twos)-1,2))],y=twos[c(seq(2,length(twos),2))])
  }
}














