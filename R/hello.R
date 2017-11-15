#' what is the function does: Single variable Rejection Sampling
#'
#' This function implements single variabel rejection sampling for rvs with bounded support and which have have bounded pdf.
#'
#' The second paragraph will show up somewhere else and should be addition

#' @param f the pdf that we are sampling from
#' @param N the nimber of attempted samples.
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f

#' @return A vector containing samples from pdf
#'
#' @export
#'
#' @example
#'
#' f<- function(x) {ifelse(0 < x & x < 1, 2*x, 0)}
#' hist(oneDsample(f = f, N=1000000, lb = 0, ub = 1))
#'
#' f<-function(x) 2*(1-x)
#' hist(oneDsample(f, 100000, 0, 1))
#'
#' f = function(x) 1/2/pi *(sin(x) + 1)
#' hist(oneDsample(f, 100000, 0, 2*pi))
#'
#'
#'
oneDsample <- function(f, N, lb, ub) {
  if (abs(integrate(f,lb,ub)$val-1)>0.001){
    stop("Error: not a pdf.The area under the function you given should be 1")
  }
  else{
    ones <- runif(N, lb, ub)
    maxf<-max(ones)+1
    unis <- runif(N, 0, maxf)
    ones[unis < f(ones)]
  }
}



