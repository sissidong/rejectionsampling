

#' @param f the pdf that we are sampling from
#' @param N the nimber of attempted samples.
#' @param lb lower bound of support of f
#' @param ub upper bound of support of f

#' @param maxf bond of f
#' @param k if function is linear function then k is the slope
#'
#' @return A vector containing samples from pdf
#' @export
oneDsample <- function(f, N, lb, ub) {
  ones <- runif(N, lb, ub)
  #maxf<-c
  maxf<-max(ones)+1
  unis <- runif(N, 0, maxf)
  ones[unis < f(ones)]
}
