#' Sets portfolio custom moments
#'
#' @param R asset returns as in PortfoloAnalytics
#' @param portfolio a portfolio object as in PortfolioAnalytics
#' @param user_moments a list of four matices containing user-specified values for the first four return moments
#' @param user_method user-specified method for computing moment matrices. Defaults to calculation used by PortfolioAnalytics "sample" method,
#' which uses PerformanceAnalytics functions to computer the higher-order moments
#'
#'
set_custom_moments = function(R, portfolio, user_moments=NULL, user_method=c(returns, input, two_moment)) {

  if(!methods::hasArg(user_method) | is.null(user_method)) user_method <- "returns" #If user_method not set, assume returns
  #Pass R to tmpR
  tmpR <- R
  switch(user_method,
         #If user_method is returns
         returns = {
           momentargs <- list()
           momentargs$mu  <- matrix(as.vector(apply(tmpR,2, "mean")), ncol = 1) #Sets mu as col means
           momentargs$sigma  <- cov(tmpR)
           momentargs$m3  <-  PerformanceAnalytics:::M3.MM(tmpR)
           momentargs$m4  <-  PerformanceAnalytics:::M4.MM(tmpR)
         },
         #If user_method is inputs (correct way to do it)
         input = {
           momentargs <- user_moments #Pass user_moments
         },
         #If user_method is two_moment (correct way to do it)
         two_moment = {
           momentargs <- list()
           momentargs$mu <- matrix(as.vector(apply(tmpR,2, "mean")), ncol = 1)
           momentargs$sigma <- cov(tmpR)
           momentargs$m3 <- matrix(0, nrow=ncol(R), ncol=ncol(R)^2)
           momentargs$m4 <- matrix(0, nrow=ncol(R), ncol=ncol(R)^3)
         } )

  return(momentargs)
}
