#' Estimate a covariance matrix based on stock returns
#'
#' @param assets A dataframe with a tickers column to specifiy to which stocks/signal portfolios covariance is to be calculated.
#' @param returns_d_ref A dataframe in which columns represent tickers present in current_universe_df and row represent days
#' It should include all stocks in assets and a dates column with at least covariance_matrix_sample_size days before current_date
#' @param covariance_matrix_sample_size Number of periods to subset returns_d_ref sample when estimating the covariance_matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @param covariance_estimation_method One of SAM (Sample), EWMA, CC (Constant Correlation), PCA1, PCA2, Shrink_ID or Shrink_CC.
#' If NULL, blending_method can only be EW or IR
#' @param fill If TRUE, will fill rows NAs with group medians. If group median are NAs, it will fill with row's median
#' @param groups_m_d_ref A dataframe with id, tickers and dates with dummy group classifications to be used to fill NAs
#'
#' @return
#' @export
#'
#' @examples
estimate_covariance_matrix <- function(tickers, returns_upd_ref,
                                       covariance_matrix_sample_size, covariance_estimation_method,
                                       groups_m_d_ref = NULL, verbose = TRUE){

  #Checks
  if(!all(tickers %in% colnames(returns_upd_ref))){
    stop("Tickers without correspondence in returns_upd_ref")
  }

  if(!covariance_estimation_method %in% c("SAM", "EWMA", "CC", "PCA1", "PCA2", "Shrink_ID", "Shrink_CC")){
    stop("covariance_estimation_method not supported")
  }

  #Generate return sample
  ###############################

  #Get dates sequence and tickers to create sample
  n_dates <- length(returns_upd_ref$dates)
  if(is.null(covariance_matrix_sample_size)){
    dates_to_sample <- returns_upd_ref$dates #In case of covariance_matrix_sample_size = NULL, use whole period
  } else {
    dates_to_sample <- returns_upd_ref$dates[(n_dates - covariance_matrix_sample_size):n_dates]
  }

  #Get all rows that comprehend current_date - covariance_matrix_sample_size
  returns_sample <- returns_upd_ref[which(returns_upd_ref$dates %in% dates_to_sample), #Get all dates in dates_to_sample
                                    c("dates", tickers)] #Get all tickers

  #Clean (just to be sure)
  returns_sample_clean <- clean_returns_sample(returns_sample = returns_sample, #Returns
                                               groups_m_d_ref = groups_m_d_ref, #Groups to fill NAs
                                               verbose = verbose
  )

  #Turn in XTS
  returns_sample_xts <- xts::xts(returns_sample_clean[,-1], order.by =  returns_sample_clean$dates)
  ################################

  #Set covariance estimator
  covariance_estimator <- switch(covariance_estimation_method,
                                 #Sample Estimator
                                 SAM = function(R){
                                   out <- list(sigma = cov(R))
                                   out
                                 },
                                 #EWMA Estimator
                                 EWMA = function(R){
                                   out <- list(sigma = PerformanceAnalytics::M2.ewma(R))
                                   out
                                 },
                                 #Constant Correlation
                                 CC = function(R){
                                   out <- list(sigma = PerformanceAnalytics::M2.struct(R, "CC"))
                                   out
                                 },
                                 #PCA1
                                 PCA1 = function(R){
                                   out <- list(sigma = PortfolioAnalytics::extractCovariance(
                                     PortfolioAnalytics::statistical.factor.model(R,
                                                                                  #Number of factors = number that explains 90% of total variance
                                                                                  k = which(
                                                                                    #Get the cumulative proportion of variance explanation
                                                                                    cumsum(stats::prcomp(cov(R))$sdev/sum(stats::prcomp(cov(R))$sdev))
                                                                                                      #Which number equates 90% explained
                                                                                                      >= 0.90)[1])))
                                        out
                                 },
                                 #PCA2
                                 PCA2 = function(R){
                                        out <- list(sigma = PortfolioAnalytics::extractCovariance(
                                                    PortfolioAnalytics::statistical.factor.model(R,
                                                                                                 #Number of factors = number that explains 66% of total variance
                                                                                                 k = round(log(ncol(R))))))
                                        out
                                 },
                                 #Shrink ID
                                 Shrink_ID = function(R){
                                             out <- list(sigma = PerformanceAnalytics::M2.shrink(R, target = 2)$M2sh)
                                             out
                                 },
                                 #Shrink CC
                                 Shrink_CC = function(R){
                                             out <- list(sigma = PerformanceAnalytics::M2.shrink(R, target = 4)$M2sh)
                                             out
                                 }

  )

  #Estimate covariance matrix
  covariance_matrix <- covariance_estimator(returns_sample_xts)$sigma
  colnames(covariance_matrix) <- colnames(returns_sample_xts)

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Covariance matrix estimated using", covariance_estimation_method, "method.")))
  }

  #Return
  return(covariance_matrix)
}
