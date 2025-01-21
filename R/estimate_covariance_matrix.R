#' Estimate a covariance matrix based on stock returns
#'
#' @param assets A dataframe with a tickers column to specifiy to which stocks/signal portfolios covariance is to be calculated.
#' @param returns_m_xts_upd_ref A dataframe in which columns represent tickers present in current_universe_df and row represent days
#' It should include all stocks in assets and a dates column with at least cov_matrix_sample_size days before current_date
#' @param cov_matrix_sample_size Number of periods to subset returns_d_ref sample when estimating the covariance_matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @param cov_estimation_method One of SAM (Sample), EWMA, CC (Constant Correlation), PCA1, PCA2, Shrink_ID or Shrink_CC.
#' If NULL, blending_method can only be EW or IR
#' @param fill If TRUE, will fill rows NAs with group medians. If group median are NAs, it will fill with row's median
#' @param groups_m_d_ref A dataframe with id, tickers and dates with dummy group classifications to be used to fill NAs
#' @param active_returns A character string indicating whether covariance matrix should be calculated based on active returns or raw returns. If TRUE,
#' returns_m_xts_upd_ref will be adjusted by subtracting the selected market factor proxy in benchmark_returns_m_xts.
#'
#' @return
#' @export
#'
#' @examples
estimate_covariance_matrix <- function(tickers, returns_m_xts_upd_ref,
                                       cov_matrix_sample_size, cov_estimation_method,
                                       groups_m_d_ref = NULL,
                                       active_returns, selected_benchmark_m_xts_upd_ref,
                                       verbose = TRUE){

  #Checks
  if(!all(tickers %in% colnames(returns_m_xts_upd_ref))){
    stop("Tickers without correspondence in returns_m_xts_upd_ref")
  }

  if(!cov_estimation_method %in% c("sample", "ewma", "cc", "pca1", "pca2", "shrink_id", "shrink_cc")){
    stop("cov_estimation_method not supported")
  }

  #Message
  if(verbose){
    tictoc::tic()
    cat("\n")
    cat("Starting covariance estimation.")
  }
  #Generate return sample
  ###############################

  #Get dates sequence and tickers to create sample
  returns_m_xts_upd_ref_dates <- zoo::index(returns_m_xts_upd_ref)
  n_dates <- length(returns_m_xts_upd_ref_dates)

    ##check
    if(n_dates < cov_matrix_sample_size){
      stop("Not enough dates to estimate covariance matrix")
    }

  if(is.null(cov_matrix_sample_size)){
    dates_to_sample <- returns_m_xts_upd_ref_dates #In case of cov_matrix_sample_size = NULL, use whole period
  } else {
    dates_to_sample <- returns_m_xts_upd_ref_dates[(n_dates - cov_matrix_sample_size):n_dates]
  }

  #Get all rows that comprehend current_date - cov_matrix_sample_size
  returns_m_xts_sample <- returns_m_xts_upd_ref[which(returns_m_xts_upd_ref_dates %in% dates_to_sample), #Get all dates in dates_to_sample
                                                tickers] #Get all tickers


  ###############################

  #Define active returns if the case
  ###############################
  if(active_returns){
    if(verbose) cat("\n  Calculating active returns.")
    #Get benchmark returns
    selected_benchmark_m_xts_sample <- selected_benchmark_m_xts_upd_ref[which(zoo::index(selected_benchmark_m_xts_upd_ref) %in% dates_to_sample),]

    #Get decimals
    returns_m_xts_sample_decimals <- returns_m_xts_sample/100
    selected_benchmark_m_xts_sample_decimals <- as.vector(selected_benchmark_m_xts_sample/100)

    ##Get geometric active returns
    ###returns_m_xts_sample_decimals
    returns_m_xts_sample_decimals <- xts::xts(
      sapply(
        #For each series
        colnames(returns_m_xts_sample_decimals), function(series) {
          #Apply geometric return difference formula
          purrr::map2_dbl(
            returns_m_xts_sample_decimals[, series], #.x
            selected_benchmark_m_xts_sample_decimals, #.y
            ~ (1 + .x) / (1 + .y) - 1 #.f
          )
        }
      ),
      order.by = zoo::index(returns_m_xts_sample_decimals)
    )

    #Turn back to percentages
    returns_m_xts_sample <- returns_m_xts_sample_decimals*100
    selected_benchmark_m_xts_sample <- selected_benchmark_m_xts_sample_decimals*100

  }
  ###############################


  #Clean (just to be sure)
  ###############################
  returns_m_xts_sample_clean <- clean_returns_sample(returns_m_xts_sample = returns_m_xts_sample, #Returns
                                                      groups_m_d_ref = groups_m_d_ref, #Groups to fill NAs
                                                      verbose = verbose
  )
  ################################

  #Set covariance estimator
  ################################
  covariance_estimator <- switch(cov_estimation_method,
                                 #Sample Estimator
                                 sample = function(R){
                                   out <- list(sigma = cov(R))
                                   out
                                 },
                                 #EWMA Estimator
                                 ewma = function(R){
                                   out <- list(sigma = PerformanceAnalytics::M2.ewma(R))
                                   out
                                 },
                                 #Constant Correlation
                                 cc = function(R){
                                   out <- list(sigma = PerformanceAnalytics::M2.struct(R, "CC"))
                                   out
                                 },
                                 #PCA1
                                 pca1 = function(R){
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
                                 pca2 = function(R){
                                   out <- list(sigma = PortfolioAnalytics::extractCovariance(
                                     PortfolioAnalytics::statistical.factor.model(R,
                                                                                  #Number of factors = number that explains 66% of total variance
                                                                                  k = round(log(ncol(R))))))
                                   out
                                 },
                                 #Shrink ID
                                 shrink_id = function(R){
                                   out <- list(sigma = PerformanceAnalytics::M2.shrink(R, target = 2)$M2sh)
                                   out
                                 },
                                 #Shrink CC
                                 shrink_cc = function(R){
                                   out <- list(sigma = PerformanceAnalytics::M2.shrink(R, target = 4)$M2sh)
                                   out
                                 }

  )

  ################################

  #Estimate covariance matrix
  ################################
  covariance_matrix <- covariance_estimator(returns_m_xts_sample_clean)$sigma
  colnames(covariance_matrix) <- colnames(returns_m_xts_sample_clean)
  rownames(covariance_matrix) <- colnames(returns_m_xts_sample_clean)
  ################################

  #Message
  if(verbose){
    cat("\n")
    cat(crayon::green(paste("Covariance matrix estimated using", cov_estimation_method, "method.")))
    cat("\n")
    elapsed_time <- tictoc::toc()
  }

  #Return
  return(covariance_matrix)
}

