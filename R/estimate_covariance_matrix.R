#' Estimate a covariance matrix based on stock returns
#'
#' @param tickers A character vector with tickers to be used to estimate the covariance matrix
#' @param returns_m_xts_upd_ref A dataframe in which columns represent tickers present in current_universe_df and row represent days
#' It should include all stocks in assets and a dates column with at least cov_matrix_sample_size days before current_date
#' @param cov_matrix_sample_size Number of periods to subset returns_d_ref sample when estimating the covariance_matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @param cov_estimation_method One of SAM (Sample), EWMA, CC (Constant Correlation), PCA1, PCA2, Shrink_ID or Shrink_CC.
#' If NULL, blending_method can only be EW or IR
#' @param groups_m_d_ref A dataframe with id, tickers and dates with dummy group classifications to be used to fill NAs
#' @param active_returns A character string indicating whether covariance matrix should be calculated based on active returns or raw returns. If TRUE,
#' returns_m_xts_upd_ref will be adjusted by subtracting the selected market factor proxy in benchmark_returns_m_xts.
#' @param selected_benchmark_m_xts_upd_ref A dataframe in which columns represent benchmarks returns and row represent days
#' @param verbose If TRUE, will print messages to the console
#'
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
  if(!is.null(cov_matrix_sample_size) && n_dates < cov_matrix_sample_size){
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

  #Clean (just to be sure)
  ###############################
  returns_m_xts_sample_clean <- clean_returns_sample(returns_m_xts_sample = returns_m_xts_sample, #Returns
                                                     groups_m_d_ref = groups_m_d_ref, #Groups to fill NAs
                                                     verbose = verbose
  )
  ################################

  #Define active returns if the case
  ###############################
  if(active_returns){
    if(verbose) cat("\n  Calculating active returns.")
    #Get benchmark returns
    clean_dates_to_sample <- zoo::index(returns_m_xts_sample_clean)
    selected_benchmark_m_xts_sample <- selected_benchmark_m_xts_upd_ref[which(zoo::index(selected_benchmark_m_xts_upd_ref) %in% clean_dates_to_sample),]

    #Get decimals
    returns_m_xts_sample_clean_decimals <- returns_m_xts_sample_clean/100
    selected_benchmark_m_xts_sample_decimals <- as.vector(selected_benchmark_m_xts_sample/100)

    ##Get geometric active returns
    ###returns_m_xts_sample_clean_decimals
    returns_m_xts_sample_clean_decimals <- xts::xts(
      sapply(
        #For each series
        colnames(returns_m_xts_sample_clean_decimals), function(series) {
          #Apply geometric return difference formula
          purrr::map2_dbl(
            returns_m_xts_sample_clean_decimals[, series], #.x
            selected_benchmark_m_xts_sample_decimals, #.y
            ~ (1 + .x) / (1 + .y) - 1 #.f
          )
        }
      ),
      order.by = zoo::index(returns_m_xts_sample_clean_decimals)
    )

    #Turn back to percentages
    returns_m_xts_sample_clean <- returns_m_xts_sample_clean_decimals*100
    selected_benchmark_m_xts_sample <- selected_benchmark_m_xts_sample_decimals*100

  }
  ###############################

  #Set covariance estimator
  ################################
  covariance_estimator <- switch(cov_estimation_method,
                                 #Sample Estimator
                                 sample = function(R){
                                   out <- list(sigma = stats::cov(R))
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

                                   #cov with NA safety
                                   S <- stats::cov(R, use = "pairwise.complete.obs")
                                   eig <- base::eigen(S, symmetric = TRUE, only.values = TRUE)$values

                                   # Guard
                                   tot_var <- sum(eig)
                                   if (!is.finite(tot_var) || tot_var <= 0) stop("Covariance is singular / zero variance.")

                                   #Explained variance by eigenvalues
                                   prop <- eig/tot_var
                                   #Number of factors = number that explains 90% of total variance
                                   k90  <- which(cumsum(prop) >= 0.90)[1]

                                   # guard k
                                   k_max <- min(ncol(R), nrow(R) - 1)
                                   if (k_max < 1) stop("Not enough observations to estimate factors.")
                                   k_use <- max(1, min(k90, k_max))

                                   #Get covariance matrix
                                   out <- list(sigma = PortfolioAnalytics::extractCovariance(
                                     PortfolioAnalytics::statistical.factor.model(R, k = k_use)
                                   ))

                                   out
                                 },
                                 #PCA2
                                 pca2 = function(R){

                                   #Get k based on log ncol
                                   k_raw <- round(log(ncol(R)))
                                   k_max <- min(ncol(R), nrow(R) - 1)
                                   if (k_max < 1) stop("Not enough observations to estimate factors.")
                                   k_use <- max(1, min(k_raw, k_max))


                                   out <- list(sigma = PortfolioAnalytics::extractCovariance(
                                     PortfolioAnalytics::statistical.factor.model(R,
                                                                                  #Number of factors = round log ncol
                                                                                  k = k_use)))
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
    tictoc::toc()
  }

  #Return
  return(covariance_matrix)
}

