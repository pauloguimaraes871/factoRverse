#' Generate a sample of returns to estimate covariance matrix
#'
#' @param assets A dataframe with tickers column
#' @param returns_d_ref A dataframe in which columns represent tickers present in current_universe_df and row represent days
#' It should include all stocks in assets and a dates column with at least covariance_matrix_sample_size days before current_date
#' @param covariance_matrix_sample_size Number of periods to subset returns_d_ref sample when estimating the covariance_matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to
#' @param fill If TRUE, will fill rows NAs with groups medians. If groups_median are NAs, it will fill with row's median
#' @param groups_m_d_ref A dataframe with id, tickers and dates with group classifications to be used to fill NAs
#'
#' @return
#' @export
#'
#' @examples
clean_returns_sample <- function(returns_xts_sample, groups_m_d_ref = NULL, fill = TRUE, fill_by = NULL, verbose = TRUE){

  #Remove holidays
  returns_xts_clean <- returns_xts_sample %>% as.data.frame() %>% dplyr::filter(rowSums(is.na(.)) != ncol(returns_xts_sample))  #Rows with only NAs

  ##################n

  #Fill according to groups
  ##########################
  #Check if filling is necessary
  if(any(apply(returns_xts_clean, 2, function(x) any(is.na(x)))) && fill){

    #Get what will be used to fill
    if(!is.null(fill_by)){
      group <- fill_by
    } else {
      group <- colnames(groups_m_d_ref)[length(colnames(groups_m_d_ref))] #Last name of groups_m_d_ref will be used to fill
    }

    #message
    if(verbose){
      cat("\n")
      cat(paste0("Using ", group, " information to fill NAs in returns sample"))
    }

    #Fill NAs (by row) with groups medians or with rows-median
    for(i in 1:nrow(returns_xts_clean)){
      #Merge tickers with daily returns and groups
      tickers <- colnames(returns_xts_clean)

      tickers_and_returns <- merge(
        data.frame(tickers = tickers, period_return = as.numeric(returns_xts_clean[i, ])),
        groups_m_d_ref, by = "tickers")

      #Group Medians
      groups_medians <- tickers_and_returns %>%
        dplyr::group_by(!!rlang::sym(group)) %>% dplyr::summarise(group_median_period_return = median(period_return, na.rm= TRUE)) #calculate median by groups

      #Merge eveything
      tickers_and_returns_and_groups <- dplyr::left_join(tickers_and_returns, groups_medians, by = group)

      #Re-order
      tickers_and_returns_and_groups <- tickers_and_returns_and_groups[order(tickers_and_returns_and_groups$tickers), c("tickers", group, "period_return", "group_median_period_return")]

      #Fill NAs with groups medians
      tickers_and_returns_and_groups_filled <- tickers_and_returns_and_groups %>%
        dplyr::mutate(period_return = dplyr::coalesce(period_return, group_median_period_return)) #replace with group_median_period_return

      #If there are remaining NAs, use series median
      tickers_and_returns_and_groups_filled[which(is.na(tickers_and_returns_and_groups_filled$period_return)),"period_return"] <-
        median(tickers_and_returns_and_groups_filled$period_return, na.rm = TRUE)

      #Place return vector back into returns sample clean
      returns_xts_clean[i, ] <- tickers_and_returns_and_groups_filled$period_return
    }
    ###################

  }
  #Return
  return(returns_xts_clean)

}
