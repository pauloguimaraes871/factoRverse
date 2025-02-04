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
clean_returns_sample <- function(returns_m_xts_sample, groups_m_d_ref = NULL, fill = TRUE, fill_by = NULL, verbose = TRUE){

  #Remove holidays
  ##################
  returns_df_clean <- returns_m_xts_sample %>% as.data.frame() %>% dplyr::filter(rowSums(is.na(.)) != ncol(returns_m_xts_sample))  #Rows with only NAs
  filtered_index <- zoo::index(returns_m_xts_sample)[rowSums(is.na(as.data.frame(returns_m_xts_sample))) != ncol(returns_m_xts_sample)] #Dates with at least one non-NA
  ##################

  #Fill according to groups
  ##########################
  #Check if filling is necessary
  if(any(apply(returns_df_clean, 2, function(x) any(is.na(x)))) && fill){

    ##Get what will be used to fill
    if(!is.null(fill_by)){
      group <- fill_by
    } else {
      group <- colnames(groups_m_d_ref)[length(colnames(groups_m_d_ref))] #Last name of groups_m_d_ref will be used to fill
    }

    ##message
    if(verbose){
      cat("\n")
      cat(paste0("Using ", group, " information to fill NAs in returns sample"))
    }

    ##Add a row identifier as a new column
    returns_df_clean$row_id <- seq_len(nrow(returns_df_clean))

    ##Pivot to long format, in which each row represent one ticker's return for a period
    returns_df_clean_long <- tidyr::pivot_longer(
      returns_df_clean,
      cols = -row_id,
      names_to = "tickers",
      values_to = "period_return"
    ) %>% as.data.frame()

    ##Merge group information from groups_m_d_ref
    returns_df_clean_long <- dplyr::left_join(returns_df_clean_long, groups_m_d_ref, by = "tickers")

    ##Compute group medians by period
    returns_df_clean_long <- returns_df_clean_long %>%
      dplyr::group_by(row_id, !!rlang::sym(group)) %>%
      dplyr::mutate(group_median_period_return = median(period_return, na.rm= TRUE)) %>% #calculate median by groups
      dplyr::ungroup() %>%
      as.data.frame()

    ##Replace NAs with group medians
    returns_df_clean_long$period_return <- dplyr::coalesce(returns_df_clean_long$period_return, returns_df_clean_long$group_median_period_return)
    ##########################

  #For any remaining NA, replace with overall median
  ##########################
    ##Calcute overall median and replace
    returns_df_clean_long <- returns_df_clean_long %>%
      dplyr::group_by(row_id) %>%
      dplyr::mutate(period_return = ifelse(is.na(period_return),
                                           median(period_return, na.rm = TRUE),
                                           period_return)) %>%
      dplyr::ungroup() %>%
      as.data.frame() %>%
      dplyr::select(tickers, period_return, row_id)

    ##Pivot back to wide
    returns_df_clean <- tidyr::pivot_wider(returns_df_clean_long, names_from = tickers, values_from = period_return) %>% as.data.frame()

    ##Order by row_id and remove the row_id column to restore the original structure.
    returns_df_clean <- returns_df_clean[order(returns_df_clean$row_id), ]
    returns_df_clean <- returns_df_clean %>% dplyr::select(-row_id)


  ###################

  }
  #Return a xts object
  returns_m_xts_clean <- xts::xts(returns_df_clean, order.by = filtered_index)
  return(returns_m_xts_clean)

}
