#' Un-dummy sector features
#'
#' This functions takes dummy sectors columns and melt them into the single original column that created the dummies
#'
#' @param sectors_m_df A dataframe with id, tickers and dates with dummy sectors classifications to be used to fill NAs
#'
undummy_sector_features <- function(sectors_m_df){
  ##Get original sector_vector
  ###Define sector names (excluding the id, tickers and dates column)
  sectors <- colnames(sectors_m_df)[-(1:3)]

  ##Transform dummies back to original vector with sector names
  sectors_df <- sectors_m_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sectors = sectors[which.max(dplyr::c_across(-(1:3)))]) %>%
    dplyr::select(tickers, sectors) %>% as.data.frame()

  #Return
  return(sectors_df)

}
