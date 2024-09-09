#' Create Signal Blending Benchmark
#'
#' Creates signal-blending benchmark weights for signals based on their statistical significance
#'
#' @param signal_universe_m_d_ref A data frame containing the signal universe. Must include columns:
#'   - `id`
#'   - `tickers`
#'   - `dates`
#'   - `top_assets` (indicating which signals are statistically significant)
#'
#' @param signals_groups_m_d_ref An optional data frame with signal themes. Must include columns:
#'   - `tickers`
#'   - `theme` (the classification theme for each signal)
#'
#' @return A data frame with benchmark weights. Columns include:
#'   - `id`
#'   - `tickers`
#'   - `dates`
#'   - `individual_sb` (weight assigned to each signal)
#'   - `theme_sb` (weight assigned to each signal given their theme, if `signals_groups_m_d_ref` is provided)
#'
#' @details The function calculates weights for signals based on their statistical significance.
#'   If `signals_groups_m_d_ref` is provided, it also calculates theme-based weights and merges them
#'   back into the main data frame.
#'
#' @export
create_sb_benchmark <- function(signal_universe_m_d_ref, signals_groups_m_d_ref = NULL){

  #Create benchmark_weights_m_d_ref object for signals
  sb_benchmark_weights_m_d_ref <- signal_universe_m_d_ref %>% dplyr::filter(top_assets == 1) %>% dplyr::select(id, tickers, dates) #Initialize sb_benchmark_weights obj

  # Check if there are any significant signals
  if (nrow(sb_benchmark_weights_m_d_ref) == 0) {
    stop("No statistically significant signals to build sb_benchmark")
  }

  ##Individual weights
  sb_benchmark_weights_m_d_ref$individual_sb = 1/length(sb_benchmark_weights_m_d_ref$tickers)
  #Coerce to numeric
  sb_benchmark_weights_m_d_ref$individual_sb <- as.numeric(sb_benchmark_weights_m_d_ref$individual_sb)

  ##Theme weights
  if(!is.null(signals_groups_m_d_ref)){
    sb_benchmark_weights_m_d_ref <- dplyr::left_join(sb_benchmark_weights_m_d_ref, dplyr::select(signals_groups_m_d_ref, -id, -dates), by = "tickers") ##Merge signals and themes
    themes <- unique(sb_benchmark_weights_m_d_ref$theme) #get unique classifications
    num_themes <- length(themes) #get number of classifications

    ##Theme weights
    theme_weight <- 1/num_themes #weight to groups
    sb_benchmark_weights_m_d_ref$theme_sb <- NA #init col
    sb_benchmark_weights_m_d_ref$theme_sb <- ave(sb_benchmark_weights_m_d_ref$theme, sb_benchmark_weights_m_d_ref$theme, FUN = function(x) {
      # For each theme, assign the weight divided by the number of stocks in that classification
      num_stocks <- length(x)
      theme_weight / num_stocks
    })

    #Remove themes to adjust format
    sb_benchmark_weights_m_d_ref <- sb_benchmark_weights_m_d_ref %>% dplyr::select(id, tickers, dates, individual_sb, theme_sb)
    #Corce to numeric
    sb_benchmark_weights_m_d_ref$theme_sb <- as.numeric(sb_benchmark_weights_m_d_ref$theme_sb)
  }

  #Join back with non-significant signals
  sb_benchmark_weights_m_d_ref <- dplyr::left_join(dplyr::select(signal_universe_m_d_ref, id, tickers, dates), dplyr::select(sb_benchmark_weights_m_d_ref, -tickers, -dates), by = "id")

  #Replace NAs with zero
  sb_benchmark_weights_m_d_ref$individual_sb[which(is.na(sb_benchmark_weights_m_d_ref$individual_sb))] <- 0
  try(sb_benchmark_weights_m_d_ref$theme_sb[which(is.na(sb_benchmark_weights_m_d_ref$theme_sb))] <- 0, silent = TRUE)


  return(sb_benchmark_weights_m_d_ref)

}

