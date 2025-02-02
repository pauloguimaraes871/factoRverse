#' Create Signal Engineering Benchmarks
#'
#' Creates Signal-Selection and Signal-Blending benchmark weights:
#' \itemize{
#'  \item{Signal-Selection Benchmark}: A benchmark that is built using the universe of all signals in `chosen_signals`.
#'   It is used to evaluate the performance of the signal selection process.
#'  \item{Signal-Blending Benchmark}: A benchmark that is built using only signals derived from signal selection process (those with pre_eligible_assets assigned as 1).
#'  It is used to evaluate the performance of the signal blending process.
#'  }
#'
#' @param signal_universe_m_d_ref A data frame containing the signal universe. Must include columns:
#'   - `id`
#'   - `tickers`
#'   - `dates`
#'   - `pre_eligible_assets` (indicating which signals are statistically significant)
#'
#' @param selected_signal_themes_m_d_ref An optional data frame with signal themes. Must include columns:
#'   - `tickers`
#'   - `theme` (the classification theme for each signal)
#' This parameter is mandatory if one wants to calculate theme-weighted benchmarks.
#'
#' @return A list with two data frames, each with benchmark weights following Signal-Selection and Signal-Blending methods. The columns of each data.frame include:
#'   - `id`
#'   - `tickers`
#'   - `dates`
#'   - `individual` (weight assigned to each signal)
#'   - `theme` (weight assigned to each signal given their theme, if `selected_signal_themes_m_d_ref` is provided)
#'
#' @details The function calculates weights for signals based on their statistical significance.
#'   If `selected_signal_themes_m_d_ref` is provided, it also calculates theme-based weights and merges them
#'   back into the main data frame.
#'
#' @export
create_se_benchmarks <- function(signal_universe_m_d_ref, selected_signal_themes_m_d_ref){

  #Create benchmark_weights_m_d_ref object for signals
  sb_benchmark_weights_m_d_ref <- signal_universe_m_d_ref %>% dplyr::filter(pre_eligible_assets == 1) %>% dplyr::select(id, tickers, dates) #Initialize sb_benchmark_weights obj
  ss_benchmark_weights_m_d_ref <- signal_universe_m_d_ref %>% dplyr::select(id, tickers, dates) #Initialize ss_benchmark_weights obj

  # Check if there are any significant signals
  if (nrow(sb_benchmark_weights_m_d_ref) == 0) {
    stop("No statistically significant signals to build se_benchmarks")
  }

  #Check if any tickers does not have a theme
  if (any(!signal_universe_m_d_ref$tickers %in% selected_signal_themes_m_d_ref$tickers)) {
    stop("Some tickers do not have a theme assigned")
  }

  #Define set se_benchmarks_weights function
  set_se_benchmark_weights <- function(benchmark_weights_m_d_ref){

   ##Theme weights
      benchmark_weights_m_d_ref <- dplyr::left_join(benchmark_weights_m_d_ref,
                                                    dplyr::select(selected_signal_themes_m_d_ref, -id, -dates), by = "tickers") ##Merge signals and themes
      themes <- unique(benchmark_weights_m_d_ref$theme) #get unique classifications
      num_themes <- length(themes) #get number of classifications

      ##Theme weights
      theme_weight <- 1/num_themes #weight to groups
      benchmark_weights_m_d_ref$theme <- ave(benchmark_weights_m_d_ref$theme, benchmark_weights_m_d_ref$theme, FUN = function(x) {
        # For each theme, assign the weight divided by the number of stocks in that classification
        num_stocks <- length(x)
        theme_weight / num_stocks
      })

      #Remove themes to adjust format
      benchmark_weights_m_d_ref <- benchmark_weights_m_d_ref %>% dplyr::select(id, tickers, dates, theme)
      #Corce to numeric
      benchmark_weights_m_d_ref$theme <- as.numeric(benchmark_weights_m_d_ref$theme)


    #Join back with non-significant signals
    benchmark_weights_m_d_ref <- dplyr::left_join(dplyr::select(signal_universe_m_d_ref, id, tickers, dates),
                                                  dplyr::select(benchmark_weights_m_d_ref, -tickers, -dates), by = "id")

    #Replace NAs with zero
    benchmark_weights_m_d_ref$theme[which(is.na(benchmark_weights_m_d_ref$theme))] <- 0

    return(benchmark_weights_m_d_ref)
  }

  #Create ss and sb benchmarks
  ss_and_sb_benchmark_weights_m_d_ref_list <- purrr::map(list(sb_benchmark_weights_m_d_ref, ss_benchmark_weights_m_d_ref), set_se_benchmark_weights)
  names(ss_and_sb_benchmark_weights_m_d_ref_list) <- c("sb_benchmark_weights_m_d_ref", "ss_benchmark_weights_m_d_ref")

  #Change colnames
  colnames(ss_and_sb_benchmark_weights_m_d_ref_list$sb_benchmark_weights_m_d_ref) <- c("id", "tickers", "dates", "theme_sb")
  colnames(ss_and_sb_benchmark_weights_m_d_ref_list$ss_benchmark_weights_m_d_ref) <- c("id", "tickers", "dates", "theme_ss")
  #Join into a consolidated
  se_benchmark_weights_m_d_ref <- dplyr::left_join(ss_and_sb_benchmark_weights_m_d_ref_list$ss_benchmark_weights_m_d_ref,
                                                   dplyr::select(ss_and_sb_benchmark_weights_m_d_ref_list$sb_benchmark_weights_m_d_ref, id, theme_sb), by = c("id"))

  #Return
  return(se_benchmark_weights_m_d_ref)

}

