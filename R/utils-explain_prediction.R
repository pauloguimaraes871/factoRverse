#' Explain Prediction
#'
#' @description
#' The `explain_prediction` function decomposes the out‐of‐sample (OOS) prediction for a selected ticker and date using
#' the OLS Global Signal Model (GSM) backtest results. It supports both standard backtest results and meta-learner backtest results.
#' If a meta-learner backtest object is passed, the function decomposes base learners' contributions before computing the final breakdown.
#'
#' @param sb_backtest_results An S4 object containing the backtest results, either of class `sb_backtest_results` or `sb_metabacktest_results`.
#' @param features_m_df An S4 object containing the features metadata used in the backtest. It must match the one stored in `sb_backtest_results`.
#' @param selected_ticker A character string representing the ticker (e.g., `"AAPL"`) for which to explain the prediction.
#' @param selected_date A Date object representing the prediction date.
#'
#' @details
#' The function operates in several stages:
#'
#' \strong{Initial Checks:}
#'   \itemize{
#'     \item Ensures that the provided backtest object is valid.
#'     \item Ensures that the `features_m_df` matches the backtest object's expected feature metadata.
#'     \item Ensures the selected ticker and date exist in the data.
#'   }
#'
#' \strong{Feature Importance and Partial Contributions:}
#'   \itemize{
#'     \item Extracts the most recent feature importance data up to the `selected_date`.
#'     \item If a meta-learner backtest is used, decomposes base learners' contributions before processing further.
#'     \item Calculates the partial contribution for each feature as the product of its importance and current value.
#'   }
#'
#' \strong{Plot Generation:}
#'   \itemize{
#'     \item Generates a waterfall plot using `ggplot2`, showing how each feature (including the intercept) contributes to the prediction.
#'   }
#'
#' @return A `ggplot2` waterfall plot displaying the decomposition of the OOS prediction.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   explain_prediction(sb_backtest_results, features_m_df, "AAPL", as.Date("2023-07-15"))
#' }
#'
#' @export
setGeneric("explain_prediction", function(sb_backtest_results, features_m_df, selected_ticker, selected_date) {
  standardGeneric("explain_prediction")
})


#' Explain Prediction for sb_backtest_results
#' @export
setMethod("explain_prediction",
          signature(sb_backtest_results = "sb_backtest_results", features_m_df = "meta_dataframe", selected_ticker = "character", selected_date = "Date"),
          function(sb_backtest_results, features_m_df, selected_ticker, selected_date) {


            # Extract objects
            ##################
            sb_backtest_workflow <- sb_backtest_results@sb_backtest_workflow[[length(sb_backtest_results@sb_backtest_workflow)]]
            oos_sb_outputs_m_df <- sb_backtest_results@oos_sb_outputs_m_df@data
            feature_importance_m_df <- sb_backtest_results@feature_importance_m_df@data
            gsm_algorithm <- sb_backtest_workflow$gsm_algorithm
            features_obj_name <- features_m_df@meta_dataframe_name
            features_m_df <- features_m_df@data

            #Initial Checks
            ##############
            ##Check if features_m_df is the one used in the backtest
            if(sb_backtest_workflow$features_object_name != features_obj_name){
              stop("The features_m_df object used in the backtest is different from the one provided. Please provide the correct features_m_df object.")
            }
            if(any(!sb_backtest_workflow$features %in% colnames(features_m_df))){
              stop("The features_m_df object provided does not contain the features used in backtest. Please provide the correct features_m_df object.")
            }

            # Call the existing function
            explain_prediction_inner(sb_backtest_workflow = sb_backtest_workflow, oos_sb_outputs_m_df = oos_sb_outputs_m_df,
                                     feature_importance_m_df = feature_importance_m_df, gsm_algorithm = gsm_algorithm,
                                     features_m_df = features_m_df, selected_ticker = selected_ticker, selected_date = selected_date)
          }
)


#' Explain Prediction for sb_metabacktest_results
#' @export
setMethod("explain_prediction",
          signature(sb_backtest_results = "sb_metabacktest_results", features_m_df = "meta_dataframe", selected_ticker = "character", selected_date = "Date"),
          function(sb_backtest_results, features_m_df, selected_ticker, selected_date) {


            ##Get Objects
            ##############
            sb_backtest_workflow <- sb_backtest_results@meta_sb_backtest_results@sb_backtest_workflow
            oos_sb_outputs_m_df <- sb_backtest_results@meta_sb_backtest_results@oos_sb_outputs_m_df@data
            gsm_algorithm <- sb_backtest_workflow$gsm_algorithm
            features_obj_name <- features_m_df@meta_dataframe_name
            features_m_df <- features_m_df@data
            ##Extract identifiers
            base_identifiers <- sapply(sb_backtest_results@base_sb_backtest_results_list, function(x) x@backtest_identifier)
            base_features <- sapply(sb_backtest_results@base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$features) %>% unique()


            #Initial Checks
            ##############
            base_learners_features_object_name <- sapply(sb_backtest_results@base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$features_object_name)
            ##Check if features_m_df is the one used in the base backtest
            if(any(base_learners_features_object_name != features_obj_name)){
              stop("The features_m_df object used in base backtests is different from the one provided. Please provide the correct features_m_df object.")
            }
            if(any(!setdiff(sb_backtest_results@meta_sb_backtest_results@sb_backtest_workflow$features, base_identifiers) %in% colnames(features_m_df))){
              stop("The features_m_df object provided does not contain the features used in meta-learner backtest. Please provide the correct features_m_df object.")
            }
            if(any(!base_features %in% colnames(features_m_df))){
              stop("The features_m_df object provided does not contain the features used in base-learner backtests. Please provide the correct features_m_df object.")
            }


            # Extract meta learner feature importance
            meta_feature_importance <- sb_backtest_results@meta_sb_backtest_results@feature_importance_m_df@data

            # Extract list of base learners feature importance
            base_feature_importance_list <- lapply(sb_backtest_results@base_sb_backtest_results_list,
                                                   function(x) x@feature_importance_m_df@data)



            # Step 1: Get most recent date for meta learner feature importance ≤ selected_date
            most_recent_meta_date <- meta_feature_importance %>%
              dplyr::filter(dates <= selected_date) %>%
              dplyr::pull(dates) %>%
              unique() %>%
              max()

            # Filter meta feature importance to that date
            meta_feature_importance <- meta_feature_importance %>%
              dplyr::filter(dates == most_recent_meta_date)

            # Step 2: Get most recent dates for base learners, ensuring they precede the meta learner
            base_feature_importance_filtered <- lapply(base_feature_importance_list, function(df) {
              most_recent_base_date <- df %>%
                dplyr::filter(dates < most_recent_meta_date) %>%
                dplyr::pull(dates) %>%
                unique() %>%
                max()

              df %>% dplyr::filter(dates == most_recent_base_date)
            })

            # Step 3: Decompose feature importance of base learners in meta learner
            # Iterate through meta_feature_importance rows, replacing base learner rows
            meta_feature_importance_decomposed_consolidated_m_df <- decompose_feature_importance(
              most_recent_meta_date = most_recent_meta_date,
              meta_feature_importance = meta_feature_importance,
              base_identifiers = base_identifiers,
              base_feature_importance_filtered = base_feature_importance_filtered
            )

            # Step 4: Call the existing function
            explain_prediction_inner(sb_backtest_workflow = sb_backtest_workflow, oos_sb_outputs_m_df = oos_sb_outputs_m_df,
                                     feature_importance_m_df = meta_feature_importance_decomposed_consolidated_m_df, gsm_algorithm = gsm_algorithm,
                                     features_m_df = features_m_df, selected_ticker = selected_ticker, selected_date = selected_date)


          }
)

#' inner function
explain_prediction_inner <- function(sb_backtest_workflow, oos_sb_outputs_m_df, feature_importance_m_df, gsm_algorithm,
                                     features_m_df, selected_ticker = selected_ticker, selected_date = selected_date){

  #Initial Checks
  ##############
  ##Check for gsm
  if(gsm_algorithm != "ols"){
    stop("Currently, this function is only available for OLS GSM algorithm.")
  }
  ##Check if id is avaiable
  selected_id <- paste0(selected_ticker,"-",selected_date)
  if(!(selected_id %in% oos_sb_outputs_m_df$id)){
    stop("Selected ticker and date combination is not available in the backtest results.
               Please select another ticker and date combination.")
  }
  if(!(selected_id %in% features_m_df$id)){
    stop("Selected id is not available in the features_m_df object provided. Please provide the correct features_m_df.")
  }

  ##Check if first_rebalancing_date occurs before selected_date
  ###Get first rebalancing date
  first_rebalancing_date <- sb_backtest_workflow$rebalance_dates %>% min()
  if(selected_date < first_rebalancing_date){
    stop("Selected date is before the first rebalancing date. Please select a date after the first rebalancing date.")
  }

  ##############

  #Get feature importance info
  ############################
  ##Get most recent feature importance info
  most_recent_feature_imp_date <- feature_importance_m_df %>%
    dplyr::filter(dates <= selected_date) %>% dplyr::pull(dates) %>% unique() %>% max()

  ##Get feature importance corresponding to most recent rebalance_date
  feature_importance <- feature_importance_m_df %>%
    dplyr::filter(dates == most_recent_feature_imp_date) %>% #Get most recent feature importance
    dplyr::select(tickers, importance) #For OLS, importance is raw coef (has direction). For tree, it is impurity (always positive)

  ##Get intercept and features
  intercept_importance <- feature_importance %>% dplyr::filter(tickers == "(Intercept)")
  features_importance <- feature_importance %>% dplyr::filter(tickers != "(Intercept)")

  ####################

  #Get features
  #####################
  ##Reconstruct chosen_signals_and_positions
  chosen_signals_and_positions <- ifelse(stringr::str_detect(features_importance %>% dplyr::pull(tickers), pattern = "low_"), "short", "long")
  names(chosen_signals_and_positions) <- stringr::str_remove_all(features_importance %>% dplyr::pull(tickers), pattern = "low_")

  ##Select and correct signals
  selected_and_corrected_features_m_df <- select_and_correct_signals(features_m_df, chosen_signals_and_positions)$selected_signals_corrected_positions_m_df

  ##Join features (corrected) with importance
  selected_features <- selected_and_corrected_features_m_df %>%
    dplyr::filter(id == selected_id) %>% dplyr::select(-id, -tickers, -dates) %>%  #Select id and features (corrected)
    tidyr::pivot_longer(dplyr::everything(), names_to = "feature", values_to = "feature_value") %>% #Transform to long format
    as.data.frame() %>% #Convert to data.frame
    dplyr::left_join(features_importance, by = c("feature" = "tickers"))

  #####################

  #Get partial contribution and prediction
  #####################
  ##Get partial contribution
  partial_contribution <- selected_features %>%
    dplyr::mutate(partial_contribution = importance * feature_value) %>% #Calculate feature importance times feature value
    dplyr::arrange(desc(partial_contribution))

  ##Most important partial contributions
  ###Positive
  n_positive_partial_contributions <- partial_contribution %>% dplyr::filter(partial_contribution > 0) %>% nrow() #Get n of positive
  ####Most important positive partial contributions
  most_important_positive_partial_contributions <- partial_contribution %>%
    dplyr::slice_head(n = min(5, n_positive_partial_contributions)) %>% dplyr::arrange(desc(partial_contribution)) #Extract n_positive contrib or 10
  most_important_positive_partial_contributions_sum <- sum(most_important_positive_partial_contributions %>% dplyr::pull(partial_contribution))
  ####Less important positive partial contributions
  less_important_positive_partial_contributions <- partial_contribution %>% dplyr::filter(partial_contribution > 0) %>%
    dplyr::filter(!(feature %in% most_important_positive_partial_contributions$feature))
  less_important_positive_partial_contributions_sum <- sum(less_important_positive_partial_contributions %>% dplyr::pull(partial_contribution))

  ###Negative
  n_negative_partial_contributions <- partial_contribution %>% dplyr::filter(partial_contribution < 0) %>% nrow() #Get n of negative
  ####Most important negative partial contributions
  most_important_negative_partial_contributions <- partial_contribution %>%
    dplyr::slice_tail(n = min(5, n_negative_partial_contributions)) %>% dplyr::arrange(partial_contribution) #Extract n_negative contrib or 10
  most_important_negative_partial_contributions_sum <- sum(most_important_negative_partial_contributions %>% dplyr::pull(partial_contribution))
  ####Less important negative partial contributions
  less_important_negative_partial_contributions <- partial_contribution %>% dplyr::filter(partial_contribution < 0) %>%
    dplyr::filter(!(feature %in% most_important_negative_partial_contributions$feature))
  less_important_negative_partial_contributions_sum <- sum(less_important_negative_partial_contributions %>% dplyr::pull(partial_contribution))


  ##Extract prediction of gsm model for OLS
  gsm_prediction <- sum(partial_contribution %>% dplyr::pull(partial_contribution)) + intercept_importance$importance

  ##Extract prediction of complex model
  oos_pred_selected_ticker <- oos_sb_outputs_m_df %>% dplyr::filter(id == selected_id) %>% dplyr::pull(pred)

  ##Extract non-linearity
  complexity <- oos_pred_selected_ticker - gsm_prediction

  #####################

  #Plot!!
  #####################
  ## Prepare data for plotting with feature names for important contributions
  plot_data_df <- data.frame(
    tickers = c(
      most_important_positive_partial_contributions$feature,
      "other_pos",
      most_important_negative_partial_contributions$feature,
      "other_neg",
      "complexity"
    ),
    ContributionType = c(
      rep("Most Important Positive", nrow(most_important_positive_partial_contributions)),
      "Less Important Positive",
      rep("Most Important Negative", nrow(most_important_negative_partial_contributions)),
      "Less Important Negative",
      "Complexity"
    ),
    TotalContribution = c(
      most_important_positive_partial_contributions$partial_contribution,
      less_important_positive_partial_contributions_sum,
      most_important_negative_partial_contributions$partial_contribution,
      less_important_negative_partial_contributions_sum,
      complexity
    )
  )

  #Add Intercept (base_pred)
  plot_data_df <- plot_data_df %>%
    #Join with feature importance
    dplyr::bind_rows(
      intercept_importance %>% dplyr::rename(TotalContribution = "importance") %>%
        dplyr::mutate(ContributionType = "Base Prediction", .before = TotalContribution) #Add columns
    ) %>%
    # Change "(Intercept)" to "base_pred"
    dplyr::mutate(tickers = dplyr::if_else(tickers == "(Intercept)", "base_pred", tickers)) %>%
    # Reorder so that rows with "base_pred" come first
    dplyr::arrange(dplyr::if_else(tickers == "base_pred", 0, 1))

  #Add cumulative and other plot info
  plot_data_df <- plot_data_df %>%
    dplyr::mutate(
      Cumulative      = base::cumsum(TotalContribution),
      PrevCumulative  = dplyr::lag(Cumulative, default = 0),
      Midpoint        = (Cumulative + PrevCumulative) / 2,
      x               = dplyr::row_number(),
      xmin            = x - 0.4,
      xmax            = x + 0.4,
      ymin            = base::pmin(PrevCumulative, Cumulative),
      ymax            = base::pmax(PrevCumulative, Cumulative),
      fill_type       = dplyr::if_else(TotalContribution >= 0, "Positive", "Negative")
    )

  #-----------------------------------------------------------------------
  # 2) Define colors
  #-----------------------------------------------------------------------
  neon_green  <- "#39FF14"
  neon_pink   <- "#FF1493"
  blue_bg     <- "#001f3f"
  faint_blue  <- "#003366"
  white       <- "#FFFFFF"

  #-----------------------------------------------------------------------
  # 3) Build the waterfall plot with ggplot2
  #-----------------------------------------------------------------------
  p <- ggplot2::ggplot(
    data = plot_data_df,
    mapping = ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = fill_type
    )
  ) +
    ggplot2::geom_rect(
      color     = "black",
      linewidth = 0.2,   # Use linewidth (not size) in ggplot2 >= 3.4.0
      alpha     = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x     = x,
        y     = Midpoint,
        label = base::sprintf("%.4f", TotalContribution)
      ),
      color    = white,
      fontface = "bold",
      size     = 3,
      vjust    = ifelse(plot_data_df$TotalContribution >= 0, -0.4, 1.4)
    ) +
    ggplot2::scale_x_continuous(
      breaks = plot_data_df$x,
      labels = plot_data_df$tickers,
      expand = ggplot2::expansion(mult = 0.05)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = white,
      linewidth  = 0.3
    ) +
    # Add a dashed horizontal line at the final cumulative value:
    ggplot2::geom_hline(
      yintercept = tail(plot_data_df$Cumulative, 1),
      linetype = "dashed",
      color = "cyan",
      linewidth = 0.3
    ) +
    # Annotate the final cumulative value on the plot
    ggplot2::annotate(
      "text",
      x = max(plot_data_df$x) + 0.5,
      y = tail(plot_data_df$Cumulative, 1) - 0.001,
      label = base::sprintf("%.4f", tail(plot_data_df$Cumulative, 1)),
      color = "cyan",
      fontface = "bold",
      hjust = 0
    ) +
    ggplot2::scale_fill_manual(
      values = c("Positive" = neon_green, "Negative" = neon_pink)
    ) +
    ggplot2::labs(
      x     = NULL,
      y     = "Cumulative Contribution",
      title = base::paste0("OOS Prediction Decomposition of ", sb_backtest_workflow$target_fwd_name, " for ",
                           selected_ticker, " at ", selected_date),
      fill  = "Sign"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      axis.text.x      = ggplot2::element_text(color = white, angle = 45, hjust = 1),
      axis.text.y      = ggplot2::element_text(color = white),
      axis.title.y     = ggplot2::element_text(color = white),
      legend.position  = "none",
      legend.title     = ggplot2::element_text(color = white),
      legend.text      = ggplot2::element_text(color = white),
      panel.grid.major = ggplot2::element_line(color = faint_blue, linewidth  = 0.2),
      panel.grid.minor = ggplot2::element_line(color = faint_blue, linewidth  = 0.1),
      plot.title       = ggplot2::element_text(color = white, size = 16, face = "bold")
    )

  print(p)
  return(plot_data_df)

  #####################

}


#' inner helper function to decompose feature importance
decompose_feature_importance <- function(most_recent_meta_date, meta_feature_importance, base_identifiers, base_feature_importance_filtered){

  #Init
  meta_feature_importance_decomposed <- meta_feature_importance

  #Loop through base identifiers
  for (i in seq_along(base_identifiers)) {
    base_id <- base_identifiers[i]
    base_features <- base_feature_importance_filtered[[i]]

    # Extract meta row corresponding to base learner
    meta_row <- meta_feature_importance_decomposed %>%
      dplyr::filter(tickers == base_id)

    if (nrow(meta_row) > 0) {
      # Calculate relative importance for base learner features
      relative_importance <- base_features %>%
        dplyr::mutate(relative_importance = importance / sum(importance, na.rm = TRUE)) %>%
        dplyr::mutate(adjusted_importance = relative_importance * meta_row$importance) %>%
        dplyr::mutate(importance = adjusted_importance)

      # Remove meta row for base learner
      meta_feature_importance_decomposed <- meta_feature_importance_decomposed %>%
        dplyr::filter(tickers != base_id)

      # Append decomposed base learner features
      meta_feature_importance_decomposed <- dplyr::bind_rows(meta_feature_importance_decomposed, relative_importance)
    }
  }

  # Step 4: Consolidate duplicate tickers (sum their importance)
  meta_feature_importance_decomposed_consolidated <-
    meta_feature_importance_decomposed %>%
    dplyr::group_by(tickers) %>%
    dplyr::summarize(importance = sum(importance, na.rm = TRUE), .groups = "drop")

  # Adjust meta_feature_importance_consolidated object to include all needed columns
  meta_feature_importance_decomposed_consolidated_m_df <- meta_feature_importance_decomposed_consolidated %>%
    as.data.frame() %>%
    dplyr::mutate(dates = most_recent_meta_date, .before = importance) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers)

  return(meta_feature_importance_decomposed_consolidated_m_df)

}

