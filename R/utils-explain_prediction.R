#' Explain Prediction
#'
#' @description
#' The `explain_prediction` function decomposes the out‐of‐sample (OOS) prediction for a selected ticker and date using the OLS Global Signal Model (GSM) backtest results. It performs internal checks on the provided inputs, extracts feature importance data and corresponding feature values, computes partial contributions (i.e. the product of feature importance and feature value), and finally generates a waterfall plot that visualizes how each feature (including the intercept) contributes to the GSM prediction.
#'
#' @param sb_backtest_results An S4 object containing the backtest results (including the workflow, OOS outputs, and feature importance data). The workflow object (`sb_backtest_results@sb_backtest_workflow`) must have been generated with an OLS GSM algorithm.
#' @param features_m_df An object containing the features meta-data used in the backtest. Its internal name (accessible via `features_m_df@meta_dataframe_name`) must match the one stored in `sb_backtest_results@sb_backtest_workflow$features_object_name`.
#' @param selected_id A character string representing the selected identifier. **Note:** This value is overwritten internally by concatenating `selected_ticker` and `selected_date` (i.e. `paste0(selected_ticker, "-", selected_date)`).
#' @param selected_ticker A character string representing the ticker (e.g., `"AAPL"`) for which to explain the prediction.
#'
#' @details
#' The function operates in several stages:
#'
#' \strong{Initial Checks:}
#'   \itemize{
#'     \item Verifies that the GSM algorithm used is `"ols"`. If not, the function stops.
#'     \item Checks that the provided `features_m_df` object matches the one used in the backtest.
#'     \item Constructs a new `selected_id` using the selected ticker and an externally defined `selected_date`. It then ensures that this identifier exists within the OOS outputs.
#'     \item Ensures that the `selected_date` is not earlier than the first rebalancing date.
#'   }
#'
#' \strong{Feature Importance and Partial Contributions:}
#'   \itemize{
#'     \item Extracts the most recent feature importance data up to the `selected_date` and separates the intercept from the other features.
#'     \item Reconstructs the corrected feature values using the helper function `select_and_correct_signals()`.
#'     \item Calculates the partial contribution for each feature (i.e. the product of the feature's importance and its current value).
#'     \item Identifies the most important and less important contributions for both positive and negative feature effects.
#'   }
#'
#' \strong{Plot Generation:}
#'   \itemize{
#'     \item Using `dplyr::mutate()`, the function computes a running (cumulative) total of contributions and other helper columns required to create a waterfall plot (e.g., previous cumulative value, midpoint for text labels, and x-axis positions).
#'     \item A waterfall plot is then generated with `ggplot2`, which uses neon colors on a dark background to visually decompose the GSM prediction into its contributing components.
#'   }
#'
#' @return The function produces a waterfall plot (a `ggplot2` object) that displays the decomposition of the OOS prediction based on feature contributions. It does not return a value programmatically.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   # Assume that sb_backtest_results, features_m_df, selected_date, and selected_ticker are properly defined.
#'   explain_prediction(sb_backtest_results, features_m_df, "ignored_value", "AAPL")
#' }
#'
#' @export
explain_prediction <- function(sb_backtest_results, features_m_df, selected_ticker, selected_date){

  #Get objects
  sb_backtest_workflow <- sb_backtest_results@sb_backtest_workflow
  oos_sb_outputs_m_df <- sb_backtest_results@oos_sb_outputs_m_df@data
  feature_importance_m_df <- sb_backtest_results@feature_importance_m_df@data
  gsm_algorithm <- sb_backtest_workflow$gsm_algorithm
  features_obj_name <- features_m_df@meta_dataframe_name
  features_m_df <- features_m_df@data

  if(gsm_algorithm != "ols"){
    stop("Currently, this function is only available for OLS GSM algorithm.")
  }

  #Initial Checks
  ##############
    ##Check if features_m_df is the one used in the backtest
    if(sb_backtest_workflow$features_object_name != features_obj_name){
      stop("The features_m_df object used in the backtest is different from the one provided. Please provide the correct features_m_df object.")
    }

    if(any(!sb_backtest_workflow$features %in% colnames(features_m_df))){
      stop("The features_m_df object provided does not contain the features used in backtest. Please provide the correct features_m_df object.")
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
    oos_pred_selected_ticker <- sb_backtest_results@oos_sb_outputs_m_df@data %>%
      dplyr::filter(id == selected_id) %>% dplyr::pull(pred)

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
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1),
        plot.title       = ggplot2::element_text(color = white, size = 16, face = "bold")
      )

    print(p)

    #####################

}

