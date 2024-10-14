#' Plot Method for Meta Dataframe
#'
#' This method generates plots for a `meta_dataframe` object based on the specified plot type.
#'
#' @param x A `meta_dataframe` object containing the data to be plotted.
#' @param type A character string specifying the type of plot to create. Options are:
#'   \itemize{
#'     \item \strong{"distribution"}: Plots the distribution of values for numeric columns.
#'     \item \strong{"date_range"}: Visualizes the frequency of data points over time.
#'     \item \strong{"unique_tickers"}: Displays the number of unique tickers over time.
#'   }
#' @return A ggplot object representing the requested plot.
#' @export
setMethod(
  "plot",
  signature(x = "meta_dataframe"),
  function(x, type = "distribution") {
    df <- x@data

    if (type == "distribution") {
      # Plot distribution of values for numeric columns
      numeric_columns <- sapply(df, is.numeric)
      df_numeric <- df[, numeric_columns, drop = FALSE]

      if (ncol(df_numeric) == 0) {
        stop("No numeric columns found for distribution plot")
      }

      # Melt the numeric dataframe
      melted_df <- reshape2::melt(df_numeric, id.vars = NULL, variable.name = "variable", value.name = "value")

      ggplot2::ggplot(melted_df, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::facet_wrap(~ variable, scales = "free_x") +
        ggplot2::labs(title = "Distribution of Signals",
                      x = "Value",
                      y = "Frequency")

    } else if (type == "date_range") {
      # Plot the date range
      ggplot2::ggplot(df, ggplot2::aes(x = dates)) +
        ggplot2::geom_bar() +
        ggplot2::labs(title = "Date Range Visualization",
                      x = "Date",
                      y = "Count") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    } else if (type == "unique_tickers") {
      # Plot the unique tickers over time
      ticker_counts <- stats::aggregate(id ~ dates, data = df, FUN = function(x) length(unique(x)))

      ggplot2::ggplot(ticker_counts, ggplot2::aes(x = dates, y = id)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = "Number of Unique Tickers Over Time",
                      x = "Date",
                      y = "Number of Unique Tickers")

    } else {
      stop("Invalid plot type. Choose from 'distribution', 'date_range', or 'unique_tickers'.")
    }
  }
)

################################


# Define the plot method for ml_wf_val_results
################################
#' Plot Machine Learning Walk-Forward Validation Results
#'
#' This method generates various plots to visualize the performance of machine learning models using walk-forward validation metrics.
#' It creates plots comparing out-of-sample (OOS) testing metrics, validation metrics, and hyperparameter performance over time.
#'
#' @param x An object of class \code{ml_wf_val_results} containing the results of the walk-forward validation.
#'
#' @return The following plots:
#' \itemize{
#'   \item \code{chosen_val_metric_over_time}: Plot of the chosen evaluation metric over time for the test data, including overall and yearly means.
#'   \item \code{test_vs_val_chosen_eval_metric_over_time}: Plot comparing the chosen evaluation metric over time for both test and validation data.
#'   \item \code{best_hyper_over_time}: Plot of the best hyperparameter values over time, with separate facets for each hyperparameter.
#'   \item \code{hyper_vs_error}: Plot showing the performance of hyperparameter choices with respect to the chosen evaluation metric. The plot varies depending on the machine learning algorithm used.
#'   \item \code{all_eval_metrics_over_time}: Plot of all evaluation metrics over time, including dashed lines for variable means and vertical lines for rebalancing dates.
#' }
#'
#' @export
setMethod("plot", "ml_wf_val_results", function(x) {

  # Extract relevant data from the S4 object
  oos_testing_eval_metrics <- x@oos_testing_eval_metrics
  validation_eval_metrics_hyper_choice <- x@validation_eval_metrics_hyper_choice
  hyper_choice_df <- x@best_hyperparameters
  chosen_eval_metric <- x@metadata$chosen_eval_metric
  chosen_eval_metric_validation <- x@chosen_eval_metric_validation
  ml_algorithm <- x@metadata$ml_algorithm
  rebalance_dates <- x@metadata$rebalance_dates


  #Define global variables to pass R Cmd Check
  dates <- value <- year <- overall_mean <- yearly_mean <- quantile <- concatenation <- median <- lambda.min.ratio <- alpha <- q75 <- median_chosen_eval_metric <-
    q25 <- variable_mean <- x <- y <- label <- variable <- mtry <- num.trees <- min.bucket <- max.depth <- eta <- max_depth <- colsample_bytree <-
    lr <- droprate <- regularizer_l2 <- regularizer_l2 <- NULL

  #Get object
  plots_list <- list()


  #Treatments to oos_testing and validation metrics
  #Some treatments to oos_testing_eval
  #Change colnames
  colnames(oos_testing_eval_metrics) <- paste("oos_testing_",colnames(oos_testing_eval_metrics), sep = "")
  #Add dates column
  oos_testing_eval_metrics <- oos_testing_eval_metrics %>% dplyr::mutate(dates = rownames(oos_testing_eval_metrics))
  oos_testing_eval_metrics$dates <- as.Date(oos_testing_eval_metrics$dates, format = "%Y-%m-%d") #Coerce to dates
  #Extract dates
  oos_testing_dates <- as.Date(oos_testing_eval_metrics$dates, format = "%Y-%m-%d")

  if(ml_algorithm != "ols"){
    #Some treatments to the validation_eval
    #Change colnames
    colnames(validation_eval_metrics_hyper_choice) <- paste("validation_",colnames(validation_eval_metrics_hyper_choice), sep = "")
    #Add dates column
    validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice %>% dplyr::mutate(dates = rownames(validation_eval_metrics_hyper_choice))
    validation_eval_metrics_hyper_choice$dates <- as.Date(validation_eval_metrics_hyper_choice$dates, format = "%Y-%m-%d") #Coerce to dates
    #Extract dates
    validation_dates <- as.Date(validation_eval_metrics_hyper_choice$dates, format = "%Y-%m-%d")
    #Join test and validation
    oos_testing_and_validation <- dplyr::left_join(oos_testing_eval_metrics, validation_eval_metrics_hyper_choice, by = 'dates')

    #Melt
    oos_testing_and_validation <- oos_testing_and_validation %>% reshape::melt(id.vars="dates")
    oos_testing_and_validation$dates <- as.Date(oos_testing_and_validation$dates, format = "%Y-%m-%d")

    #OOS test data
    oos_testing_data <-  oos_testing_and_validation %>%
      dplyr::filter(stringr::str_detect(variable, "oos_testing_")) %>% #Filter only OOS test
      dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
      dplyr::group_by(variable) %>% # Group by year
      dplyr::mutate(variable_mean = mean(value, na.rm = TRUE)) %>% # Take yearly mean
      dplyr::ungroup() # Ungroup

    #Chosen eval metric - test data
    chosen_eval_testing_data <- oos_testing_and_validation %>%
      dplyr::filter(variable == paste("oos_testing_", chosen_eval_metric, sep = "")) %>% #Filter only OOS test chosen eval metric
      dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
      dplyr::mutate(overall_mean = mean(value, na.rm = TRUE)) %>% #Add overall mean
      dplyr::group_by(year) %>% #Group by year
      dplyr::mutate(yearly_mean = mean(value, na.rm = TRUE)) %>% #Take yearly mean
      dplyr::ungroup() #Ungroup

    #Validation data
    validation_data <- oos_testing_and_validation %>%
      dplyr::filter(stringr::str_detect(variable, "validation_")) %>% #Filter only validation
      dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
      dplyr::filter(!is.na(value)) #Filter out nas

    #Chosen eval metric - validation data
    chosen_eval_validation_data <- oos_testing_and_validation %>%
      dplyr::filter(variable == paste("validation_", chosen_eval_metric, sep = "")) %>% #Filter only chosen validation metric
      dplyr::filter(!is.na(value)) #Filter out nas

    #PLOT 1 - Test chosen validation metric over time
    plots_list$chosen_val_metric_over_time <-
      ggplot2::ggplot(chosen_eval_testing_data,
                      ggplot2::aes(x = dates, y = value, color = paste(chosen_eval_metric))) +
      ggplot2::geom_line(alpha = 0.5, ggplot2::aes(group = 1)) + # Draw line
      ggplot2::geom_point() +  # Add points
      ggplot2::labs(x = "Date", y = chosen_eval_metric) + # Add labels
      ggplot2::theme_bw() + # Set minimal theme
      ggplot2::ggtitle(paste("Test", chosen_eval_metric, "over time")) +
      ggplot2::facet_wrap(~year, scales = "free") +  # Ensure free y-axis scales
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +  # Format x-axis labels
      ggplot2::geom_hline(ggplot2::aes(yintercept = overall_mean, color = "Overall Mean"), linetype = "dashed") + # Add dashed line for overall mean
      ggplot2::geom_hline(ggplot2::aes(yintercept = yearly_mean, color = "Yearly Mean"), linetype = "dashed") + # Add dashed line for yearly mean
      ggplot2::scale_color_manual(values = c("red", "black", "black"),
                                  breaks = c("Overall Mean", "Yearly Mean", "Metric"),
                                  labels = c("Overall Mean", "Yearly Mean", "Metric")) + # Define legend colors and labels
      ggplot2::guides(color = ggplot2::guide_legend(title = "")) + # Customize legend title
      ggplot2::theme(legend.position = "bottom") + # Move legend to bottom
      ggplot2::scale_y_continuous(limits = c(min(chosen_eval_testing_data$value), max(chosen_eval_testing_data$value))) # Set y-axis limits

      print(plots_list$chosen_val_metric_over_time)


    #PLOT 2 - Test vs Validation chosen eval metric over time
    plots_list$test_vs_val_chosen_eval_metric_over_time <-
      ggplot2::ggplot() +
      ggplot2::geom_line(data = chosen_eval_testing_data, ggplot2::aes(x = dates, y = value, color = "Test"), alpha = 0.5) +
      ggplot2::geom_point(data = chosen_eval_testing_data, ggplot2::aes(x = dates, y = value, color = "Test"), size = 2) +  # Add test data points
      ggplot2::geom_point(data = chosen_eval_validation_data, ggplot2::aes(x = dates, y = value, color = "Validation"), size = 2) +
      ggplot2::labs(x = "Date", y = chosen_eval_metric, color = "") +
      ggplot2::ggtitle(paste("Test and validation", chosen_eval_metric, "over time")) +
      ggplot2::scale_color_manual(values = c("Test" = "black", "Validation" = "blue")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_text(data = chosen_eval_validation_data, ggplot2::aes(x = dates, y = value, label = dates),
                         vjust = -1.5, hjust = 0, size = 3, color = "blue") +
      ggplot2::geom_vline(data = chosen_eval_validation_data, ggplot2::aes(xintercept = dates),
                          linetype = "dashed", color = "blue")

      print(plots_list$test_vs_val_chosen_eval_metric_over_time)


    #PLOT 3 - Best Hyperparameters over time
    plots_list$best_hyper_over_time <-
      ggplot2::ggplot(hyper_choice_df %>% dplyr::mutate(dates = as.Date(rownames(hyper_choice_df), format = "%Y-%m-%d")) %>% reshape::melt(id.vars="dates"),
                      ggplot2::aes(x = dates, y = value, color = variable)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_point() +
      ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), vjust = -0.5, size = 3) +  # Add text labels for values
      ggplot2::labs(x = "Date", y = "Best hyperparameter") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Hyper choice over time") +
      ggplot2::facet_wrap(~variable, scales = "free") +  # Create subplots for each group specified by the variable column
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
      ggplot2::theme(legend.position = "bottom")


      print(plots_list$best_hyper_over_time)


    #PLOT 4 - Hyperparameters vs Error
    #Transform the list in a big rbinded data frame
    chosen_eval_metric_validation_df <- do.call(rbind, chosen_eval_metric_validation)

    #For each column of hyperparameters, turn into categories
    for(j in 1:(ncol(chosen_eval_metric_validation_df)-1)){
      tryCatch({
        chosen_eval_metric_validation_df[,j] <- as.factor(#As category
          cut(chosen_eval_metric_validation_df[,j], #Cut is specially useful for random_search
              breaks=unique(stats::quantile(chosen_eval_metric_validation_df[,j], probs = seq(0,1,by=0.1))),
              include.lowest = TRUE))

      }, error = function(e) {
        message(paste("Only one unique value identified for", names(chosen_eval_metric_validation_df)[j]))
        chosen_eval_metric_validation_df[,j] <- chosen_eval_metric_validation_df[,j]
      })
    }

    #Concatenation
    if(ml_algorithm == "glmnet"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$alpha, chosen_eval_metric_validation_df$lambda.min.ratio)
    } else {}
    if(ml_algorithm == "rf"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$mtry, chosen_eval_metric_validation_df$num.trees,
                                                              chosen_eval_metric_validation_df$max.depth, chosen_eval_metric_validation_df$min.bucket)
    } else {}
    if(ml_algorithm == "xgb"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$min_child_weight,
                                                              chosen_eval_metric_validation_df$max_depth,
                                                              chosen_eval_metric_validation_df$subsample,
                                                              chosen_eval_metric_validation_df$colsample_bytree,
                                                              chosen_eval_metric_validation_df$eta,
                                                              chosen_eval_metric_validation_df$alpha,
                                                              chosen_eval_metric_validation_df$gamma,
                                                              chosen_eval_metric_validation_df$nrounds)
    } else {}
    if(ml_algorithm == "nn"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$regularizer_l1,
                                                              chosen_eval_metric_validation_df$regularizer_l2,
                                                              chosen_eval_metric_validation_df$droprate,
                                                              chosen_eval_metric_validation_df$lr)

    } else {}


    ###Summarize main quantiles
    chosen_eval_metric_validation_summary <- as.data.frame(chosen_eval_metric_validation_df %>%
                                                             dplyr::group_by(concatenation) %>% #Take by group of hyper combinations
                                                             dplyr::summarise(median_chosen_eval_metric = stats::median(chosen_eval_metric), #Q50
                                                                              q25 = stats::quantile(chosen_eval_metric, 0.25), #Q25
                                                                              q75 = stats::quantile(chosen_eval_metric, 0.75), #Q75
                                                                              max = stats::quantile(chosen_eval_metric, 1), #Q100
                                                                              min = stats::quantile(chosen_eval_metric, 0))) #Q0
    #Join with summary
    chosen_eval_metric_validation_df <- chosen_eval_metric_validation_df %>%
      dplyr::left_join(chosen_eval_metric_validation_summary, by = "concatenation")

    #Take last hyper tuning
    chosen_eval_metric_validation_last_tuning <- chosen_eval_metric_validation_df[ #Take rows from beginning to end of last hyper tuning
      (nrow(chosen_eval_metric_validation_df) - nrow(chosen_eval_metric_validation[[length(chosen_eval_metric_validation)]])):nrow(chosen_eval_metric_validation_df),]



    if(ml_algorithm  == "glmnet"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = lambda.min.ratio, y = chosen_eval_metric, fill = alpha)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by alpha and lambda.min.ratio")) +
        ggplot2::facet_grid(rows = ggplot2::vars(alpha)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

        print(plots_list$hyper_vs_error)

    } else {} #end glmnet_specific

    if(ml_algorithm  == "rf"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = mtry, y = chosen_eval_metric, fill = mtry)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by max.depth and min.bucket")) +
        ggplot2::facet_grid(rows = ggplot2::vars(max.depth), cols = ggplot2::vars(min.bucket)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

        print(plots_list$hyper_vs_error)


    } else {} #end rf_specific

    if(ml_algorithm  == "xgb"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = eta, y = chosen_eval_metric, fill = eta)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by max_depth and colsample_bytree")) +
        ggplot2::facet_grid(rows = ggplot2::vars(max_depth), cols = ggplot2::vars(colsample_bytree)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")


        print(plots_list$hyper_vs_error)


    } else {} #end xgb_specific

    if(ml_algorithm  == "nn"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = lr, y = chosen_eval_metric, fill = lr)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by droprate and regularizer_l1")) +
        ggplot2::facet_grid(rows = ggplot2::vars(droprate), cols = ggplot2::vars(regularizer_l1)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")


        print(plots_list$hyper_vs_error)


    } else {} #end xgb_specific

  } else {} #end ols_restrictions

  #PLOT 5 - All eval metrics over time
  plots_list$all_eval_metrics_over_time <-
    ggplot2::ggplot(oos_testing_eval_metrics %>% reshape::melt(id.vars="dates") %>%
                      dplyr::group_by(variable) %>% #group by variable
                      dplyr::mutate(variable_mean = mean(value, na.rm = TRUE)) %>% #Create new variable
                      dplyr::ungroup(),
                    ggplot2::aes(x = dates, y = value, color = variable)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Date", y = "Metric") +
    ggplot2::theme_light() +
    ggplot2::ggtitle("All eval metrics over time") +
    ggplot2::facet_wrap(~variable, scales = "free") +  # Create subplots for each group specified by the variable column
    ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = variable_mean, color = variable), linetype = "dashed") + # Map color to variable
    ggplot2::guides(color = ggplot2::guide_legend(title = "Metric")) +
    ggplot2::geom_vline(xintercept = rebalance_dates, color = "blue", linetype = "dashed") +  # Add blue dashed lines at specific dates
    ggplot2::geom_text(data = data.frame(x = rebalance_dates, y = -Inf, label = rebalance_dates),
                       ggplot2::aes(x = x, y = y, label = label), vjust = -0.5, hjust = -0.5, size = 2, color = "black") +  # Display date labels below the plot
    ggplot2::theme(legend.position = "bottom")


    print(plots_list$all_eval_metrics_over_time)


})
