# Define the plot method for meta_dataframe
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
