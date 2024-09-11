#' Calculate Returns from a Portfolio and Update Weights
#'
#' This function calculates the returns of a portfolio, including transaction costs and updates the portfolio weights based on the latest data.
#'
#' @param current_date A Date object representing the current date.
#' @param is_rebalancing_month A logical indicating whether the current month is a rebalancing month.
#' @param stock_universe_m_d_ref A data frame containing stock universe data with columns for tickers and current weights.
#' @param target_m_d_ref A data frame containing target stock data with columns for tickers and forward returns.
#' @param portfolio_weights_m_d_ref A data frame containing current portfolio weights with columns for tickers and weights.
#' @param portfolio_weights_m_lstd_ref A data frame containing portfolio weights from the last period, including delisted stocks.
#' @param portfolio_returns_df A data frame to which portfolio returns will be added, including raw and net returns.
#' @param benchmark_returns_df A data frame containing benchmark returns.
#' @param main_liquidity_metric A string specifying the liquidity metric used to estimate transaction costs. Default is "mean_volfin_3m".
#' @param liquidity_m_d_ref A data frame containing liquidity metrics for the stocks.
#' @param volatility_m_d_ref A data frame containing volatility metrics for the stocks.
#' @param transaction_costs_list A list containing parameters to estimate transaction costs, including:
#'   \itemize{
#'     \item \code{strategy_aum} - The assets under management for the strategy.
#'     \item \code{alpha} - The BARRA model alpha parameter.
#'     \item \code{lambda} - The BARRA model lambda parameter.
#'     \item \code{direct_transaction_cost} - The direct transaction cost per trade.
#'   }
#'
#' @return A list with updated portfolio data, including:
#'   \itemize{
#'     \item \code{portfolio_weights_m_d_ref} - Updated portfolio weights.
#'     \item \code{transactions_m_d_ref} - Data frame with rebalancing information including transaction costs.
#'     \item \code{portfolio_returns_df} - Data frame with updated portfolio returns.
#'     \item \code{updated_portfolio_weights} - Updated portfolio weights after applying returns.
#'   }
#' @export
calculate_portfolio_returns <- function(
    current_date, is_rebalancing_month,
    stock_universe_m_d_ref = NULL, target_m_d_ref, portfolio_weights_m_d_ref, portfolio_weights_m_lstd_ref,
    #Portfolio and benchmark returns
    portfolio_returns_df, selected_benchmark_returns_df,
    #Parameters and data to estimate direct and indirect transaction costs
    main_liquidity_metric = "mean_volfin_3m", liquidity_m_d_ref, volatility_m_d_ref, transaction_costs_list,
    verbose = TRUE
    ){

  #Initial prep
  ####################
  return_calculation_date <- lubridate::add_with_rollback(current_date, months(1)) #Get next month date

  #Transaction cost info
  strategy_aum <- transaction_costs_list$strategy_aum
  alpha <- transaction_costs_list$alpha
  lambda <- transaction_costs_list$lambda
  direct_transaction_cost <- transaction_costs_list$direct_transaction_cost

  #Get tickers info
    ##From old portfolio (portfolio with last composition but updated weights)
    tickers_in_old_universe <- portfolio_weights_m_lstd_ref$tickers
    tickers_in_old_portfolio <- portfolio_weights_m_lstd_ref$tickers[which(portfolio_weights_m_lstd_ref$old_portfolio_weights != 0)]
    ##Get tickers in current portfolio
    tickers_in_current_universe <- portfolio_weights_m_d_ref$tickers
    ##Tickers in common
    tickers_in_both_universes <- dplyr::intersect(tickers_in_old_universe, tickers_in_current_universe)
    ##Deslited stocks
    delisted_tickers <- dplyr::setdiff(tickers_in_old_universe, tickers_in_current_universe)
    delisted_tickers_of_old_portfolio <- dplyr::setdiff(tickers_in_old_portfolio, tickers_in_current_universe)

    ##IPOs
    ipo_tickers <- dplyr::setdiff(tickers_in_current_universe, tickers_in_old_universe)

    ##Message
    if(verbose){
      ###Deslisted tickers
      if(length(delisted_tickers) != 0){
        cat("\n")
        cat(paste0("Delisted tickers: ", delisted_tickers, ". Of those, the following were in the portfolio: "))
      }
      ###Deslited tickers of portfolio
      if(length(delisted_tickers_of_old_portfolio) != 0){
        cat(crayon::yellow(delisted_tickers_of_old_portfolio))
      }
      ###IPOs
      if(length(ipo_tickers) != 0){
        cat("\n")
        cat(paste("IPOs:", ipo_tickers))
      }
    }

  ####################

  #Constitute new portfolio
  ###########################
    ##If it is a rebalancing_month
    if(is_rebalancing_month){
      #Pass weights
      portfolio_weights_m_d_ref$portfolio_weights <- stock_universe_m_d_ref$weights #portfolio_weights_m_d_ref and stock_universe_m_d_ref have same ids
    } else {
      #Pass weights
      portfolio_weights_m_d_ref[which(portfolio_weights_m_d_ref$tickers %in% tickers_in_both_universes), "portfolio_weights"] <-
        portfolio_weights_m_lstd_ref[which(portfolio_weights_m_lstd_ref$tickers %in% tickers_in_both_universes), "old_portfolio_weights"]
      #Rescale to sum 100%
      portfolio_weights_m_d_ref$portfolio_weights <- portfolio_weights_m_d_ref$portfolio_weights/sum(portfolio_weights_m_d_ref$portfolio_weights)
    }
  ###########################

  #Transactions data frame
  ###########################
    ##Create a rebalancing dataframe to support calculations -> Multiple joins
    transactions_m_d_ref <- dplyr::left_join(portfolio_weights_m_d_ref, dplyr::select(target_m_d_ref, tickers, fwd_return_1m), by = "tickers") %>%  #Join weights and returns
      dplyr::left_join(dplyr::select(liquidity_m_d_ref, -id, -dates), by = "tickers") %>% #Join liquidity data
      dplyr::left_join(dplyr::select(volatility_m_d_ref, -id, -dates), by = "tickers") %>%
      dplyr::full_join(dplyr::select(portfolio_weights_m_lstd_ref, tickers, old_portfolio_weights), by = "tickers") #Full join because one wants to consider delisted stocks

    ##Treat NAs
      ###remove NAs in weights with 0 (NAs are possible if stocks are delisted (were present in last portfolio and not in current)
      transactions_m_d_ref$portfolio_weights[which(is.na(transactions_m_d_ref$portfolio_weights))] <- 0
      ###remove NAs in old weights with 0 (NAs are possible if stocks are IPOs
      transactions_m_d_ref$old_portfolio_weights[which(is.na(transactions_m_d_ref$old_portfolio_weights))] <- 0
      ###replace NAs in liquidity with low quantile (more conservative for a delisting stock)
      transactions_m_d_ref[, main_liquidity_metric][which(is.na(transactions_m_d_ref[, main_liquidity_metric]))] <- quantile(transactions_m_d_ref[, main_liquidity_metric], 0.25, na.rm = TRUE)
      ###replace NAs in volatility_m_d_ref with median (conservative, as, usually, when there is an OPA, there is a pre-defined price that limits stock vol)
      transactions_m_d_ref[, "daily_vol"][which(is.na(transactions_m_d_ref$daily_vol))] <- median(transactions_m_d_ref[, "daily_vol"], na.rm = TRUE)

      ##Add details
      transactions_m_d_ref$obs <- "none"
      transactions_m_d_ref$obs[which(transactions_m_d_ref$tickers %in% delisted_tickers)] <- "delisted"
      transactions_m_d_ref$obs[which(transactions_m_d_ref$tickers %in% ipo_tickers)] <- "IPO"

  ############################

  #Calculate transaction costs
  #############################
    ##Messages
      if(verbose){
        cat("\n")
        cat("Calculating net portfolio returns")
      }

    ##Add order data
      transactions_m_d_ref <- transactions_m_d_ref %>%
       dplyr::mutate(delta = portfolio_weights - old_portfolio_weights) %>%
       dplyr::mutate(order = delta*strategy_aum) %>%
       dplyr::mutate(relative_order_size = abs(order)/!!rlang::sym(main_liquidity_metric))

    ##BARRA model parameters
     alpha <- alpha
     if(lambda == "dynamic"){
       #Dynamically adjust lambda according to trade size
        ##For very small trades, use lambda = 1
       transactions_m_d_ref[which(transactions_m_d_ref$relative_order_size <= 0.002),"lambda"] <- 1
        ##For small trades, use lambda = 0.5
       transactions_m_d_ref[which(transactions_m_d_ref$relative_order_size > 0.002 & transactions_m_d_ref$relative_order_size <= 0.05),"lambda"] <- 0.5
        ##For medium trades, use lambda = 0.25
       transactions_m_d_ref[which(transactions_m_d_ref$relative_order_size > 0.05 & transactions_m_d_ref$relative_order_size <= 0.1),"lambda"] <- 0.25
        ##For medium trades, use lambda = 0.10
       transactions_m_d_ref[which(transactions_m_d_ref$relative_order_size > 0.1),"lambda"] <- 0.1
     } else {
       transactions_m_d_ref[,"lambda"] <- lambda
     }

      ##Calculate costs
      transactions_m_d_ref <- transactions_m_d_ref %>%
        dplyr::mutate(direct_cost = (direct_transaction_cost * abs(order))/strategy_aum) %>%
        dplyr::mutate(market_impact_cost = alpha*(relative_order_size^lambda)*daily_vol)

      ##Get costs
      total_direct_cost <- sum(transactions_m_d_ref$direct_cost)
      total_market_impact_cost <- sum(transactions_m_d_ref$market_impact_cost)
      total_cost <- total_direct_cost + total_market_impact_cost


  ###########################

  #Calculate and add returns
  ############################
    ##Get benchmark return
    selected_benchmark_fwd_return <- selected_benchmark_returns_df[which(selected_benchmark_returns_df$dates == return_calculation_date), 2]
    ##Calculate raw forward portfolio return
      ###Raw
      fwd_raw_return <- sum(transactions_m_d_ref$portfolio_weights * transactions_m_d_ref$fwd_return_1m, na.rm = TRUE)
      ###Raw Active
      fwd_raw_active_return <- fwd_raw_return - selected_benchmark_fwd_return
    ##Calculate net forward portfolio return
      ##Net
      fwd_net_return <- fwd_raw_return - total_cost
      ##Net fwd active return
      fwd_net_active_return <- fwd_net_return - selected_benchmark_fwd_return
    ##Turnover
    turnover <- mean(abs(transactions_m_d_ref$delta))

   ##Add to portfolio_returns_df
     ###Raw returns
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "raw_return"] <- fwd_raw_return
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "raw_active_return"] <- fwd_raw_active_return

     ###Net Returns
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "net_return"] <- fwd_net_return
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "net_active_return"] <- fwd_net_active_return

     ###Costs
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "direct_cost"] <- total_direct_cost
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "market_impact_cost"] <- total_market_impact_cost
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "total_cost"] <- total_cost
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), "turnover"] <- turnover

     ###Benchmark returns
     portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date), ncol(portfolio_returns_df)] <- selected_benchmark_fwd_return

  ###########################

  #Update portfolio_weights
  ############################
  updated_portfolio_weights <- portfolio_weights_m_d_ref
  updated_portfolio_weights$portfolio_weights <- portfolio_weights_m_d_ref$portfolio_weights * (target_m_d_ref$fwd_return_1m/100+1) #Multiply by returns
  updated_portfolio_weights$portfolio_weights <- updated_portfolio_weights$portfolio_weights/sum(updated_portfolio_weights$portfolio_weights)
  ############################

  ##Messages
  if(verbose){
    cat("\n")
    cat(crayon::green("Portfolio returns succesfully calculated:"))
    cat("\n")
    print(portfolio_returns_df[which(portfolio_returns_df$dates == return_calculation_date),-1])
  }

  #Re-order transactions df
  transactions_m_d_ref <- transactions_m_d_ref[, c(setdiff(colnames(transactions_m_d_ref), "obs"), "obs")]

  #Gather results
  portfolio_returns_results_list <- list(
    portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
    transactions_m_d_ref = transactions_m_d_ref,
    portfolio_returns_df = portfolio_returns_df,
    updated_portfolio_weights = updated_portfolio_weights
  )

  return(portfolio_returns_results_list)

}
