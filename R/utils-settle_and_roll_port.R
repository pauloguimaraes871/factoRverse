#' Settle and Roll the Portfolio
#'
#' Orchestrates the monthly portfolio workflow by merging existing portfolio weights,
#' calculating the transactions required to transition to the updated portfolio,
#' applying transaction costs, computing the portfolio’s forward returns,
#' and rolling the portfolio weights to the next period.
#'
#' @param port_weights_placeholder_m_d_ref A \code{data.frame} or \code{tibble} containing
#'   the current (placeholder) portfolio weights. Must include a column \code{dates}.
#' @param updated_port_weights_m_lstd_ref A \code{data.frame} or \code{tibble} with the
#'   updated portfolio weights for the upcoming period. Must include a column \code{dates}.
#' @param stock_universe_m_d_ref A \code{data.frame} or \code{tibble} describing the stock
#'   universe (e.g., \code{id}, \code{tickers}, listing/delisting status).
#' @param liquidity_m_d_ref A \code{data.frame} or \code{tibble} with liquidity metrics
#'   for each stock, over time (e.g., daily volume). Must include a column \code{dates}.
#' @param volatility_m_d_ref A \code{data.frame} or \code{tibble} with volatility metrics
#'   for each stock, over time (e.g., standard deviation of returns). Must include
#'   a column \code{dates}.
#' @param main_liquidity_metric A character string specifying the column in
#'   \code{liquidity_m_d_ref} to be used as the primary liquidity proxy.
#' @param transaction_cost_list A named list of parameters for transaction cost
#'   estimation, containing:
#'   \itemize{
#'     \item \code{strategy_aum}: Numeric, the asset under management for the strategy.
#'     \item \code{alpha}: Numeric, the market-impact parameter for the transaction cost model.
#'     \item \code{lambda}: Numeric, the penalty or sensitivity parameter for the model.
#'     \item \code{direct_transaction_cost}: Numeric, the direct commission or fee rate.
#'   }
#' @param target_m_d_ref A \code{data.frame} or \code{tibble} with forward returns
#'   for each stock. Must include columns \code{tickers}, \code{dates}, \code{fwd_return_1m}.
#' @param selected_benchmark_reutrns_m_xts_d_ref An \code{xts} object containing the monthly
#'   returns of the chosen benchmark. The function will look for the row corresponding
#'   to \code{return_calculation_date} in order to compute active returns.
#' @param verbose Logical. If \code{TRUE}, prints progress and diagnostic messages.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{filled_transactions_m_d_ref}}{A \code{data.frame} or \code{tibble}
#'       detailing the executed transactions after applying transaction costs.}
#'     \item{\code{port_weights_m_d_ref}}{The merged portfolio weights used as a base
#'       for current period calculations.}
#'     \item{\code{rolled_fwd_port_weights_m_d_ref}}{The portfolio weights rolled
#'       forward to the next period after returns.}
#'     \item{\code{transactions_costs_m_xts_d_ref}}{An \code{xts} object with the total,
#'       direct, and market impact costs recorded at the \code{return_calculation_date}.}
#'     \item{\code{fwd_port_returns_m_xts_d_ref}}{An \code{xts} object with forward
#'       monthly (raw and net) returns, benchmark returns, and turnover for the period.}
#'   }

settle_and_roll_port <- function(
  #Portfolio Weights
  port_weights_placeholder_m_d_ref, updated_port_weights_m_lstd_ref,
  #Stock Universe
  stock_universe_m_d_ref,
  #Transaction costs
  liquidity_m_d_ref, volatility_m_d_ref, main_liquidity_metric = main_liquidity_metric, ##Vol and Liquidity
  transaction_cost_list, ##BARRA model parameters and direct transaction cost
  #Stock returns
  target_m_d_ref, selected_benchmark_reutrns_m_xts_d_ref,
  #Misc
  verbose = TRUE
){

  #Initial prep
  ####################
  ##Dates
  current_date <- port_weights_placeholder_m_d_ref %>% dplyr::filter(dates) %>% unique()
  return_calculation_date <- lubridate::add_with_rollback(current_date, months(1)) #Get next month date

  ##Get transaction_cost parameters
  strategy_aum <- transaction_cost_list$strategy_aum
  alpha <- transaction_cost_list$alpha
  lambda <- transaction_cost_list$lambda
  direct_transaction_cost <- transaction_cost_list$direct_transaction_cost

  ####################

  #Merge stock weights
  ####################
  ###Get merged portfolio and tickers that were listed and delisted
  merged_port_results_list <- merge_and_rescale_weights(
    #Portfolios EOP and BOP
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    #Stock universe
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    #Verbose
    verbose = verbose
  )
  ###Get merged port weights
  port_weights_m_d_ref <- merged_port_results_list$port_weights_m_d_ref

  ####################

  #Calculate transactions and costs
  ####################

  ##Transactions
  transactions_m_d_ref <- calculate_trade_orders(
    #Merged Portfolio
    merged_port_results_list = merged_port_results_list,
    #BOP Portfolio
    updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    #Liquidity and vol data
    liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref,
    main_liquidity_metric = main_liquidity_metric,
    #Strategy AuM
    strategy_aum = strategy_aum,
    #Verbose
    verbose = verbose
  )

  ##Costs
  transaction_costs_results_list <- calculate_transaction_costs(
    #Transactions
    transactions_m_d_ref = transactions_m_d_ref,
    #Indirect transaction costs parameters
    alpha = alpha, lambda = lambda,
    #Direct transaction cost
    direct_transaction_cost = direct_transaction_cost,
    #Verbose
    verbose = verbose
  )

    ###Extract Results
    transactions_m_d_ref <- transaction_costs_results_list$transactions_m_d_ref

    ###Aggregate in m_xts
    transactions_costs_m_xts_d_ref <- xts::xts(data.frame(
      total_direct_cost = transaction_costs_results_list$total_direct_cost, #Direct Costs
      total_market_impact_cost = transaction_costs_results_list$total_market_impact_cost, #Indirect costs
      total_cost = transaction_costs_results_list$total_cost #Total costs
    ), order.by = return_calculation_date)


  #Calculate returns and roll portfolio weights
  ############################

    ##Get benchmark_fwd_return
    selected_benchmark_fwd_return <- selected_benchmark_returns_m_xts[which(zoo::index(selected_benchmark_returns_m_xts) == return_calculation_date), ] %>% as.numeric()

    ##Get fwd_1m stock returns
    clean_fwd_return_1m_m_d_ref <- target_m_d_ref %>%
      dplyr::select(id, tickers, dates, fwd_return_1m) %>% #Select only relevant columns
      tidyr::drop_na(fwd_return_1m) #Filter out NA values

    ##Checks if it is an up-to-date month
    if (length(clean_fwd_return_1m_m_d_ref) > 0){

      ##Calculate portfolio returns
      port_returns_results_list <- calculate_port_returns(
        #Fwd Stock and Benchmark Returns
        clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref, selected_benchmark_fwd_return = selected_benchmark_fwd_return,
        #Transactions
        transactions_m_d_ref = transactions_m_d_ref,
        #Costs
        transactions_costs_m_xts_d_ref = transactions_costs_m_xts_d_ref,
        #Misc
        verbose = verbose
      )

        ###Aggregate in m_xts
        fwd_port_returns_m_xts_d_ref <- xts::xts(data.frame(
          fwd_raw_return = fwd_raw_return, fwd_raw_active_return = fwd_raw_active_return, #Raw Returns
          fwd_net_return = fwd_net_return, fwd_net_active_return = fwd_net_active_return, #Net Returns
          selected_benchmark_fwd_return = selected_benchmark_fwd_return, #Benchmark Returns
          turnover = turnover #Turnover
          ), order.by = return_calculation_date)


      ##Roll portfolio weights to next period
      rolled_fwd_port_weights_m_d_ref <- roll_fwd_port_weights(
        #Port Weights
        port_weights_m_d_ref = port_weights_m_d_ref,
        #Port Returns
        clean_fwd_return_1m_m_d_ref = clean_fwd_return_1m_m_d_ref,
        #Verbose
        verbose = verbose
      )
    } else {
      fwd_port_returns_m_xts_d_ref <- NULL
      rolled_fwd_port_weights_m_d_ref <- NULL
    }

    ############################

  #Return results
  settle_and_roll_port_results_list <- list(
    filled_transactions_m_d_ref = port_returns_results_list$filled_transactions_m_d_ref,
    port_weights_m_d_ref = port_weights_m_d_ref,
    rolled_fwd_port_weights_m_d_ref = rolled_fwd_port_weights_m_d_ref,
    transactions_costs_m_xts_d_ref = transactions_costs_m_xts_d_ref,
    fwd_port_returns_m_xts_d_ref = fwd_port_returns_m_xts_d_ref
  )

  return(settle_and_roll_port_results_list)


  ####################


}
