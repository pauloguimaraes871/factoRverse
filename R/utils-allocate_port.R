#' Allocate Portfolio and Compute Transaction Costs
#'
#' This function performs several steps to allocate a portfolio by merging and rescaling portfolio weights,
#' calculating trade orders, computing transaction costs, and enhancing the portfolio allocation log with
#' additional metrics and (optionally) benchmark weights.
#'
#' @param port_weights_placeholder_m_d_ref A data frame containing the placeholder portfolio weights.
#' @param updated_port_weights_m_lstd_ref A data frame containing the updated portfolio weights.
#' @param stock_universe_m_d_ref A data frame representing the stock universe.
#' @param liquidity_m_d_ref A data frame containing liquidity metrics.
#' @param volatility_m_d_ref A data frame containing volatility metrics.
#' @param main_liquidity_metric The name (or indicator) of the primary liquidity metric to use.
#'   By default, it is expected to be defined externally if not explicitly passed.
#' @param transaction_costs_parameters A list of transaction cost parameters. Expected elements are:
#'   \itemize{
#'     \item \code{strategy_aum} - Strategy Assets Under Management.
#'     \item \code{alpha} - Parameter for indirect transaction cost calculation.
#'     \item \code{lambda} - Parameter for indirect transaction cost calculation.
#'     \item \code{direct_transaction_cost} - Direct transaction cost value.
#'   }
#' @param selected_benchmark_weights_m_d_ref (Optional) A data frame containing benchmark weights.
#'   If provided, these weights will be merged into the portfolio allocation log and missing values set to 0.
#' @param verbose Logical indicating whether to print additional messages (default is \code{TRUE}).
#'
#' @return A list with the following components:
#'   \itemize{
#'     \item \code{port_allocation_log_m_d_ref} - An enhanced portfolio allocation log containing transactions,
#'           costs, and strategic data.
#'     \item \code{port_weights_m_d_ref} - The merged and rescaled portfolio weights.
#'     \item \code{port_costs_d_ref} - The calculated portfolio transaction costs.
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item **Initial Prep:** Extracts transaction cost parameters from \code{transaction_costs_parameters}.
#'   \item **Merge Portfolio Weights:** Uses \code{merge_and_rescale_weights()} to merge placeholder and updated weights,
#'         ensuring alignment with the stock universe.
#'   \item **Calculate Transactions:** Computes trade orders using \code{calculate_trade_orders()} with liquidity and
#'         volatility data.
#'   \item **Compute Transaction Costs:** Uses \code{calculate_transaction_costs()} to determine both indirect and direct costs.
#'   \item **Enhance Allocation Log:** Generates an enhanced allocation log from the transaction costs results.
#'   \item **Merge Benchmark Weights (Optional):** If \code{selected_benchmark_weights_m_d_ref} is provided, it is merged
#'         into the allocation log and any missing \code{bench_weights} are set to 0.
#' }
#'
#' @examples
#' \dontrun{
#'   # Define input objects (for example, using your actual data frames)
#'   result <- allocate_port(
#'     port_weights_placeholder_m_d_ref = port_weights_placeholder,
#'     updated_port_weights_m_lstd_ref = updated_port_weights,
#'     stock_universe_m_d_ref = stock_universe,
#'     liquidity_m_d_ref = liquidity_data,
#'     volatility_m_d_ref = volatility_data,
#'     main_liquidity_metric = "liquidity_metric",
#'     transaction_costs_parameters = list(
#'       strategy_aum = 1000000,
#'       alpha = 0.01,
#'       lambda = 0.02,
#'       direct_transaction_cost = 0.005
#'     ),
#'     selected_benchmark_weights_m_d_ref = benchmark_weights,
#'     verbose = TRUE
#'   )
#'
#'   # Access the enhanced portfolio allocation log:
#'   allocation_log <- result$port_allocation_log_m_d_ref
#'
#'   # Print the allocation log using cat and paste:
#'   cat(paste(capture.output(print(allocation_log)), collapse = "\n"))
#' }
#'
allocate_port <- function(
  #Portfolio Weights
  port_weights_placeholder_m_d_ref, updated_port_weights_m_lstd_ref,
  #Stock Universe
  stock_universe_m_d_ref,
  #Transaction costs
  liquidity_m_d_ref, volatility_m_d_ref, main_liquidity_metric = main_liquidity_metric, ##Vol and Liquidity
  transaction_costs_parameters, ##BARRA model parameters and direct transaction cost
  #Selected bench weights
  selected_benchmark_weights_m_d_ref,
  #Misc
  verbose = TRUE
){

  #Initial prep
  ####################

  ##Get transaction_cost parameters
  strategy_aum <- transaction_costs_parameters$strategy_aum
  alpha <- transaction_costs_parameters$alpha
  lambda <- transaction_costs_parameters$lambda
  direct_transaction_cost <- transaction_costs_parameters$direct_transaction_cost

  ####################

  #Merge stock weights
  ####################
  ##Get merged portfolio and tickers that were listed and delisted
  merged_port_results_list <- merge_and_rescale_weights(
    #Portfolios EOP and BOP
    port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
    #Stock universe
    stock_universe_m_d_ref = stock_universe_m_d_ref,
    #Selected Benchmark Weights
    selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,
    #Verbose
    verbose = verbose
  )

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
    #Strategy AuM
    strategy_aum = strategy_aum,
    #Verbose
    verbose = verbose
  )

  ####################

  #Create porfolio_allocation_log (an enhanced transactions_and_costs_m_d_ref, containing more strategic data)
  ####################
  transactions_log_m_d_ref <- transaction_costs_results_list$transactions_and_costs_m_d_ref

  ####################

  #Return results
  ####################
  port_allocation_results_list <- list(
    transactions_log_m_d_ref = transactions_log_m_d_ref,
    port_weights_m_d_ref = merged_port_results_list$port_weights_m_d_ref,
    port_costs_d_ref = transaction_costs_results_list$port_costs_d_ref
  )

  return(port_allocation_results_list)

  ####################
}
