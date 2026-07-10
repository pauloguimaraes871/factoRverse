#' @keywords internal
#'
#' @section Workflows:
#' factoRverse organises the factor-investing pipeline into four consistent
#' `config + data objects -> run_*() -> *_results` workflows:
#' \enumerate{
#'   \item \strong{Characteristic portfolios}: rank stocks on a signal and backtest
#'     top-quantile, long-only portfolios with [run_port_backtest()].
#'   \item \strong{Signal selection}: control the factor zoo with multiple-testing
#'     and hierarchical Bayesian methods via [run_ss_backtest()].
#'   \item \strong{Signal blending}: combine selected signals with heuristics or
#'     machine learning via [run_sb_backtest()].
#'   \item \strong{Deployment}: feed the blended score back into [run_port_backtest()]
#'     to build the final, constraint- and cost-aware book.
#' }
#' Data enters through [create_meta_dataframe()] / [create_meta_xts()] and is
#' engineered with the `compute_*()` family and [map_recipe_timewise()].
#'
#' @seealso Useful links:
#'   \itemize{
#'     \item Package website: \url{https://pauloguimaraes871.github.io/factoRverse/}
#'     \item Source and bug reports: \url{https://github.com/pauloguimaraes871/factoRverse}
#'   }
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
