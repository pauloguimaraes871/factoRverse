#' Perform validation checks on inputs for Signal Selection (SS) Workflow
#'
#' This function validates and checks various inputs required for signal selection in the context of run_ss_backtest function.
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals A vector with user-defined characteristics to be considered.
#' @param signal_positions A named vector with same length and names as chosen_signals describing whether positions should be taken "long" or "short".
#' @param backtest_returns_df A data frame with a 'dates' column and remaining columns named according to signals in signals_m_df, containing historical backtested returns.
#' @param initial_sample_size A numeric indicating the minimum number of observations required to begin the backtest.
#' @param data_availability_cutoff The minimum number of non-NA observations required for a backtest to be considered.
#' @param rebalancing_months Months (numeric) when signal selection should be implemented.
#' @param selected_benchmark_returns_df A data frame with a 'dates' column and a column with benchmark returns, named accordingly.
#' @param p_correction_method The method for p-value correction. Possible options are:
#'\itemize{
#'  \item{"none"}: No correction.
#'  \item{"bayesian"}: When bayesian is set, a hierarchical mixed-effects bayesian linear model is fitted to the data, using the `brms` package,
#'  which is an interface to the `Stan` probabilistic programming language.
#'  The user can also choose one of the following frequentist methods, which will control Family-Wise Error Rate (FWER) or the False Discovery Rate (FDR).
#'  FDR is less stringent than FWER.
#'  For FWER, possible options are:
#'  \item{"bonferroni"}: Bonferroni correction, which is dominated by Holm's method.
#'  \item{"holm"}: Holm's (1979) method.
#'  \item{"hochberg"}: Hochberg's (1988) method, valid when hypothesis tests are independent or non-negatively associated. Less powerful than Hommel's (1988) method, but
#'  faster to compute.
#'  \item{"hommel"}: Hommel's (1988) method, also valid when hypothesis tests are independent or non-negatively associated, but is more powerful than Hochberg (1988).
#'  For FDR, possible options are:
#'  \item{"BH" or "fdr"}: Benjamini-Hochberg (1995) procedure.
#'  \item{"BY"}: Benjamini-Yekutieli (2001) procedure.
#'  }
#' @param signal_significance_threshold A decimal indicating the hypothesis testing zero-alpha null-hypothesis rejection criteria. If one wants to select all chosen_signals,
#' provide 1. In any case, a signal being selected demands a significant CAPM alpha.
#' @param enable_theme_representativeness If TRUE, in case a given theme in `signal_themes_m_df` does not have any eligible signal, the signal
#' with highest alpha t-stat will be elected.
#' @param priors_m_df A (meta) data frame with columns including "id", "characteristic/signal", "dates", "theme" (used for clustering in hierarchical bayesian model)
#' and values for alpha (mean and se), beta (mean and se) and sigma, which are used to build priors.
#' @param model_spec_theme_level A character string specifying the desired Bayesian model structure.
#'   Options include:
#'   - `"random_intercept"`: Includes random effects for the intercept at the theme level.
#'   - `"fixed_intercepts"`: Uses fixed intercepts for each theme.
#'   - `"fixed_intercepts_and_slopes"`: Includes fixed intercepts and slopes for each theme.
#'   - `"none"`: Omits theme-level intercepts but includes random effects at the theme:signal level.
#'
#' @param prior_derivation_control A list of additional parameters to be passed to the `lme4::lmer` function:
#' \itemize{
#' \item{half_t_df} A numeric indicating the degrees of freedom in the half-t distribution to be applied in sd parameters.
#'
#' \item{lmer_optimizer} A character string specifying the optimizer to be used in the `lme4::lmer` function.
#' It will be passed to lme4::lmerControl, which will be used in the `lme4::lmer` function.
#' Options include: 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'
#'
#' \item{lmer_optimization_objective} A character string indicating whether estimates should be chosen to optimize the 'REML' criterion or the 'likelihood'.
#' }
#'
#' @param brms_control Other additional parameters to be passed to brms::brm function:
#' \itemize{
#' \item{chains} Integer.
#' The number of Markov chains to run for the MCMC sampling. Default is `4`.
#'
#' \item{iter} Integer.
#' The total number of iterations per chain for the MCMC sampling. Default is `2000`.
#'
#' \item{warmup} Integer.
#' The number of warmup (burn-in) iterations per chain for the MCMC sampling. Default is `floor(iter / 2)`.
#'
#' \item{thin} Integer.
#' The thinning interval for MCMC sampling. Default is `1`.
#'
#' \item{seed} Integer or `NA`.
#' The seed for random number generation to ensure reproducibility. Set to a specific integer for reproducible results or `NA` for random seeding. Default is `NA`.
#'
#' \item{adapt_delta] Numeric.
#' The target acceptance probability for the Hamiltonian Monte Carlo sampler. Higher values can lead to better convergence at the cost of slower sampling. Must be between `0` and `1`. Default is `0.99`.
#' }
#'
#' @param parallel Logical.
#'   Indicates whether to enable parallel computation using the `future` package. Only avaialable for bayesian model. Default is `TRUE`.
#' @param signal_themes_m_df A (meta) data frame with id, tickers ("signals") and dates column contemplating all signals in `signals_m_df` and a "theme" column providing group membership for each signal, which is needed
#' for defining clusters in bayesian hierarchical model.
#'
#' @param upper_quantile_winsorization Numeric value for upper winsorization.
#' @param lower_quantile_winsorization Numeric value for lower winsorization.
#'
#' @export
check_inputs_ss_backtest <- function(
  #Dates
  initial_sample_size, rebalancing_months, data_availability_cutoff, split_method,
  #Signals
  signals_m_df, chosen_signals, signal_positions,
  #Backtests and benchmark returns
  backtest_returns_df, benchmark_returns_df, market_factor_proxy,
  #P-value
  p_correction_method, signal_significance_threshold,
  #Theme Representativeness
  enable_theme_representativeness,
  #Priors
  priors_m_df, user_priors, model_spec_theme_level, brms_control, prior_derivation_control,
  #Signal Theme
  signal_themes_m_df,
  #Winsorization
  lower_quantile_winsorization, upper_quantile_winsorization
  ){

  #Structure
  #########################

  #signals_m_df
  ###Coercible
  if(!(is_coercible_to_meta_dataframe(signals_m_df))){
    stop("signals_m_df should be coercible to meta_dataframe object")
  }

  if(!all(sapply(signals_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("signals_m_df should contain only numeric columns with non-NAs.")
  }

  ###Check if all chosen_signals are present in signals_m_upd_ref
  if(any(!chosen_signals %in% colnames(signals_m_df))){
    stop("signal selection not avaiable in signals_m_df")
  }

  #initial_sample_size and data_availability_cutoff
  if(any(!is.numeric(initial_sample_size), !is.numeric(data_availability_cutoff))){
    stop("initial_sample_size and data_availability_cutoff must be numeric")
  }

  if(initial_sample_size < data_availability_cutoff){
    stop("initial_sample_size must be greater than or equal to data_availability_cutoff")
  }

  #backtest_returns_df
  if(colnames(backtest_returns_df)[1] != "dates"){
    stop("backtest_returns_df must have a 'dates' first column")
  }

  if(nrow(backtest_returns_df) < data_availability_cutoff){
    stop("backtest_returns_df must have at least data_availability_cutoff rows")
  }

  if(nrow(backtest_returns_df) < initial_sample_size){
    stop("backtest_returns_df must have at least initial_sample_size rows")
  }

  if(any(!backtest_returns_df$dates %in% signals_m_df$dates)){
    stop("all dates in backtest_returns_df must be present in signals_m_df")
  }

  if(!all(diff(as.numeric(format(backtest_returns_df$dates, "%Y")) * 12 +
              as.numeric(format(backtest_returns_df$dates, "%m"))) == 1)){
    stop("backtest_returns_df must have consecutive dates")
  }

  #benchmark_returns_df
  if(any(benchmark_returns_df$dates != backtest_returns_df$dates)){
    stop("dates in benchmark_returns_df and backtest_returns_df must be the same")
  }

  if(any(apply(benchmark_returns_df, 2, function(x) all(is.na(x))))){
    stop("benchmark_returns_df must not have any NA values")
  }

  if(!all(diff(as.numeric(format(benchmark_returns_df$dates, "%Y")) * 12 +
               as.numeric(format(benchmark_returns_df$dates, "%m"))) == 1)){
    stop("benchmark_returns_df must have consecutive dates")
  }

  #signal_themes_m_df
  ###Check if signal_themes_m_df contemplates theme column
  if(any(!colnames(signal_themes_m_df) == c("id", "tickers", "dates", "theme"))){
    stop("signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")
  }

  if(enable_theme_representativeness & is.null(signal_themes_m_df)){
    stop("signal_themes_m_df must be provided if enable_theme_representativeness is TRUE")
  }

  ###Check if theme column is character
  if(!is.character(signal_themes_m_df$theme)){
    stop("theme column in signal_themes_m_df must be character")
  }

  ###Check if dates in signal_themes_m_df and signals_m_df are the same
  signal_dates_m_vector <- as.Date(unique(signals_m_df$dates))
  signal_themes_dates_m_vector <- as.Date(unique(signal_themes_m_df$dates))
  if(any(!signal_dates_m_vector %in% signal_themes_dates_m_vector)){
    stop("dates in signal_themes_m_df and signals_m_df must be the same")
  }

  #p_correction_method
  if(!p_correction_method %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "bayesian", "none")){
    stop("p_correction_method must be one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'bayesian' or 'none'")
  }

  #signal_significance_threshold
  if(any(signal_significance_threshold < 0, signal_significance_threshold > 1)){
    stop("signal_significance_threshold must be between 0 and 1")
  }

  #priors_m_df
  if(!is.null(priors_m_df)){
  ###Coercible
  if(!(is_coercible_to_meta_dataframe(priors_m_df))){
    stop("priors_m_df should be coercible to meta_dataframe object")
  }

  ###Check if all themes are present
  if(!all(unique(priors_m_df$theme) %in% unique(signal_themes_m_df$theme)) ||
     !all(unique(signal_themes_m_df$theme) %in% unique(priors_m_df$theme))
     ){
    stop("themes in priors_m_df and signal_themes_m_df should match")
  }
  }

  #chosen signals and signal positions
  ###Check if there are repeated signals in chosen_signals
  if(!identical(chosen_signals, unique(chosen_signals))){
    stop("each signal must be chosen only once")
  }

  ###Check if all signals have a position
  if(!identical(chosen_signals, names(signal_positions))){
    stop("all chosen signals should have a matching position in signal_positions.")
  }

  ###Check if bayesian fit can be run
  if(p_correction_method == "bayesian"){
    if(is.null(user_priors) && is.null(priors_m_df)){
      stop("bayesian fit requires priors_m_df or user_priors.")
    }

    if(is.null(user_priors) && is.null(signal_themes_m_df)){
      stop("bayesian fit requires signal_themes_m_df.")
    }

    if(!all(sapply(user_priors, function(x) class(x) == "brmsprior"))){
      stop("user_priors should contain only brmsprior objects.")
    }

    #Prior derivation control
    if(!is.null(prior_derivation_control)){
      if(any(!names(prior_derivation_control) %in% c("half_t_df", "lmer_optimizer", "lmer_optimization_objective"))){
        stop("prior_derivation_control should have only 'half_t_df', 'lmer_optimizer' and 'lmer_optimization_objective' as names.")
      }

      if(!is.null(prior_derivation_control$lmer_optimizer) && !prior_derivation_control$lmer_optimizer %in% c("Nelder_Mead", "bobyqa", "nlminbwrap", "nloptwrap")){
        stop("lmer_optimizer should be one of 'Nelder_Mead', 'bobyqa', 'nlminbwrap' or 'nloptwrap'")
      }

      if(!is.null(prior_derivation_control$lmer_optimization_objective) && !prior_derivation_control$lmer_optimization_objective %in% c("likelihood", "REML")){
        stop("lmer_optimization_objective should be one of 'likelihood' or 'REML'")
      }
    }

    #BRMS control
    if(!is.null(brms_control)){
      if(any(!names(brms_control) %in% c("chains", "iter", "warmup", "thin", "seed", "adapt_delta"))){
        stop("brms_control must be a list containing 'chains', 'iter', 'warmup', 'thin', 'seed' and/or 'adapt_delta'.")
      }
      #chains
      if(!is.null(brms_control$chains) && !is.numeric(brms_control$chains) || brms_control$chains <= 0){
        stop("chains must be a positive number.")
      }
      #iter
      if(!is.null(brms_control$iter) && !is.numeric(brms_control$iter) || brms_control$iter <= 0){
        stop("iter must be a positive number.")
      }
      #warmup
      if(!is.null(brms_control$warmup) && !is.numeric(brms_control$warmup) || brms_control$warmup <= 0){
        stop("warmup must be a positive number.")
      }
      #thin
      if(!is.null(brms_control$thin) && !is.numeric(brms_control$thin) || brms_control$thin <= 0){
        stop("thin must be a positive number.")
      }
      #seed
      if(!is.null(brms_control$seed) && !is.numeric(brms_control$seed) || brms_control$seed <= 0){
        stop("thin must be a positive number.")
      }
      #adapt_delta
      if(!is.null(brms_control$adapt_delta) && !is.numeric(brms_control$adapt_delta) || brms_control$adapt_delta <= 0 || brms_control$adapt_delta > 1){
        stop("adapt_delta should be between 0 and 1.")
      }
      #warmup and iter
      if(!is.null(brms_control$warmup) && !is.null(brms_control$iter) && brms_control$warmup >= brms_control$iter){
        stop("warmup must be less than iter.")
      }
    }

    #User priors
    if(!is.null(user_priors)){
      if (!is.data.frame(user_priors)) {
        stop("user_priors must be a data.frame.")
      }

      # Extract themes from signal_themes_m_df
      themes <- unique(signal_themes_m_df$theme)
      n_themes <- length(themes)

      # Define expected structures based on `model_spec_theme_level`
      expected_rows <- switch(
        model_spec_theme_level,
        "random_intercept" = 7,
        "fixed_intercepts" = n_themes + 5,
        "fixed_intercepts_and_slopes" = n_themes * 2 + 4,
        "none" = 6,
        stop("Invalid model_spec_theme_level.")
      )

      # Check number of rows
      if (nrow(user_priors) != expected_rows) {
        stop(sprintf("Expected %d rows for model_spec_theme_level '%s', but got %d.",
                     expected_rows, model_spec_theme_level, nrow(user_priors)))
      }

      # Define validation rules for each model_spec_theme_level
      validate_structure <- switch(
        model_spec_theme_level,
        "random_intercept" = {
          required_rows <- data.frame(
            class = c("Intercept", "b", "sd", "sd", "sd", "sigma", "cor"),
            coef = c("", "market_factor_proxy", "Intercept", "market_factor_proxy", "Intercept", "", ""),
            group = c("", "", "theme:tickers", "theme:tickers", "theme", "", "")
          )
          all(apply(required_rows, 1, function(row) {
            any(user_priors$class == row["class"] &
                  user_priors$coef == row["coef"] &
                  user_priors$group == row["group"])
          }))
        },
        "fixed_intercepts" = {
          intercept_rows <- data.frame(
            class = "b",
            coef = sprintf("theme%s", themes),
            group = ""
          )
          common_rows <- data.frame(
            class = c("b", "sd", "sd", "sigma", "cor"),
            coef = c("market_factor_proxy", "Intercept", "market_factor_proxy", "", ""),
            group = c("", "theme:tickers", "theme:tickers", "", "")
          )
          all(apply(rbind(intercept_rows, common_rows), 1, function(row) {
            any(user_priors$class == row["class"] &
                  user_priors$coef == row["coef"] &
                  user_priors$group == row["group"])
          }))
        },
        "fixed_intercepts_and_slopes" = {
          intercept_rows <- data.frame(
            class = "b",
            coef = sprintf("theme%s", themes),
            group = ""
          )
          slope_rows <- data.frame(
            class = "b",
            coef = sprintf("theme%s:market_factor_proxy", themes),
            group = ""
          )
          common_rows <- data.frame(
            class = c("sd", "sd", "sigma", "cor"),
            coef = c("Intercept", "market_factor_proxy", "", ""),
            group = c("theme:tickers", "theme:tickers", "", "")
          )
          all(apply(rbind(intercept_rows, slope_rows, common_rows), 1, function(row) {
            any(user_priors$class == row["class"] &
                  user_priors$coef == row["coef"] &
                  user_priors$group == row["group"])
          }))
        },
        "none" = {
          required_rows <- data.frame(
            class = c("Intercept", "b", "sd", "sd", "sigma", "cor"),
            coef = c("", "market_factor_proxy", "Intercept", "market_factor_proxy", "", ""),
            group = c("", "", "theme:tickers", "theme:tickers", "", "")
          )
          all(apply(required_rows, 1, function(row) {
            any(user_priors$class == row["class"] &
                  user_priors$coef == row["coef"] &
                  user_priors$group == row["group"])
          }))
        }
      )

      if (!validate_structure) {
        stop(sprintf("user_priors structure is invalid for model_spec_theme_level '%s'.", model_spec_theme_level))
      }

    }

 }

  #Check structure of rebalancing_months
  if(!is.numeric(rebalancing_months)){
    stop("rebalancing_months should be numeric.")
  }

  if(rebalancing_months < 0 || rebalancing_months > 12){
    stop("rebalancing_months should be between 1 and 12.")
  }

  #market_factor_proxy
  if(!is.character(market_factor_proxy)){
    stop("market_factor_proxy must be character")
  }

  if(!market_factor_proxy %in% colnames(benchmark_returns_df)){
    stop("market_factor_proxy must be present in benchmark_returns_df")
  }



}

