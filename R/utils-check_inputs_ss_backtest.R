#' Perform validation checks on inputs for Signal Selection (SS) Workflow
#'
#' This function validates and checks various inputs required for signal selection in the context of run_ss_backtest function.
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals_and_positions A named vector indicating signals and their corresponding positions (long or short).
#' For example, chosen_signals_and_positions = c(book_yield = "long", vol_36m = "short").
#' @param backtest_returns_xts A xts containing historical backtested returns named according to signals in `signals_m_df`,
#' @param initial_sample_size A numeric indicating the minimum number of observations required to begin the backtest.
#' @param data_availability_cutoff The minimum number of non-NA observations required for a backtest to be considered.
#' @param rebalancing_months Months (numeric) when signal selection should be implemented.
#' @param benchmark_returns_xts A xts with benchmark returns, named accordingly.
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
#' @param signal_significance_threshold A decimal indicating the hypothesis testing negative-alpha null-hypothesis rejection criteria. If one wants to select all signals,
#' provide 1. In any case, a signal being selected demands a significant CAPM alpha.
#' @param enable_theme_representativeness If TRUE, in case a given theme in `signal_themes_m_df` does not have any eligible signal, the signal
#' with highest alpha t-stat will be elected.
#' @param priors_m_df A (meta) data frame with columns including "id", "characteristic/signal", "dates", "theme" (used for clustering in hierarchical bayesian model)
#' and values for alpha (mean and se), beta (mean and se) and sigma, which are used to build priors.
#' @param model_structure A character indicating whether the model should be estimated using a ´partial_pooled´ or ´no_pooled´ approach.
#' @param theme_level_intercept A character specifying the specification of effects of the intercept at the theme level.
#' @param theme_level_slope A character specifying the specification of effects of the slope at the theme level.
#' @param lmer_control Other additional parameters to be passed to `lme4::lmer` function.
#' \itemize{
#' #' \item{lmer_optimizer} A character string specifying the optimizer to be used in the
#' It will be passed to lme4::lmerControl, which will be used in the `lme4::lmer` function.
#' Options include: 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'
#'
#' \item{lmer_optimization_objective} A character string indicating whether estimates should be chosen to optimize the 'REML' criterion or the 'likelihood'.
#' }
#'
#' @param active_returns A character string indicating whether performance metrics should be calculated based on active returns or raw returns. If TRUE,
#' backtest_returns_xts will be adjusted by subtracting the selected market factor proxy in benchmark_returns_xts.
#'
#' @param prior_derivation_control A list of additional parameters to be passed to the `lme4::lmer` function:
#' \itemize{
#' \item{half_t_df} A numeric indicating the degrees of freedom in the half-t distribution to be applied in sd parameters.
#'}
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
  signals_m_df, chosen_signals_and_positions, forced_signals,
  #Backtests and benchmark returns
  backtest_returns_xts, benchmark_returns_xts, market_factor_proxy, active_returns,
  #P-value
  p_correction_method, signal_significance_threshold,
  #Theme Representativeness
  enable_theme_representativeness,
  #Model Structure
  model_structure, theme_level_intercept, theme_level_slope, lmer_control,
  #Priors
  priors_m_df, user_priors, brms_control, prior_derivation_control,
  #Signal Theme
  signal_themes_m_df,
  #Winsorization
  lower_quantile_winsorization, upper_quantile_winsorization
){

  #Structure
  #########################

  #chosen_signals_and_positions
  if(length(chosen_signals_and_positions) == 1){
    stop("More than one signal must be provided in order to run a ss_backtest")
  }

  #signals_m_df
  ###Coercible
  if(!(is_coercible_to_meta_dataframe(signals_m_df))){
    stop("signals_m_df should be coercible to meta_dataframe object")
  }

  if(!all(sapply(signals_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("signals_m_df should contain only numeric columns with non-NAs.")
  }

  ###Check if all chosen_signals are present in signals_m_df
  if(any(!names(chosen_signals_and_positions) %in% colnames(signals_m_df))){
    stop("signal selection not avaiable in signals_m_df")
  }

  ###Check if categorical variables are being selected
  ####Get categorical signals (which are elected by default)
  categorical_signals <- signals_m_df %>% dplyr::select_if(function(x){
    # Convert column to character to unify comparison
    vals <- unique(as.character(x))
    all(vals %in% c("0", "1"))
  }) %>% colnames()

  if(any(names(chosen_signals_and_positions) %in% categorical_signals)){
    warning("Categorical signals included in chosen_signals_and_positions.")
  }

  ###Check for forced_signals presence in signals_m_df
  if(!is.null(forced_signals)){
    if(any(!forced_signals %in% colnames(signals_m_df))){
      warning("forced_signals not available in signals_m_df")
    }
  }

  #initial_sample_size and data_availability_cutoff
  if(any(!is.numeric(initial_sample_size), !is.numeric(data_availability_cutoff))){
    stop("initial_sample_size and data_availability_cutoff must be numeric")
  }

  if(initial_sample_size < data_availability_cutoff){
    stop("initial_sample_size must be greater than or equal to data_availability_cutoff")
  }

  #backtest_returns_xts
  if(!xts::is.xts(backtest_returns_xts)){
    stop("backtest_returns_xts must be a xts object")
  }
  #get dates
  backtest_returns_dates <- zoo::index(backtest_returns_xts)

  if(class(backtest_returns_dates) != "Date"){
    stop("dates in backtest_returns_xts must be of class Date")
  }

  chosen_signals_corrected_positions <- chosen_signals_and_positions
  names(chosen_signals_corrected_positions)[which(chosen_signals_corrected_positions == "short")] <- paste0("low_", names(chosen_signals_and_positions)[which(chosen_signals_and_positions == "short")])

  if(any(!names(chosen_signals_corrected_positions) %in% colnames(backtest_returns_xts))){
    stop("all chosen_signals_and_positions with their corrected position should be present in backtest_returns_xts")
  }

  if(nrow(backtest_returns_xts) < data_availability_cutoff){
    stop("backtest_returns_xts must have at least data_availability_cutoff rows")
  }

  if(nrow(backtest_returns_xts) < initial_sample_size){
    stop("backtest_returns_xts must have at least initial_sample_size rows")
  }

  if(any(!backtest_returns_dates %in% (signals_m_df %>% dplyr::pull(dates)))){
    stop("all dates in backtest_returns_xts must be present in signals_m_df")
  }

  if(any(!signals_m_df %>% dplyr::pull(dates) %>% unique() %in% backtest_returns_dates)){
    stop("all dates in signals_m_df must be present in backtest_returns_xts")
  }

  if(!all(diff(as.numeric(format(backtest_returns_dates, "%Y")) * 12 +
               as.numeric(format(backtest_returns_dates, "%m"))) == 1)){
    stop("backtest_returns_xts must have consecutive dates")
  }


  #benchmark_returns_xts
  if(!xts::is.xts(benchmark_returns_xts)){
    stop("benchmark_returns_xts must be a xts object")
  }
  #get dates
  benchmark_returns_dates <- zoo::index(benchmark_returns_xts)
  if(class(benchmark_returns_dates) != "Date"){
    stop("dates in benchmark_returns_xts must be of class Date")
  }

  if(any(benchmark_returns_dates != backtest_returns_dates)){
    stop("dates in benchmark_returns_xts and backtest_returns_xts must be the same")
  }

  if(any(apply(benchmark_returns_xts, 2, function(x) all(is.na(x))))){
    stop("benchmark_returns_xts must not have any NA values")
  }

  if(!all(diff(as.numeric(format(benchmark_returns_dates, "%Y")) * 12 +
               as.numeric(format(benchmark_returns_dates, "%m"))) == 1)){
    stop("benchmark_returns_xts must have consecutive dates")
  }

  if(!market_factor_proxy %in% colnames(benchmark_returns_xts)){
    stop("market_factor_proxy must be present in benchmark_returns_xts")
  }

  #signal_themes_m_df
  ###Check if signal_themes_m_df contemplates theme column
  if(any(!colnames(signal_themes_m_df) == c("id", "tickers", "dates", "theme"))){
    stop("signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")
  }

  if(enable_theme_representativeness & is.null(signal_themes_m_df)){
    stop("signal_themes_m_df must be provided if enable_theme_representativeness is TRUE")
  }

  ##Check if all signals of signals_m_df are covered and vice-versa
  if(any(!names(chosen_signals_corrected_positions) %in% (signal_themes_m_df %>% dplyr::pull(tickers)))){
    stop("all chosen_signals_and_positions with their corrected position should be present in signal_themes_m_df")
  }

    ###Check if theme column is character
    if(!is.character(signal_themes_m_df %>% dplyr::pull(theme))){
      stop("theme column in signal_themes_m_df must be character")
    }

    ###Check format in signal_themes_m_df
    if(any(grepl("_", signal_themes_m_df %>% dplyr::pull(theme)))){
      stop("No underscores allowed in signal_themes_m_df theme names")
    }

    ###Check if dates in signal_themes_m_df and signals_m_df are the same
    signal_dates_m_vector <- as.Date(unique(signals_m_df %>% dplyr::pull(dates)))
    signal_themes_dates_m_vector <- as.Date(unique(signal_themes_m_df %>% dplyr::pull(dates)))
    if(any(!signal_dates_m_vector %in% signal_themes_dates_m_vector)){
      stop("dates in signal_themes_m_df and signals_m_df must be the same")
    }

    #model_structure
    if(!model_structure %in% c("no_pooled", "partial_pooled")){
      stop("model_structure must be one of 'no_pooled' or 'partial_pooled'")
    }
    if(model_structure == "partial_pooled"){
      #lmer control
      if(!is.null(lmer_control)){
        if(any(!names(lmer_control) %in% c("lmer_optimizer", "lmer_optimization_objective", "hierarchical_p_value_method"))){
          stop("lmer_control should have only 'lmer_optimizer', 'lmer_optimization_objective' or 'hierarchical_p_value_method' as names.")
        }

        if(!is.null(lmer_control$lmer_optimizer) && !lmer_control$lmer_optimizer %in% c("Nelder_Mead", "bobyqa", "nlminbwrap", "nloptwrap")){
          stop("lmer_optimizer should be one of 'Nelder_Mead', 'bobyqa', 'nlminbwrap' or 'nloptwrap'")
        }

        if(!is.null(lmer_control$lmer_optimization_objective) && !lmer_control$lmer_optimization_objective %in% c("likelihood", "REML")){
          stop("lmer_optimization_objective should be one of 'likelihood' or 'REML'")
        }

        if(!is.null(lmer_control$hierarchical_p_value_method) && !lmer_control$hierarchical_p_value_method %in% c("Satterthwaite", "Kenward-Roger", "lme4")){
          stop("hierarchical_p_value_method should be one of 'Satterthwaite', 'Kenward-Roger'  or 'REML'")
        }
        if(any(is.null(theme_level_intercept), is.null(theme_level_slope))){
          stop("For 'partial_pooled' model structure, 'theme_level_intercept' and 'theme_level_slope' must be provided.")
        }
      }
    }
    #p_correction_method
    if(!p_correction_method %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "bayesian", "none")){
      stop("p_correction_method must be one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'bayesian' or 'none'")
    }

    if(p_correction_method == "bayesian" && model_structure == "no_pooled"){
      stop("bayesian p_correction_method is currently only available for partial_pooled model_structure")
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
      if(!all(unique(priors_m_df %>% dplyr::pull(theme)) %in% unique(signal_themes_m_df %>% dplyr::pull(theme))) ||
         !all(unique(signal_themes_m_df %>% dplyr::pull(theme)) %in% unique(priors_m_df %>% dplyr::pull(theme)))
      ){
        stop("themes in priors_m_df and signal_themes_m_df should match")
      }
    }

    #chosen signals and signal positions
    ###Check if there are repeated signals in chosen_signals
    if(!identical(names(chosen_signals_and_positions), unique(names(chosen_signals_and_positions)))){
      stop("each signal must be chosen only once")
    }


    ###Check if bayesian fit can be run
    if(p_correction_method == "bayesian"){
      if (is.null(user_priors) && is.null(priors_m_df)){
        stop("Currently, bayesian fit requires user_priors or priors_m_df.") #This warning is because using uninformative priors has not been tested yet.
      }

      if(is.null(user_priors) && is.null(signal_themes_m_df)){
        stop("bayesian fit requires signal_themes_m_df.")
      }

      if(!all(class(user_priors) == c("brmsprior", "data.frame"))){
        stop("user_priors should contain only brmsprior objects.")
      }

      #Prior derivation control
      if(!is.null(prior_derivation_control)){
        if(any(!names(prior_derivation_control) %in% c("half_t_df"))){
          stop("prior_derivation_control should have only 'half_t_df' as names.")
        }
      }

      #BRMS control
      if(!is.null(brms_control)){
        if(any(!names(brms_control) %in% c("chains", "iter", "warmup", "thin", "seed", "adapt_delta"))){
          stop("brms_control must be a list containing 'chains', 'iter', 'warmup', 'thin', 'seed' and/or 'adapt_delta'.")
        }
        #chains
        if(!is.null(brms_control$chains) && (!is.numeric(brms_control$chains) || brms_control$chains <= 0)){
          stop("chains must be a positive number.")
        }
      #iter
      if(!is.null(brms_control$iter) && (!is.numeric(brms_control$iter) || brms_control$iter <= 0)){
        stop("iter must be a positive number.")
      }
      #warmup
      if(!is.null(brms_control$warmup) && (!is.numeric(brms_control$warmup) || brms_control$warmup <= 0)){
        stop("warmup must be a positive number.")
      }
      #thin
      if(!is.null(brms_control$thin) && (!is.numeric(brms_control$thin) || brms_control$thin <= 0)){
        stop("thin must be a positive number.")
      }
      #adapt_delta
      if(!is.null(brms_control$adapt_delta) && (!is.numeric(brms_control$adapt_delta) || brms_control$adapt_delta <= 0 || brms_control$adapt_delta > 1)){
        stop("adapt_delta should be between 0 and 1.")
      }
      #warmup and iter
      if(!is.null(brms_control$warmup) && (!is.null(brms_control$iter) && brms_control$warmup >= brms_control$iter)){
        stop("warmup must be less than iter.")
      }
    }

    #User priors
    if(!is.null(user_priors)){
      if (!is.data.frame(user_priors)) {
        stop("user_priors must be a data.frame.")
      }

      # Extract themes from signal_themes_m_df
      themes <- unique(signal_themes_m_df %>% dplyr::pull(theme))
      n_themes <- length(themes)

      # Define expected structures based on `model_spec_theme_level`
      model_spec_theme_level <- paste0(theme_level_intercept, "_intercept_", theme_level_slope, "_slope")
      if(!model_spec_theme_level %in% c("random_intercept_fixed_slope", "theme_specific_intercept_fixed_slope", "theme_specific_intercept_theme_specific_slope", "fixed_intercept_fixed_slope")){
        stop("Invalid model specification at theme-level")
      }

      expected_rows <- switch(
        model_spec_theme_level,
        "random_intercept_fixed_slope" = 7,
        "theme_specific_intercept_fixed_slope" = n_themes + 5,
        "theme_specific_intercept_theme_specific_slope" = n_themes * 2 + 4,
        "fixed_intercept_fixed_slope" = 6,
        stop("Invalid model specification at themelevel")
      )

      # Check number of rows
      if (nrow(user_priors) != expected_rows) {
        stop(sprintf("Expected %d rows for theme-level model specification '%s', but got %d.",
                     expected_rows, model_spec_theme_level, nrow(user_priors)))
      }

      # Define validation rules for each model_spec_theme_level
      validate_structure <- switch(
        model_spec_theme_level,
        "random_intercept_fixed_slope" = {
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
        "theme_specific_intercept_fixed_slope" = {
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
        "theme_specific_intercept_theme_specific_slope" = {
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
        "fixed_intercept_fixed_slope" = {
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
        stop(sprintf("user_priors structure is invalid for theme-level model specification '%s'.", model_spec_theme_level))
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


  #active_returns
  if(!is.logical(active_returns)){
    stop("active_returns must be logical")
  }

}

