#' Run a Signal Selection Backtest
#'
#' This function performs iterative signal selection based on frequentist or Bayesian methods, which are applied to portfolio returns backtests to identify which signals can be considered significant in stock-level return prediction.
#'
#' To determine whether a signal matters in cross-sectional predictability, the literature typically runs regressions of signal portfolios against a benchmark factor model (e.g., CAPM),
#' computing alphas and corresponding t-stats. Due to the large number of signals identified in the literature (the "factor zoo"), methods to control for multiple testing are often advocated.
#'
#' @param config An object of class `ss_backtest_config` specifying the backtest configuration.
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.

#' @param backtest_returns_m_xts A xts containing historical backtested returns named according to signals in `signals_m_df`,
#' @param benchmark_returns_m_xts A xts with benchmark returns, named accordingly.
#' @param priors_m_df A (meta) data frame with columns including "id", "ticker", "dates", "theme" (used for clustering in the Bayesian hierarchical model),
#' and values for active_return, bench_return, alpha (mean and se), beta (mean and se), and sigma. Data should be exogenous, as it will be used to set priors for the hierarchical Bayesian model.
#' @param signal_themes_m_df A (meta) data frame with "id", "tickers" ("signals"), and "dates" columns, including all signals in `signals_m_df`, and a "theme" column providing group membership for each signal.
#' @param upper_quantile_winsorization Numeric value for upper winsorization.
#' @param lower_quantile_winsorization Numeric value for lower winsorization.
#' @param verbose A boolean indicating whether to print messages.
#' @param parallel A boolean indicating whether to run the backtest in parallel.
#' @param ... Additional arguments (not used in this method).
#' @export
setGeneric("run_ss_backtest", function(config, signals_m_df, backtest_returns_m_xts, port_backtest_cohort, benchmark_returns_m_xts, signal_themes_m_df, ...) {
  standardGeneric("run_ss_backtest")
})

#' @describeIn run_ss_backtest Run Signal Selection Backtest
#' @description Runs signal selection backtest given bayesian or frequentist approaches for p-value correction. This acts as a wrapper for backtest_returns_m_xts
#' @param config An object of class `ss_backtest_config` specifying the backtest configuration.
#' @export
setMethod("run_ss_backtest",
          signature(config = "ss_backtest_config", signals_m_df = "meta_dataframe", backtest_returns_m_xts = "missing", port_backtest_cohort = "port_backtest_cohort", benchmark_returns_m_xts = "meta_xts",
                    signal_themes_m_df = "meta_dataframe"),

          function(config, signals_m_df, port_backtest_cohort, benchmark_returns_m_xts, signal_themes_m_df,
                   priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                   verbose = TRUE, parallel = TRUE, winsorization_probs = c(0.025, 0.975)){


            ## Extract backtest_returns_m_xts from port_backtest_cohort
            #######################
            backtest_returns_m_xts <- extract_backtest_returns_m_xts(
              port_backtest_cohort = port_backtest_cohort,
              signals_m_df = signals_m_df, benchmark_returns_m_xts = benchmark_returns_m_xts,
              verbose = verbose
              )

            #######################

            ##Run SS Backtest
            #######################
            ss_backtest_results <- run_ss_backtest(
              config = config, signals_m_df = signals_m_df,
              backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, signal_themes_m_df = signal_themes_m_df,
              priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
              verbose = verbose, parallel = parallel, winsorization_probs = winsorization_probs
              )

            return(ss_backtest_results)
            #######################


          })



#' @describeIn run_ss_backtest Run Signal Selection Backtest
#' @description Runs signal selection backtest given bayesian or frequentist approaches for p-value correction. This acts as a wrapper for run_ss_backtest_internal
#' @param config An object of class `ss_backtest_config` specifying the backtest configuration.
#' @export
setMethod("run_ss_backtest",
          signature(config = "ss_backtest_config", signals_m_df = "meta_dataframe", backtest_returns_m_xts = "meta_xts", port_backtest_cohort = "missing", benchmark_returns_m_xts = "meta_xts",
                    signal_themes_m_df = "meta_dataframe"),

          function(config, signals_m_df, backtest_returns_m_xts, benchmark_returns_m_xts, signal_themes_m_df,
                   priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                   verbose = TRUE, parallel = TRUE, winsorization_probs = c(0.025, 0.975)){

            ## Initial Preparations
            #######################
            #Assign default values for internal function (to avoid getting vars from global environ)
            active_returns <- TRUE
            split_method <- "expanding"
            market_factor_proxy <- "IBOV"
            p_correction_method <- "none"
            signal_significance_threshold <- 0.05
            enable_theme_representativeness <- TRUE
            user_priors <- NULL
            model_structure <- "no_pooled"
            theme_level_intercept <- NULL
            theme_level_slope <- NULL
            brms_control <- list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.80, warmup = 1000)
            prior_derivation_control <- list(half_t_df = 30)
            lmer_control <- list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite")


            #Winsorization probs
            lower_quantile_winsorization <- min(winsorization_probs)
            upper_quantile_winsorization <- max(winsorization_probs)

            ##Set missing values to TRUE
            if(missing(verbose)){
              verbose <- TRUE
            }
            if(missing(parallel)){
              parallel <- TRUE
            }
            #######################

            #Get data from S4 objects
            #########################
            ##chosen_signals_and_positions
            chosen_signals_and_positions <- config@chosen_signals_and_positions
              ##Convert 'all' chosen_signals_and_positions
              if(length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions == "all"){
                if (verbose) cat("According to user choice, SS backtest will contemplate all signals in signals_m_df, assuming a 'long' position.")
                chosen_signals <- colnames(signals_m_df@data)[-c(1:3)] #Get all signals in signals_m_df
                chosen_signals_and_positions <- rep("long", length(chosen_signals)) #Set all positions as 'long'
                names(chosen_signals_and_positions) <- chosen_signals
              } else {
                if (verbose) cat("According to user choice, SS backtest will contemplate the following signals in signals_m_df:")
                if (verbose) print(chosen_signals_and_positions)
              }

            ##signals_m_df
            signals_workflow <- signals_m_df@workflow #Get workflow
            signals_object_name <- signals_m_df@meta_dataframe_name #Get mdf name
            signals_m_df <- signals_m_df@data #Get signals_m_df

            ##signal_themes_m_df
            signal_themes_workflow <- signal_themes_m_df@workflow #Get workflow
            signal_themes_object_name <- signal_themes_m_df@meta_dataframe_name #Get mdf name
            signal_themes_m_df <- signal_themes_m_df@data #Get signal_themes_m_df

            ##custom_signal_universe_metrics_m_df
            if(!is.null(custom_signal_universe_metrics_m_df)){
              custom_signal_universe_metrics_workflow <- custom_signal_universe_metrics_m_df@workflow #Get workflow
              custom_signal_universe_metrics_object_name <- custom_signal_universe_metrics_m_df@meta_dataframe_name #Get mdf name
              custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df@data #Get custom_signal_universe_metrics_m_df
            }

            ##backtest_returns_m_xts
            backtest_returns_workflow <- backtest_returns_m_xts@workflow #Get workflow
            backtest_returns_object_name <- backtest_returns_m_xts@meta_xts_name #Get mxts name
            backtest_returns_m_xts <- backtest_returns_m_xts@data #Get backtest_returns_m_xts

            ##benchmark_returns_m_xts
            benchmark_returns_workflow <- benchmark_returns_m_xts@workflow #Get workflow
            benchmark_returns_object_name <- benchmark_returns_m_xts@meta_xts_name #Get mxts name
            benchmark_returns_m_xts <- benchmark_returns_m_xts@data #Get benchmark_returns_m_xts


            ##Get general info from contig
            initial_sample_size <- config@initial_sample_size
            rebalancing_months <- config@rebalancing_months
            active_returns <- config@active_returns
            split_method <- config@split_method
            config_name <- config@config_name
            alpha_test_strategy <- config@alpha_test_strategy

            ##Get general info from alpha_test_strategy
            signal_significance_threshold <- alpha_test_strategy@signal_significance_threshold
            p_correction_method <- alpha_test_strategy@p_correction_method
            market_factor_proxy <- alpha_test_strategy@market_factor_proxy
            enable_theme_representativeness <- alpha_test_strategy@enable_theme_representativeness
            model_structure <- alpha_test_strategy@model_structure
            theme_level_intercept <- alpha_test_strategy@theme_level_intercept
            theme_level_slope <- alpha_test_strategy@theme_level_slope
            lmer_control <- if(!is.null(alpha_test_strategy@lmer_control)) alpha_test_strategy@lmer_control else lmer_control


            if(p_correction_method == "bayesian"){
              #Get information that is specific to bayesian approach

              ##priors_m_df
              if(!is.null(priors_m_df)){
                priors_workflow <- priors_m_df@workflow #Get workflow
                priors_object_name <- priors_m_df@meta_dataframe_name #Get mdf name
                priors_m_df <- priors_m_df@data #Get priors_m_df
              }

              bayesian_model_parameters <- alpha_test_strategy@bayesian_model_parameters
              user_priors <- bayesian_model_parameters@user_priors


              brms_control <- if(!is.null(bayesian_model_parameters@brms_control)) bayesian_model_parameters@brms_control else brms_control
              prior_derivation_control <- if(!is.null(bayesian_model_parameters@prior_derivation_control)) bayesian_model_parameters@prior_derivation_control else prior_derivation_control
            }

            #########################

            #Run SS Backtest
            #########################
            ss_backtest_results <- run_ss_backtest_internal(
              #Signals Data
              signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
              #Return Data
              backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, market_factor_proxy = market_factor_proxy,
              #Signal Themes Data
              signal_themes_m_df,
              #Training Scheme
              initial_sample_size = initial_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
              #Alpha Test Strategy
              p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold, enable_theme_representativeness = enable_theme_representativeness,
              model_structure = model_structure, theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope, lmer_control = lmer_control,
              active_returns = active_returns,
              #Bayesian Model Parameters
              priors_m_df = priors_m_df, user_priors = user_priors,
              brms_control = brms_control, prior_derivation_control = prior_derivation_control,
              #Misc
              lower_quantile_winsorization =  lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization, verbose = verbose, parallel = parallel
              )
            #########################

            #Adjust SS Backtest WF
            ###########################

            ##Add config
            ss_backtest_results@ss_backtest_config <- config

            #Add workflows, config_name and objects for target and features
            ###Signals
            ss_backtest_results@ss_backtest_workflow$signals_object_name <- signals_object_name
            ss_backtest_results@ss_backtest_workflow$signals_workflow <- signals_workflow

            ###Signal Themes
            ss_backtest_results@ss_backtest_workflow$signal_themes_object_name <- signal_themes_object_name
            ss_backtest_results@ss_backtest_workflow$signal_themes_workflow <- signal_themes_workflow

            ###Priors Themes
            if(!is.null(priors_m_df)){
            ss_backtest_results@ss_backtest_workflow$priors_object_name <- priors_object_name
            ss_backtest_results@ss_backtest_workflow$priors_workflow <- priors_workflow
            }

            ###Custom Signal Universe Metrics
            if(!is.null(custom_signal_universe_metrics_m_df)){
              ss_backtest_results@ss_backtest_workflow$custom_signal_universe_metrics_object_name <- custom_signal_universe_metrics_object_name
              ss_backtest_results@ss_backtest_workflow$custom_signal_universe_metrics_workflow <- custom_signal_universe_metrics_workflow
            }

            ###Backtest Returns
            ss_backtest_results@ss_backtest_workflow$backtest_returns_object_name <- backtest_returns_object_name
            ss_backtest_results@ss_backtest_workflow$backtest_returns_workflow <- backtest_returns_workflow

            ###Benchmark Returns
            ss_backtest_results@ss_backtest_workflow$benchmark_returns_object_name <- benchmark_returns_object_name
            ss_backtest_results@ss_backtest_workflow$benchmark_returns_workflow <- benchmark_returns_workflow


            ###IDs
            ss_backtest_results@ss_backtest_workflow$config_name <- config@config_name
            ss_backtest_results@ss_backtest_workflow$backtest_identifier <-
              paste0("c:",config@config_name, "_s:", signals_object_name, "_st:", signal_themes_object_name)
              #Add for priors_m_df
              if(!is.null(priors_m_df)){
                ss_backtest_results@ss_backtest_workflow$backtest_identifier <-
                  paste0(ss_backtest_results@ss_backtest_workflow$backtest_identifier, "_p:", priors_object_name)
              }
            ss_backtest_results@backtest_identifier <- ss_backtest_results@ss_backtest_workflow$backtest_identifier

            ###Workflow and names for signal_universe_m_df
              ####Workflow
              ss_backtest_results@signal_universe_m_df@workflow <- list(paste0("signal_universe_m_df result of ", ss_backtest_results@backtest_identifier))
              ss_backtest_results@final_signal_universe_m_d_ref@workflow <- list(paste0("final_signal_universe_m_d_ref result of ", ss_backtest_results@backtest_identifier))
              ####Names
              ss_backtest_results@signal_universe_m_df@meta_dataframe_name <- paste0("ss_backtest___:",ss_backtest_results@ss_backtest_workflow$backtest_identifier)
              ss_backtest_results@final_signal_universe_m_d_ref@meta_dataframe_name <- paste0("ss_backtest___:",ss_backtest_results@ss_backtest_workflow$backtest_identifier)

              ###Call
              ss_backtest_results@ss_backtest_workflow$call <- sys.call(-2)

              return(ss_backtest_results)

          }
)

#' @describeIn run_ss_backtest Run Signal Selection Backtest
#' Run a Signal Selection Backtest
#'
#' This function performs iterative signal selection based on frequentist or Bayesian methods, which are applied to portfolio returns backtests to identify which signals can be considered significant in stock-level return prediction.
#'
#' To determine whether a signal matters in cross-sectional predictability, the literature typically runs regressions of signal portfolios against a benchmark factor model (e.g., CAPM), computing alphas and corresponding t-stats. Due to the large number of signals identified in the literature (the "factor zoo"), methods to control for multiple testing are often advocated.
#'
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals_and_positions A named vector indicating signals and their corresponding positions (long or short).
#' For example, chosen_signals_and_positions = c(book_yield = "long", vol_36m = "short").
#' @param backtest_returns_m_xts A xts containing historical backtested returns named according to signals in `signals_m_df`,
#' @param initial_sample_size A numeric indicating the minimum number of observations required to begin the backtest.
#' @param  The minimum number of non-NA observations required for a backtest to be considered.
#' @param split_method The method used for splitting the data, either "expanding" or "rolling" (default is "expanding").
#' @param rebalancing_months Numeric months when signal selection should be implemented.
#' @param benchmark_returns_m_xts A xts with benchmark returns, named accordingly.
#' @param p_correction_method The method for p-value correction. Possible options are:
#' \itemize{
#'   \item \strong{"none"}: No correction.
#'   \item \strong{"bayesian"}: A hierarchical mixed-effects Bayesian linear model is fitted to the data using the `brms` package.
#'
#'   The user can also choose one of the following frequentist methods, which control the Family-Wise Error Rate (FWER) or the False Discovery Rate (FDR). FDR is less stringent than FWER.
#'
#'   For FWER, possible options are:
#'   \itemize{
#'     \item \strong{"bonferroni"}: Bonferroni correction.
#'     \item \strong{"holm"}: Holm's (1979) method.
#'     \item \strong{"hochberg"}: Hochberg's (1988) method.
#'     \item \strong{"hommel"}: Hommel's (1988) method.
#'   }
#'
#'   For FDR, possible options are:
#'   \itemize{
#'     \item \strong{"BH"} or \strong{"fdr"}: Benjamini-Hochberg (1995) procedure.
#'     \item \strong{"BY"}: Benjamini-Yekutieli (2001) procedure.
#'   }
#' }
#' @param signal_significance_threshold A decimal indicating the hypothesis testing negative-alpha null-hypothesis rejection criteria. If you want to select all `chosen_signals`, provide 1.
#' @param enable_theme_representativeness If TRUE, in case a given theme in `signal_themes_m_df` does not have any eligible signal, the signal
#' with highest alpha t-stat will be elected.
#' @param priors_m_df A (meta) data frame with columns including "id", "ticker", "dates", "theme" (used for clustering in the Bayesian hierarchical model),
#' and values for active_return, bench_return, alpha (mean and se), beta (mean and se), and sigma. Data should be exogenous, as it will be used to set priors for the hierarchical Bayesian model.
#' @param signal_themes_m_df A (meta) data frame with "id", "tickers" ("signals"), and "dates" columns, including all signals in `signals_m_df`, and a "theme" column providing group membership for each signal.
#' @param user_priors An object with user-defined priors for the hierarchical Bayesian model, used when `priors_type = "user"`.
#' Each element of the list represents a theme and should contain priors set with brms::set_prior (class `brmsprior`). For instance, setting priors only for
#' alpha and beta global parameters can be done with:
#' c(brms::set_prior("normal(0,1)", class = "Intercept"), brms::set_prior("normal(0,1)", class = "b", coef = "bench_return"))
#' @param model_structure A character indicating whether the model should be estimated using a ´partial_pooled´ or ´no_pooled´ approach.
#' @param theme_level_intercept A character specifying the specification of effects of the intercept at the theme level.
#' @param theme_level_slope A character specifying the specification of effects of the slope at the theme level.
#' @param lmer_control Other additional parameters to be passed to `lme4::lmer` function.
#' \itemize{
#' \item{lmer_optimizer} A character string specifying the optimizer to be used in the
#' It will be passed to lme4::lmerControl, which will be used in the `lme4::lmer` function.
#' Options include: 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'
#'
#' \item{lmer_optimization_objective}: A character string indicating whether estimates should be chosen to optimize the 'REML' criterion or the 'likelihood'.
#'
#' \item{hierarchical_p_value_method}: One of "Satterthwaite", "Kenward-Roger" and "lme4". Default is "Satterthwaite".
#' }
#'
#' @param active_returns A character string indicating whether performance metrics should be calculated based on active returns or raw returns. If TRUE,
#' backtest_returns_m_xts will be adjusted by subtracting the selected market factor proxy in benchmark_returns_m_xts. This does not
#' impact calculation of the CAPM model, whether it is frequentist or bayesian, pooled or partial pooled.
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
#' @param upper_quantile_winsorization Numeric value for upper winsorization.
#' @param lower_quantile_winsorization Numeric value for lower winsorization.
#' @param verbose A boolean indicating whether to print messages.
#' @param parallel A boolean indicating whether to use parallel processing.
#'
#' @section Bayesian Hierarchical Model
#'
#' One way of introducing shrinkage to alpha estimates from signal portfolios is by using bayesian statistics, which might be specially useful in the context
#' of small samples of strategy returns. Bayesian statistics allows for the incorporation of prior information about the parameters of interest (alpha and beta),
#' which can be particularly useful in multiple testing.
#'
#' A Bayesian model depends on parameters that are themselves random variables. For instance, one can assume a normal distribution for the CAPM alpha parameter,
#' with a mean of 0 (the strategy is not profitable) and a given standard deviation, effectively shrinking posterior estimates towards the prior mean, with an
#' intensity that depends on the prior standard deviation and the sample distribution. Alternatively, one can utilize priors derived from signal strategies
#' from other countries or asset classes than the ones being studied. Either way, the researcher will incorporate the reasoning process people usually have, as
#' we usually have prior beliefs about a subject and then will update our beliefs based on new information. If another research possesses other priors,
#' he or she can incorporate those into Bayes rule and derive other posterior distribution for parameters of interest, which is kind of more
#' transparent than the frequentist approach, who will inherently give ultimate importance to associational evidence from (possibly overfit) data.
#' Most of the time, in a frequentist setting, the researcher usually does not disclose the reasoning process behind the model. Therefore, bayesian statistics
#' are being often recommended as a method to deal with multiple testing problems.
#'
#' Suppose one has return observations for multiple signal portfolios across time and wants to estimate the parameters of a single-factor model (CAPM)
#' that describes the relationship between these signals portfolios and the market factor, being interested in the significance of the alpha parameter, while
#' accounting for its exposure to systematic market risk. A complete pooled model will treat all observations as being independent and treating as irrelevant any
#' information about individual strategies, which might be misleading, because it does not account for the hierarchical structure of the data.
#' Observations of a given signal portfolio are possible correlated. On the other hand, a no pooling model will estimate a separate model for each signal portfolio,
#' fitting specific parameters for each individual strategy, which might be inefficient, as it does not borrow information across signals. Signals belonging
#' to a given theme tend to be similar. Signals in the value theme, for instance, are usually valuation multiples (eg book yield, earnings yield, fcf yield, sales yield etc)
#' and so are very similar, thus it would be unfortunate to ignore information from other value strategies when analyzing, for example, book yield alpha.
#' By fitting a hierarchical model, alpha estimates are first shrunk towards theme mean and then towards priors. In this setting, there is top layer represented
#' by the population of all signal strategies under a theme (eg. the value theme alpha) and a second layer for the backtested strategies that fall under that theme,
#' for which he have repeated observations.
#'
#' By using a hierarchical model, one can study both within-signal variability, examining how consistent the signal is across time, and between-signal variability,
#' examining how performance patterns vary across different signals in a theme. Total variance is given by sum of within-signal variance and between-signal variance.
#'
#' Threfore, there are three layers for R_s,t (t-th observation of return of signal s):
#' \itemize{
#'  \item{Signal-specific layer:}{R_s,t ~ N(alpha_s + beta_s * R_m,t, sigma_s^2). This represents how returns vary within strategy s}
#'  \item{Theme-specific layer:}{alpha_s ~ N(alpha_theme, sigma_a_theme^2); beta_s ~ N(beta_theme, sigma_b_theme^2).
#'   This represents how the typical alpha/beta vary across strategies in a theme}
#'  \item{Priors:}{alpha_theme, sigma_a_theme, beta_theme, sigma_b_theme, sigma_s. Global parameters shared between strategies.}
#'  }
#' More specifically, signal-specific mean parameters are treated as deviations from global parameters (u_1, u_2 and u_sigma for alpha, beta and sigma respectively,
#' with corresponding mean and standard deviations mu_u1, tau_u1,  mu_u2, tau_u2, mu_u_sigma and tau_u_sigma).
#' If the user provide a `priors_m_df`, the function will, for each theme in respective column (that should match possible options in `signal_themes_m_df`):
#' \itemize{
#'    \item{For each date, calculate the average and standard deviation of individual signals alphas/betas/sigmas. Based on these average values,
#'    a prior for overall alpha parameters will be chosen based on maximum likelihood}
#'    \item{For each date, calculate the differential of individual signals alpha/betas/sigmas from overall counterparts. For alpha, this means
#'     getting mu_u1 and tau_u1, mean and standard deviation of differentials of individual signals to overall mean.
#'     In particular, tau_u1 measures alpha variability between signals, the dispersion of differentials of individual signals from theme alpha.}
#'     \item{Use maximum likelihood to derive priors for each tau and also for correlation, according to `priors_type`}
#'  }
#'
#' For location parameters, priors are chosen between normal and t distributions, given the option that minimizes BIC. For scale parameters,
#' other candidate distributions are cauchy, inverse-gamma and log-normal. For correlation, the LKJ distribution is used.
#'
#' By considering the hierarchical structure of the data, it is possible to borrow information across signals and, thus, hierarchical models are better at
#' balancing bias and variance than estimates from complete pooling (high bias) or no pooling models (high variance).
#'
#' To speed up computation, Bayesian models are fitted in parallel using the `future` framework.
#'
#' @section Signal Engineering Benchmarks
#'
#' The process of generating a final signal (also known as Signal Engineering) incorporates two steps:
#' \itemize{
#'   \item \strong{Signal Selection}: Selecting signals deemed significant based on a hypothesis testing zero-alpha null-hypothesis rejection criteria applied to associated signal portfolios returns in `backtest_returns_m_xts`.
#'   \item \strong{Signal Blending}: Blending selected signals into a final signal used to generate the final portfolio at the stock level.
#' }
#'
#' The Signal Engineering Benchmarks (SE Benchmarks) evaluate the performance of both steps:
#' \itemize{
#'   \item \strong{Signal Selection Benchmark}: Built using the universe of all signals in `chosen_signals`. It evaluates the performance of the signal selection process.
#'   \item \strong{Signal Blending Benchmark}: Built using only signals derived from the signal selection process. It evaluates the performance of the signal blending process.
#' }
#'
#' Comparing predictive and return performance of the SS and SB Benchmarks provides insights into the effectiveness of the signal selection process. Additionally, comparing performance between the SB Benchmark and the final portfolio evaluates the performance of the signal blending process. SE Benchmarks are built based on themes; weights are first equally distributed among themes and then equally distributed among signals within each theme.
#'
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Extracts the user-defined chosen signals from `signals_m_df` and subsets the data frame to include only these signals.
#'   \item Checks for consistency in `chosen_signals_and_positions`.
#'   \item Adjusts the signal positions based on whether they are "short" by multiplying their values by -1.
#'   \item Updates column names in the data frame to reflect the corrected positions of the signals.
#'   \item Validates that all adjusted signals have corresponding columns in `backtest_returns_m_xts`.
#'   \item Calculates time-series metrics from backtested returns, including mean active return, IR, alpha, t-stat, beta, Treynor ratio, and p-value. Signals without adequate data (less than `` periods) are removed.
#'   \item Adjusts p-values based on the method selected by the user.
#'   \item Defines which signals are eligible for blending.
#' }
#'
#' @return A list containing eligible signals and signal universes throughout time.
#' @export
run_ss_backtest_internal <- function(
  #Dates
  initial_sample_size, rebalancing_months, split_method = "expanding",
  #Signals
  signals_m_df, chosen_signals_and_positions, custom_signal_universe_metrics_m_df = NULL,
  #Backtests and benchmarks
  backtest_returns_m_xts, benchmark_returns_m_xts, market_factor_proxy = "IBOV",
  #P-value
  p_correction_method = "none", signal_significance_threshold = 0.05,
  #Theme Representativeness Eligiblity
  enable_theme_representativeness = TRUE,
  #Model Structure
  model_structure = "no_pooled", theme_level_intercept = NULL, theme_level_slope = NULL,
  lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
  active_returns = TRUE,
  #Bayesian variables
  priors_m_df = NULL, user_priors = NULL,
  brms_control = list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.80),
  prior_derivation_control = list(half_t_df = 30),
  #Signal Themes
  signal_themes_m_df,
  #Winsorization
  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
  verbose = TRUE, parallel = TRUE
){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    ##Initial Setup
    #########################
      ###Get forced signals and exclude them from chosen_signals_and_positions
      if(any(chosen_signals_and_positions == "force")){
        forced_signals <- chosen_signals_and_positions[which(chosen_signals_and_positions == "force")]
      } else {
        forced_signals <- NULL
      }
      original_chosen_signals_and_positions <- chosen_signals_and_positions #Get original object in case forced_signals were taken out
      chosen_signals_and_positions <- chosen_signals_and_positions[which(chosen_signals_and_positions != "force")]

    #########################

    ##Check Parameters
    #########################
    check_inputs_ss_backtest(
      #Dates
      initial_sample_size = initial_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
      #Signals
      signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = forced_signals, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
      #Backtest and benchmarks
      backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, market_factor_proxy = market_factor_proxy,
      #P-value
      p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold,
      #Theme Representativeness Eligiblity
      enable_theme_representativeness = enable_theme_representativeness,
      #Model Structure
      model_structure = model_structure, theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope, lmer_control = lmer_control, active_returns = active_returns,
      #Priors
      priors_m_df = priors_m_df, user_priors = user_priors,
      brms_control = brms_control, prior_derivation_control = prior_derivation_control,
      #Signal Themes
      signal_themes_m_df = signal_themes_m_df,
      #Winsorization
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization
    )

    #########################

    ##Init objects
    #########################
      ###Extract dates
      dates_m_vector <- unique(as.Date(signals_m_df$dates, format = "%Y-%m-%d")) #coerce just to be sure
      dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure

      ###Backtest length
      backtest_length <- length(dates_m_vector) - initial_sample_size + 1 #calculate backtest_length

      ###Rebalancing Dates
      dates_backtest <- dates_m_vector[initial_sample_size:(initial_sample_size + backtest_length - 1)] #These are dates inside backtest

      first_rebalance_date <- min(dates_backtest) #Get first rebalancing date
      rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
        c(first_rebalance_date, dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
      )
      rebalance_dates <- rebalance_dates[order(rebalance_dates)] #Re-order

      ###Number of rebalancing months
      n_rebalance_months <- length(rebalance_dates)

      ###Last rebalance date
      last_rebalance_date <- max(rebalance_dates)

      ###Create signal_universe list structure to get results
      signal_universe_m_d_ref_list <- list()

    #########################

    #Initial Prints
    #########################
    if(verbose){

      #Text otherwise
      cat("\n")
      cat(crayon::cyan(paste("Starting signal selection backtest")))
      cat("\n")
      cat(paste("Performance metrics calculated with", if(active_returns) "active returns" else "raw returns"))
      cat("\n")
      cat(paste("Factor model:", model_structure, "CAPM with", market_factor_proxy, "as proxy for market factor"))
      cat("\n")
      if(p_correction_method == "none"){
        cat(crayon::yellow(paste("P-values will not be adjusted. Beware for multiple testing bias")))
      } else {
        cat(crayon::yellow(paste("P-values will be adjusted following the", p_correction_method, "method")))
      }
      cat("\n")
      cat(paste("Signal significance threshold set as", signal_significance_threshold))
      cat("\n")
      cat(paste("Theme representativeness eligibility is set to", enable_theme_representativeness))
      cat("\n")
      if(!is.null(forced_signals)){
        cat("Forced signals:", paste(names(forced_signals), collapse = ", "))
        cat("\n")
      }

    }

    #################

    ##Select signals, benchmark and respective backtests based on user choice
    ########################
    selected_signals_and_backtest_list <- select_and_correct_signals(
      #signals_m_df
      signals_m_df = signals_m_df,
      #Signal Themes
      signal_themes_m_df = signal_themes_m_df,
      #Chosen signals and positions
      chosen_signals_and_positions = chosen_signals_and_positions,
      #Signals backtest
      backtest_returns_m_xts = backtest_returns_m_xts
    )

    ###Selected signals_m_df with corrected positions
    selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
    ###Select signals_themes_m_df
    selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df
    ###Selected signals backtest returns
    selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts

    ###Check if both are contemplated in signal_themes
    if(any(!colnames(dplyr::select(selected_signals_corrected_positions_m_df, -id, -tickers, -dates)) %in% unique(selected_signal_themes_m_df$tickers))){
      stop("all selected signals (with corrected positions) should have a theme classification in selected_signal_themes_m_df")
    }
    if(any(!colnames(selected_backtest_returns_corrected_positions_m_xts) %in% selected_signal_themes_m_df$tickers)){
      stop("all selected signals in backtests (with corrected positions) should have a theme classification in selected_signal_themes_m_df")
    }

    ###Select market factor proxy from benchmark returns xts
    selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, market_factor_proxy]

    ########################

    ##Start Backtest##
    ##Apply backtest only if multiple signals are being provided
    #################
    for (d in initial_sample_size:(initial_sample_size + backtest_length - 1)){
      #Extract date and references
      current_date <- dates_m_vector[d]
      if (verbose) print(current_date)

      #Rebalance if it's a rebalancing month
      is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (initial_sample_size)
      if (is_rebalancing_month){
        #Print refitting message
        if (verbose){
          cat("\n")
          cat(crayon::yellow(paste("Running signal selection backtest at:", current_date)))
          cat("\n")
        }

        #Subset signals, backtest, market factor, priors and custom_signal_universe_metrics
        selected_signals_corrected_positions_m_upd_ref <- selected_signals_corrected_positions_m_df %>% dplyr::filter(dates <= current_date)
        selected_signal_themes_m_d_ref <- selected_signal_themes_m_df %>% dplyr::filter(dates == current_date) #Just one date reference needed
        priors_m_upd_ref <- if (!is.null(priors_m_df)) priors_m_df %>% dplyr::filter(dates <= current_date) else NULL
        custom_signal_universe_metrics_m_upd_ref <- if (!is.null(custom_signal_universe_metrics_m_df)) custom_signal_universe_metrics_m_df %>% dplyr::filter(dates <= current_date) else NULL

        selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[which(zoo::index(selected_backtest_returns_corrected_positions_m_xts) <= current_date), ]
        selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[which(zoo::index(selected_market_factor_proxy_m_xts) <= current_date), ]


        ###Elect signals
        signal_eligibility_results_list <- define_signal_eligibility(
          #Backtests
          selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
          selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
          custom_signal_universe_metrics_m_upd_ref = custom_signal_universe_metrics_m_upd_ref,
          #P adjustment
          p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold,
          #Theme Representativeness Eligibiility
          enable_theme_representativeness = enable_theme_representativeness,
          #Model Structure
          model_structure = model_structure, theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope, lmer_control = lmer_control,
          active_returns = active_returns,
          #Bayesian method
          priors_m_upd_ref = priors_m_upd_ref, user_priors = user_priors,
          brms_control = brms_control, prior_derivation_control = prior_derivation_control,
          #Signal Themes
          selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
          #Winsorization
          lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
          #Verbose & Parallel
          verbose = verbose, parallel = parallel
        )

        ###Get results
        signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref

        ##Add forced signals
        if (length(forced_signals) > 0){
          ###Create a forced_signals_m_d_ref
          forced_signals_m_d_ref <- expand.grid( #Create an expanded grid with the forced variable
            tickers = names(forced_signals),
            dates = unique(signal_universe_m_d_ref$dates),
            theme = "forced", #Assume a general 'forced' theme
            theme_ss_bench_weights = 0,
            theme_sb_bench_weights = 0,
            stringsAsFactors = FALSE
          ) %>%
            dplyr::mutate(
              id = paste0(tickers, "-", dates),
              pre_eligible_assets = 0, #Forced variables are not treated as pre eligible assets
              is_eligible = 1 #But they are being elected in a forced way (same as enable_theme_representativeness)
            ) %>%
            dplyr::select(
              id, tickers, dates, theme_ss_bench_weights, theme_sb_bench_weights, theme, is_eligible #Select those columns
            )

          ###Add to signal_universe_m_d_ref
          signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::bind_rows(forced_signals_m_d_ref)
        }

        ###Print results
        if(verbose){
          cat("\n")
          cat(crayon::green(paste0("Eligible signals defined: ")))
          cat(paste(signal_universe_m_d_ref$tickers[which(signal_universe_m_d_ref$is_eligible == 1)], collapse = ", "))
        }

        ###Get results
        signal_universe_m_d_ref_list[[which(rebalance_dates %in% current_date)]] <-  signal_universe_m_d_ref



      } #End rebalancing month
    } #End loop

    #Assign dates to names of objects
    names(signal_universe_m_d_ref_list) <- rebalance_dates

    #Turn signal_universe_m_d_ref_list into a signle meta_dataframe
    signal_universe_m_df <- do.call(rbind, signal_universe_m_d_ref_list)
    rownames(signal_universe_m_df) <- NULL #erase rownames

    #End timer
  })
  print(elapsed_time)

  #Get workflow
  ss_backtest_workflow <- list(
    config_name = "not_identified",
    backtest_identifier = "not_identified",
    backtest_type = if(p_correction_method == "bayesian") "bayesian" else "frequentist",
    #Hypothesis tests
    p_correction_method = p_correction_method,
    signal_significance_threshold = signal_significance_threshold,
    market_factor_proxy = market_factor_proxy,
    active_returns = active_returns,
    model_structure = model_structure,
    enable_theme_representativeness = enable_theme_representativeness,
    theme_level_intercept = theme_level_intercept,
    theme_level_slope = theme_level_slope,
    lmer_control = lmer_control,
    #Bayesian
    user_priors = user_priors,
    brms_control = brms_control,
    prior_derivation_control = prior_derivation_control,
    #Dates
    initial_sample_size = initial_sample_size,
    rebalancing_months = rebalancing_months,
    dates_covered = dates_m_vector,
    n_dates = length(dates_m_vector),
    dates_backtest = dates_backtest,
    rebalance_dates = rebalance_dates,
    n_rebalance_months = n_rebalance_months,
    first_rebalance_date = first_rebalance_date,
    split_method = split_method,
    #Signals
    chosen_signals_and_positions = original_chosen_signals_and_positions,
    selected_signals_corrected_positions = colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref),
    n_signals = length(original_chosen_signals_and_positions),
    signals_workflow = NULL,
    signals_object_name = "not_identified",
    signal_themes_workflow = NULL,
    signal_themes_object_name = "not_identified",
    priors_workflow = NULL,
    priors_object_name = "not_present",
    backtest_returns_object_name = "not_identified",
    benchmark_returns_object_name = "not_identified",
    custom_signal_universe_metrics_workflow = NULL,
    custom_signal_universe_metrics_object_name = "not_identified",
    lower_quantile_winsorization = lower_quantile_winsorization,
    upper_quantile_winsorization = upper_quantile_winsorization,
    #Performance
    elapsed_time = elapsed_time,
    timestamps = c(initialization = Sys.time()),
    #Call
    call = match.call()
  )

  ##Create meta_dataframes
  ###Create signal_universe_m_df
  signal_universe_m_df <- withCallingHandlers({
    signal_universe_m_df <- signal_universe_m_df %>% dplyr::arrange(id)
    create_meta_dataframe(signal_universe_m_df, ss_backtest_workflow = ss_backtest_workflow, type = "signal_universe")
  },
  warning = function(w) {
    warning("Signal universe creation warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  ###Create final_signal_universe_m_d_ref
  final_signal_universe_m_d_ref <- withCallingHandlers({
    signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::arrange(id)
    create_meta_dataframe(signal_universe_m_d_ref, ss_backtest_workflow = ss_backtest_workflow, type = "signal_universe")
  },
  warning = function(w) {
    warning("Final signal universe creation warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  ##Create meta_xts
  ###Create selected_market_factor_proxy_m_xts
  selected_market_factor_proxy_m_xts <- withCallingHandlers({
    create_meta_xts(selected_market_factor_proxy_m_xts, type = "returns", asset_type = "benchmark", source = ss_backtest_workflow$backtest_identifier)
  },
  warning = function(w) {
    warning("Market factor proxy creation warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  #Get final object
  ss_backtest_results <- new("ss_backtest_results",
                             ss_backtest_config = NULL,
                             signal_universe_m_df = signal_universe_m_df,
                             final_signal_universe_m_d_ref = final_signal_universe_m_d_ref,
                             selected_market_factor_proxy_m_xts = selected_market_factor_proxy_m_xts,
                             frequentist_results = signal_eligibility_results_list$frequentist_results,
                             bayesian_results = signal_eligibility_results_list$bayesian_results,
                             p_correction_method = p_correction_method,
                             ss_backtest_workflow = ss_backtest_workflow,
                             backtest_identifier = "not_identified"
  )

  #Return
  return(ss_backtest_results)

}




