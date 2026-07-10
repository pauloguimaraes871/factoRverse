# Run Signal Blending Backtest

Executes a signal blending backtest, supporting both base learners and
meta learners in a walk-forward setting. It is capable of handling
advanced ML configurations, hyperparameter tuning strategies, custom
optimization objectives, and interpretability tools (e.g., global
surrogate models). Designed to run flexibly with either
`sb_backtest_config` or `sb_metabacktest_config` objects.

This method iteratively evaluates several base learners defined in an
`sb_metabacktest_config`, then fits a meta learner on top of their
predictions. It allows winsorization, normalization, and feature
passthrough control when aggregating base learner outputs.

## Usage

``` r
run_sb_backtest(
  features_m_df,
  target_m_df,
  config,
  base_sb_backtest_results_list,
  ...
)

# S4 method for class 'meta_dataframe,meta_dataframe,sb_backtest_config,missing'
run_sb_backtest(
  features_m_df,
  target_m_df,
  config,
  ss_backtest_results = NULL,
  port_backtest_cohort = NULL,
  backtest_returns_m_xts = NULL,
  benchmark_returns_m_xts = NULL,
  signal_themes_m_df = NULL,
  target_port_m_df = NULL,
  custom_signal_weights_m_df = NULL,
  custom_signal_universe_metrics_m_df = NULL,
  winsorization_probs = c(0.025, 0.975),
  gsm_algorithm = "ols",
  verbose = TRUE,
  parallel = TRUE,
  .test_seed = NULL,
  .update = FALSE,
  .old_backtest_covered_dates = NULL,
  .old_oos_sb_outputs_m_df = NULL,
  .old_sb_model_fit = NULL
)

# S4 method for class 'meta_dataframe,meta_dataframe,sb_metabacktest_config,list'
run_sb_backtest(
  features_m_df,
  target_m_df,
  config,
  base_sb_backtest_results_list,
  base_port_backtest_cohort = NULL,
  base_backtest_returns_m_xts = NULL,
  base_benchmark_returns_m_xts = NULL,
  base_signal_themes_m_df = NULL,
  base_custom_signal_weights_m_df = NULL,
  base_custom_signal_universe_metrics_m_df = NULL,
  meta_port_backtest_cohort = NULL,
  meta_backtest_returns_m_xts = NULL,
  meta_benchmark_returns_m_xts = NULL,
  meta_signal_themes_m_df = NULL,
  meta_custom_signal_weights_m_df = NULL,
  meta_custom_signal_universe_metrics_m_df = NULL,
  winsorization_probs = c(0.025, 0.975),
  gsm_algorithm = "ols",
  verbose = TRUE,
  parallel = TRUE,
  .test_seed = NULL,
  .update = FALSE,
  .old_meta_sb_backtest_results = NULL
)
```

## Arguments

- features_m_df:

  A `meta_dataframe` containing features used for the meta learner.
  These may include passthrough features and/or out-of-sample
  predictions from base learners.

- target_m_df:

  A `meta_dataframe` containing the target variable to be predicted by
  the meta learner. It must be aligned with `features_m_df`.

- config:

  An object of class `sb_metabacktest_config`, which contains the
  configuration for all base learners and the meta learner.

- base_sb_backtest_results_list:

  A named list of `sb_backtest_results` objects. These are the results
  of base learners whose predictions will be used as input for the meta
  learner.

- ...:

  Additional arguments for `sb_backtest_config` or
  `sb_metabacktest_config`.

- ss_backtest_results:

  An `ss_backtest_results` object (optional).

- port_backtest_cohort:

  A `port_backtest_cohort` object (optional).

- backtest_returns_m_xts:

  A `meta_xts` object of returns (optional).

- benchmark_returns_m_xts:

  A `meta_xts` benchmark (optional).

- signal_themes_m_df:

  A `meta_dataframe` with theme classifications (optional).

- target_port_m_df:

  (Optional) Target portfolio weights for shrinkage.

- custom_signal_weights_m_df:

  A `meta_dataframe` with predefined signal weights (optional).

- custom_signal_universe_metrics_m_df:

  A `meta_dataframe` with signal-level metrics (optional).

- winsorization_probs:

  A numeric vector of length 2 specifying the lower and upper quantiles
  for winsorizing predictions before training the meta learner. Default
  is `c(0.025, 0.975)`.

- gsm_algorithm:

  Character string indicating the global surrogate model used for
  interpretability. Options include `"ols"` and `"tree"`. Default is
  `"ols"`.

- verbose:

  Logical. If `TRUE`, prints progress messages. Default is `TRUE`.

- parallel:

  Logical. If `TRUE`, runs the backtest and hyperparameter tuning steps
  in parallel. Default is `TRUE`.

- .test_seed:

  (Internal) A numeric seed used to control randomness for reproducible
  testing. Default is `NULL`.

- .update:

  (Internal) Logical flag. If `TRUE`, updates a previously computed meta
  learner backtest instead of running from scratch. Default is `FALSE`.

- .old_backtest_covered_dates:

  Vector of covered dates for update.

- .old_oos_sb_outputs_m_df:

  Out-of-sample predictions from previous run.

- .old_sb_model_fit:

  Trained model from previous run.

- base_port_backtest_cohort:

  (Optional) A `port_backtest_cohort` object containing results of
  portfolio backtests associated with the base learners. Used to extract
  return series when needed.

- base_backtest_returns_m_xts:

  (Optional) A `meta_xts` object with historical returns for the base
  learner signal portfolios. Used in RP/MVO for covariance estimation.

- base_benchmark_returns_m_xts:

  (Optional) A `meta_xts` object with benchmark returns used alongside
  base learner portfolios.

- base_signal_themes_m_df:

  (Optional) A `meta_dataframe` mapping base learner signals to groups
  or themes, needed for group constraints in RP/MVO.

- base_custom_signal_weights_m_df:

  (Optional) A `meta_dataframe` specifying weights for base learner
  signals. Only used when `sb_algorithm = "custom_weights"` for base
  learners.

- base_custom_signal_universe_metrics_m_df:

  (Optional) A `meta_dataframe` of evaluation metrics for base learner
  signals. Can be used as additional features in the meta learner.

- meta_port_backtest_cohort:

  (Optional) A `port_backtest_cohort` object for the meta learner signal
  portfolio. Used to extract return series when needed.

- meta_backtest_returns_m_xts:

  (Optional) A `meta_xts` object with historical returns for the meta
  learner’s constructed signals or predictions. Used in RP/MVO.

- meta_benchmark_returns_m_xts:

  (Optional) A `meta_xts` object with benchmark returns for the meta
  learner.

- meta_signal_themes_m_df:

  (Optional) A `meta_dataframe` with signal group classification for the
  meta learner. Required if using group constraints.

- meta_custom_signal_weights_m_df:

  (Optional) A `meta_dataframe` with weights for the meta learner
  signals. Used when `sb_algorithm = "custom_weights"`.

- meta_custom_signal_universe_metrics_m_df:

  (Optional) A `meta_dataframe` of evaluation metrics to be used as
  custom signal-level features by the meta learner.

- .old_meta_sb_backtest_results:

  (Internal) A previously computed `sb_backtest_results` object for the
  meta learner, used only if `.update = TRUE`.

## Value

An S4 object of class:

**sb_backtest_results**

:   For a base learner, containing:

**sb_metabacktest_results**

:   For a meta learner, additionally containing:

An object of class `sb_metabacktest_results`, containing:

- **meta_sb_backtest_results**: The results from the meta learner.

- **base_sb_backtest_results_list**: List of base learner results.

- **oos_predictions_m_df**: Out-of-sample predictions used to train the
  meta learner.

- **sb_metabacktest_config**: The input configuration object.

An object of class `sb_metabacktest_results`.

## Details

The function has two main methods:

### 1. **Base Learner Backtest (`sb_backtest_config`)**

Executes a time-series cross-validation procedure, with refitting at
specified rebalancing months. The data is divided into:

- **Training window** (fixed size, expanding or rolling)

- **Optional validation window** (for tuning)

- **Testing window** (evaluated sequentially for each rebalancing date)

Key steps:

- Signal selection based on `is_eligible` flags from
  `signal_universe_m_df`.

- Correction of signal orientation (e.g., multiply by -1 if `low_`
  prefixed).

- Optionally override signal selection via custom weights.

- Hyperparameter tuning via:

  - `grid_search`

  - `random_search` (with distribution sampling)

  - `bayesian_opt` (with `ParBayesianOptimization`)

- Refit and predict using ML algorithm (OLS, glmnet, xgboost, rf, nn,
  etc.).

- Global Surrogate Model (`gsm_algorithm`) fitted post-hoc for
  interpretability.

- Walk-forward out-of-sample testing and metric computation.

### 2. **Meta Learner Backtest (`sb_metabacktest_config`)**

Iterates over base learners (each with a `sb_backtest_config`),
consolidates their predictions into a unified meta feature set, and
then:

- Winsorizes and/or normalizes predictions (optional)

- Adds user-selected pass-through features

- Fits a meta learner using a new `sb_backtest_config`

The meta learner backtest can be updated via `.update = TRUE` to extend
its horizon without re-running all base learners.

## Methods (by class)

- `run_sb_backtest( features_m_df = meta_dataframe, target_m_df = meta_dataframe, config = sb_backtest_config, base_sb_backtest_results_list = missing )`:
  Run SB backtest for a single configuration

- `run_sb_backtest( features_m_df = meta_dataframe, target_m_df = meta_dataframe, config = sb_metabacktest_config, base_sb_backtest_results_list = list )`:
  Run SB meta-backtest using multiple base learners and a meta learner

## Parallel Execution

By default, tuning_method %in% c("random_search", "grid_search")
utilizes furrr::future_pmap, which means they can run according to the
built-in backends from the future package. Therefore, if the user does
not specify a different evaluation strategy with future::plan(), tuning
will be done sequentially by default (equivalent to
future::plan(sequential)). In this case, however, random number
generator will be set to RNGkind("L'Ecuyer-CMRG"), instead of R default
(RNGkind("Mersenne-Twister")), making results not reproducible regarding
using purrr:pmap(). In order to run using R's default random number
generator, set parallel = FALSE. Using a different evaluation strategy
(e.g., future::plan(multisession)) will tune hyperparameters
asynchronously (in parallel).

For tuning_method = "bayesian_opt", the
ParBayesianOptimization::bayesOpt function runs in parallel by using
foreach::foreach with the %dopar% operator. Therefore, in this case, the
user can either: (i) use doFuture::registerDoFuture(), in order to use
the %dofuture% foreach adapter (actually, in this case,
doFuture::withDoRNG is used to turn %dopar% into %dorng% in order to use
parallel-safe RNG), which allows usage of backends from the future
package or (ii) use parallel::makeCluster(),
doParallel::registerDoParallel(), doParallel::clusterExport() and
doParallel::clusterEvalQ(), as exemplified by ParBayesianOptimization.
If parallel = TRUE and neither strategy is being used, code will result
in error. Therefore, to run bayesian_opt synchronously, either use
doFuture::registerDoFuture() with plan(sequential) or set parallel =
FALSE.

Keras has some limitations when working in parallel, especially when
using bayesian optimization as tuning method.

## Update Workflow

- When `.update = TRUE`, previously computed predictions are reused for
  existing dates.

- Models are only refitted for new rebalancing dates not in
  `.old_backtest_covered_dates`.

- The final object will append results while keeping the full history
  intact.
