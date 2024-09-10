
<!-- README.md is generated from README.Rmd. Please edit that file -->

# factoRverse

<!-- badges: start -->
<!-- badges: end -->

The goal of factoRverse is to provide functions for feature engineering, data pre-processing, signal blending (including machine-learning models) and backtesting.
it was build under the metafactor framework, under which one combines not only signals, but also signal blending methods. 
In a practical standpoint, at any given point in time, an equity factor investors must decide which signals are going to be used, how those signals are going to be processed and, finally, how they will be combined.

## Overview

![Beware the factoRverse](inst/images/factoRverse.jpeg)

The package provides functions designed to create new signals, covering many popular factor definitions.
It also includes a variety of data pre-processing tools designed to factor characteristics, such as filling missing information based on sector median, date-wise winsorization and normalization (which prevents forward-looking bias) and row filtering based on market cap.

Regarding signal-blending, the package covers heuristics methods (EW, SW), Risk Parity, Mean-Tracking Error Optimization (with box and groups constraints) and Machine-Learning algorithms.
Signal-blending methods are based on individual signals backtests. Machine-learning methods leverage famous packages such as glmnet, ranger, xgboost and keras, through an unified interface and a walk-forward validation scheme, covering 
grid-search, random-search and Bayesian Optimization, besides many possible objectives for parameter estimation and evaluation metrics for hyperparameter tuning.
Covariances can be estimated via sample, ewma, principal component analysis and shrinkage.
Therefore, blending signals may utilize many of the methods usually applied to the stock level.
At the stock level, besides the methods applied to signal blending, one also has CW and CS.

Signal selection is usually conducted by alpha hypothesis testing. 
The package covers many methods to multiple testing, including frequentist and hierarchical bayesian methods.

Finally, it is also worth mentioning that the package handles paralallelization and functional programming in order to speed up computation. Parallelization is implemented through futures package and backends can be set via the `future::plan()` function.



## Installation

You can install the development version of factoRverse from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pauloguimaraes871/factoRverse")
```

## Usage


