
<!-- README.md is generated from README.Rmd. Please edit that file -->

# factoRverse

<!-- badges: start -->
<!-- badges: end -->

The goal of factoRverse is to provide functions for feature engineering, data pre-processing, signal blending (including machine-learning models) and backtesting.
it was build under the metafactor framework, under which one combines not only signals, but also signal blending methods. 
In a practical standpoint, at any given point in time, an equity factor investors must decide which signals are going to be used, how those signals are going to be processed and, finally, how they will be combined.
Therefore, the package provides functions designed to create new signals, covering many popular factor definitions.
It also includes a variety of data pre-processing tools designed to factor characteristics, such as filling missing information based on sector median, date-wise winsorization and normalization (which prevents forward-looking bias) and row filtering based on market cap.

Regarding signal-blending, the package covers heuristics methods (EW, SW), Risk Parity, Mean-Tracking Error Optimization (with box and groups constraints) and Machine-Learning algorithms.
Signal-blending methods are based on individual signals backtests. Machine-learning methods leverage famous packages such as glmnet, ranger, xgboost and keras, through an unified interface and a walk-forward validation scheme, covering 
grid-search, random-search and Bayesian Optimization, besides many possible objectives for parameter estimation and evaluation metrics for hyperparameter tuning.
Covariances can be estimated via sample, ewma, principal component analysis and shrinkage.
Therefore, blending signals may utilize many of the methods usually applied to the stock level.
At the stock level, besides the methods applied to signal blending, one also has CW and CS.

Signal selection is usually conducted by alpha hypothesis testing. 
The package covers many methods to multiple testing, including frequentist and hierarchical bayesian methods.

Finally, it is also worth mentioning that the package handles paralallelization and functional programming in order to speed up computation. Parallelization is implemented through futures package and backends can be set via the plan() function.



## Installation

You can install the development version of factoRverse from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pauloguimaraes871/factorverse")
```

## Usage

This is a basic example which shows how to calculate element-wise
medians of three matrices.

``` r
library(factorverse)
 median_matrix(
      matrix(c(1,2,3,4), nrow=2, ncol=2),
      matrix(c(5,6,7,8), nrow=2, ncol=2),
      matrix(c(1,1,1,1), nrow=2, ncol=2))
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
