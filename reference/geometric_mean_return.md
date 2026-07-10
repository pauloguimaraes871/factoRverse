# Geometric Mean Return (percentage-point scale)

Computes the geometric mean of a vector of returns expressed **in
percentage points** (e.g. `2` for 2 %, `-0.75` for -0.75 %). Internally
the function converts the input to decimal form, performs the
compounding, and returns the result **in the same percentage-point
scale**.

## Usage

``` r
geometric_mean_return(
  returns,
  na.rm = FALSE,
  mult_last_n = 0,
  mult_by = 0,
  scale = 100
)
```

## Arguments

- returns:

  Numeric vector of returns. Each return must be greater than `-100`
  (i.e. no return worse than -100 %).

- na.rm:

  Logical. Should `NA` values be removed? Default `FALSE`.

- mult_last_n:

  = Integer \>= 0. If \> 0, multiply the last n (in time order) by a
  number.

- mult_by:

  Numeric scalar. The number to multiply the last n values by. Default
  is 0 (skip).

- scale:

  Numeric divisor/multiplier used to convert between percentage points
  and decimals. Default `100`.

## Value

A single numeric value (in percentage points) representing the
compounded average return.

## Details

The computation is \$\$\left(\prod\_{i=1}^{n}\left(1 +
\frac{r_i}{\text{scale}}\right)\right)^{1/n} - 1\$\$ converted back to
percentage-point scale by multiplying the decimal result by `scale`.

## Examples

``` r
# 2 % , 0.5 % and -1 % expressed as 2, 0.5, -1
geometric_mean_return(c(2, 0.5, -1))        # ≈ 0.498 (% points)
#> [1] 0.4925368
geometric_mean_return(c(2, NA, 1), na.rm = TRUE)
#> [1] 1.498768
```
