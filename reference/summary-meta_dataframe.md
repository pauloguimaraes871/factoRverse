# Summary for meta_dataframe

S4 `summary` method for `meta_dataframe` objects. Produces interactive,
styled summaries that help inspect variable distributions, ticker
coverage and categorical diagnostics. Output is printed (DT tables or
ggplot2 plots) and the input object is returned invisibly.

## Usage

``` r
# S4 method for class 'meta_dataframe'
summary(object, summary_id = NULL)
```

## Arguments

- object:

  A `meta_dataframe` instance.

- summary_id:

  Character or numeric. Which summary to display. Accepts either the
  name or the index of one of:

  - `"Numeric Summary Table"` — detailed numeric statistics (min, Q1,
    median, mean, Q3, max, NA counts).

  - `"Tickers Frequency Table"` — frequency counts per ticker.

  - `"Categorical Variables Plots"` — bar plots for character/factor
    columns.

  If `NULL` (default) the method lists available summaries and prompts
  for a selection (interactive).

## Value

Invisibly returns the original `meta_dataframe` object (side effects:
prints DT tables / plots).

## Details

Summary method for meta_dataframe objects

- Required packages: DT, htmltools, htmlwidgets, and ggplot2. The
  function stops with an informative message if these are not available.

- When a `dates` column exists the method can prompt to filter the data
  to a single date before summarizing.

- Numeric summary displays per-variable stats and an aggregated
  "Average" row; tables are rendered with scrolling and custom styling.

- Ticker frequencies are shown as a styled DT table. Categorical
  variables are displayed as bar charts with readable labels.

- Designed for interactive inspection; all behaviors may be called
  programmatically by supplying `summary_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
# List summaries and choose interactively
summary(mdf)

# Programmatically show the numeric summary
summary(mdf, summary_id = "Numeric Summary Table")

# Show tickers frequency
summary(mdf, summary_id = "Tickers Frequency Table")

# Show categorical plots
summary(mdf, summary_id = "Categorical Variables Plots")
} # }
```
