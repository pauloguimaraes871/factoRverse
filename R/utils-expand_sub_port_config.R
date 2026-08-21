#' Expand a Sub Portfolio Configuration into set_portfolio_weights Arguments
#'
#' @description
#' Turns a `sub_port_config` into the named argument list a recursive
#' \code{\link{set_portfolio_weights}} call expects. Layered (`*af`) methods use it so
#' that an inner portfolio is always parameterized from its own configuration, never
#' from whatever the parent method happened to be using.
#'
#' @details
#' Only the parameters of the configured method are expanded. A configuration carrying,
#' say, `rp_parameters` while its method is `"ew"` produces no risk-parity arguments, so
#' the inner call falls back to the documented defaults of
#' \code{\link{set_portfolio_weights}} rather than silently inheriting a stray setting.
#'
#' When the method requires a parameter object and none was supplied, the corresponding
#' default parameter object is created, matching the behaviour of
#' \code{\link{create_port_backtest_config}}.
#'
#' @param sub_port_config An object of class `sub_port_config`, or of a class extending
#'   it such as `mmaf_sub_port_config`.
#'
#' @return A named list of arguments, always containing `port_construction_method` and,
#'   for covariance-based methods, that method's parameters. Intended to be spliced into
#'   a \code{do.call(set_portfolio_weights, ...)}.
#'
#' @seealso \code{\link{create_sub_port_config}}, \code{\link{set_portfolio_weights}}
expand_sub_port_config <- function(sub_port_config){

  # Validate input--------------------------------------------------------------

    ## Must be a sub-portfolio configuration
    if (!methods::is(sub_port_config, "sub_port_config")){
      stop("sub_port_config must be an object of class 'sub_port_config'.")
    }

    ## Revalidate defensively: the object may have been modified after construction
    validate_sub_port_config(sub_port_config)

  # Expand the parameters of the configured method------------------------------

    port_construction_method <- sub_port_config@port_construction_method

    ## Every expansion carries the method itself
    expanded_args <- list(port_construction_method = port_construction_method)

    ## Risk Parity
    if (port_construction_method == "rp"){

      rp_parameters <- sub_port_config@rp_parameters
      if (is.null(rp_parameters)) rp_parameters <- create_rp_parameters()

      expanded_args$rp_method             <- rp_parameters@rp_method
      expanded_args$exp_ret_score_tilt    <- rp_parameters@exp_ret_score_tilt
      expanded_args$exp_ret_score_tilt_eta <- rp_parameters@exp_ret_score_tilt_eta
    }

    ## Hierarchical Risk Parity
    if (port_construction_method == "hrp"){

      hrp_parameters <- sub_port_config@hrp_parameters
      if (is.null(hrp_parameters)) hrp_parameters <- create_hrp_parameters()

      expanded_args$linkage               <- hrp_parameters@linkage
      expanded_args$exp_ret_score_tilt    <- hrp_parameters@exp_ret_score_tilt
      expanded_args$exp_ret_score_tilt_eta <- hrp_parameters@exp_ret_score_tilt_eta
    }

    ## Mean-Variance Optimization
    if (port_construction_method == "mvo"){

      mvo_parameters <- sub_port_config@mvo_parameters
      if (is.null(mvo_parameters)) mvo_parameters <- create_mvo_parameters()

      expanded_args$opt_method           <- mvo_parameters@opt_method
      expanded_args$random_ports_method  <- mvo_parameters@random_ports_method
      expanded_args$n_random_ports       <- mvo_parameters@n_random_ports
      expanded_args$opt_objective        <- mvo_parameters@opt_objective
      expanded_args$ridge_pen            <- mvo_parameters@ridge_pen
      expanded_args$n_resamples          <- mvo_parameters@n_resamples
      expanded_args$exp_ret_score_jitter <- mvo_parameters@exp_ret_score_jitter
      expanded_args$cov_eigval_jitter    <- mvo_parameters@cov_eigval_jitter
    }

    ### 'ew', 'sw', 'cw' and 'cs' carry no parameters of their own

  # Return----------------------------------------------------------------------

    ## Every expanded name must be a formal argument of set_portfolio_weights, otherwise
    ## the do.call downstream would fail in a way that is hard to trace back to here
    unknown_args <- setdiff(names(expanded_args), names(formals(set_portfolio_weights)))
    if (length(unknown_args) > 0){
      stop(paste0("Expanded arguments are not formals of set_portfolio_weights: ",
                  paste(unknown_args, collapse = ", ")))
    }

    return(expanded_args)
}
