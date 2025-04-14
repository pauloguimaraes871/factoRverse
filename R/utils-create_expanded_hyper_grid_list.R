#' Create expanded hyperparameter grid list based on tuning method
#'
#' This function generates an expanded hyperparameter grid list based on the specified
#' tuning method ('grid_search' or 'random_search') and the corresponding hyperparameter
#' domain list.
#'
#' @param hyper_grid_domain_list List containing hyperparameter domains for tuning.
#'   Each element should include 'distribution_choice', 'pars', and 'value' for 'random_search'.
#' @param tuning_method Character, method of hyperparameter tuning ("grid_search" or "random_search").
#' @param n_iter Integer, number of iterations for random search (ignored for grid search).
#'
#' @return List of expanded hyperparameter combinations ready for grid or random search.
#'
#' @details
#' Depending on the specified tuning method:
#' - For 'grid_search', it expands the grid of hyperparameter values.
#' - For 'random_search', it generates random samples based on specified distributions.
#'   - 'constant': Returns a constant value.
#'   - 'normal': Samples from a normal distribution.
#'   - 'uniform': Samples from a uniform distribution.
#'   - 'lognormal': Samples from a lognormal distribution.
#' All generated values are rounded to integers if specified in 'hyper_grid_domain_list'.
#' Duplicates are removed to prevent repeated inputs.
#'
#'
#'
#' @seealso
#' Use with functions that require hyperparameter tuning such as 'train' or 'caret'.
#'
#' @keywords internal hyperparameter-tuning machine-learning
#'
create_expanded_hyper_grid_list <- function(hyper_grid_domain_list, tuning_method, n_iter, ml_algorithm){

  #Set Hyperparameters grid
  #Apply Grid Search
  if(tuning_method == c("grid_search")){

    #Eliminate repeated inputs
    hyper_grid_domain_list <- lapply(hyper_grid_domain_list, function(x) unique(x))
    #For glmnet, it is better to provide a sequence of lambdas
    expanded_hyper_grid_list <- lapply(do.call(expand.grid, hyper_grid_domain_list), as.vector)
  } else {}


  #Apply Random Search
  if(tuning_method == c("random_search")){

  #Creat object
  expanded_hyper_grid_list <- list()

  expanded_hyper_grid_list <-
    lapply(hyper_grid_domain_list, function(x){
      if(x$distribution_choice == "constant") {#If a constant
        return(x$value)
      } else {}
      if(x$distribution_choice == "normal") { #If normal distribution
        return(stats::rnorm(n = n_iter, mean = x$pars['mean'], sd = x$pars['sd'])) #Return x from normal distribution
      } else {}
      if(x$distribution_choice == "uniform") { #If uniform distribution
        return(stats::runif(n = n_iter, min = x$pars['min'], max = x$pars['max'])) #Return x from uniform distribution
      } else {}
      if(x$distribution_choice == "lognormal") { #If lognormal distribution
        return(stats::rlnorm(n = n_iter, meanlog = x$pars['meanlog'], sdlog = x$pars['sdlog'])) #Return x from lognormal distribution
      } else {}
    })

  #Convert to integers
  integer_hyperparameter <- lapply(hyper_grid_domain_list, function(x) is.integer(x$pars)) #Check if any of parameters should be integers
  expanded_hyper_grid_list <- Map(function(int_param, grid) if (int_param) round(grid, 0) else grid, integer_hyperparameter, expanded_hyper_grid_list) #Round for integers


  #Prevent from using repeated inputs
  expanded_hyper_grid_list <- lapply(expanded_hyper_grid_list, function(x) unique(x))

  #Expand
  expanded_hyper_grid_list <- lapply(do.call(expand.grid, expanded_hyper_grid_list), as.vector)

} else {}

  #Check for adequate outputs
  #################################
  if(ml_algorithm == "glmnet"){
    #Alpha
    if(!all(is.null(expanded_hyper_grid_list$alpha))){
    if(
      !all(0 <= expanded_hyper_grid_list$alpha, expanded_hyper_grid_list$alpha <= 1))
      {
        stop("alpha should be set in interval [0,1]")
      }
    }

    #Lambda.min.ratio
    if(!all(is.null(expanded_hyper_grid_list$lambda.min.ratio))){
    if((
       !all(0 <= expanded_hyper_grid_list$lambda.min.ratio, expanded_hyper_grid_list$lambda.min.ratio < 1)
       )
       ){
          stop("lambda.min.ratio should be set in interval [0,1)")
     }
    }
  }

  if(ml_algorithm == "rf"){
    #Num tress
    if(!all(is.null(expanded_hyper_grid_list$num.trees))){
    if(
      !all(expanded_hyper_grid_list$num.trees == floor(expanded_hyper_grid_list$num.trees)
             )){
      stop("num.trees should have no decimals")
    }
    }


    #mtry
    if(!all(is.null(expanded_hyper_grid_list$mtry))){
    if((
      !all(0 <= expanded_hyper_grid_list$mtry, expanded_hyper_grid_list$mtry <= 1)
      )
    ){
      stop("mtry should be set in interval [0,1]")
    }
    }

    #max.depth
    if(!all(is.null(expanded_hyper_grid_list$max.depth))){
    if((
      any(
      !all(expanded_hyper_grid_list$max.depth > 0),
      !all(expanded_hyper_grid_list$max.depth == floor(expanded_hyper_grid_list$max.depth)
      )
    )
    )){
      stop("max.depth should be positive with no decimals")
    }
    }
  }

  if(ml_algorithm == "xgb"){
    #eta
    if(!all(is.null(expanded_hyper_grid_list$eta))){
    if((
      !all(0 <= expanded_hyper_grid_list$eta, expanded_hyper_grid_list$eta <= 1)
      )){
      stop("eta should be set in interval [0,1]")
    }
    }

    #max_depth
    if(!all(is.null(expanded_hyper_grid_list$max_depth))){
    if((
      any(
        !all(expanded_hyper_grid_list$max_depth > 0),
        !all(expanded_hyper_grid_list$max_depth == floor(expanded_hyper_grid_list$max_depth)
        )
      )
    )){
      stop("max_depth should be positive with no decimals")
    }
    }


    #colsample_bytree
    if(!all(is.null(expanded_hyper_grid_list$colsample_bytree))){
    if((
      !all(0 <= expanded_hyper_grid_list$colsample_bytree, expanded_hyper_grid_list$colsample_bytree <= 1))){
      stop("colsample_bytree should be set in interval [0,1]")
    }
    }

    #subsample
    if(!all(is.null(expanded_hyper_grid_list$subsample))){
    if((
      !all(0 <= expanded_hyper_grid_list$subsample, expanded_hyper_grid_list$subsample <= 1))){
      stop("subsample should be set in interval [0,1]")
    }
    }
  }

  if(ml_algorithm == "nn"){
  #droprate
  if(!all(is.null(expanded_hyper_grid_list$droprate))){
  if((
    !all(0 <= expanded_hyper_grid_list$droprate, expanded_hyper_grid_list$droprate < 1))){
    stop("droprate should be set in interval [0,1)")
  }
  }

  #batch size
  if(!all(is.null(expanded_hyper_grid_list$size_of_batch))){
    if((
      any(
        !all(expanded_hyper_grid_list$size_of_batch > 0),
        !all(expanded_hyper_grid_list$size_of_batch == floor(expanded_hyper_grid_list$size_of_batch)
        )
      )
    )){
      stop("size_of_batch should be positive with no decimals")
    }
    if(
      !all(log2(expanded_hyper_grid_list$size_of_batch) == floor(log2(expanded_hyper_grid_list$size_of_batch)))
    ){
      warning("size_of_batch should preferably be power of 2.")
    }

  }

   #epochs
   if(!all(is.null(expanded_hyper_grid_list$number_of_epochs))){
     if((
       any(
         !all(expanded_hyper_grid_list$number_of_epochs > 0),
         !all(expanded_hyper_grid_list$number_of_epochs == floor(expanded_hyper_grid_list$number_of_epochs)
         )
       )
     )){
       stop("number_of_epochs should be positive with no decimals")
     }
   }

  }
  ################################

  return(expanded_hyper_grid_list)

}
