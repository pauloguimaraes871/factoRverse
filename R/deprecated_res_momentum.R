#' Residual Momentum Calculation
#'
#' Calculate residual momentum based on two sets of asset returns (main and complementary),
#' two sets of benchmark returns (main and complementary), beta, and alpha.
#'
#' @param ret_assets_main A matrix, data.frame, or tibble of asset returns.
#'                        Rows represent assets and columns represent time periods.
#' @param ret_assets_complementary A matrix, data.frame, or tibble of complementary asset returns.
#' @param ret_bench_main A matrix, data.frame, or tibble of benchmark returns.
#'                       Rows represent benchmarks and columns represent time periods.
#' @param ret_bench_complementary A matrix, data.frame, or tibble of complementary benchmark returns.
#' @param beta_bench A matrix, data.frame, or tibble of beta values corresponding to each asset.
#' @param alpha_bench A matrix, data.frame, or tibble of alpha values corresponding to each asset.
#'
#' @return A matrix containing the residual momentum values. Rows represent assets and columns represent time periods.
#'
#' @examples
#' ret_assets_main <- matrix(rnorm(100), nrow = 10)
#' ret_assets_complementary <- matrix(rnorm(100), nrow = 10)
#' ret_bench_main <- matrix(rnorm(100), nrow = 10)
#' ret_bench_complementary <- matrix(rnorm(100), nrow = 10)
#' beta_bench <- matrix(runif(100), nrow = 10)
#' alpha_bench <- matrix(runif(100), nrow = 10)
#' res_momentum(ret_assets_main, ret_assets_complementary, ret_bench_main,
#'               ret_bench_complementary, beta_bench, alpha_bench)
#'
#' @export
res_momentum <- function(ret_assets_main, ret_assets_complementary,
                         ret_bench_main, ret_bench_complementary, beta_bench, alpha_bench){

  # Check that all inputs are either matrices, data frames, or tibbles
  if (!all(sapply(list(ret_assets_main, ret_assets_complementary,
                       ret_bench_main, ret_bench_complementary,
                       beta_bench, alpha_bench), function(mat) {
                         is.matrix(mat) || is.data.frame(mat) || tibble::is_tibble(mat)
                       }))) {
    stop("All inputs must be matrices, data.frames, or tibbles.")
  }

  # Convert data frames and tibbles to matrices
  matrices <- lapply(list(ret_assets_main, ret_assets_complementary,
                          ret_bench_main, ret_bench_complementary,
                          beta_bench, alpha_bench), function(mat) {
                            if (is.data.frame(mat) || tibble::is_tibble(mat)) {
                              return(as.matrix(mat))
                            }
                            return(mat)
                          })

  # Extract matrices after conversion
  ret_assets_main <- matrices[[1]]
  ret_assets_complementary <- matrices[[2]]
  ret_bench_main <- matrices[[3]]
  ret_bench_complementary <- matrices[[4]]
  beta_bench <- matrices[[5]]
  alpha_bench <- matrices[[6]]

  # Validate dimensions
  if(nrow(ret_assets_main) != nrow(ret_assets_complementary)){
    stop("ret_assets_main and ret_assets_complementary should have same number of rows.")
  }

  if(nrow(ret_bench_main) != nrow(ret_bench_complementary)){
    stop("ret_bench_main and ret_bench_complementary should have same number of rows.")
  }

  if(ncol(ret_bench_main) != ncol(ret_assets_main)){
    stop("ret_bench_main and ret_assets_main should have same number of columns")
  }

  if(ncol(ret_bench_complementary) != ncol(ret_assets_complementary)){
    stop("ret_assets_complementary and ret_bench_complementary should have same number of columns")
  }

  if(any(dim(beta_bench) != dim(ret_assets_main))){
    stop("beta_bench and ret_assets_main should have same dimension")
  }

  if(any(dim(alpha_bench) != dim(ret_assets_main))){
    stop("alpha_bench and ret_assets_main should have same dimension")
  }

  if(any(is.na(ret_bench_main)) | any(is.na(ret_bench_complementary))){
    stop("There should not be any NAs in Bench Vector")
  }


  res_momentum_matrix <- matrix(NA, nrow = nrow(ret_assets_main), ncol = (ncol(ret_assets_main)))
  complete_matrix_ret_assets <- cbind(ret_assets_complementary, ret_assets_main) #Join both matrices
  complete_matrix_ret_bench <- cbind(ret_bench_complementary, ret_bench_main) #Join both matrices
  size_complementary_matrix <- ncol(ret_assets_complementary) #Size of complementary matrix

  for(i in 1:(nrow(ret_assets_main))){
    for(j in 1:(ncol(ret_assets_main))){

      past_plus_present_ret_assets <- unlist(complete_matrix_ret_assets[i,j:(j+size_complementary_matrix)]) #Include more recent month in calculation
      past_plus_present_ret_bench <- unlist(complete_matrix_ret_bench[1,j:(j+size_complementary_matrix)]) #Include more recent month in calculation - 1 to provide only ibov return

      if(all(is.na(past_plus_present_ret_assets)) | is.na(beta_bench[i,j]) | is.na(alpha_bench[i,j])){ #If all is NA, result should be NA
        res_momentum_matrix[i,j] <- NA
      } else {
        est_ret <- alpha_bench[i,j] + beta_bench[i,j] * past_plus_present_ret_bench
        errors <- (past_plus_present_ret_assets - est_ret)
        res_momentum_matrix[i,j] <- sum(errors, na.rm = TRUE)
      }
    }
  }
  return(res_momentum_matrix)
}
