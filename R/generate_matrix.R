#' Generate a Random Correlation Matrix
#'
#' This function creates a symmetric matrix of random correlations given a set of variable names.
#' The diagonal elements are always set to 1 (since any variable has a perfect correlation with itself).
#'
#' @param var_names A character vector containing the names of the variables.
#' @param range A numeric vector of length 2 specifying the range for the random correlations, default is c(-0.01, 0.01).
#'              The provided values should lie between -1 and 1.
#' @return A symmetric matrix of size length(var_names) x length(var_names) with random correlation values.
#' @examples
#' generate_cor_matrix(c("var1", "var2", "var3"))
#' generate_cor_matrix(c("var1", "var2"), range = c(-0.5, 0.5))
#' @export
generate_matrix <- function(var_names, range = c(-0.01, 0.01)) {
  # Input checks
  if (length(range) != 2 || any(range < -1) || any(range > 1)) {
    stop("Range should be a vector of length 2 with values between -1 and 1.")
  }
  
  n <- length(var_names)
  rho <- matrix(NA, nrow = n, ncol = n, dimnames = list(var_names, var_names))
  
  try(
    for (i in 1:n) {
      rho[i, i] <- 1  # Set diagonal value to 1
      for (j in (i + 1):n) {
        rho[i, j] <- runif(1, range[1], range[2])
        rho[j, i] <- rho[i, j]  # Make the matrix symmetric
      }
    }
    ,silent = TRUE
  )
  
  return(rho)
}
