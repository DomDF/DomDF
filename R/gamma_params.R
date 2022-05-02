#' gamma_params
#'
#' Obtain the parameters from a Gamma distribution based on the equivalent parameters from a normal distribution.
#' Returns a list of 'loc' and 'scale'.
#'
#'
#' @param norm_mean The mean value of the associated normal distribution - REQUIRED.
#' @param norm_sd  The standard deviation of the associated normal distribution - REQUIRED.
#' @keywords Gamma, Distribution Parameters
#' @export
#' @examples
#' gamma_parameters <- gamma_params(norm_mean = 1, norm_sd = 1)
#' gamma_scale <- gamma_parameters$scale

gamma_params <- function(norm_mean, norm_sd){

  gamma <- 0.5772

  scale <- (1/pi) * norm_sd * sqrt(6)
  loc <- norm_mean - gamma * scale

  gamma_parameter_list <- list('loc' = loc, 'scale' = scale)

  return(gamma_parameter_list)

}



