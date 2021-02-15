#' lnorm_params
#'
#' Obtain the parameters from a lognormal distribution based on the equivalent parameters from a normal distribution.
#' Returns a list of 'meanlog' and 'sdlog' (equivalent to the arguments from the lnorm family of functions).
#'
#'
#' @param norm_mean The mean value of the associated normal distribution - REQUIRED.
#' @param norm_sd  The standard deviation of the associated normal distribution - REQUIRED.
#' @keywords Lognormal, Distribution Parameters
#' @export
#' @examples
#' lognormal_parameters <- lnorm_params(norm_mean = 1, norm_sd = 1)
#' lognormal_mean <- lognormal_parameters$meanlog

lnorm_params <- function(norm_mean, norm_sd){

  sdlog <- sqrt(log(1 + norm_sd^2 / norm_mean^2))
  meanlog <- log(norm_mean) - 0.5 * sdlog^2

  lnorm_parameter_list <- list('meanlog' = meanlog, 'sdlog' = sdlog)

  return(lnorm_parameter_list)

}



