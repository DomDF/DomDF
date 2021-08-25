#' tidy_mcmc_draws
#'
#' Obtain MCMC samples from a CmdStanR model, formatted for ggplot2 plotting.
#' Returns a a long, tidy tibble.
#'
#'
#' @param cmdstan_gq A CmdStan generated quantities object. These are generated using the $generate_quantities method from a CmdStanModel. REQUIRED
#' @param params  A vector of characters containing the parameters to be extracted. Defaults to 'all_params', which returns all parameters.
#' @keywords CmdStanR, MCMC, Tidy, Tibble
#' @export
#' @examples
#' gq_df <- tidy_mcmc_gq(cmdstan_gq = Bayesian_model_gq)
#' reduced_mcmc_gq <- tidy_mcmc_gq(cmdstan_fit = Bayesian_model_gq, params = c('A', 'm')) # Where 'A' and 'm' are parameters in the Stan program.

tidy_mcmc_gq <- function(cmdstan_gq, params = 'all_params'){

  require(cmdstanr); require(data.table); require(tibble); require(dplyr)

  if(cmdstan_gq$metadata()$method != 'generate_quantities') stop('Please provide a valid CmdStanGQ Object')

  if (length(params) == 1 & params[1] == 'all_params'){
    vars <- cmdstan_gq$metadata()$model_params
  } else {vars <- params}

  n_params <- length(vars)
  n_chains <- cmdstan_gq$num_chains()
  n_draws <- cmdstan_gq$metadata()$stan_variable_dims[[1]]

#  thinning <- cmdstan_fit$metadata()$thin

  draws_array <- cmdstan_gq$draws(variables = vars)

  gq_datatable <- data.table(Parameter = rep(x = vars[1:n_params], each = n_draws * n_chains),
                             Chain = rep(x = rep(x = 1:n_chains, each = n_draws), times = n_params),
                             Iteration = rep(1:n_draws, times = n_params * n_chains),
                             value = draws_array %>% as.vector())

  gq_tibble <- gq_datatable %>% as_tibble()

  return(gq_tibble)

}



