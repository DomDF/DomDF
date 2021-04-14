#' tidy_mcmc_draws
#'
#' Obtain MCMC samples from a CmdStanR model, formatted for ggplot2 plotting.
#' Returns a a long, tidy tibble.
#'
#'
#' @param cmdstan_fit A CmdStanMCMC object. These are generated using the $sample method from a CmdStanModel. REQUIRED
#' @param params  A vector of characters containing the parameters to be extracted. Defaults to 'all_params', which returns all parameters.
#' @keywords CmdStanR, MCMC, Tidy, Tibble
#' @export
#' @examples
#' mcmc_df <- tidy_mcmc_draws(cmdstan_fit = Bayesian_model_fit)
#' reduced_mcmc_df <- tidy_mcmc_draws(cmdstan_fit = Bayesian_model_fit, params = c('A', 'm')) #Where 'A' and 'm' are parameters in the Stan program.

tidy_mcmc_draws <- function(cmdstan_fit, params = 'all_params'){

  require(cmdstanr); require(data.table); require(tibble); require(dplyr)

  if(cmdstan_fit$metadata()$method != 'sample') stop('Please provide a valid CmdStanMCMC Object')

  if (length(params) == 1 & params[1] == 'all_params'){
    vars <- cmdstan_fit$metadata()$model_params
  } else {vars <- params}

  n_params <- length(vars)
  n_chains <- cmdstan_fit$num_chains()
  n_draws <- cmdstan_fit$metadata()$iter_sampling

  thinning <- cmdstan_fit$metadata()$thin

  draws_array <- cmdstan_fit$draws(variables = vars)

  hmc_datatable <- data.table(Parameter = rep(x = vars[1:n_params], each = n_draws * n_chains),
                              Chain = rep(x = rep(x = 1:n_chains, each = n_draws), times = n_params),
                              Iteration = rep(1:n_draws, times = n_params * n_chains),
                              value = draws_array %>% as.vector())

  hmc_tibble <- hmc_datatable %>% as_tibble()

  return(hmc_tibble)

}



