#' Metropolis_RW
#'
#' A Metroplis random walk Markov Chain Monte Carlo sampler to draw from an n-dimensional posterior distribution defined with Gaussian priors and likelihoods.
#' Returns a tibble (tidy, long format) with details of each iteration, for each parameter, for each chain.
#'
#'
#' @param n_dims The number of dimensions of the distributions considered - REQUIRED.
#' @param iterations The total number of samples, including burn-in, to draw from each chain - REQUIRED.
#' @param mu_prior An n-dimesional vector of mean values for the prior model - REQUIRED.
#' @param var_prior An n-dimesional covriance matrix for the prior model - REQUIRED.
#' @param mu_lik An n-dimesional vector of mean values for the likelihood - REQUIRED.
#' @param var_lik An n-dimesional covriance matrix for the likelihood - REQUIRED.
#' @param n_chains The number of separate chains (with their own initial co-ordinates) to run - defaults to 4 chains.
#' @param n_burn_in The number of samples to discard as part f the burn in phase - defaults to half of all samples.
#' @param x_initials An n-dimensional vector of initial values for each chain - defaults to a sequence between +/- n_dims
#' @param prop_var The variance of the n-dimensional symmetric proposal distribution. A tuning parameter - defaults to 1
#' @param annealing A multiplicative simulated annealing parameter - defaults to 1 (equivalent of no annealing)
#' @param MCMC_seed  A random seed value for reproducable analysis - defaults to 1008
#' @param thinning  A parameter determining how many successive iterations should be disregarding to mitigate excessive auto-correlation - defaults to 1
#' @keywords
#' @export
#' @examples
#' one_dim_mcmc_samples <- Metropolis_RW(n_dims = 1, iterations = 1e3, mu_prior = 0, var_prior = 10, mu_lik = 3, var_lik = 1)
#' two_dim_mcmc_samples <- Metropolis_RW(n_dims = 2, iterations = 1e3, mu_prior = c(0, 0), var_prior = diag(2), mu_lik = c(3, 3), var_lik = diag(2))

Metropolis_RW <- function(n_dims, iterations, mu_prior, var_prior, mu_lik, var_lik,
                          n_chains = 4, n_burn_in = iterations/2, x_initials = 'default',
                          prop_var = 1, annealing = 1, MCMC_seed = 1008, thinning = 1){

  require(mvtnorm); require(tidyverse)

  n_dims <- as.integer(n_dims); if(n_dims < 1) stop('Please provide a positive integer number of dimensions: n_dim')

  if(x_intials == 'default') {

    x_initials <- seq(from = -n_dims, to = n_dims, length.out = n_dims * n_chains)

  } else (x_initials <- x_initials)

  set.seed(seed = MCMC_seed)

  if(n_dims > 1){

    x <- data.frame(matrix(ncol = n_dims))

    df_MetRW <- cbind(data.frame(chain = integer(length = 1), iter = integer(length = 1), t = double(length = 1),
                                 d_trial = double(length = 1), d_prop = double(length = 1)), x)

  } else {

    df_MetRW <- data.frame(chain = integer(), iter = integer(), t = double(),
                           d_trial = double(), d_prop = double(), x = double())

  }

  for(chain in seq(from = 1, to = n_chains, by = 1)){

    t <- 1; iter <- 0; d_trial <- 0; d_prop <- 0; n_iter <- iterations

    if(n_dims > 1) {

      x <- x_initials[seq(from = chain, to = n_dims * n_chains, by = n_chains)]

      for(i in seq(from = 1, to = n_iter, by = 1)){

        new_iter <- iter + 1;

        prop <- x + rmvnorm(n = 1, mean = rep(x = 0, times = n_dims),
                            sigma = diag(x = prop_var,
                                         nrow = n_dims, ncol = n_dims))

        d_trial <- prod(c(mvtnorm::dmvnorm(x = x, mean = mu_prior, sigma = var_prior),
                          mvtnorm::dmvnorm(x = x, mean = mu_lik, sigma = var_lik)))
        d_prop <- prod(c(mvtnorm::dmvnorm(x = prop, mean = mu_prior, sigma = var_prior),
                         mvtnorm::dmvnorm(x = prop, mean = mu_lik, sigma = var_lik)))

        t <- t * annealing  #'Simulated Annealing'

        if(d_prop > d_trial){
          iter <- new_iter; x <- prop
        } else{
          if(t * runif(1) < (d_prop / d_trial)){
            iter <- new_iter; x <- prop
          } else {
            iter <- new_iter
          }
        }

        df_MetRW <- rbind(df_MetRW,
                          cbind(data.frame(chain, iter, t, d_trial, d_prop),
                                data.frame(matrix(data = x, ncol = n_dims))))

      }

    } else {

      x <- x_initials[chain]

      for(i in seq(from = 1, to = n_iter, by = 1)){

        new_iter <- iter + 1;

        prop <- x + rnorm(n = 1, sd = sqrt(prop_var))

        d_trial <- prod(c(dnorm(x = x, mean = mu_prior, sd = sqrt(var_prior)),
                          dnorm(x = x, mean = mu_lik, sd = sqrt(var_lik))))
        d_prop <- prod(c(dnorm(x = prop, mean = mu_prior, sd = sqrt(var_prior)),
                         dnorm(x = prop, mean = mu_lik, sd = sqrt(var_lik))))

        t <- t * annealing  #'Simulated Annealing'

        if(d_prop > d_trial){
          iter <- new_iter; x <- prop
        } else{
          if(t * runif(1) < (d_prop / d_trial)){
            iter <- new_iter; x <- prop
          } else {
            iter <- new_iter
          }
        }

        df_MetRW <- rbind(df_MetRW, data.frame(chain, iter, x, t, d_trial, d_prop))

      }

    }

  }

  if(n_dims > 1){

    df_MetRW <- df_MetRW[-1,]

    df_MetRW <- df_MetRW %>%
      dplyr::mutate(status = dplyr::case_when(
        iter < n_burn_in ~ 'burn_in',
        TRUE ~ 'sample'
      )) %>%
      tidyr::pivot_longer(cols = colnames(df_MetRW[,grepl(pattern = 'X|x',
                                                          x = colnames(df_MetRW))]),
                          names_to = 'parameter')
  } else{

    df_MetRW <- df_MetRW %>%
      dplyr::mutate(status = dplyr::case_when(
        iter < n_burn_in ~ 'burn_in',
        TRUE ~ 'sample'
      )) %>%
      tidyr::pivot_longer(cols = colnames(df_MetRW %>%
                                            select(x)),
                          names_to = 'parameter')

  }

  if (thinning != 1) {

    df_MetRW <- df_MetRW %>%
      filter(iter %% thinning == 0) %>%
      mutate(iter = iter / thinning)

  }


  return(df_MetRW)

}
