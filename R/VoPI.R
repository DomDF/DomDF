#' VoPI
#'
#' A Latin Hypercube (or Crude Monte Carlo) Estimation of the Value of Perfect Information for a One-Dimensional Decision Analysis Under Uncertainty Problem
#' 
#' @param prior_dist A string representing the prior distribution function, for example 'norm', or 'exp' - REQUIRED.
#' @param prior_args A list containing the parameter values of the prior distribution, for example list(mean = 1, sd = 1), or list(rate = 1) - REQUIRED.
#' @param lim_dist A string representing the limit distribution function (that which has an associated cost if exceeded), for example 'norm', or 'exp' - REQUIRED.
#' @param lim_args A list containing the parameter values of the limit distribution function (that which has an associated cost if exceeded), for example list(mean = 1, sd = 1), or list(rate = 1) - REQUIRED.
#' @param EC_lim_ex A numeric cost associated with exceeding the limit distribution - REQUIRED.
#' @param n_samples The total number of samples from which an expected value of perfect information will be estimated - Defaults to 10^4.
#' @param method The sampling method used in the calculation. 'lhs' is Latin Hypercube sampling and 'mc' is Monte Carlo sampling - Defaults to 'lhs'.
#' @param VoPI_seed A random seed for reproduceable sampling. Enter NULL if this is not required. - Defaults to 1008.
#' 
#' @keywords
#' @export
#' @examples
#' estimated_VoPI <- VoPI(prior_dist = 'exp', prior_args = list(rate = 3), lim_dist = 'norm', lim_args = list(mean = 3, sd = 1), EC_lim_ex = 1e5)

VoPI <- function(prior_dist, prior_args, lim_dist, lim_args, ExpCost_lim_ex, n_samples = 1e4, method = 'lhs', VoPI_seed = 1008){

  require(tidyverse); require(lhs)
  
  if(!grepl(pattern = 'beta|binom|cauchy|exp|gamma|lnorm|norm|unif|weibull', 
            x = prior_dist) == TRUE) stop('Please select a valid prior distribution')
  
  if(!grepl(pattern = 'beta|binom|cauchy|exp|gamma|lnorm|norm|unif|weibull', 
            x = lim_dist) == TRUE) stop('Please select a valid limit distribution')
  
  if (method != 'lhs' & method != 'mc') stop("Please enter a valid sampling method (currently, only 'lhs' and 'mc' are available")
  
  set.seed(seed = VoPI_seed)
  
  if (method == 'lhs'){
    
    probs <- lhs::randomLHS(n = n_samples, k = 1)
    
  } else if (method == 'mc') {
    
    probs <- runif(n = n_samples)
    
  }
  
  process_args <- function(args){
    
    processed_args <- paste(as.numeric(args[1]))
    
    if(length(args) >= 2){
      
      for (i in seq(from = 2, to = length(args))){
        
        processed_args <- paste(processed_args, ',', as.numeric(args[i]))
        
      }
    } else {}
    
    return(processed_args)
    
  }
  
  prior_args_proc <- process_args(args = prior_args); lim_args_proc <- process_args(args = lim_args)
  
  samples_df <- data.frame(prior_samples = double(length = n_samples), lim_samples = double(length = n_samples))
  
  for (i in seq(from = 1, to = n_samples, by = 1)){
    
    samples_df$prior_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', prior_dist, '(', probs[i], ',', prior_args_proc, ')')))
    samples_df$lim_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', lim_dist, '(', probs[i], ',', lim_args_proc, ')')))
    
  }

  ExpCost_prior <- sum(samples_df$prior_samples > samples_df$lim_samples) * (ExpCost_lim_ex / n_samples)
  
  VoPI_df <- data.frame(samples_df$prior_samples, prob_lim_ex = double(length = n_samples), ExpCost = double(length = n_samples)) %>% 
    dplyr::arrange(samples_df$prior_samples)
  
  for(i in seq(from = 1, to = n_samples, by = 1)){
    
    VoPI_df$prob_lim_ex[i] <- rlang::eval_tidy(str2lang(paste0('p', lim_dist, '(', VoPI_df$samples_df.prior_samples[i], ',', lim_args_proc, ')')))
    VoPI_df$ExpCost[i] <- VoPI_df$prob_lim_ex[i] * (ExpCost_lim_ex / n_samples)
    
  }
  
  ExpCost_prepost <- mean(VoPI_df$ExpCost)
  
  Exp_VoPI <- max(0, (ExpCost_prior - ExpCost_prepost))
  
  VoPI_df <- data.frame(ExpCost_prior, ExpCost_prepost, Exp_VoPI, sampling_method = method, n_samples = n_samples)
  
  return(VoPI_df)

}
