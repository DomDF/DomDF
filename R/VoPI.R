#' VoPI
#'
#' A Crude Monte Carlo Estimation of the Value of Perfect Information for a One-Dimensional Decision Analysis Under Uncertainty Problem
#' 
#' @param prior_dist A string representing the prior distribution function, for example 'norm', or 'exp' - REQUIRED.
#' @param prior_args A list containing the parameter values of the prior distribution, for example list(mean = 1, sd = 1), or list(rate = 1) - REQUIRED.
#' @param lim_dist A string representing the limit distribution function (that which has an associated cost if exceeded), for example 'norm', or 'exp' - REQUIRED.
#' @param lim_args A list containing the parameter values of the limit distribution function (that which has an associated cost if exceeded), for example list(mean = 1, sd = 1), or list(rate = 1) - REQUIRED.
#' @param EC_lim_ex A numeric cost associated with exceeding the limit distribution - REQUIRED.
#' @param n_samples The total number of samples from which an expected value of perfect information will be estimated - Defaults to 10^4.

#' @keywords
#' @export
#' @examples
#' estimated_VoPI <- VoPI(prior_dist = 'exp', prior_args = list(rate = 3), lim_dist = 'norm', lim_args = list(mean = 3, sd = 1), EC_lim_ex = 1e5)

VoPI <- function(prior_dist, prior_args, lim_dist, lim_args, EC_lim_ex, n_samples = 1e4){

  require(tidyverse)
  
  if(!grepl(pattern = 'beta|binom|cauchy|exp|gamma|lnorm|norm|unif|weibull', 
            x = prior_dist) == TRUE) stop('Please select a valid prior distribution')
  
  if(!grepl(pattern = 'beta|binom|cauchy|exp|gamma|lnorm|norm|unif|weibull', 
            x = lim_dist) == TRUE) stop('Please select a valid limit distribution')

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
  
  prior_samples <- rlang::eval_tidy(str2lang(paste0('r', prior_dist, '(', n_samples, ',', prior_args_proc, ')'))) 
  lim_samples <- rlang::eval_tidy(str2lang(paste0('r', lim_dist, '(', n_samples, ',', lim_args_proc, ')'))) 
  
  EC_prior <- sum(prior_samples > lim_samples) * (EC_lim_ex / n_samples)
  
  VoPI_df <- data.frame(prior_samples, prob_lim_ex = double(length = length(prior_samples)), EC = double(length = length(prior_samples))) %>% 
    dplyr::arrange(prior_samples)
  
  for(i in seq(from = 1, to = n_samples, by = 1)){
    
    VoPI_df$prob_lim_ex[i] <- rlang::eval_tidy(str2lang(paste0('p', lim_dist, '(', VoPI_df$prior_samples[i], ',', lim_args_proc, ')')))
    VoPI_df$EC[i] <- VoPI_df$prob_lim_ex[i] * (EC_lim_ex / n_samples)
    
  }
  
  EC_prepost <- mean(VoPI_df$EC)
  
  VoPI <- max(0, (EC_prior - EC_prepost))
  
  return(VoPI)

}
