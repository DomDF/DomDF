#' VoPI
#'
#' An Estimation of the Value of Perfect Information for a One-Dimensional Decision Analysis Under Uncertainty Problem.
#' This function uses a combination of importance sampling and latin hypercube sampling by default, but other algorithms are available (see the 'method' parameter).
#' 
#' @param prior_dist A string representing the prior distribution function, for example 'norm', or 'exp' - REQUIRED.
#' @param prior_args A list containing the parameter values of the prior distribution, for example list(mean = 1, sd = 1), or list(rate = 1) - REQUIRED.
#' @param lim_dist A string representing the limit distribution function (that which has an associated cost if exceeded), for example 'norm', or 'exp' - REQUIRED.
#' @param lim_args A list containing the parameter values of the limit distribution function (that which has an associated cost if exceeded), for example list(mean = 1, sd = 1), or list(rate = 1) - REQUIRED.
#' @param ExpCost_lim_ex A numeric cost associated with exceeding the limit distribution - REQUIRED.
#' @param ExpCost_rm A numeric cost associated with performing the risk mitigation option being evaluated - REQUIRED.
#' @param rm_lim_ex The probability of exceeding the limit function following completion of the risk mitigation option being evaluated - Defaults to 0.
#' @param n_samples The total number of samples from which an expected value of perfect information will be estimated - Defaults to 10^4.
#' @param method The sampling method used in the calculation. 'is' is Importance Sampling, 'lhs' is Latin Hypercube sampling and 'mc' is Monte Carlo sampling - Defaults to 'is'.
#' @param VoPI_seed A random seed for reproduceable sampling. Enter NULL if this is not required. - Defaults to 1008.
#' 
#' @keywords
#' @export
#' @examples
#' estimated_VoPI <- VoPI(prior_dist = 'exp', prior_args = list(rate = 3), lim_dist = 'norm', lim_args = list(mean = 3, sd = 1), EC_lim_ex = 1e5)

VoPI <- function(prior_dist, prior_args, lim_dist, lim_args, ExpCost_lim_ex,
                 ExpCost_rm, rm_lim_ex = 0,
                 n_samples = 1e4, method = 'is+lhs', VoPI_seed = 1008){

  require(tidyverse); require(lhs)
  
  if(!grepl(pattern = 'beta|binom|cauchy|exp|gamma|lnorm|norm|unif|weibull', 
            x = prior_dist) == TRUE) stop('Please select a valid prior distribution')
  
  if(!grepl(pattern = 'beta|binom|cauchy|exp|gamma|lnorm|norm|unif|weibull', 
            x = lim_dist) == TRUE) stop('Please select a valid limit distribution')
  
  if (method != 'lhs' & method != 'mc' & method != 'is' & method != 'is+lhs') {
    
    stop("Please enter a valid sampling method (currently, only 'is+lhs', 'is', 'lhs' and 'mc' are available")
    
  }
  
  set.seed(seed = VoPI_seed)
  
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
  
 if (grepl(pattern = 'lhs', x = method)){

    prior_probs <- lhs::randomLHS(n = n_samples, k = 1)
    lim_probs <- lhs::randomLHS(n = n_samples, k = 1)
    
  } else {
    
    prior_probs <- runif(n = n_samples)
    lim_probs <- runif(n = n_samples)
    
  }
  
  if (grepl(pattern = 'is', x = method)) {
    
    samples_df <- data.frame(prior_samples = double(length = n_samples), is_prior_samples = double(length = n_samples), 
                             lim_samples = double(length = n_samples), 
                             is_lik = double(length = n_samples), corr_lik = double(length = n_samples))
    
    is_prior_probs <- 0
    
    for (i in seq(from = 1, to = nrow(samples_df), by = 1)){
      
      is_prior_probs[i] <- rlang::eval_tidy(str2lang(paste0('p', prior_dist, '(', 'q', lim_dist, '(', prior_probs[i], ',', lim_args_proc, ')', ',', prior_args_proc, ')')))
      
      samples_df$prior_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', prior_dist, '(', prior_probs[i], ',', prior_args_proc, ')')))
      samples_df$is_prior_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', prior_dist, '(', is_prior_probs[i], ',', prior_args_proc, ')')))
      
      samples_df$lim_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', lim_dist, '(', lim_probs[i], ',', lim_args_proc, ')')))
      
      samples_df$is_lik[i] <- rlang::eval_tidy(str2lang(paste0('d', lim_dist, '(x = ', samples_df$is_prior_samples[i], ',',  lim_args_proc, ', log = TRUE)')))
      samples_df$corr_lik[i] <- rlang::eval_tidy(str2lang(paste0('d', prior_dist, '(x = ', samples_df$is_prior_samples[i], ',',  prior_args_proc, ', log = TRUE)')))
      
      samples_df$pr_lim_ex[i] <- rlang::eval_tidy(str2lang(paste0('p', lim_dist, '(', samples_df$prior_samples[i], ',', lim_args_proc, ')')))
      
      samples_df$is_pr_lim_ex[i] <- exp(samples_df$corr_lik[i] - samples_df$is_lik[i]) * rlang::eval_tidy(str2lang(paste0('p', lim_dist, '(', samples_df$is_prior_samples[i], ',', lim_args_proc, ')')))
        
    }
    
    ExpCost_prior <- min(mean(samples_df$is_pr_lim_ex) * (ExpCost_lim_ex),
                         ExpCost_rm + (rm_lim_ex * ExpCost_lim_ex))
    
  } else {
    
    samples_df <- data.frame(prior_samples = double(length = n_samples), lim_samples = double(length = n_samples))
    
    for (i in seq(from = 1, to = n_samples, by = 1)){
      
      samples_df$prior_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', prior_dist, '(', prior_probs[i], ',', prior_args_proc, ')')))
      samples_df$lim_samples[i] <- rlang::eval_tidy(str2lang(paste0('q', lim_dist, '(', lim_probs[i], ',', lim_args_proc, ')')))
      
      samples_df$pr_lim_ex[i] <- rlang::eval_tidy(str2lang(paste0('p', lim_dist, '(', samples_df$prior_samples[i], ',', lim_args_proc, ')')))
      
    }
    
    ExpCost_prior <- min(mean(samples_df$pr_lim_ex) * (ExpCost_lim_ex),
                         ExpCost_rm + (rm_lim_ex * ExpCost_lim_ex))
                         
  }
  
  VoPI_int_df <- samples_df %>% 
    mutate(ExpCost = min(pr_lim_ex * (ExpCost_lim_ex),
                         ExpCost_rm + (rm_lim_ex * ExpCost_lim_ex))) %>% 
    mutate(opt_act = case_when(
      ExpCost == ExpCost_rm + (rm_lim_ex * ExpCost_lim_ex) ~ 'Risk mitigation',
      TRUE ~ 'No action'
    ))
  
  opt_action_prior <- case_when(
    ExpCost_prior == ExpCost_rm + (rm_lim_ex * ExpCost_lim_ex) ~ 'Risk mitigation',
    TRUE ~ 'No action'
  )
  
  ExpCost_prepost <- mean(VoPI_int_df$ExpCost)
  opt_action_prepost <- case_when(
    ExpCost_prepost == ExpCost_rm + (rm_lim_ex * ExpCost_lim_ex) ~ 'Risk mitigation',
    TRUE ~ 'No action'
  )
  
  Exp_VoPI <- max(0, (ExpCost_prior - ExpCost_prepost))
  
  VoPI_df <- data.frame(ExpCost_prior, opt_action_prior, ExpCost_prepost, opt_action_prepost, 
                        Exp_VoPI, sampling_method = method, n_samples = n_samples)
  
  return(VoPI_df)

}
