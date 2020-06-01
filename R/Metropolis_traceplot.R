#' Metropolis_traceplot
#'
#' Returns a ggplot object of a traceplot of samples from a Metroplis random walk Markov Chain Monte Carlo sampler (see DomDF::Metropolis_RW())
#' @param mcmc_data Data of the form produced by DomDF::Metroplois_RW() - REQUIRED.
#' @param include_burn_in Whether burn-in samples should be included in the plot. A vertical line is added to show this division when set to TRUE. - Defaults to FALSE.
#' @param alpha Transparancy of each chain in the plot - Defaults to 0.4. 
#' @keywords 
#' @export
#' @examples
#' Metropolis_traceplot(mcmc_data = four_dim_mcmc_samples)
#' Metropolis_traceplot(mcmc_data = one_dim_mcmc_samples)
#' 
Metropolis_traceplot <- function(mcmc_data, include_burn_in = FALSE, alpha = 0.4){
  
  require(tidyverse)
  
  if (include_burn_in == FALSE) {
    
    ggplot(data = mcmc_data %>% 
             dplyr::filter(status == 'sample'), mapping = aes(x = iter, y = value))+
      geom_line(mapping = aes(col = as.factor(chain)), alpha = alpha)+
      facet_wrap(facets = ~ parameter, ncol = 1)+
      theme_ddf_light() + labs(col = 'Chain') + theme(legend.title = element_text())
    
    
  } else {
    
    ggplot(data = mcmc_data,
           mapping = aes(x = iter, y = value))+
      geom_line(mapping = aes(col = as.factor(chain)), alpha = alpha)+
      geom_vline(xintercept = min((mcmc_data %>% 
                                     dplyr::filter(status == 'sample'))$iter), 
                 linetype = 2)+
    facet_wrap(facets = ~ parameter, ncol = 1)+
      theme_ddf_light() + labs(col = 'Chain') + theme(legend.title = element_text())
    
  }
  
}
