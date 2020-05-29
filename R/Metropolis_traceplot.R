#' Metropolis_traceplot
#'
#' A traceplot of samples from a Metroplis random walk Markov Chain Monte Carlo sampler
#' @param Metropolis_RW_dataframe A dataframe produced by DomDF::Metroplois_RW() - REQUIRED.
#' @param include_burn_in Whether burn-in samples should be included in the plot - Defaults to FALSE.
#' @param alpha Transparancy of each chain in the plot - Defaults to 0.4. 
#' @keywords 
#' @export
#' @examples
#' theme_ddf_light()

Metropolis_traceplot <- function(Metropolis_RW_dataframe, include_burn_in = FALSE, alpha = 0.4){
  
  if (include_burn_in == FALSE) {
    
    ggplot(data = Metropolis_RW_dataframe %>% 
             dplyr::filter(status == 'sample'), mapping = aes(x = iter, y = value))+
      geom_line(mapping = aes(col = as.factor(chain)), alpha = alpha)+
      facet_wrap(facets = ~ parameter, ncol = 1)+
      theme_ddf_light() + labs(col = 'Chain') + theme(legend.title = element_text())
    
    
  } else {
    
    ggplot(data = Metropolis_RW_dataframe,
           mapping = aes(x = iter, y = value))+
      geom_line(mapping = aes(col = as.factor(chain)), alpha = alpha)+
      geom_vline(xintercept = max((Metropolis_RW_2d_dataframe %>% 
                                     dplyr::filter(status == 'burn-in'))$iter))
    facet_wrap(facets = ~ parameter, ncol = 1)+
      theme_ddf_light() + labs(col = 'Chain') + theme(legend.title = element_text())
    
  }
  
}
