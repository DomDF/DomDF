library(DomDF)

Pugsley <- dplyr::tribble(
  
  ~wing_strength_lower, ~wing_strength_upper, ~number_of_wings, ~load_freq, ~failures,
  0, 2, 0, NA, 0,
  2, 4, 0, 30000, 0,
  4, 6, 0, 5000, 0,
  6, 8, 0, 100, 0,
  8, 10, 2, 10, 20, 
  10, 12, 12, 1, 12,
  12, 14, 74, 0, 0,
  14, 16, 12, 0, 0,
  16, 18, 2, 0, 0
  
)

# load_freq = 'frequency of loads sufficient to break wings in each interval (in 10^4 flying hours)'
# failures = 'wing failures in 10^6 hours'

std_plot <- ggplot(data = txhousing, mapping = aes(x = date, y = median))+
  geom_point(alpha = 0.2, col = 'black')+
  ggtitle('ggplot(data = txhousing, \nmapping = aes(x = date, y = median)) +\ngeom_point(alpha = 0.2)')

ddf_light_plot <- std_plot +
  geom_point(alpha = 0.2, col = 'grey20')+
  theme_ddf_light()+
  ggtitle('... + theme_ddf_light()')

ddf_dark_plot <- std_plot+
  geom_point(alpha = 0.2, col = 'white')+
  theme_ddf_dark()+
  ggtitle('... + theme_ddf_dark()')

library(patchwork)

comp_plot <- std_plot + ddf_light_plot + ddf_dark_plot

LNs <- function(m,s) sqrt(log(1+s^2/m^2)); LNm <- function(m,s) log(m)-0.5*LNs(m,s)^2
Gscale <- function(s) s*sqrt(6)/pi; Glocation <- function(m,s) m-gamma*Gscale(s)
