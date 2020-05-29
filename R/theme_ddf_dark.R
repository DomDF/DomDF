#' theme_ddf_dark
#'
#' A pleasing dark theme for ggplot graphics.
#' @param base_family The font to display. Defaults to 'Bahnschrift' - a font in the extrafont library.
#' @param base_size The font size to display. Defaults to 12.
#' @keywords 
#' @export
#' @examples
#' ggplot(data = txhousing, mapping = aes(x = date, y = median)) + geom_point(alpha = 0.1) + theme_ddf_dark()

theme_ddf_dark <- function (base_size = 12, base_family = extrafont::choose_font(fonts = 'Bahnschrift')) 
{
  
  require(ggplot2); require(extrafont)
  
  half_line <- base_size
  
  background <- 'grey20'; foreground <- 'white'
  # params <- ls(pattern = '^geom_', env = as.environment('package:ggplot2')); geoms <- gsub('geom_', '', params)
  # lapply(X = unlist(geoms), FUN = update_geom_defaults(list(col = 'foreground', fill = 'foreground')))
  
  update_geom_defaults(geom = 'point', list(col = foreground, fill = foreground))
  #  update_geom_defaults(geom = 'jitter', list(col = foreground, fill = foreground))
  #  update_geom_defaults(geom = 'count', list(col = foreground, fill = foreground))
  
  update_geom_defaults(geom = 'line', list(col = foreground))
  update_geom_defaults(geom = 'path', list(col = foreground))
  update_geom_defaults(geom = 'errorbar', list(col = foreground))
  update_geom_defaults(geom = 'curve', list(col = foreground))
  update_geom_defaults(geom = 'polygon', list(col = foreground))
  
  update_geom_defaults(geom = 'contour', list(col = foreground))
  #  update_geom_defaults(geom = 'contour_filled', list(col = foreground, fill = foreground))
  update_geom_defaults(geom = 'density', list(col = foreground))
  update_geom_defaults(geom = 'density_2d', list(col = foreground))
  
  update_geom_defaults(geom = 'bar', list(col = foreground, fill = foreground))
  update_geom_defaults(geom = 'col', list(col = foreground, fill = foreground))
  #  update_geom_defaults(geom = 'histogram', list(col = foreground, fill = foreground))
  
  update_geom_defaults(geom = 'text', list(col = foreground))  
  theme(
    line = element_line(colour = 'black', size = 0.5, 
                        linetype = 1, lineend = 'round'), 
    rect = element_rect(fill = background, colour = NA),
    text = element_text(family = base_family, face = 'plain',
                        colour = foreground, size = base_size,
                        lineheight = 0.9,  hjust = 0.5,
                        vjust = 0.5, angle = 0, 
                        margin = margin(), debug = FALSE), 
    
    axis.line = element_blank(), 
    axis.text = element_text(size = base_size*2/3, family = base_family, colour = foreground),
    axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), 
                               vjust = 1, angle = 30), 
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2),
                               hjust = 1, angle = 30),
    axis.ticks = element_line(colour = foreground), 
    axis.ticks.length = unit(half_line / 2, 'pt'), 
    axis.title.x = element_text(margin = margin(t = 0.8 * half_line/3,
                                                b = 0.8 * half_line/2)),
    axis.title.y = element_text(angle = 90, 
                                margin = margin(r = 0.8 * half_line/3,
                                                l = 0.8 * half_line/2)),
    
    legend.background = element_rect(colour = background), 
    #legend.margin = unit(0.2, 'cm'), 
    legend.key = element_rect(fill = background, colour = NA),
    legend.key.size = unit(1.2, 'lines'), 
    legend.key.height = NULL,
    legend.key.width = NULL, 
    legend.text = element_text(colour = foreground, family = base_family, size = base_size*2/3),
    legend.text.align = NULL,
    legend.title = element_blank(), #text(hjust = 0), 
    legend.title.align = NULL, 
    legend.position = 'top', 
    legend.direction = NULL,
    legend.justification = 'center', 
    legend.box = NULL, 
    
    panel.background = element_rect(fill = background, colour = NA),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), #element_line(colour = foreground, linetype = 2, size = 0.1), 
    panel.grid.minor = element_blank(), 
    #panel.margin = unit(half_line, "pt"), panel.margin.x = NULL, 
    #panel.margin.y = NULL, panel.ontop = FALSE, 
    
    strip.background = element_blank(), #element_rect(fill = "grey85", colour = NA),
    strip.text = element_text(family = base_family, colour = foreground, size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line/2,
                                                b = 0)),#half_line/4)), 
    strip.text.y = element_text(angle = -90, 
                                margin = margin(l = half_line/2, 
                                                r = 0)),#half_line/4)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    
    #    plot.background = element_rect(colour = background), 
    plot.title = element_text(size = rel(1.2), 
                              margin = margin(b = half_line, unit = 'pt'),
                              hjust = 0),
    plot.margin = margin(t = half_line/2, r = half_line/2, b = half_line/2, l = half_line/2),
    complete = TRUE)
}
