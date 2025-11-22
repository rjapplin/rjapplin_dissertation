plotIbisStats <- function(scale = 1e3,
                          group_colors = c(color_pal$sunset,
                                           color_pal$river_green,
                                           color_pal$wildcat_blue),
                          group_shapes = c(0, 1, 2),
                          transformation = identity,
                          xaxis = "Year",
                          yaxis,
                          variable = "Revenue",
                          les_pos = "bottom",
                          line_size = 1,
                          point_size = 1){
  
  ibisStats <- wrangleIbisData()
  g <- ibisStats %>%
    mutate(
      group = paste0(Ind, Type),
    ) %>%
    filter(Year <= 2022) %>%
    ggplot(
      aes(Year, 
          transformation(eval(as.name(variable))/scale), 
          color = group,
          shape = group)
    )  +
    geom_line(size = line_size) +
    geom_point(size = point_size) +
    theme_economist_white(gray_bg = FALSE) +
    labs(x = xaxis, y = yaxis) +
    scale_color_manual(values = group_colors,
                       labels = c("E-Cigarette Manufacturers",
                                  "Tobacco Manufacturers",
                                  "Tobacco Wholesalers"),
                       name = ""
                       ) +
    scale_shape_manual(values = group_shapes,
                       labels = c("E-Cigarette Manufacturers",
                                  "Tobacco Manufacturers",
                                  "Tobacco Wholesalers"),
                       name = ""
                       ) +
    theme(legend.position = les_pos) +
    guides(color = guide_legend(title = ""))

  return(g)
  
}
