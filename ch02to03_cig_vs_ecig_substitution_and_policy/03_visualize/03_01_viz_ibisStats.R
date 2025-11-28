plotIbisStats(transformation = identity,
              yaxis = "Revenue (Billions of Dollars",
              line_size = 1.5,
              point_size = 2.5) %>%
  ggsave(
    paste0(direcs$pap.fig, "ibis/ibis_rev.png"), .
  )

plotIbisStats(transformation = log,
              yaxis = "Revenue (Billions of Dollars) - Log Scale",
              line_size = 1.5,
              point_size = 2.5) %>%
  ggsave(
    paste0(direcs$pap.fig, "ibis/ibis_rev_log.png"), .
  )



