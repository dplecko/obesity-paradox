
# splines
root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

# ANZICS full
p1 <- cowplot::plot_grid(
  plotlist = two_spline_plot("anzics", "bmi", "none"),
  ncol = 2L
)
ggsave(file.path(root, "results", "Figure2.png"), plot = p1,
       height = 5, width = 12)


# ANZICS sense
p2 <- cowplot::plot_grid(
  plotlist = two_spline_plot("anzics", "bmi_10pc", "none"),
  ncol = 2L
)
ggsave(file.path(root, "results", "eFigure2.png"), plot = p2,
       height = 5, width = 12)


# MIMIC-IV + SICdb
p3 <- cowplot::plot_grid(
  plotlist = c(
    two_spline_plot("miiv", "bmi", "none"),
    two_spline_plot("sic", "bmi", "none")
  ),
  ncol = 2L
)
ggsave(file.path(root, "results", "eFigure3.png"), plot = p3,
       height = 10, width = 12)
