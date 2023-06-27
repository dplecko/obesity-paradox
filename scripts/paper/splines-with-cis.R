
# splines with CIs
root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

# (need to run parallel on Euler)
# p4 <- cowplot::plot_grid(
#   plotlist = c(
#     two_spline_plot("anzics", "bmi", "nonpar"),
#     two_spline_plot("anzics", "bmi_10pc", "nonpar"),
#     two_spline_plot("miiv", "bmi", "nonpar"),
#     two_spline_plot("sic", "bmi", "nonpar")
#   ),
#   ncol = 2L
# )
# 
# ggsave(file.path(root, "results", "eFigure4.png"), plot = p4,
#        height = 20, width = 12)