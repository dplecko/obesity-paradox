#!/usr/bin/env Rscript
#SBATCH --cpus-per-task=32
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=2048MB

root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

p1 <- cowplot::plot_grid(
  plotlist = two_spline_plot("anzics", "bmi", "nonpar", "anzics"),
  ncol = 2L, labels = c("(A)", "(B)")
)
ggsave(file.path(root, "results", "Figure2.tiff"), plot = p1,
       height = 5, width = 12, compression = "lzw")


# ANZICS sense
p2 <- cowplot::plot_grid(
  plotlist = two_spline_plot("anzics", "bmi_10pc", "nonpar", "anzics-lm"),
  ncol = 2L, labels = c("(A)", "(B)")
)
ggsave(file.path(root, "results", "eFigure4.png"), plot = p2,
       height = 5, width = 12)

p3 <- cowplot::plot_grid(
  plotlist = c(
    two_spline_plot("miiv", "bmi", "nonpar", "miiv")
  ),
  ncol = 2L, labels = c("(A)", "(B)")
)

ggsave(file.path(root, "results", "eFigure5.png"), plot = p3,
       height = 5, width = 12)

p4 <- cowplot::plot_grid(
  plotlist = c(
    two_spline_plot("sic", "bmi", "nonpar", "sic")
  ),
  ncol = 2L, labels = c("(A)", "(B)")
)

ggsave(file.path(root, "results", "eFigure6.png"), plot = p4,
       height = 5, width = 12)