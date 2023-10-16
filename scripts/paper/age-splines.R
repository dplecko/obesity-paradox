#!/usr/bin/env Rscript
#SBATCH --cpus-per-task=32
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=2048MB

root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- "anzics"
data <- load_data(src, coh = "bmi", bmi_cts = TRUE, outcome = "pci_or_death")
oth_reg <- setdiff(names(data), c(id_vars(data), "outcome", "bmi_all", "sex"))

m_lwr <- data[sex == "Male" & age <= 50]
m_upr <- data[sex == "Male" & age > 50]

f_lwr <- data[sex == "Female" & age <= 50]
f_upr <- data[sex == "Female" & age > 50]

y_mf <- rbind(
  rr_splines(m_lwr, oth_reg, "Male", "nonpar", parallel = TRUE, 
             model_save = "anzics-young"),
  rr_splines(f_lwr, oth_reg, "Female", "nonpar", parallel = TRUE, 
             model_save = "anzics-young")
)

o_mf <- rbind(
  rr_splines(m_upr, oth_reg, "Male", "nonpar", parallel = TRUE, 
             model_save = "anzics-old"),
  rr_splines(f_upr, oth_reg, "Female", "nonpar", parallel = TRUE, 
             model_save = "anzics-old")
)

save(y_mf, o_mf, file = file.path(root, "cache", "age_split_nonpar.rda"))
# load(file.path(root, "cache", "age_split_nonpar.rda"))

p_y <- ggplot(y_mf, aes(x = bmi_all, y = OR, color = tag)) +
  geom_line(linewidth = 0.75) + theme_bw() +
  ylab("Odds Ratio (OR)") +
  xlab(latex2exp::TeX("BMI $(kg/m^2)$")) +
  geom_ribbon(
    aes(ymin = 2 * OR - upr, ymax = 2 * OR - lwr, fill = tag),
    linewidth = 0, alpha = 0.15
  ) +
  xlab(latex2exp::TeX("BMI $(kg/m^2)$")) +
  scale_color_discrete(name = "Sex") +
  scale_fill_discrete(name = "Sex") +
  theme(legend.position = c(0.4, 0.77),
        legend.box.background = element_rect()) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ggtitle(latex2exp::TeX("ANZICS APD (Age$\\leq 50$): Death + PerCI"))

p_o <- ggplot(o_mf, aes(x = bmi_all, y = OR, color = tag)) +
  geom_line(linewidth = 0.75) + theme_bw() +
  ylab("Odds Ratio (OR)") +
  xlab(latex2exp::TeX("Body Mass Index $(kg/m^2)$")) +
  geom_ribbon(
    aes(ymin = 2 * OR - upr, ymax = 2 * OR - lwr, fill = tag),
    linewidth = 0, alpha = 0.15
  ) +
  xlab(latex2exp::TeX("BMI $(kg/m^2)$")) +
  scale_color_discrete(name = "Sex") +
  scale_fill_discrete(name = "Sex") +
  theme(legend.position = c(0.4, 0.77),
        legend.box.background = element_rect()) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ggtitle(latex2exp::TeX("ANZICS APD (Age$> 50$): Death + PerCI"))

p_age <- cowplot::plot_grid(
  p_y, p_o, ncol = 2L, labels = c("(A)", "(B)")
)

ggsave(file.path(root, "results", "eFigure2.png"), 
       plot = p_age, width = 12, height = 5)
