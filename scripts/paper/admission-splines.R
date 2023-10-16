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

load(file.path(root, "cache", "anzics_adm_types.rda"))
data <- merge(data, adm_data, all.x = TRUE)

m_med <- data[sex == "Male" & adm_type == "med"]
m_surg <- data[sex == "Male" & adm_type != "med"]

f_med <- data[sex == "Female" & adm_type == "med"]
f_surg <- data[sex == "Female" & adm_type != "med"]

ops_med <- rbind(
  rr_splines(m_med, oth_reg, "Male", "nonpar", parallel = TRUE, 
             model_save = "anzics-med"),
  rr_splines(f_med, oth_reg, "Female", "nonpar", parallel = TRUE, 
             model_save = "anzics-med")
)

ops_surg <- rbind(
  rr_splines(m_surg, oth_reg, "Male", "nonpar", parallel = TRUE, 
             model_save = "anzics-surg"),
  rr_splines(f_surg, oth_reg, "Female", "nonpar", parallel = TRUE, 
             model_save = "anzics-surg")
)

save(ops_med, ops_surg,
     file = file.path(root, "cache", "adm_split_nonpar.rda"))

# load(file.path(root, "cache", "adm_split_nonpar.rda"))

p_med <- ggplot(ops_med, aes(x = bmi_all, y = OR, color = tag)) +
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
  ggtitle("ANZICS APD (Medical): Death + PerCI")

p_surg <- ggplot(ops_surg, aes(x = bmi_all, y = OR, color = tag)) +
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
  ggtitle("ANZICS APD (Surgical): Death + PerCI")

p_sub <- cowplot::plot_grid(
  p_med, p_surg, ncol = 2L, 
  labels = c("(A)", "(B)", "(C)", "(D)")
)

ggsave(file.path(root, "results", "eFigure3.png"), 
       plot = p_sub, width = 12, height = 5)
