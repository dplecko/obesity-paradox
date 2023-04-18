
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))


dat <- load_concepts(c("cmbd_anzics", "bmi", "sex"), "anzics",
                     patient_ids = config("cohort")[["anzics"]][["bmi"]])

dat <- dat[complete.cases(dat)]

who_bins <- config("bmi-bins")[["who"]]
dat[, bmi_bin := factor(.bincode(bmi, c(-Inf, who_bins, Inf)), 
                        labels = bin_labels(who_bins, "kg/m2"))]

dat <- rename_cols(dat, "charlson", "cmbd_anzics")
dt <- get_sd(dat)
dt$group <- ifelse(dt$sex == "Female", 1, 2)

p_cmb_anzics <- ggplot(dt, aes(x = bmi_bin, y = avg_cmb, color = sex, group = group)) +
  geom_ribbon(aes(ymin = avg_cmb - 1.96 * stdv, ymax = avg_cmb + 1.96 * stdv,
                  fill = sex), alpha = 0.4) +
  geom_point() + geom_line() + theme_bw() +
  xlab("BMI group") + ylab("Average # of comorbidities") +
  ggtitle("Comorbidity score on ANZICS (740k patients)") +
  theme(legend.position = c(0.3, 0.8), legend.box.background = element_rect())

ggsave("~/Desktop/comorbidities-anzics.png",
       width = 6, height = 5)
