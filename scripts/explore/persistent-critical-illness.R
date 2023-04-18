
# univariate
dat <- load_concepts(c("pci_or_death", "adm_episode", "bmi_bins", "sex"), 
                     "anzics", patient_ids = config("cohort")[["anzics"]][["bmi"]])

dat <- dat[adm_episode %in% c(0, 1)]
dat <- dat[complete.cases(dat)]

# dat[, pci := lostay_icu > 240]
dat$group <- ifelse(dat$sex == "Female", 1, 2)

ggplot(
  dat[, list(pci_prop = mean(pci_or_death)), by = c("bmi_bins", "sex", "group")],
  aes(x = bmi_bins, y = pci_prop, color = sex, group = factor(group))
) +
  geom_point()+ geom_line() + theme_bw() +
  xlab("BMI group") + ylab("Proportion with PCI or Death") +
  scale_y_continuous(labels = scales::percent)
ggsave("~/Desktop/pci-marginal.png", width = 8, height = 5)

# multivariate
dat <- load_data("anzics", coh = "bmi", add_pci = TRUE)
mod_man <- fit_log(dat, "manual", ref_bin = "[18.5-25] kg/m^2")

plot_or(mod_man, ref_bin = "[18.5-25] kg/m^2") +
  ylab("Odds Ratio (OR) for PCI or Death")
ggsave("~/Desktop/pci-multivariate.png", width = 8, height = 5)
