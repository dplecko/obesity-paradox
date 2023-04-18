
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

get_sd <- function(dat, no_weight = TRUE) {
  
  res <- lapply(
    unique(dat$bmi_bin),
    function(bin) {
      
      res <- NULL
      bt <- dat[bmi_bin == bin]
      if (no_weight) bt[, weight := 1]
      for (i in 1:100) {
        set.seed(i)
        idx <- sample.int(nrow(bt), replace = TRUE)
        res <- rbind(res, bt[idx, sum(score * weight) / sum(weight), 
                             by = c("sex", "bmi_bin")])
      }
      res[, list(avg_cmb = mean(V1), stdv = sd(V1)), by = c("sex", "bmi_bin")]
    }
  )
  
  do.call(rbind, res)
}

score <- "elix"
who_bins <- config("bmi-bins")[["who"]]
src <- "miiv"
dat <- load_concepts(c("sex", "age", "bmi_all", score), "miiv",
                     patient_ids = config("cohort")[[src]][["comorbid"]],
                     id_type = "patient")
dat <- rename_cols(dat, "score", score)
dat <- replace_na(dat, 0, vars = "score")

dat[, bmi_bin := factor(.bincode(bmi_all, c(-Inf, who_bins, Inf)), 
                        labels = bin_labels(who_bins, "kg/m2"))]

#' * non-age matched *
# dt <- get_sd(dat)
# dt$group <- ifelse(dt$sex == "Female", 1, 2)
# 
# p_nom <- ggplot(dt, aes(x = bmi_bin, y = avg_cmb, color = sex, group = group)) +
#   geom_ribbon(aes(ymin = avg_cmb - 1.96 * stdv, ymax = avg_cmb + 1.96 * stdv,
#                   fill = sex), alpha = 0.4) +
#   geom_point() + geom_line() + theme_bw() +
#   xlab("BMI group") + ylab("Average # of comorbidities") +
#   ggtitle("Charlson score on MIMIC-IV (149k patients)") +
#   theme(legend.position = c(0.3, 0.8), legend.box.background = element_rect())

#' * age matched *
dat <- add_age_std(dat)
fnw <- fnw("anzics", config("cohort")[["anzics"]][["bmi"]], "icustay",
           "miiv", config("cohort")[["miiv"]][["comorbid"]], "patient")

dat <- merge(dat, fnw, by = c("sex", "age_std"))

dtw <- get_sd(dat, no_weight = FALSE)
dtw$group <- ifelse(dtw$sex == "Female", 1, 2)

# textual output
dtw[, eci_lw := avg_cmb - 1.96 * stdv]
dtw[, eci_up := avg_cmb + 1.96 * stdv]

cat(
  paste0(dtw$sex, dtw$bmi_bin, ": ECI ", spec_dec(dtw$avg_cmb, 1), ", CI [",
         spec_dec(dtw$eci_lw, 1), ", ", spec_dec(dtw$eci_up, 1), "]"), sep = "\n"
)

p_agm <- ggplot(dtw, aes(x = bmi_bin, y = avg_cmb, color = sex, group = group)) + 
  geom_point() + geom_line() + theme_bw() +
  geom_ribbon(aes(ymin = avg_cmb - 1.96 * stdv, ymax = avg_cmb + 1.96 * stdv,
                  fill = sex), alpha = 0.4) +
  xlab("BMI group") + ylab("Average Elixhauser Comorbidity Index") +
  theme(legend.position = c(0.3, 0.8), legend.box.background = element_rect()) +
  scale_fill_discrete(name = "Sex") + scale_color_discrete(name = "Sex")

ggsave(file.path(root, "results", "Figure3.png"), p_agm, width = 7.5, height = 5)

wilcox.test(x = dat[bmi_bin == "[18.5-25] kg/m2" & sex == "Male"]$score,
            y = dat[bmi_bin == "[25-30] kg/m2" & sex == "Male"]$score)

wilcox.test(x = dat[bmi_bin == "[18.5-25] kg/m2" & sex == "Female"]$score,
            y = dat[bmi_bin == "[25-30] kg/m2" & sex == "Female"]$score)

# get a p-value for comparing [18.5-25] vs. [25-30] for males / females
# design <- svydesign(
#   ids = ~0, 
#   data = dat[bmi_bin %in% c("[18.5-25] kg/m2", "[25-30] kg/m2") & sex == "Female"], 
#   weights = ~weight
# )
# svyranktest(formula = score ~ bmi_bin, design)$p.value

#' * age distributions comparison *
# src <- load_concepts(c("age", "sex"), "anzics", 
#                      patient_ids = config("cohort")[["anzics"]][["bmi"]])
# 
# targ <- load_concepts(c("age", "sex"), "miiv", id_type = "patient",
#                       patient_ids = config("cohort")[["miiv"]][["comorbid"]])
# 
# dat <- rbind(cbind(targ, source = "miiv"), cbind(src, source = "anzics"))
# 
# p_age <- ggplot(dat, aes(x = age, fill = interaction(sex, source)))+
#   geom_density(alpha = 0.4) + theme_bw() +
#   ggtitle("Age MIMIC-IV hospital vs. ANZICS") +
#   scale_fill_discrete(name = "Group") +
#   theme(legend.position = c(0.3, 0.8), legend.box.background = element_rect())
# 
# 
# cowplot::plot_grid(p_age, p_nom, p_agm, ncol = 3)
