
#' * ANZICS filtering *
all <- anzics$main$ICUStayID

# remove non-adults
all_adults <- setdiff(all, id_col(load_concepts("age", "anzics")[age < 18]))

# remove missing sex
all_adults_ws <- intersect(all_adults, id_col(load_concepts("sex", "anzics")))

# remove repeated admissions
all_adults_wsf <- intersect(
  all_adults_ws,
  id_col(load_concepts("adm_episode", "anzics")[adm_episode %in% c(0, 1)])
)

# bmi cohort
bmi_coh <- id_col(load_concepts(c("bmi"), "anzics", patient_ids = all_adults_wsf))

# proportion with bmi
cat("Proportion with BMI recorded",
    spec_dec(100 * length(bmi_coh) / length(all_adults_wsf), 1), "%\n")

# safe female diagnoses
rm_diag <- c(304, 902, 903, 1801, 1802, 1803)
diag <- load_concepts("apache_iii_diag", "anzics", patient_ids = bmi_coh)
rm_diag_ids <- id_col(diag[apache_iii_diag %in% rm_diag])
cat("Removing", length(rm_diag_ids), "patients due to pregnancy related diagnosis\n")

# low missingness hospital
lm <- load_concepts(c("bmi", "site", "adm_year", "sex"), "anzics")
lm10 <- lm[, mean(is.na(bmi)), by = "site"][V1 < 0.1]$site
bmi_10pc <- intersect(id_col(lm[site %in% lm10]), bmi_coh)


#' * MIMIC-IV filtering *
mimic4 <- load_concepts(c("age", "sex", "adm_episode"), "miiv")

all_wi <- id_col(mimic4[!is.na(sex) & age >= 18 & adm_episode %in% c(0, 1)])
miiv_bmi_coh <- id_col(load_concepts("bmi_all", "miiv", patient_ids = all_wi))

cat("Proportion with BMI recorded",
    spec_dec(100 * length(miiv_bmi_coh) / length(all_wi), 1), "%\n")


# cohort for comorbidities study
cmb <- load_concepts(c("sex", "age", "bmi_all"), "miiv", id_type = "patient")

#' * generate cohort *
cohort <- list(
  anzics = list(
    all = anzics$main$ICUStayID,
    bmi = setdiff(bmi_coh, c(rm_diag_ids)),
    bmi_5pc = setdiff(bmi_5pc, c(rm_diag_ids))
  ),
  miiv = list(
    all = miiv$icustays$stay_id,
    bmi = miiv_bmi_coh,
    comorbid = id_col(cmb[!is.na(sex) & age >= 18 & !is.na(bmi_all)])
  )
)

config("cohort", value = cohort)
