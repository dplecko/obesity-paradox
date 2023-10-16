
#' * ANZICS filtering *
all <- anzics$main$ICUStayID
cat("Starting with", length(all), "patients\n")

# remove non-adults
a_a <- setdiff(all, id_col(load_concepts("age", "anzics", 
                                         verbose=FALSE)[age < 18]))

cat("Non-adults", length(all) - length(a_a), "\n")
cat("Remaining", length(a_a), "patients\n")

# remove missing sex
a_as <- intersect(a_a, id_col(load_concepts("sex", "anzics", verbose = FALSE)))

cat("No sex recorded", length(a_a) - length(a_as), "\n")
cat("Remaining", length(a_as), "patients\n")

# remove repeated admissions
a_asf <- intersect(
  a_as,
  id_col(load_concepts("adm_episode", "anzics", 
                       verbose = FALSE)[adm_episode %in% c(0, 1)])
)
cat("Repeated admissions", length(a_as) - length(a_asf), "\n")
cat("Remaining", length(a_asf), "patients\n")

# safe female diagnoses
rm_diag <- c(304, 902, 903, 1801, 1802, 1803)
diag <- load_concepts("apache_iii_diag", "anzics", patient_ids = a_asf, 
                      verbose = FALSE)
rm_diag_ids <- id_col(diag[apache_iii_diag %in% rm_diag])
rm_treat_ids <- id_col(load_concepts("treat_goal", "anzics", 
                                     patient_ids = a_asf, 
                                     verbose = FALSE)[treat_goal %in% c(3, 4)])
rm_adm <- unique(union(rm_diag_ids, rm_treat_ids))

a_asfd <- setdiff(a_asf, rm_adm)
cat("Removing", length(rm_adm), "patients due to pregnancy related diagnosis,",
    "palliative care,", "and organ donation.\n")
cat("Remaining", length(a_asfd), "patients\n")

# bmi cohort
bmi_coh <- id_col(load_concepts(c("bmi"), "anzics", patient_ids = a_asfd, 
                                verbose = FALSE))
cat("No BMI recorded", length(a_asfd) - length(bmi_coh), "\n")
cat("Remaining", length(bmi_coh), "patients\n")
print(
  load_concepts(c("bmi", "sex"), "anzics", 
                patient_ids = bmi_coh, verbose = FALSE)[, mean(bmi), by = "sex"]
)

# proportion with bmi
cat("Proportion with BMI recorded",
    spec_dec(100 * length(bmi_coh) / length(a_asfd), 1), "%\n")

# low missingness hospitals
lm <- load_concepts(c("bmi", "site", "adm_year", "sex"), "anzics",
                    patient_ids = a_asfd, verbose = FALSE)
lm10 <- lm[, mean(is.na(bmi)), by = "site"][V1 < 0.1]$site
cat("Centers in sensitivity analysis =", length(unique(lm10)), "\n")
all_10pc <- id_col(lm[site %in% lm10])
bmi_10pc <- intersect(all_10pc, bmi_coh)
cat("Patients in sensitivity analysis =", length(bmi_10pc), "\n")

# proportion with bmi
cat("BMI missingness in the low-miss sensitivity",
    spec_dec(100 - 100 * length(bmi_10pc) / length(all_10pc), 1), "%\n")

#' * MIMIC-IV filtering *
mimic4 <- load_concepts(c("age", "sex", "adm_episode", "diag"), "miiv", 
                        verbose = FALSE)

all_wi <- id_col(
  mimic4[!is.na(sex) & age >= 18 & adm_episode %in% c(0, 1) & diag != "OBS"]
)
miiv_bmi_coh <- id_col(load_concepts("bmi_all", "miiv", patient_ids = all_wi,
                                     verbose = FALSE))
miiv_omr_only <- id_col(load_concepts("bmi_omr", "miiv", patient_ids = all_wi,
                                      verbose = FALSE))

cat("Proportion with BMI recorded",
    spec_dec(100 * length(miiv_bmi_coh) / length(all_wi), 1), "%\n")


# cohort for comorbidities study
cmb <- load_concepts(c("sex", "age", "bmi_all"), "miiv", id_type = "patient",
                     verbose = FALSE)


#' * SICdb filtering *
sz <- load_concepts(c("age", "sex", "adm_episode", "surg_site"), "sic", 
                    verbose = FALSE)
all_sz <- id_col(
  sz[!is.na(sex) & age >= 18 & adm_episode %in% c(0, 1) & surg_site != "Birth"]
)
sz_bmi <- id_col(load_concepts("bmi_all", "sic", patient_ids = all_sz, 
                               verbose = FALSE))

cat("Proportion with BMI recorded (SICdb)",
    spec_dec(100 * length(sz_bmi) / length(all_sz), 1), "%\n")


#' * generate cohort *
cohort <- list(
  anzics = list(
    all = anzics$main$ICUStayID,
    bmi_miss = setdiff(a_asfd, bmi_coh),
    bmi = bmi_coh,
    bmi_male = id_col(load_concepts("sex", "anzics", verbose = FALSE, 
                                    patient_ids = bmi_coh)[sex == "Male"]),
    bmi_female = id_col(load_concepts("sex", "anzics", verbose = FALSE, 
                                      patient_ids = bmi_coh)[sex == "Female"]),
    bmi_10pc = bmi_10pc
  ),
  miiv = list(
    all = miiv$icustays$stay_id,
    bmi = miiv_bmi_coh,
    omr_only = miiv_omr_only,
    comorbid = id_col(cmb[!is.na(sex) & age >= 18 & !is.na(bmi_all)])
  ),
  sic = list(
    all = all_sz,
    bmi = sz_bmi
  )
)

config("cohort", value = cohort)
