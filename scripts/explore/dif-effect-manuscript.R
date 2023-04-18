
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

# BMI missingness by year

ggplot(
  dat[, mean(is.na(bmi)), by = "adm_year"],
  aes(x = adm_year, y = V1)
) + geom_bar(stat = "identity") + theme_bw()

# BMI missingness by site
ggplot(
  dat[, mean(is.na(bmi)), by = "site"],
  aes(x = site, y = V1)
) + geom_bar(stat = "identity") + theme_bw()



sum(dat$site %in% lm_sites)

chisq.test(
  table(
    is.na(dat[site %in% lm_sites]$bmi),
    dat[site %in% lm_sites]$sex
  )
)

# re-run differential-effects.R with this hospital subset (!)

# for patient tables: use patient-tables.R from bmi repository

# open a new repo, with minimal overhead code

# safe female diagnoses:
dat <- load_concepts(c("apache_iii_diag", "death", "sex"), "anzics")
dat[is.na(death), death := FALSE]
dat <- dat[complete.cases(dat)]
diags <- dat[, list(mortality = mean(death), prop_female = mean(sex == "Female")),
             by = "apache_iii_diag"]
setorderv(diags, cols = c("mortality", "prop_female"), order = c(1L, -1L))


