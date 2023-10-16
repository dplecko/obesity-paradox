library(stringr)
library(magrittr)
library(officer)
library(assertthat)
library(plyr)

root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("anzics", "anzics")
cohorts <- list( #lapply(src, function(x) config("cohort")[[x]][["bmi"]])
  A = config("cohort")[["anzics"]][["bmi"]],
  B = config("cohort")[["anzics"]][["bmi_miss"]]
)

vars <- list(
  age = list(
    concept = "age",
    callback = med_iqr
  ),
  admission = list(
    concept = "adm_type",
    callback = tab_design
  ),
  death = list(
    concept = "death",
    callback = percent_fun1
  ),
  pci = list(
    concept = "pci",
    callback = percent_fun1
  ),
  pci_or_death = list(
    concept = "pci_or_death",
    callback = percent_fun1
  ),
  los_icu = list(
    concept = "los_icu",
    callback = med_iqr
  ),
  los_hosp = list(
    concept = "los_hosp",
    callback = med_iqr
  ),
  sex = list(
    concept = "sex",
    callback = tab_design
  ),
  diab = list(
    concept = "diab",
    callback = percent_fun1
  ),
  is_vent = list(
    concept = "is_vent",
    callback = percent_fun1
  ),
  is_chr = list(
    concept = "is_chr",
    callback = percent_fun1
  ),
  apache_iii = list(
    concept = "apache_iii",
    callback = mean_med_iqr
  ),
  apache_iii_risk = list(
    concept = "apache_iii_risk",
    callback = mean_med_iqr
  ),
  anzrod_risk = list(
    concept = "anzrod_risk",
    callback = mean_med_iqr
  )
)

res <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = F),
  Map(pts_source_sum, src, cohorts)
)

my_doc <- read_docx()

my_doc <- my_doc %>%
  body_add_table(res, style = "table_template")

print(my_doc, target = file.path(root, "tables", "Table_BMI_miss.docx"))


p_stats <- pts_source_sum("anzics", config("cohort")[["anzics"]][["bmi"]],
                          p_vals = TRUE)
m_stats <- pts_source_sum("anzics", config("cohort")[["anzics"]][["bmi_miss"]],
                          p_vals = TRUE)
pvals <- Map(compute_pval, p_stats, m_stats, names(vars))

