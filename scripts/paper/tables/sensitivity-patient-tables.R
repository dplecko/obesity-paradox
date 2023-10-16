library(stringr)
library(magrittr)
library(officer)
library(assertthat)
library(plyr)

root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("anzics", "miiv", "sic")
cohorts <- list( #lapply(src, function(x) config("cohort")[[x]][["bmi"]])
  A = config("cohort")[["anzics"]][["bmi_10pc"]],
  B = config("cohort")[["miiv"]][["bmi"]],
  C = config("cohort")[["sic"]][["bmi"]]
)

vars <- list(
  age = list(
    concept = "age",
    callback = med_iqr
  ),
  bmi = list(
    concept = "bmi_bins",
    callback = tab_design_np
  ),
  admission = list(
    concept = "adm",
    callback = tab_design_np
  ),
  death = list(
    concept = "death",
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
    callback = tab_design_np
  ),
  apache_iii = list(
    concept = "apache_iii",
    callback = mean_med_iqr
  ),
  saps3 = list(
    concept = "saps3",
    callback = mean_med_iqr
  ),
  sofa = list(
    concept = "sofa",
    callback = mean_med_iqr
  )
)

pts_tbl <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = F, all = TRUE),
  Map(pts_source_sens, src, cohorts)
)

my_doc <- read_docx()

my_doc <- my_doc %>%
  body_add_table(pts_tbl, style = "table_template")

print(my_doc, target = file.path(root, "tables", "eTable2.docx"))
