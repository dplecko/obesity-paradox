library(stringr)
library(magrittr)
library(officer)
library(assertthat)
library(plyr)

root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("miiv")
cohorts <- list( #lapply(src, function(x) config("cohort")[[x]][["bmi"]])
  A = config("cohort")[["miiv"]][["comorbid"]]
)

vars <- list(
  age = list(
    concept = "age",
    callback = med_iqr
  ),
  bmi = list(
    concept = "bmi_bins",
    callback = tab_design
  ),
  death = list(
    concept = "death",
    callback = percent_fun1
  ),
  los_hosp = list(
    concept = "los_hosp",
    callback = med_iqr
  ),
  sex = list(
    concept = "sex",
    callback = tab_design
  ),
  elixhauser = list(
    concept = "elix",
    callback = mean_med_iqr
  )
)

pts_tbl <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = F),
  Map(pts_source_hosp, src, cohorts)
)

my_doc <- read_docx()

my_doc <- my_doc %>%
  body_add_table(pts_tbl, style = "table_template")

print(my_doc, target = file.path(root, "tables", "eTable3.docx"))
