library(stringr)
library(magrittr)
library(officer)
library(assertthat)
library(plyr)
library(flextable)

root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- rep("anzics", 7)
bmi_coh <- config("cohort")[["anzics"]][["bmi"]]
bmi_info <- load_concepts("bmi_bins", "anzics", patient_ids = bmi_coh)
cohorts <- list( #lapply(src, function(x) config("cohort")[[x]][["bmi"]])
  A = bmi_coh,
  B = id_col(bmi_info[bmi_bins == "[0-18.5] kg/m^2"]),
  C = id_col(bmi_info[bmi_bins == "[18.5-25] kg/m^2"]),
  D = id_col(bmi_info[bmi_bins == "[25-30] kg/m^2"]),
  E = id_col(bmi_info[bmi_bins == "[30-35] kg/m^2"]),
  G = id_col(bmi_info[bmi_bins == "[35-40] kg/m^2"]),
  H = id_col(bmi_info[bmi_bins == "> 40 kg/m^2"])
)

vars <- list(
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
  )
)

outcomes <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = F),
  Map(pts_source_sum, src, cohorts, skip_reord = TRUE)
)

names(outcomes) <- c("Variable", "Reported", "All", levels(bmi_info$bmi_bins))

df_to_word(outcomes, file.path(root, "tables", "Table_Outcomes.docx"),
           landscape = TRUE)

# my_doc <- read_docx()
# 
# my_doc <- my_doc %>%
#   body_add_table(outcomes, style = "table_template")
# 
# print(my_doc, target = file.path(root, "tables", "Table_Outcomes.docx"))
