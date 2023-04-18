
# miiv$icustays$subject_id
# 
# 
# load_concepts("bmi", "miiv", id_type = "icustay")
# 
# omr <- as.data.table(
#   fst::read.fst("/Users/pleckod/Library/Application Support/ricu/miiv/omr.fst")
# )

omr <- load_src("omr", "miiv")
omr <- omr[grepl("BMI", result_name)]
omr$result_value <- as.numeric(omr$result_value)
omr <- omr[, list(bmi = mean(result_value)), by = "subject_id"]

omr_ids <- omr$subject_id

x <- load_concepts(c("los_icu", "bmi"), "miiv")

x <- merge(x, load_src("icustays", "miiv")[, c("stay_id", "subject_id")],
           by = c("stay_id"))

x[, omr_bmi := is.element(subject_id, omr_ids)]
x[, has_bmi := omr_bmi | !is.na(bmi)]

mean(x[is.na(bmi)]$subject_id %in% omr_ids)

prop_bmi <- length(unique(x[has_bmi == TRUE]$subject_id)) / length(unique(miiv$icustays$subject_id))

# beyond just bmi

omr <- load_src("omr", "miiv")
omr <- omr[, bmi := grepl("BMI \\(", result_name)]
omr <- omr[, height := grepl("height", result_name, ignore.case = TRUE)]
omr <- omr[, weight := grepl("weight", result_name, ignore.case = TRUE)]

length(unique(omr[bmi == TRUE]$subject_id))

length(unique(intersect(omr[height == TRUE]$subject_id, omr[weight == TRUE]$subject_id)))
