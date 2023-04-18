
bin_labels <- function(breaks, unit, lower0 = TRUE) {
  
  x_labels <- sapply(1:(length(breaks)-1),
                     function(x) paste0("[", breaks[x], "-", breaks[x+1], "]")
  )
  first_label <- paste0("< ", breaks[1])
  if (lower0) first_label <- paste0("[0-", breaks[1], "]")
  x_labels <- c(first_label, x_labels, paste0("> ", breaks[length(breaks)]))
  x_labels <- paste(x_labels, unit)
  
  return(x_labels)
}

med_iqr <- function(x, patient_ids) {
  val_col <- setdiff(names(x), meta_vars(x))
  if(is_ts_tbl(x)) x <- x[get(index_var(x)) == 24L]
  quants <- quantile(x[[val_col]], probs = c(0.25, 0.5, 0.75), na.rm = T)
  res <- paste0(
    round(quants[2], 2), " (",
    round(quants[1], 2), "-",
    round(quants[3], 2), ")"
  )
  
  list(val_col, "Median (IQR)", res)
}

multi_med_iqr <- function(x, patient_ids) {
  
  val_cols <- setdiff(names(x), meta_vars(x))
  res <- lapply(
    val_cols, function(vcol) med_iqr(x[, c(meta_vars(x), vcol), with = FALSE], 
                                     patient_ids)
  )
  
  lapply(1:3, function(i) {
    Reduce(c, lapply(res, `[[`, i))
  })
  
}

tab_design <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  res <- table(x[[val_col]])
  res <- round(100 * res / sum(res))
  
  if(val_col == "adm" & nrow(x) == 0L) {
    
    return(
      list(c("med", "surg", "other"), "%", rep(NA, 3))
    )
    
  }
  
  list(names(res), "%", as.integer(res))
  
}

percent_fun <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  
  if (val_col == "death") {
    
    return(list(val_col, "%", round(100 * sum(x[[val_col]]) / 
                                      length(patient_ids))))
    
  }
  
  list(val_col, "%", round(100 * mean(x[[val_col]])))
  
}

pts_source_sum <- function(source, patient_ids) {
  
  tbl_list <- lapply(
    vars,
    function(x) x[["callback"]](
      load_concepts(x[["concept"]], source, patient_ids = patient_ids, 
                    keep_components = T), unlist(patient_ids)
    )
  )
  
  pts_tbl <- Reduce(rbind,
                    lapply(
                      tbl_list,
                      function(x) data.frame(Reduce(cbind, x))
                    )
  )
  
  cohort_info <- as.data.frame(cbind("Cohort size", "n", 
                                     length(unlist(patient_ids))))
  names(cohort_info) <- names(pts_tbl)
  
  pts_tbl <- rbind(
    cohort_info,
    pts_tbl
  )
  
  names(pts_tbl) <- c("Variable", "Reported", 
                      paste(srcwrap(source), collapse = "-"))
  
  pts_tbl$Variable <- mapvalues(pts_tbl$Variable,
                                from = names(concept_translator),
                                to = sapply(names(concept_translator), 
                                            function(x) concept_translator[[x]])
  )
  
  pts_tbl
  
}

concept_translator <- list(
  age = "Age (years)",
  med = "- Medical",
  other = "- Other",
  surg = "- Surgical",
  death = "Mortality",
  `Cohort size` = "Cohort size",
  los_icu = "ICU LOS (days)",
  los_hosp = "Hospital LOS (days)",
  Male = "Sex (Male)",
  Female = "Sex (Female)",
  apache_iii = "APACHE-III Score"
)

pol_varnames <- function(x) {
  
  subs <- list(
    list("bmi", "BMI"),
    list("glu", "Blood glucose"),
    list("lact", "Blood lactate"),
    list("ins_ifx", "Insulin"),
    list("shock_yes", "MAP < 60 mmHg or vasopressor therapy"),
    list("shock_no", "MAP ≥ 60 mmHg, no vasopressor therapy"),
    list("sofa_cns_comp", "SOFA CNS"),
    list("sofa_coag_comp", "SOFA Coagulation"),
    list("sofa_renal_comp", "SOFA Renal"),
    list("sofa_resp_comp", "SOFA Respiratory"),
    list("DM", "Diabetes"),
    list("source", ""),
    list("cortico", "Corticosteroids"),
    list("enteral", "Enteral nutrition"),
    list("TPN", "Parenteral nutrition"),
    list("dex_amount", "Dextrose 10%"),
    list("sofa_wo_cardio", "SOFA*")
  )
  
  for (i in seq_len(length(subs))) 
    x <- gsub(subs[[i]][[1]], subs[[i]][[2]], x)
  
  adds <- list(
    list("lactate", "mmol/L"),
    list("glucose", "mg/dL"),
    list("BMI", "kg/m2"),
    list("Insulin", "u/h"),
    list("Dextrose", "mL/h")
  )
  
  for (i in seq_len(length(adds))) 
    x <- ifelse(grepl(adds[[i]][[1]], x), paste(x, adds[[i]][[2]]), x)
  
  x
}

spec_dec <- function(x, k) trimws(format(round(x, k), nsmall=k))
