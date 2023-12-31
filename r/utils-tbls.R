
bin_labels <- function(breaks, unit, lower0 = TRUE) {
  
  x_labels <- sapply(1:(length(breaks)-1),
                     function(x) paste0("", breaks[x], "-", breaks[x+1], "")
  )
  first_label <- paste0("< ", breaks[1])
  if (lower0) first_label <- paste0("0-", breaks[1], "")
  x_labels <- c(first_label, x_labels, paste0("> ", breaks[length(breaks)]))
  x_labels <- paste(x_labels, unit)
  
  return(x_labels)
}

med_iqr <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  if(is_ts_tbl(x)) x <- x[get(index_var(x)) == 24L]
  quants <- quantile(x[[val_col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  res <- paste0(
    round(quants[2], 2), " (",
    round(quants[1], 2), "-",
    round(quants[3], 2), ")"
  )
  
  list(val_col, "Median (IQR)", res)
}

mean_med_iqr <- function(x, patient_ids) {

  val_col <- setdiff(names(x), meta_vars(x))
  if(is_ts_tbl(x)) x <- x[get(index_var(x)) == 24L]
  quants <- quantile(x[[val_col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  res <- paste0(
    spec_dec(mean(x[[val_col]], na.rm = TRUE), 3), ", ",
    spec_dec(quants[2], 3), " (",
    spec_dec(quants[1], 3), "-",
    spec_dec(quants[3], 3), ")"
  )
  
  list(val_col, "Mean, Median (IQR)", res)
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

tab_design_np <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  res <- x[[val_col]]
  
  if (length(res) < length(patient_ids)) {
    
    tbres <- table(res)
    imp_val <- names(tbres)[which.max(tbres)]
    if (is.logical(res)) imp_val <- as.logical(imp_val)
    res <- c(res, rep(imp_val, length(patient_ids) - length(res)))
  }
  
  res <- table(res)
  labs <- names(res)
  res <- paste0(
    res, " (", round(100 * res / sum(res)), ")"
  )
  
  if(val_col == "adm" & nrow(x) == 0L) {
    
    return(
      list(c("med", "surg", "other"), "%", rep(NA, 3))
    )
    
  }
  
  list(labs, "n (%)", res)
}

percent_fun1 <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  
  list(
    val_col, 
    "n (%)", 
    paste0(
      sum(x[[val_col]]), " (",
      spec_dec(100 * sum(x[[val_col]]) / length(patient_ids), 1), "%)"
    )
  )
}

percent_fun0 <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  
  list(
    val_col, 
    "n (%)", 
    paste0(
      sum(x[[val_col]]), " (",
      spec_dec(100 * sum(x[[val_col]]) / length(patient_ids), 0), "%)"
    )
  )
}

na_fun <- function(x, patient_ids) return(c(NA, NA, NA))

compute_pval <- function(x, y, cnc) {
  
  if (length(unique(x)) < 10) {
    
    p.val <- chisq.test(
      table(
        c(rep("Male", length(x)), rep("Female", length(y))),
        c(x, y)
      )
    )$p.val
    
    cat(cnc, "(ChiSq) p-value =", p.val, "\n")
  } else {
    
    p.val <- wilcox.test(x = x, y = y)$p.val
    cat(cnc, "(Rank-Sum) p-value =", p.val,"\n")
  }
  
  p.val
}

pts_source_sum <- function(source, patient_ids, skip_reord = FALSE, 
                           p_vals = FALSE) {
  
  if (p_vals) {
    
    return(
      lapply(
        vars,
        function(x) {
          load_concepts(x[["concept"]], source, patient_ids = patient_ids, 
                        keep_components = T)[[x[["concept"]]]]
        }
      )
    )
  }
  
  tbl_list <- lapply(
    vars,
    function(x) {
      if (x[["concept"]] == "bmi") browser()
      x[["callback"]](
        load_concepts(x[["concept"]], source, patient_ids = patient_ids, 
                      keep_components = T), unlist(patient_ids)
      )
    }
  )
  browser()
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

  # change the row ordering
  if (!skip_reord) {
    
    target_ord <- c("Cohort size", "age", "death", "pci", "pci_or_death", 
                    "bmi_all",
                    grep("kg\\/m\\^2", pts_tbl$Variable, value = TRUE),
                    "med", "elective_surgery", "emergency_surgery",
                    "los_hosp", "los_icu", "diab", "is_vent", "is_chr",
                    "Male", "apache_iii", "apache_iii_risk", "anzrod_risk")
    re_ord <- match(target_ord, pts_tbl$Variable)
    pts_tbl <- pts_tbl[re_ord, ]
  }
  
  pts_tbl$Variable <- mapvalues(pts_tbl$Variable,
                                from = names(concept_translator),
                                to = sapply(names(concept_translator), 
                                            function(x) concept_translator[[x]])
  )
  
  
  pts_tbl
}

pts_source_sens <- function(source, patient_ids) {
  
  tbl_list <- lapply(
    vars,
    function(x) {
      
      if (x[["concept"]] == "sofa" & !is.element(source, c("mimic_demo", "miiv"))) 
        return(c("sofa", "Mean, Median (IQR)", NA))
      if (x[["concept"]] == "saps3" & source != "sic") 
        return(c("saps3", "Mean, Median (IQR)", NA))
      if (x[["concept"]] == "apache_iii" & source != "anzics") 
        return(c("apache_iii", "Mean, Median (IQR)", NA))
      
      x[["callback"]](
        load_concepts(x[["concept"]], source, patient_ids = patient_ids), 
        unlist(patient_ids)
      )
    }
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
  
  # change the row ordering
  # target_ord <- c("Cohort size", "age", "death",
  #                 grep("kg\\/m\\^2", pts_tbl$Variable, value = TRUE),
  #                 "med", "elective_surgery", "emergency_surgery",
  #                 "los_hosp", "los_icu", "diab", "is_vent", "is_chr",
  #                 "Male", "apache_iii", "apache_iii_risk", "anzrod_risk")
  # re_ord <- match(target_ord, pts_tbl$Variable)
  # pts_tbl <- pts_tbl[re_ord, ]
  
  pts_tbl$Variable <- mapvalues(pts_tbl$Variable,
                                from = names(concept_translator),
                                to = sapply(names(concept_translator), 
                                            function(x) concept_translator[[x]])
  )
  
  pts_tbl
}

pts_source_hosp <- function(source, patient_ids) {
  
  tbl_list <- lapply(
    vars,
    function(x) {
      
      x[["callback"]](
        load_concepts(x[["concept"]], source, patient_ids = patient_ids,
                      id_type = "patient"), 
        unlist(patient_ids)
      )
    }
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
  
  # change the row ordering
  # target_ord <- c("Cohort size", "age", "death",
  #                 grep("kg\\/m\\^2", pts_tbl$Variable, value = TRUE),
  #                 "med", "elective_surgery", "emergency_surgery",
  #                 "los_hosp", "los_icu", "diab", "is_vent", "is_chr",
  #                 "Male", "apache_iii", "apache_iii_risk", "anzrod_risk")
  # re_ord <- match(target_ord, pts_tbl$Variable)
  # pts_tbl <- pts_tbl[re_ord, ]
  
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
  emergency_surgery = "- Surgical (Emergency)",
  elective_surgery = "- Surgical (Elective)",
  death = "Mortality",
  diab = "Diabetic",
  is_vent = "Ventilated",
  elix = "Elixhauser Index",
  is_chr = "Chronic Comorbidities",
  `Cohort size` = "Cohort size",
  los_icu = "ICU LOS (days)",
  los_hosp = "Hospital LOS (days)",
  Male = "Sex (Male)",
  Female = "Sex (Female)",
  apache_iii = "APACHE-III Score",
  apache_iii_risk = "APACHE-III Risk of Death",
  anzrod_risk = "ANZROD Risk of Death",
  pci = "Persistent Critical Illness",
  pci_or_death = "Persistent Critical Illness OR Mortality"
)

pol_varnames <- function(x) {
  
  subs <- list(
    list("age", "Age"),
    list("bmi_bin_", "BMI "),
    list("apache_iii_risk", "APACHE-3 ROD"),
    list("apache_iii_diag", "APACHE-3 Diag. ")#,
    # list("bmi_bin_", "BMI ")
  )
  
  for (i in seq_len(length(subs))) 
    x <- gsub(subs[[i]][[1]], subs[[i]][[2]], x)
  
  adds <- list(
    # list("lactate", "mmol/L"),
    # list("glucose", "mg/dL"),
    # list("BMI", "kg/m2"),
    # list("Insulin", "u/h"),
    # list("Dextrose", "mL/h")
  )
  
  for (i in seq_len(length(adds))) 
    x <- ifelse(grepl(adds[[i]][[1]], x), paste(x, adds[[i]][[2]]), x)
  
  x
}

spec_dec <- function(x, k) trimws(format(round(x, k), nsmall=k))

df_to_word <- function(df, path, caption = "", landscape = FALSE, 
                       footnotes = NULL, header = NULL, fix_width = NULL, ...) {
  
  ft <- flextable(df)
  
  if (!is.null(header)) {
    # start with no header
    ft <- delete_part(ft, part = "header")
    # add another line of row at the top position
    ft <- add_header(ft, values = header, top = TRUE)
    ft <- merge_h(ft, part = "header")
  }
  
  ft <- set_caption(ft, caption = caption)
  ft <- font(ft, fontname = "Calibri (Headings)", part = "all")
  ft <- fontsize(ft, size = 10, part = "all")
  
  ft <- autofit(ft)
  if (!is.null(fix_width)) ft <- fit_to_width(ft, fix_width)
  # for (i in seq_along(fix_width)) {
  #   ft <- width(ft, j = fix_width[[i]][1], width = fix_width[[i]][2])
  # }
  
  if (!is.null(footnotes)) {
    ft <- footnote(ft, i = 1, j = rep(1, length(footnotes)),
                   value = as_paragraph(footnotes),
                   ref_symbols = rep("", length(footnotes)),
                   part = "header", inline = TRUE)
  }
  
  my_doc <- read_docx()
  
  my_doc <- body_add_flextable(my_doc, value = ft)
  
  if (landscape) my_doc <- my_doc %>% body_end_section_landscape()
  
  print(my_doc, target = path)
}
