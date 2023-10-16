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
bmi_info <- load_data("anzics", keep_ids = TRUE)
bmi_info <- merge(bmi_info, load_concepts("adm", "anzics"), all.x = TRUE)
bmi_info <- replace_na(bmi_info, val = "surg", vars = "adm")

cohort_list <- function(bmi_info, type = c("All", "Male", "Female")) {
  
  type <- match.arg(type, c("All", "Male", "Female"))
  lapply(
    c("All", levels(bmi_info$bmi_bins)),
      function(lvl) {
        
        if (lvl == "All") lvl <- levels(bmi_info$bmi_bins)
        if (type == "All") type <- c("Male", "Female")
        
        id_col(bmi_info[bmi_bins %in% lvl & sex %in% type])
      }
  )
}

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

out_tbl <- function(bmi_info, type = c("All", "Male", "Female"), 
                    p_vals = FALSE) {
  
  type <- match.arg(type, c("All", "Male", "Female"))
  res <- Map(pts_source_sum, src, cohort_list(bmi_info, type), skip_reord = TRUE,
             p_vals = p_vals)
  
  if (p_vals) return(res)
  
  outcomes <- Reduce(
    function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = F),
    res
  )
  names(outcomes) <- c("Variable", "Reported", "All", levels(bmi_info$bmi_bins))
  outcomes
}

join_tbls <- function(bmi_info, add_pvals = FALSE) {
  
  outcomes_full <- list(
    out_tbl(bmi_info, "Male"),
    out_tbl(bmi_info, "Female")
  )
  
  reord_idx <- nrow(outcomes_full[[1]]) * 0:1 + 
    rep(seq_len(nrow(outcomes_full[[1]])), each = 2)
  
  outcomes_full <- Reduce(rbind, outcomes_full)
  outcomes_full <- outcomes_full[reord_idx, ]
  
  if (add_pvals) {
    
    p_frame <- get_pvals(bmi_info)
    names(p_frame) <- names(outcomes_full)
    p_frame <- p_frame[-1, ]
    
    reord_idx <- c(
      1, 2,
      3, 4, 13,
      5, 6, 14,
      7, 8, 15,
      9, 10, 16,
      11, 12, 17
    )
    
    outcomes_full <- rbind(outcomes_full, p_frame)
    outcomes_full <- outcomes_full[reord_idx, ]
  }
  
  outcomes_full
}

get_pvals <- function(bmi_info) {
  
  out <- list(
    out_tbl(bmi_info, "Male", p_vals = TRUE),
    out_tbl(bmi_info, "Female", p_vals = TRUE)
  )
  
  bmi_lvls <- c("All", "18.5-25 $kg/m^2$", "0-18.5 $kg/m^2$", "25-30 $kg/m^2$", 
                "30-35 $kg/m^2$", "35-40 $kg/m^2$", "> 40 $kg/m^2$")
  
  
  res <- data.frame(t(c("Variable", "Reported", bmi_lvls)))
  for (cnc in names(out[[1]][[1]])) {
    
    row_vals <- NULL
    cat("\n", cnc, "\n")
    for (i in 1:7) {
      
      cat("BMI bin", bmi_lvls[i])
      p.val <- compute_pval(
        out[[1]][[i]][[cnc]], out[[2]][[i]][[cnc]], cnc
      )
      if (p.val < 0.001) {
        
        p.val <- "p < 0.001"
      } else {
        
        p.val <- paste("p =", round(p.val, 3))
      }
      row_vals <- c(row_vals, p.val)
    }
    
    res <- rbind(res, c(cnc, "_fill_", row_vals))
  }
  
  res
}

df_to_word(join_tbls(bmi_info, add_pvals = TRUE), 
           file.path(root, "tables", "Table_Outcomes.docx"),
           landscape = TRUE)

# get outcomes for subgroups
df_to_word(join_tbls(bmi_info[age <= 50], add_pvals = TRUE), 
           file.path(root, "tables", "Table_Outcomes_Young.docx"),
           landscape = TRUE)

df_to_word(join_tbls(bmi_info[age > 50], add_pvals = TRUE), 
           file.path(root, "tables", "Table_Outcomes_Old.docx"),
           landscape = TRUE)

df_to_word(join_tbls(bmi_info[adm == "surg"], add_pvals = TRUE), 
           file.path(root, "tables", "Table_Outcomes_Surgical.docx"),
           landscape = TRUE)


df_to_word(join_tbls(bmi_info[adm == "med"], add_pvals = TRUE), 
           file.path(root, "tables", "Table_Outcomes_Medical.docx"),
           landscape = TRUE)
