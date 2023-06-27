
get_reg_tbl <- function(mod, ref_bin) {
  
  se <- sqrt(diag(chol2inv(mod$qr$qr[seq_len(mod$rank), seq_len(mod$rank)])))
  names(se) <- dimnames(mod$R)

  # Step 4: get the table with ORs and CIs
  coef <- data.frame(Estimate = coef(mod), SE = se)
  
  OR <- cbind(
    coef[, "Estimate"],
    coef[, "Estimate"] - 1.96 * coef[, "SE"],
    coef[, "Estimate"] + 1.96 * coef[, "SE"]
  )
  
  OR <- round(exp(OR), 2)
  
  res <- data.table(cbind(
    rownames(coef),
    paste0(OR[, 1], "\\n (", OR[, 2], "-", OR[, 3], ")")
  ))
  
  # add the baseline bands
  ref_names <- c("bmi_bin_[18.5-25] kg/m^2", "apache_iii_diag0")
  
  bmi_ref <- nrow(res) + 1
  diag_ref <- nrow(res) + 2
  
  diag_all <- grep("apache_iii_diag[1-9]", res$V1)
  bmi_all <- grep("bmi_bin_", res$V1)
  
  res <- rbind(
    res,
    cbind(
      ref_names, "1\\n (-)"
    ), use.names = F
  )
  res <- res[c(1, 2, bmi_ref, bmi_all, diag_ref, diag_all)]
  res$V1 <- pol_varnames(res$V1)
  
  res
}

tbl <- get_reg_tbl(mod_man)
names(tbl) <- c("Odds Ratio (95% CI)", "ANZICS APD")

my_doc <- read_docx()

my_doc <- my_doc %>%
  body_add_table(tbl, style = "table_template")

print(my_doc, target = file.path(root, "tables", "Table_ORs.docx"))
