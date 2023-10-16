#' * three-step solution * 

#' * First step - load *
load_data <- function(src = "anzics", coh = "bmi", 
                      bmi_bins = c("who", "who-adjust", "who-obese"),
                      outcome = c("death", "pci", "pci_or_death"),
                      ref_bin = "18.5-25 $kg/m^2$",
                      add_pci = FALSE, age_match = FALSE,
                      reverse_risk = FALSE,
                      bmi_cts = FALSE, keep_ids = FALSE) {
  
  bmi_bins <- match.arg(bmi_bins, c("who", "who-adjust", "who-obese"))
  outcome <- match.arg(outcome, c("death", "pci", "pci_or_death"))
  
  if (file.exists(file.path(root, "cache", paste0(src, coh, ".rda")))) {
    
    load(file.path(root, "cache", paste0(src, coh, ".rda")))
  } else {
    
    ind <- load_concepts(c("bmi_all", "pci_or_death", "pci", "death", "sex", "age"), 
                         src, patient_ids = config("cohort")[[src]][[coh]],
                         verbose = FALSE)
    
    if (is_ts_tbl(ind)) ind[, c(index_var(ind)) := NULL]

    ind[is.na(death), death := FALSE]
    ind[is.na(pci), pci := FALSE]
    ind[is.na(pci_or_death), pci_or_death := FALSE]
    
    if (!is.element(src, c("anzics", "sic"))) {
      
      ils <- load_concepts("sofa", src, explicit_wins = hours(24L), 
                           keep_components = TRUE,
                           verbose = FALSE)
      ils[, c(index_var(ils), "sofa") := NULL]
      ind <- merge(ind, ils, all.x = TRUE)
      ind <- replace_na(ind, val = 0, type = "const")
    } else if (src == "anzics") {
      
      ils <- load_concepts(c("apache_iii_risk", "apache_iii_diag"), src,
                           verbose = FALSE)
      ind <- merge(ind, ils, all.x = TRUE)
      ind <- replace_na(ind, 0, vars = "apache_iii_diag")
      risk_med <- median(ind$apache_iii_risk, na.rm = TRUE) # compute median
      ind[is.na(apache_iii_risk), apache_iii_risk := risk_med] # impute median
      age_med <- median(ind$age, na.rm = TRUE)
      ind[is.na(age), age := age_med]
      ind[, apache_iii_diag := as.factor(apache_iii_diag)]
    } else if (src == "sic") {
      
      ils <- load_concepts(c("saps3", "surg_site"), src)
      ind <- merge(ind, ils, all.x = TRUE)
      risk_med <- median(ind$saps3, na.rm = TRUE) # compute median
      ind[is.na(saps3), saps3 := risk_med] # impute median
    }
    
    if (src %in% c("mimic_demo", "mimic", "miiv", "aumc")) {
      dgs <- load_concepts("diag", src, verbose = FALSE)
      ind <- merge(ind, dgs, all.x = TRUE)
      ind <- replace_na(ind, val = "OTH", vars = "diag")
    }
    
    save(ind, file = file.path(root, "cache", paste0(src, coh, ".rda")))
  }
  
  not_outcome <- setdiff(c("death", "pci", "pci_or_death"), outcome)
  ind[, c(not_outcome) := NULL]
  ind <- rename_cols(ind, "outcome", outcome)
  
  if (any(is.na(ind))) {
    
    na_rows <- nrow(ind[!complete.cases(ind)])
    cat("Removing", na_rows, "due to NA values\n")
    ind <- ind[complete.cases(ind)]
  }
  
  if (!bmi_cts) {
    
    breaks <- c(0, config("bmi-bins")[[bmi_bins]], Inf)
    # tags <- paste("<", breaks[-1])
    ind[, bmi_bins := factor(.bincode(bmi_all, breaks),
                             labels = bin_labels(config("bmi-bins")[[bmi_bins]], 
                                                "$kg/m^2$"))]
    
    ind$bmi_bins <- relevel(ind$bmi_bins, ref = ref_bin)
  }
  
  ind$sex <- factor(ind$sex)
  ind$sex <- relevel(ind$sex, ref = "Male")
  
  if (is.element(src, c("mimic_demo", "mimic", "miiv", "aumc")) & age_match) {
    
    ind <- add_age_std(ind)
    fnw <- fnw("anzics", config("cohort")[["anzics"]][["bmi"]], "icustay",
               src, id_col(ind), "icustay")
    ind <- merge(ind, fnw, by = c("sex", "age_std"))
    attr(ind, "weights") <- ind$weight
    ind[, c("age_std", "weight") := NULL]
  }
  
  if (keep_ids) return(ind)
  if (!bmi_cts) ind[, c("bmi_all", id_vars(ind)) := NULL]
  ind$sex <- factor(ind$sex, levels = c("Male", "Female"))
  
  if (reverse_risk) {
    
    load(file.path(root, "data", "inverse_rr.rda"))
    ind <- merge(ind, irisk, by = c("sex", "bmi_bin"))
    attr(ind, "weights") <- ind$irr
    ind[, irr := NULL]
  }
  
  ind
}


#' * Second step - fit logistic models *
fit_log <- function(dat, type = c("univar", "multivar", "manual", "split"), ref_bin,
                    object = "fit") {
  
  type <- match.arg(type, c("univar", "multivar", "manual", "split"))
  
  if (type == "univar") {
    
    mod <- glm(death ~ bmi_bin * sex, data = dat, 
               family = "binomial", weights = attr(dat, "weights")) # !!!
  } else if (type == "multivar") {
    
    mod <- glm(death ~ bmi_bin * sex + ., data = dat, 
               family = "binomial", weights = attr(dat, "weights")) # !!!
  } else if (type == "split") {
    
    idx <- dat$sex == "Male"
    return(list(
      Male = glm(death ~ ., data = dat[idx, which(names(dat) != "sex"),with=F], 
                 family = "binomial", weights = attr(dat, "weights")[idx]),
      Female = glm(death ~ ., data = dat[!idx, which(names(dat) != "sex"),with=F], 
                   family = "binomial", weights = attr(dat, "weights")[!idx])
    ))
    
  } else {
    Y <- dat$death
    
    mod_mat <- model.matrix(death ~ . - sex - bmi_bin, dat)
    
    if (object == "fit") {
      
      mod_iam <- mltools::one_hot(dat[, c("bmi_bin"), with=FALSE]) * 
        (dat$sex == "Male")
      mod_iam[[paste0("bmi_bin_", ref_bin)]] <- NULL
      mod_iaf <- mltools::one_hot(dat[, c("bmi_bin"), with=FALSE]) * 
        (dat$sex == "Female")
      setnames(mod_iaf, names(mod_iaf), paste0(names(mod_iaf), ":sexFemale"))
      
      mod_mat <- cbind(mod_mat, as.matrix(mod_iam), as.matrix(mod_iaf))
      
      mod <- glm.fit(x = mod_mat, y = Y, family = binomial(),
                     weights = attr(dat, "weights"))
    } else if (object == "within-bmi") {
      
      mod_iab <-  mltools::one_hot(dat[, c("bmi_bin"), with=FALSE])
      mod_iaf <- mltools::one_hot(dat[, c("bmi_bin"), with=FALSE]) * 
        (dat$sex == "Female")
      colnames(mod_iaf) <- paste0(colnames(mod_iaf), ":sexFemale")
      
      df <- cbind(mod_mat, as.matrix(mod_iab[, -1]), as.matrix(mod_iaf))
      df <- as.data.frame(cbind(death = Y, df[, -1]))
      
      mod <- glm(death ~ ., data = df, family = "binomial", 
                 weights = attr(dat, "weights"))
    } else if (object == "within-sex") {
      
      mod_iam <- mltools::one_hot(dat[, c("bmi_bin"), with=FALSE]) * 
        (dat$sex == "Male")
      mod_iam[[paste0("bmi_bin_", ref_bin)]] <- NULL
      mod_iaf <- mltools::one_hot(dat[, c("bmi_bin"), with=FALSE]) * 
        (dat$sex == "Female")
      setnames(mod_iaf, names(mod_iaf), paste0(names(mod_iaf), ":sexFemale"))
      
      df <- cbind(mod_mat, as.matrix(mod_iam), as.matrix(mod_iaf))
      df <- as.data.frame(cbind(death = Y, df[, -1]))
      
      
      mod_full <- glm(death ~ ., data = df, family = "binomial",
                      weights = attr(dat, "weights"))
      
      dev_chisq_eff <- function(col1, col2, df, mod_full) {
        
        df_fem <- df
        if (!is.null(df[[col1]])) {
          
          df_fem[[col1]] <- pmax(df_fem[[col1]], df_fem[[col2]])
        }
        df_fem[[col2]] <- NULL
        mod_small_f <- glm(death ~ ., data = df_fem, family = "binomial",
                           weights = attr(dat, "weights"))
        anv <- anova(mod_small_f, mod_full, test="Chisq")
        cat(col1, " vs. ", col2, ": p-value = ", anv$`Pr(>Chi)`[2], "\n", 
            sep = "")
      }
      
      dev_chisq_eff("bmi_bin_[18.5-25] kg/m^2:sexFemale",
                    "bmi_bin_[25-30] kg/m^2:sexFemale", df, mod_full)
      
      dev_chisq_eff("bmi_bin_[0-18.5] kg/m^2:sexFemale",
                    "bmi_bin_[18.5-25] kg/m^2:sexFemale", df, mod_full)
      
      dev_chisq_eff("bmi_bin_[18.5-25] kg/m^2",
                    "bmi_bin_[25-30] kg/m^2", df, mod_full)
      
      dev_chisq_eff("bmi_bin_[18.5-25] kg/m^2",
                    "bmi_bin_[0-18.5] kg/m^2", df, mod_full)
      return(NULL) 
    }
  }
  
  mod
}

#' * Third step - plot the logistic coefficients * 
plot_or.glm <- function(mod, ref_bin = "[18.5-25] kg/m^2") {

  df <- summary(mod)$coefficients
  df <- rbind(df, 0)
  rownames(df)[length(rownames(df))] <- paste0("bmi_bin", ref_bin)
  df <- rbind(df, 0)
  rownames(df)[length(rownames(df))] <- paste0("bmi_bin", ref_bin, ":sexFemale")
  df <- data.table(df, keep.rownames = TRUE)
  
  dif_impact <- df[rn == "sexFemale"]$Estimate
  
  df <- df[grepl("bmi_bin", rn)]
  df[, group := ifelse(grepl("Female", rn), "Female", "Male")]
  
  df <- setorderv(df, c("group", "rn"))
  df[group == "Female"]$Estimate <- df[group == "Female"]$Estimate + 
    df[group == "Male"]$Estimate + dif_impact
  
  df[group == "Female", Estimate := Estimate + dif_impact]
  df[, bmi_bin := gsub("bmi_bin|:.*", "", df$rn)]
  df$bmi_bin <- factor(df$bmi_bin)
  
  ggplot(df, aes(x = as.integer(bmi_bin), y = exp(Estimate), color = group)) +
    geom_point() + geom_line() + theme_bw() +
    geom_errorbar(aes(ymin = exp(Estimate - 1.96 * `Std. Error`),
                      ymax = exp(Estimate + 1.96 * `Std. Error`)),
                  linewidth = 0.75, width = 0.25) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = levels(df$bmi_bin), 
                       breaks = seq_along(levels(df$bmi_bin))) +
    ylab("Estimate Odds Ratio") + xlab("BMI group") + 
    theme(
      legend.position = c(0.8, 0.8),
      legend.box.background = element_rect()
    ) + scale_color_discrete(name = "Sex")

}

plot_or.list <- function(mod, ref_bin = "[18.5-25] kg/m^2") {
  
  if (setequal(names(mod), c("Male", "Female"))) {
    
    get_coeff <- function(mod, wch) {
      
      res <- as.data.frame(summary(mod[[wch]])$coefficients[, c(1, 2)])
      res <- rbind(res, c(0, 0))
      rownames(res)[length(rownames(res))] <- paste0("bmi_bin", ref_bin)
      res <- cbind(res, group = wch)
      res <- res[grepl("bmi_bin", rownames(res)), ]
      res$bmi_bin <- gsub("bmi_bin|:sexFemale", "", rownames(res))
      res$bmi_bin <- factor(res$bmi_bin)
      names(res) <- c("Estimate", "SE", "group", "bmi_bin")
      res
    }
    
    df <- rbind(get_coeff(mod, "Male"), get_coeff(mod, "Female"))
  } else {
    
    se <- sqrt(diag(chol2inv(mod$qr$qr[seq_len(mod$rank), seq_len(mod$rank)])))
    names(se) <- dimnames(mod$R)
    
    df <- data.frame(Estimate = coef(mod), SE = se)
    df <- rbind(df, c(0, 0))
    rownames(df)[length(rownames(df))] <- paste0("bmi_bin_", ref_bin)
    df <- df[grepl("bmi_bin", rownames(df)), ]
    df$group <- ifelse(grepl("Female", rownames(df)), "Female", "Male")
    
    df$bmi_bin <- gsub("bmi_bin_|:sexFemale", "", rownames(df))
    df$bmi_bin <- factor(df$bmi_bin)
  }
  # browser()
  df$cih <- exp(df$Estimate + 1.96 * df$SE)
  df$cil <- exp(df$Estimate - 1.96 * df$SE)
  df$cic <- exp(df$Estimate)
  
  cat(
    paste0(df$group, df$bmi_bin, ": OR ", spec_dec(df$cic, 2), ", CI [", 
           spec_dec(df$cil, 2), ", ", spec_dec(df$cih, 2), "]"), sep = "\n"
  )
  
  ggplot(df, aes(x = -0.1 + as.integer(bmi_bin) + (group == "Male") / 5, 
                 y = exp(Estimate), color = group)) +
    geom_point() + 
    # geom_line() + 
    theme_bw() +
    geom_errorbar(aes(ymin = exp(Estimate - 1.96 * SE),
                      ymax = exp(Estimate + 1.96 * SE)),
                  linewidth = 0.75, width = 0.1) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = levels(df$bmi_bin), 
                       breaks = seq_along(levels(df$bmi_bin))) +
    ylab("Estimate Odds Ratio") + xlab("BMI group") + 
    theme(
      legend.position = c(0.8, 0.8),
      legend.box.background = element_rect()
    ) + scale_color_discrete(name = "Sex")
}

plot_or <- function(x, ...) UseMethod("plot_or")
