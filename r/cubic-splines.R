
rr_splines <- function(data, oth_reg, tag = NULL, 
                       boot = c("par", "nonpar", "none"), nboot = 0,
                       parallel = TRUE, model_save = NULL) {
  
  boot <- match.arg(boot, c("par", "nonpar", "none"))
  
  # manual formula construction
  form_cnstr <- function(vars) {
    
    as.formula(
      paste(
        "outcome ~ s(bmi_all) +", paste0(vars, collapse = "+") 
      )
    )
  }
  
  # Fit a cubic splines model with an interaction term
  # cat("Fitting 1st GAM\n")
  model <- gam(form_cnstr(oth_reg), 
               data = data, 
               family = binomial())
  
  if (boot == "nonpar" & !is.null(model_save)) {
    
    save(
      model,
      file = file.path(root, "cache", paste0("model-", model_save, "-", tag, ".rda"))
    )
  }
  
  # Generate a sequence of values for X and Q
  new_X <- seq(18, 40, 0.25)  # Adjust the number of points as desired
  out_len <- length(new_X)
  
  th_hat <- predict(model, cbind(data[1, oth_reg, with=FALSE], bmi_all = new_X),
                    type = "response")
  th_hat <- data.table(bmi_all = new_X, odds = th_hat / (1 - th_hat))
  th_hat[, OR := odds / odds[min(which(bmi_all >= 22.5))]]
  th_hat[, tag := tag]
  
  if (boot == "none") {
    
    return(th_hat)
  } else if (boot == "nonpar" & parallel) {
    if (nboot == 0) return(NULL)
    th_ss <- do.call(
      rbind,
      parallel::mclapply(
        seq_len(nboot),
        function(i) {
          
          set.seed(i)
          nrw <- nrow(data)
          bt_smp <- sample.int(nrw, replace = TRUE)
          cat("\r", i)
          rr_splines(data[bt_smp], oth_reg, tag, "none")
        }, mc.cores = n_cores()
      )
    )
  }
  
  alpha <- 0.05
  cis <- th_ss[, list(lwr = quantile(OR, alpha / 2), 
                      upr = quantile(OR, 1 - alpha / 2)), 
               by = "bmi_all"]
  merge(th_hat, cis, by = "bmi_all")
}

spline_plot <- function(src, coh, outcome, boot = c("nonpar", "none"),
                        nboot = 0, parallel = TRUE, model_save = NULL) {
  
  data <- load_data(src, coh = coh, bmi_cts = TRUE, outcome = outcome)
  oth_reg <- setdiff(names(data), c(id_vars(data), "outcome", "bmi_all", "sex"))
  
  if (file.exists(file.path(root, "cache", 
                            paste0("spline", src, coh, outcome, boot, ".rda")))) {
    
    load(file.path(root, "cache", paste0("spline", src, coh, outcome, boot, ".rda")))
  } else {
    
    rr_sp_np <- rbind(
      rr_splines(data[sex == "Male"], oth_reg, "Male", boot, nboot, 
                 parallel, model_save),
      rr_splines(data[sex == "Female"], oth_reg, "Female", boot, nboot, 
                 parallel, model_save)
    ) 
    save(rr_sp_np, file = file.path(root, "cache", 
                                    paste0("spline", src, coh, outcome, boot, ".rda")))
  }
  
  
  rr_sp_np[, lwr := ifelse(lwr < 0, 0, lwr)]
  
  add_to_title <- ": "
  if (coh == "bmi_10pc") add_to_title <- " (Low Missingness): "
  if (boot == "nonpar") {
    
    # if (src == "sic") rr_sp_np <- rr_sp_np[bmi_all <= 35]
    
    p <- ggplot(rr_sp_np, aes(x = bmi_all, y = OR, color = tag)) +
      geom_line(linewidth = 0.75) + theme_bw() +
      geom_ribbon(
        aes(ymin = 2 * OR - upr, ymax = 2 * OR - lwr, fill = tag), 
        linewidth = 0, alpha = 0.15
      ) +
      ylab("Odds Ratio (OR)") + xlab(latex2exp::TeX("BMI $(kg/m^2)$")) +
      scale_color_discrete(name = "Sex") +
      scale_fill_discrete(name = "Sex") +
      theme(legend.position = c(0.5, 0.77),
            legend.box.background = element_rect()) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      ggtitle(paste0(srcwrap(src), add_to_title, outwrap(outcome)))
    
    if (src == "sic") p <- p + coord_cartesian(ylim = c(0.3, 1.7))
  } else {
    
    p <- ggplot(rr_sp_np, aes(x = bmi_all, y = OR, color = tag)) +
      geom_line() + theme_bw() +
      ylab("Odds Ratio (OR)") + xlab(latex2exp::TeX("BMI $(kg/m^2)$")) +
      scale_color_discrete(name = "Sex") +
      scale_fill_discrete(name = "Sex") +
      theme(legend.position = c(0.7, 0.77),
            legend.box.background = element_rect()) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      ggtitle(paste0(srcwrap(src), add_to_title, outwrap(outcome)))
  }
  
  p
}


# three spline plot
two_spline_plot <- function(src, coh, boot, model_save = NULL) {
  
  list(
    spline_plot(src, coh, "pci_or_death", boot, model_save = model_save),
    spline_plot(src, coh, "death", boot, model_save = NULL)
  )
}
