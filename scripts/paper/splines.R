
# splines
root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

# Create a data frame with your data, where Y is the binary outcome,
# X is the continuous treatment variable, and W represents other regressors
rr_splines <- function(data, oth_reg, tag = NULL, 
                       boot = c("par", "nonpar", "none"), nboot = 50,
                       parallel = TRUE) {
  
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
  
  # Generate a sequence of values for X and Q
  out_len <- 50
  new_X <- seq(18, 35, length.out = out_len)  # Adjust the number of points as desired
  
  th_hat <- predict(model, cbind(data[1, oth_reg, with=FALSE], bmi_all = new_X))
  th_hat <- data.table(bmi_all = new_X, odds = th_hat / (1 - th_hat))
  
  # Add the other regressors to the new_data data frame if needed
  new_data <- do.call(
    rbind,
    lapply(
      new_X, 
      function(x) {
        # cat("Fitting BMI =", x, "\n")
        proba <- predict(model, newdata = ,
                         , se.fit = TRUE)
        data.table(bmi_all = x, pred_proba = proba$fit, se = proba$se.fit,
                   pid = seq_along(proba$fit))
      }
    )
  )
  browser()
  
  # get \hat \theta
  th_hat <- new_data[, mean(pred_proba), by = "bmi_all"]
  th_hat[, OR := odds / odds[min(which(bmi_all > 22.5))]]
  th_hat[, tag := tag]
  
  if (boot == "none") {
    
    return(th_hat)
  } else if (boot == "par") {
    
    th_ss <- NULL
    cat("Fitting bootstrap rep\n")
    for (i in seq_len(nboot)) {
      
      set.seed(i)
      nrw <- nrow(data)
      rw_sel <- (seq_len(out_len) - 1) * nrw
      cat("\r", i)
      bt_smp <- sample.int(nrw, replace = TRUE)
      bt_rws <- do.call(
        c, lapply(bt_smp, function(rw) rw + rw_sel)
      )
      
      # which rows does this correspond to?
      
      # need to speed up
      th_star <- new_data[bt_rws]
      th_star[, pred_proba_st := rnorm(length(pred_proba), mean = pred_proba,
                                       sd = se)]
      th_star <- th_star[, mean(pred_proba_st), by = "bmi_all"]
      th_star <- th_star[, RR := V1 / V1[min(which(bmi_all > 22.5))]]
      th_star[, rep := i]
      
      th_ss <- rbind(th_ss, th_star)
    }
  } else if (boot == "nonpar" & par == TRUE) {
    
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
        }, mc.cores = parallel::detectCores()
      )
    )
  } else if (boot == "nonpar" & par == FALSE) {
    
    th_ss <- NULL
    cat("Fitting bootstrap rep\n")
    for (i in seq_len(nboot)) {

      set.seed(i)
      nrw <- nrow(data)
      bt_smp <- sample.int(nrw, replace = TRUE)
      cat("\r", i)
      th_star <- rr_splines(data[bt_smp], oth_reg, tag, "none")
      th_ss <- rbind(th_ss, th_star)
    }
  }
  
  alpha <- 0.05
  cis <- th_ss[, list(lwr = quantile(RR, alpha / 2), 
                      upr = quantile(RR, 1 - alpha / 2)), 
               by = "bmi_all"]
  merge(th_hat, cis, by = "bmi_all")
}


spline_plot <- function(src, coh, outcome, boot = c("nonpar", "none"),
                        nboot = 50, parallel = TRUE) {
  
  data <- load_data(src, coh = coh, bmi_cts = TRUE, outcome = outcome)
  oth_reg <- setdiff(names(data), c(id_vars(data), "outcome", "bmi_all", "sex"))
  
  
  if (file.exists(file.path(root, "cache", 
                            paste0("spline", src, outcome, boot, ".rda")))) {
    
    load(file.path(root, "cache", paste0("spline", src, outcome, ".rda")))
  } else {
    
    rr_sp_np <- rbind(
      rr_splines(data[sex == "Male"], oth_reg, "Male", boot, nboot, parallel),
      rr_splines(data[sex == "Female"], oth_reg, "Female", boot, nboot, parllel)
    ) 
    save(rr_sp_np, file = file.path(root, "cache", 
                                    paste0("spline", src, outcome, ".rda")))
  }

  if (boot == "nonpar") {
    
    p <- ggplot(rr_sp_np, aes(x = bmi_all, y = RR, color = tag)) +
      geom_line() + theme_bw() +
      geom_ribbon(
        aes(ymin = 2 * RR - upr, ymax = 2 * RR - lwr, fill = tag), 
        linewidth = 0, alpha = 0.2
      ) +
      ylab("Risk Ratio") + xlab("BMI (kg/m^2)") +
      scale_color_discrete(name = "Sex") +
      scale_fill_discrete(name = "Sex") +
      theme(legend.position = c(0.7, 0.77),
            legend.box.background = element_rect()) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      ggtitle(paste(srcwrap(src), outwrap(outcome)))
  } else {
    
    p <- ggplot(rr_sp_np, aes(x = bmi_all, y = RR, color = tag)) +
      geom_line() + theme_bw() +
      ylab("Risk Ratio") + xlab("BMI (kg/m^2)") +
      scale_color_discrete(name = "Sex") +
      scale_fill_discrete(name = "Sex") +
      theme(legend.position = c(0.7, 0.77),
            legend.box.background = element_rect()) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      ggtitle(paste(srcwrap(src), outwrap(outcome)))
  }
  
  p
}


# three spline plot
two_spline_plot <- function(src, coh, boot) {
  
  list(
    spline_plot(src, coh, "death", boot),
    # spline_plot(src, coh, "pci", boot),
    spline_plot(src, coh, "pci_or_death", boot)
  )
}

# ANZICS full
p1 <- cowplot::plot_grid(
  plotlist = two_spline_plot("anzics", "bmi", "none"),
  ncol = 2L
)
ggsave(file.path(root, "results", "Figure2.png"), plot = p1,
       height = 5, width = 12)


# ANZICS sense
p2 <- cowplot::plot_grid(
  plotlist = two_spline_plot("anzics", "bmi_10pc", "none"),
  ncol = 2L
)
ggsave(file.path(root, "results", "eFigure2.png"), plot = p2,
       height = 5, width = 12)


# MIMIC-IV + SICdb
p3 <- cowplot::plot_grid(
  plotlist = c(
    two_spline_plot("miiv", "bmi", "none"),
    two_spline_plot("sic", "bmi", "none")
  ),
  ncol = 2L
)
ggsave(file.path(root, "results", "eFigure3.png"), plot = p3,
       height = 10, width = 12)

# splines with CIs (need to run parallel on Euler)
# p4 <- cowplot::plot_grid(
#   plotlist = c(
#     two_spline_plot("anzics", "bmi", "nonpar"),
#     two_spline_plot("anzics", "bmi_10pc", "nonpar"),
#     two_spline_plot("miiv", "bmi", "nonpar"),
#     two_spline_plot("sic", "bmi", "nonpar")
#   ),
#   ncol = 2L
# )
# 
# ggsave(file.path(root, "results", "eFigure4.png"), plot = p4,
#        height = 20, width = 12)
