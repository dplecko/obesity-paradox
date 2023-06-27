
bmi_death_pattern <- function(src) {
  
  val_var <- "pci_or_death"
  
  pids <- load_concepts("adm_episode", src)[adm_episode %in% c(0, 1)]
  res <- load_concepts(c("sex", val_var, "bmi_all"), src, patient_ids = pids)
  
  res[is.na(get(val_var)), c(val_var) := FALSE]
  res[, has_bmi := !is.na(bmi_all)]
  
  # Plot histogram
  # Plot bar plot
  df <- res[, mean(get(val_var)), by = c("has_bmi", "sex")]
  df <- df[complete.cases(df)]
  ggplot(df, aes(x = sex, y = V1, fill = has_bmi)) +
    geom_col(position = "dodge") +
    labs(title = srcwrap(src),
         x = "Sex", y = "Mortality") +
    scale_fill_manual(
      values = c("FALSE" = "blue", "TRUE" = "red"),
      name = "BMI Recorded"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom")
}

cowplot::plot_grid(
  bmi_death_pattern("anzics"),
  bmi_death_pattern("miiv"),
  ncol = 2L
)

ggsave("~/Desktop/bmi-death-pattern.png", width = 10, height = 5, bg = "white")


# Set the number of parallel threads
ncores <- parallel::detectCores()

library(mgcv)

# Set the number of parallel threads
ncores <- parallel::detectCores()
nthreads <- ifelse(ncores > 1, ncores - 1, 1)

# Set the parallel control
control <- gam.control(nthreads = nthreads)

# Fit the GAM with parallel computation
gam_model <- gam(mpg ~ s(wt) + s(hp), data = mtcars, control = control)

# Print the model summary
summary(gam_model)

form_cnstr <- function(vars) {
  
  as.formula(
    paste(
      "death ~ s(bmi_all) +", paste0(vars, collapse = "+") 
    )
  )
}

# choose the source
src <- "sic"
data <- load_data(src, bmi_cts = TRUE, add_pci = TRUE)

# get other regressors
oth_reg <- setdiff(names(data), c(id_vars(data), "death", "bmi_all", "sex"))

# do some benchmarking for the parallelization

# benchmark the model building
microbenchmark::microbenchmark(
  model <- gam(form_cnstr(oth_reg), 
               data = data, 
               family = binomial()),
  times = 20L
)

microbenchmark::microbenchmark(
  model_par <- gam(form_cnstr(oth_reg), data = data, family = binomial(), 
               control = control),
  times = 20L
)

# benchmark the predictions
out_len <- 50
new_X <- seq(18, 35, length.out = out_len)
microbenchmark::microbenchmark(
  new_data <- do.call(
    rbind,
    lapply(
      new_X, 
      function(x) {
        # cat("Fitting BMI =", x, "\n")
        proba <- predict(model, newdata = cbind(data[, oth_reg, with=FALSE], 
                                                bmi_all = x),
                         type = "response", se.fit = TRUE)
        data.table(bmi_all = x, pred_proba = proba$fit, se = proba$se.fit,
                   pid = seq_along(proba$fit))
      }
    )
  ),
  times = 5L
)

microbenchmark::microbenchmark(
  new_data <- do.call(
    rbind,
    lapply(
      new_X, 
      function(x) {
        # cat("Fitting BMI =", x, "\n")
        proba <- predict(model_par, newdata = cbind(data[, oth_reg, with=FALSE], 
                                                bmi_all = x),
                         type = "response", se.fit = TRUE, control = control)
        data.table(bmi_all = x, pred_proba = proba$fit, se = proba$se.fit,
                   pid = seq_along(proba$fit))
      }
    )
  ),
  times = 5L
)

