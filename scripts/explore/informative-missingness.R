
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
