
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

src <- c("anzics")
cohs <- c("bmi")
ttl <- list(bmi = "- entire cohort", bmi_5pc = "- BMI missingness < 5%")
bmi_bins <- "who"
ref_bin <- ifelse(bmi_bins == "who", "[18.5-25] kg/m^2", "[22.5-25] kg/m^2")
plt <- list()

for (add_pci in c(FALSE, TRUE)) {
  
  for (curr_coh in cohs) {
    
    for (dsrc in src) {
      
      dat <- load_data(dsrc, coh = curr_coh, bmi_bins = bmi_bins, 
                       ref_bin = ref_bin, add_pci = add_pci, age_match = TRUE,
                       reverse_risk = TRUE)
      mod_uni <- fit_log(dat, "univar", ref_bin = ref_bin)
      mod_wths <- fit_log(dat, "manual", ref_bin = ref_bin,
                          object = "within-sex")
      mod_wthb <- fit_log(dat, "manual", ref_bin = ref_bin,
                          object = "within-bmi")
      print(summary(mod_wthb))
      boot_dat <- dat[sample(nrow(dat), replace = TRUE, 
                             prob = attr(dat, "weights"))]
      attr(boot_dat, "weights") <- NULL
      mod_man <- fit_log(boot_dat, "manual", ref_bin = ref_bin)
      
      plt[[length(plt) + 1]] <- plot_or(mod_man, ref_bin = ref_bin)
    }
  }
}

cowplot::plot_grid(plotlist = plt,
                   labels = paste0("(", LETTERS[seq_along(plt)], ")"),
                   ncol = 2L)

ggsave(file.path(root, "results", "Figure1.png"), width = 15, height = 6)