
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

src <- "anzics" # c("sic", "anzics", "aumc", "miiv")
cohs <- c("bmi")
ttl <- list(bmi = "- entire cohort", bmi_10pc = "- BMI missingness < 5%")
bmi_bins <- "who-obese"
ref_bin <- ifelse(bmi_bins %in% c("who", "who-obese"), "[18.5-25] kg/m^2", 
                  "[22.5-25] kg/m^2")
plt <- list()

for (add_pci in c(FALSE, TRUE)) {
  
  for (curr_coh in cohs) {
    
    for (dsrc in src) {
      
      dat <- load_data(dsrc, coh = curr_coh, bmi_bins = bmi_bins, 
                       ref_bin = ref_bin, add_pci = add_pci, age_match = TRUE)
      # mod_uni <- fit_log(dat, "univar", ref_bin = ref_bin)
      # mod_wths <- fit_log(dat, "manual", ref_bin = ref_bin,
      #                     object = "within-sex")
      # mod_wthb <- fit_log(dat, "manual", ref_bin = ref_bin,
      #                     object = "within-bmi")
      # print(summary(mod_wthb))
      mod_man <- fit_log(dat, "split", ref_bin = ref_bin)
      
      plt[[length(plt) + 1]] <- plot_or(mod_man, ref_bin = ref_bin) + 
        ggtitle(dsrc)
    }
  }
}

cowplot::plot_grid(plotlist = plt,
                   labels = paste0("(", LETTERS[seq_along(plt)], ")"),
                   ncol = 2L, vjust = 2.5) # vjust = 1.5 is default

ggsave(file.path(root, "results", "anzics-ors.png"), width = 10, height = 6)

#' * External analyses *
ext_plt <- list()
src1 <- "miiv"
src2 <- "aumc"
for (add_pci in c(FALSE, TRUE)) {
  
  dat1 <- load_data(src1, coh = "bmi", bmi_bins = bmi_bins, 
                    ref_bin = ref_bin, add_pci = add_pci, age_match = TRUE)
  dat1[, src := src1]
  dat2 <- load_data(src2, coh = "bmi", bmi_bins = bmi_bins, 
                    ref_bin = ref_bin, add_pci = add_pci, age_match = TRUE)
  dat2[, src := src2]
  mod_man <- fit_log(rbind(dat1, dat2), "manual", ref_bin = ref_bin)
  ext_plt[[length(ext_plt) + 1]] <- plot_or(mod_man, ref_bin = ref_bin)
}

cowplot::plot_grid(plotlist = ext_plt,
                   labels = paste0("(", LETTERS[seq_along(plt)], ")"),
                   ncol = 2L, vjust = 2.5)
ggsave("~/Desktop/miiv_aumc.png", height = 5, width = 12)
