
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

load_apd <- function() {
  
  get_apd <- function(x) {
    
    am <- readxl::read_xlsx(file.path(root, "data", 
                                      paste0("australia-", x, ".xlsx")))
    
    am <- as.data.table(am)
    am <- am[-c(2, 3)]
    am <- setnames(am, "Measured Body Mass Index", "bmi_bins")
    am$bmi_bins <- bin_labels(config("bmi-bins")[["who"]], "$kg/m^2$")
    
    for (i in seq.int(2, ncol(am))) {
      
      am[[i]] <- am[[i]] / sum(am[[i]]) * 100
    }
    
    am
  }
  
  list(
    Male = get_apd("males"),
    Female = get_apd("females")
  )
}

apd <- load_apd()

age_breaks <- c(18, 25, 35, 45, 55, 65, 75, 85) - 0.001

#' * filtering for Australia only * 
coh <- load_concepts("country", "anzics", 
                     patient_ids = config("cohort")[["anzics"]][["bmi"]])
coh_ids <- id_col(coh[country != "NZ"])

anz <- load_concepts(c("bmi", "sex", "age", "bmi_bins"), "anzics",
                     patient_ids = coh_ids)
anz <- anz[complete.cases(anz)]

anz[, age_group := .bincode(age, c(-Inf, age_breaks, Inf))]
anz <- anz[age_group > 1]

anz[, total := .N, by = c("sex")]
anz[, weight := 1 / total]

anz_props <- anz[, list(prop = 100 * sum(weight)), by = c("sex", "bmi_bins")]
anz_props[, loc := "ANZICS APD"]

apd_props_cmp <- function() {
  
  apd <- load_apd()
  res <- lapply(
    c("Male", "Female"),
    function(sx) {
      
      idx <- anz[sex == sx]$age_group
      apd_props <- apd[[sx]][, ..idx]
      
      data.table(bmi_bins = apd[[sx]][[1]], sex = sx, 
                 prop = rowMeans(apd_props))
    }
  )
  cbind(do.call(rbind, res), loc = "Australia")
}

apd_props <- apd_props_cmp()
anz_vs_aus <- rbind(apd_props, anz_props)

library(ggpattern)
pA <- ggplot(anz_vs_aus, aes(y = prop / 100, x = bmi_bins, fill = sex, pattern = loc)) +
  geom_col_pattern(linewidth = 1,
                   position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 1) + theme_bw() +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  scale_pattern_manual(name = "Population", values = c(Australia = "stripe", `ANZICS APD` = "none")) +
  theme(
    legend.position = c(0.8, 0.6),
    legend.box.background = element_rect()
  ) + xlab("BMI group") + ylab("Group proportion") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Sex") +
  scale_x_discrete(
    labels = do.call(c, lapply(levels(anz_vs_aus$bmi_bins), latex2exp::TeX))
  )

# baseline risk ratio plot
# bootstrap uncertainty quantification
anz_prop_boot <- function(dat, nboot = 50) {
  
  res <- lapply(
    seq_len(nboot),
    function(boot_samp) {
      
      set.seed(boot_samp)
      
      datb <- dat[sample(nrow(dat), replace = TRUE)]
      
      datb[, total := .N, by = c("sex")]
      datb[, weight := 1 / total]
      
      anz_props <- datb[, list(prop = 100 * sum(weight)), by = c("sex", "bmi_bins")]
      anz_props[, loc := "ANZICS APD"]
      anz_props[, boot_rep := boot_samp]
      
      anz_props
    }
  )
  do.call(rbind, res)
}

anz_boot <- anz_prop_boot(anz, nboot = 50)

dat <- merge(anz_boot[, list(avg = mean(prop), stdev = sd(prop)), 
                      by = c("sex", "bmi_bins")], apd_props, 
             by = c("sex", "bmi_bins"))

dat[, bmi_bins := factor(bmi_bins, levels = levels(anz_boot$bmi_bins))]
dat[, rr := avg / prop]
dat[, up := (avg + 1.96 * stdev) / prop]
dat[, lw := (avg - 1.96 * stdev) / prop]
dat[, irr := 1 / rr]

cat(
  paste0(dat$sex, dat$bmi_bin, ": RR ", spec_dec(dat$rr, 2), ", CI [",
         spec_dec(dat$lw, 2), ", ", spec_dec(dat$up, 2), "]"), sep = "\n"
)

pB <- ggplot(dat, aes(x = bmi_bins, y = avg / prop, color = sex, group = sex)) +
  geom_line() + geom_point() +
  geom_ribbon(aes(ymin = (avg - 1.96 * stdev) / prop, 
                  ymax = (avg + 1.96 * stdev) / prop,
                  fill = sex), alpha = 0.3) +
  theme_bw() + xlab("BMI group") + ylab("Baseline ICU Admission Risk Ratio") +
  scale_fill_discrete(name = "Sex") +
  scale_color_discrete(name = "Sex") +
  scale_x_discrete(
    labels = do.call(c, lapply(levels(anz_vs_aus$bmi_bins), latex2exp::TeX))
  ) +
  theme(legend.position = c(0.6, 0.6),
        legend.box.background = element_rect())

p_ava <- cowplot::plot_grid(pA, pB, ncol = 2L, labels = c("(A)", "(B)"))

ggsave(file.path(root, "results", "Figure1.tiff"), 
       plot = p_ava, width = 15, height = 6,
       compression = "lzw")

#' #' * proportion of the paradox explained *
#' irisk <- dat[, c("sex", "bmi_bins", "irr"), with=FALSE]
#' irisk <- setnames(irisk, "bmi_bins", "bmi_bin")
#' irisk[, bmi_bin := as.factor(bmi_bin)]
#' irisk$bmi_bin <- relevel(irisk$bmi_bin, ref = "[18.5-25] kg/m^2")
#' save(irisk, file = file.path(root, "data", "inverse_rr.rda"))

