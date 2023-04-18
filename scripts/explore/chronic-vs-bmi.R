

cil <- load_concepts(c(chr_il, "bmi_bins", "sex"), "anzics")

cil[, chr_score := chr_resp+chr_cvs+chr_liv+chr_ren+chr_imun+chr_imunrx+aids+
      hep_fail+lymphoma+met_canc+leukaemia+chr_imunsup+cirrhosis, .SDcols = chr_il]

cil <- cil[complete.cases(cil)]
cil$group <- ifelse(cil$sex == "Female", 1, 2)
dat <- cil[, list(chronic_score = mean(chr_score)), by = c("sex", "bmi_bins")]
ggplot(
  dat,
  aes(x = bmi_bins, y = chronic_score, color = sex, group = group)
) + geom_point() + geom_line() + theme_bw()