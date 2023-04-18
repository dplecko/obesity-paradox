
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

cmbd <- c("chr_resp", "chr_cvs", "chr_liv", "chr_ren", "chr_imun", "chr_imunrx", 
  "aids", "hep_fail", "lymphoma", "met_canc", "leukaemia", "chr_imunsup", 
  "cirrhosis")

data <- load_concepts(c(cmbd, "bmi", "death"), "anzics")
data <- data[!is.na(bmi)]
data[is.na(death), death := FALSE]
data <- replace_na(data, 0)

cormat <- cor(data[, c(cmbd), with=FALSE])

melted_cormat <- reshape2::melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-0.1,1), space = "Lab") +
  geom_text(aes(Var2, Var1, label = round(value, 2)))


max(
  cor(data[, c(cmbd), with=FALSE]) - cor(data[death == TRUE, c(cmbd), with=FALSE])
)