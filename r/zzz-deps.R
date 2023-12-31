
pkgs <- c("ricu", "readr", "ranger", "ggplot2", "plotly",
          "latex2exp", "stringr", "assertthat", "data.table", "scales",
          "Rcpp", "mgcv")

Sys.setenv("RICU_CONFIG_PATH" = file.path(root, "config"))
Sys.setenv("RICU_SRC_LOAD" = 
             "mimic,miiv,aumc,hirid,eicu,eicu_demo,mimic_demo,anzics,sic")

library(ricu)

if (!all(vapply(pkgs, requireNamespace, logical(1L)))) {
  stop("Packages {pkgs} are required in order to proceed.")
  if (!interactive()) q("no", status = 1, runLast = FALSE)
}

library(causalweight)
library(readr)
library(ranger)
library(ggplot2)
library(latex2exp)
library(stringr)
library(grf)
library(assertthat)
library(data.table)
library(plotly)
library(scales)
library(Rcpp)
library(mgcv)
