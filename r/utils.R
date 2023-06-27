
srcwrap <- function(src) {
  
  if (length(src) > 1L) {
    return(vapply(src, srcwrap, character(1L)))
  }
  
  switch(src,
         mimic = "MIMIC-III",
         miiv = "MIMIC-IV",
         eicu = "eICU",
         hirid = "HiRID",
         aumc = "AUMC",
         mimic_demo = "MIMIC Demo",
         eicu_demo = "eICU Demo",
         anzics = "ANZICS APD",
         sic = "SICdb",
         stop("unknown data source")
  )
}

outwrap <- function(out) {
  
  if (length(out) > 1L) {
    return(vapply(out, outwrap, character(1L)))
  }
  
  switch(out,
         death = "Death",
         pci = "PerCI",
         pci_or_death = "Death + PerCI",
         stop("unknown outcome")
  )
}

n_cores <- function() {
  as.integer(
    Sys.getenv("LSB_DJOB_NUMPROC", unset = parallel::detectCores() / 2L)
  )
}