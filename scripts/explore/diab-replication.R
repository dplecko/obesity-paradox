

root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE, recursive = TRUE), source))

src <- "mimic_demo"
ins_breaks <- c(0, 1, 10, 20, 40, Inf)

ins_cb <- function(ins, ...) {
  
  day_one <- function(x) x >= hours(0L) & x <= hours(24L)
  idx_var <- index_var(ins)
  ids_var <- id_vars(ins)
  
  ins <- ins[
    day_one(get(idx_var)), list(ins24 = sum(ins)), 
    by = c(ids_var)
  ]
  ins <- ins[,
             ins24 := list(cut(ins24, breaks = ins_breaks, 
                               right = FALSE))
  ]
  ins
}

grep_diab <- function(x) {
   grepl("^250\\.?[0-9]{2}$", x)
}

diab  <- item(src, table = "diagnoses_icd",
              callback = transform_fun(grep_diab),
              class = "col_itm")

diab  <- concept("diab", diab, "diabetes", target = "id_tbl",
                 class = "lgl_cncpt")

dat <- load_concepts(c("ins24", "diab"), src, id_type = "icustay",
                     verbose = FALSE)
dat <- replace_na(dat, "[0,1)", vars = "ins24")

