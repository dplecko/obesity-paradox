
add_age_std <- function(dat) {
  
  bins <- seq.int(19, 90)
  dat$age_std <- .bincode(round(dat$age), 
                          breaks = c(-Inf, bins - 0.001, Inf)) + 17
  dat
}

fnw <- function(targ, targ_coh, targ_idt, src, src_coh, src_idt) {
  
  std_age <- function(dat) {
    
    bins <- seq.int(19, 90)
    dat$age_std <- .bincode(round(dat$age), 
                            breaks = c(-Inf, bins - 0.001, Inf)) + 17
    dat[, wgh := 1/.N, by = c("sex")]
    res <- dat[, sum(wgh), by = c("sex", "age_std")]
    setorderv(res, c("sex", "age_std"))
    res
  }
  
  t1 <- load_concepts(c("age", "sex"), targ, patient_ids = targ_coh,
                      id_type = targ_idt)
  
  s1 <- load_concepts(c("age", "sex"), src, patient_ids = src_coh,
                      id_type = src_idt)
  
  # standardize age
  fnw <- merge(std_age(s1), std_age(t1), by = c("sex", "age_std"))
  fnw[, weight := V1.y / V1.x]
  fnw[, c("sex", "age_std", "weight"), with=FALSE]
}