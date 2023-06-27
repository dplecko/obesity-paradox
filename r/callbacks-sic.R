

sic_omr_bmi_cb <- function(x, ...) {
  
  x[integer(0L)]
}

sic_sex_cb <- function(x, val_var, ...) {
  
  x[, c(val_var) := ifelse(get(val_var) == 735, "Male", "Female")]
  x
}

sic_adm_epi_cb <- function(x, val_var, grp_var, off_var, ...) {
  
  x <- setorderv(x, c("PatientID", "CaseID"))
  x[, c(val_var) := seq_along(get(val_var)), by = grp_var]
  setorderv(x, "CaseID")
}

sic_inr_pt_cb <- function(x, val_var, ...) {
  
  x[, c(val_var) := 1 / (get(val_var) / 100)]
}

sic_gcs_cb <- function(x, ...) {
  
  x[, c(index_var(x)) := hours(0L)]
  x
}

sic_mv_cb <- function(x, val_var, dur_var, add_var, ...) {
  
  x[, c(dur_var) := ricu:::min_as_mins(get(add_var))]
  x[, c(val_var) := as.character(get(val_var))]
  x[, c(val_var) := "invasive"]
  x
}

sic_samp_cb <- function(x, val_var, ...) {
  
  x[, c(index_var(x)) := get(index_var(x)) - hours(48L)] # subtract 48h wait
  x[, c(val_var) := as.logical(get(val_var))]
  x[, c(val_var) := FALSE]
  x
}

sic_los_icu_cb <- function(x, ...) {
  
  x[, TimeOfStay := as.numeric(TimeOfStay / 60)]
}

sic_vent_start_cb_lab <- function(x, ...) {
  
  x[, LaboratoryValue := LaboratoryValue > 21]
}

sic_vent_start_cb_float <- function(x, ...) {
  
  x[, Val := Val > 21]
}

sic_death_cb <- function(x, ...) {
  
  x[, HospitalDischargeType := HospitalDischargeType == 2028]
  x[, OffsetAfterFirstAdmission := NA]
  x[HospitalDischargeType == TRUE, 
    OffsetAfterFirstAdmission := TimeOfStay]
  x[!is.na(OffsetOfDeath) & HospitalDischargeType == TRUE, 
    OffsetAfterFirstAdmission := OffsetOfDeath]
  x
}

sic_rate_kg <- function (x, val_var, stop_var, 
                         env, ...) 
{
  res <- add_weight(x, env, "weight")
  
  res[, AmountPerMinute := AmountPerMinute * 10^6 / weight]
  expand(res, index_var(x), stop_var, keep_vars = c(id_vars(x), val_var))
}

sic_dur <- function (x, val_var, stop_var, grp_var = NULL, ...) {
  
  calc_dur(x, val_var, index_var(x), stop_var, grp_var)
}