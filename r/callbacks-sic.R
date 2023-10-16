
sic_adm_cb <- function(x, ...) {
  
  x[, SurgicalSite := NULL]
  x[, SurgicalSite := "surg"]
}

sic_surg_site_cb <- function(x, ...) {
  
  category_list <- list(
    Abdomen = c(
      2211, # Abdomen: Unterer GI-Trakt
      2219, # Abdomen: Oberer GI-Trakt (inkl. Jejunum)
      2225, # Abdomen: Leber-Chriurgie
      2242, # Abdomen: Pankreas
      2253, # Abdomen: Biliärtrakt
      2259  # Abdomen: endokrine Chirurgie
    ),
    Vascular = c(
      2214, # Gefäß: Aortenchirurgie
      2234, # Gefäß: Karotischirurgie
      2243, # Gefäß: große Gefäße (Thorax und Bauch)
      2258, # Gefäß: Andere
      2229  # Gefäß: periphere Gefäße
    ),
    Heart = c(
      2227, # Herz: CABG
      2237, # Herz: Klappe mit CABG
      2240, # Herz: Klappe
      2241  # Herz: Andere
    ),
    Trauma_Orthopedics = c(
      2231, # Trauma: SHT
      2246, # Trauma: Extremitäten
      2269, # Trauma: Abdomen (Assuming Abdominal trauma)
      2254, # Trauma: Thorax (Assuming Thoracic trauma)
      2239, # Trauma: Polytrauma
      2207  # Extremitäten-Chirurgie
    ),
    Neuro = c(
      2245, # Neurochirurgie: zerebrovaskulär
      2250, # Neurochirurgie: Wirbelsäule
      2264, # Neurochirurgie: Andere
      2273  # Neurochirurgie: intrakranieller Tumor
    ),
    Transplant = c(
      2268, # Transplantation: Leber
      2270, # Transplantation: Herz/Lunge
      2271, # Transplantation: Andere
      2272  # Transplantation: Herz
    ),
    Gynecological = c(2260), # Gynäkologischer Eingriff
    Birth = c(2261),         # Geburtshilfe
    Other = c(
      2205, # Unknown
      2221, # .
      2217, # HNO
      2210, # Kieferchirurgie (Oral / Maxilofacial)
      2232  # Andere Eingriffe
    ),
    Thorax = c(
      2262, # Thorax: Lobektomie
      2263, # Thorax: Pneumonektomie
      2267, # Thorax: Pleura
      2226  # Thorax: Andere
    )
  )
  
  site_map <- do.call(
    rbind,
    Map(
      function(site, nums) {
        data.table(site = site, SurgicalSite = nums)
      }, names(category_list), category_list
    )
  )
  
  x <- merge(x, site_map, by = "SurgicalSite")
  x[, SurgicalSite := NULL]
  rename_cols(x, "SurgicalSite", "site")
}

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
