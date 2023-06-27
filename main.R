
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))

# create folders if needed
if (!dir.exists(file.path(root, "tables")))
  dir.create(file.path(root, "tables"))
if (!dir.exists(file.path(root, "figures")))
  dir.create(file.path(root, "figures"))

# generate cohort
source(file.path(root, "scripts", "paper", "cohort-gen.R"))

# eFigure 1 (flow diagram)

# Table 1 (patient)
source(file.path(root, "scripts", "paper", "tables", "patient-tables.R"))

# Table 2 (outcomes)
source(file.path(root, "scripts", "paper", "tables", "outcomes.R"))

# eTable 1 (missing vs. present)
source(file.path(root, "scripts", "paper", "tables", "bmi-miss-comparison.R"))

# Figure 1 (baseline admission risk) 
source(file.path(root, "scripts", "paper", "anzics-vs-australia.R"))

# Figure 2 (splines ANZICS)
# eFigure 1 (ANZICS low miss splines)
# eFigure 2 (MIMIC, SIC splines)
source(file.path(root, "scripts", "paper", "splines.R"))

# eTable 2 (ANZICS low miss, MIMIC, SICdb)
source(file.path(root, "scripts", "paper", "tables", "sensitivity-patient-tables.R"))

# eFigure 3 (splines with CIs - Euler)
source(file.path(root, "scripts", "paper", "splines-with-cis.R"))

# eTable 3
source(file.path(root, "scripts", "paper", "tables", "miiv-hosp-patient-table.R"))

# Figure 3
source(file.path(root, "scripts", "paper", "miiv-comorbidities.R"))
