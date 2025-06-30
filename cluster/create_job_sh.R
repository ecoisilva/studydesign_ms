
# MEAN HOME RANGE ---------------------------------------------------------
## Arguments: -------------------------------------------------------------

job_name <- "hr_buf_option1"
job_time_hrs <- 3

# Arguments:
species <- "buffalo"
individuals_no <- 8
dur_value <- 1
dur_unit <- "year"
dti_value <- 15
dti_unit <- "minutes"

## Script: ----------------------------------------------------------------

file_txt <- readLines(here::here("cluster",
                                 "run_mean_hr_TEMPLATE"))

# Replace placeholder text with variables:

file_txt <- gsub(pattern = "JOBNAME",
                 replacement = as.character(job_name),
                 x = file_txt)
file_txt <- gsub(pattern = "HRS",
                 replacement = ifelse(job_time_hrs < 10,
                                      paste0("0", job_time_hrs),
                                      as.character(job_time_hrs)),
                 x = file_txt)
file_txt <- gsub(pattern = "SPECIES",
                 replacement = paste0('"', species, '"'),
                 x = file_txt)
file_txt <- gsub(pattern = "NO_IND",
                 replacement = as.numeric(individuals_no),
                 x = file_txt)
file_txt <- gsub(pattern = "DUR_UNIT",
                 replacement = paste0('"', dur_unit, '"'),
                 x = file_txt)
file_txt <- gsub(pattern = "DUR",
                 replacement = as.numeric(dur_value),
                 x = file_txt)
file_txt <- gsub(pattern = "DTI_UNIT",
                 replacement = paste0('"', dti_unit, '"'),
                 x = file_txt)
file_txt <- gsub(pattern = "DTI",
                 replacement = as.numeric(dti_value),
                 x = file_txt)

writeLines(file_txt, con = here::here("cluster",
                                      "run_mean_hr.sh"))

# MEAN MOVEMENT SPEED -----------------------------------------------------
## Arguments: -------------------------------------------------------------

job_name <- "speed_gaz_option1"
job_time_hrs <- 3

# Arguments:
species <- "gazelle"
individuals_no <- 8
dur_value <- 1
dur_unit <- "year"
dti_value <- 15
dti_unit <- "minutes"

## Script: ----------------------------------------------------------------

file_txt <- readLines(here::here("cluster",
                                 "run_mean_speed_TEMPLATE"))

# Replace placeholder text with variables:

file_txt <- gsub(pattern = "JOBNAME",
                 replacement = as.character(job_name),
                 x = file_txt)
file_txt <- gsub(pattern = "HRS",
                 replacement = ifelse(job_time_hrs < 10,
                                      paste0("0", job_time_hrs),
                                      as.character(job_time_hrs)), 
                 x = file_txt)
file_txt <- gsub(pattern = "SPECIES",
                 replacement = paste0('"', species, '"'),
                 x = file_txt)
file_txt <- gsub(pattern = "NO_IND",
                 replacement = as.numeric(individuals_no),
                 x = file_txt)
file_txt <- gsub(pattern = "DUR_UNIT",
                 replacement = paste0('"', dur_unit, '"'),
                 x = file_txt)
file_txt <- gsub(pattern = "DUR",
                 replacement = as.numeric(dur_value),
                 x = file_txt)
file_txt <- gsub(pattern = "DTI_UNIT",
                 replacement = paste0('"', dti_unit, '"'),
                 x = file_txt)
file_txt <- gsub(pattern = "DTI",
                 replacement = as.numeric(dti_value),
                 x = file_txt)

writeLines(file_txt, con = here::here("cluster",
                                      "run_mean_speed.sh"))

# META-ANALYSES [resampled] -----------------------------------------------
## Arguments: -------------------------------------------------------------

job_name <- "meta_buf_speed"
job_time_hrs <- 5
n_files <- 1

filenames <- 'filenames_array=(
  "buffalo_speed_dti1day_dur4years_60inds"
)'

## Script: ----------------------------------------------------------------

file_txt <- readLines(here::here("cluster",
                                 "run_meta_resampled_TEMPLATE"))

# Replace placeholder text with variables:

file_txt <- gsub(pattern = "JOBNAME",
                 replacement = as.character(job_name),
                 x = file_txt)
file_txt <- gsub(pattern = "HRS",
                 replacement = ifelse(job_time_hrs < 10,
                                      paste0("0", job_time_hrs),
                                      as.character(job_time_hrs)),
                 x = file_txt)
file_txt <- gsub(pattern = "NFILES",
                 replacement = as.character(n_files),
                 x = file_txt)
file_txt <- gsub(pattern = "FILENAMEARRAY",
                 replacement = filenames,
                 x = file_txt)

writeLines(file_txt, con = here::here("cluster",
                                      "run_meta_resampled.sh"))
