
source("global.R")

error_threshold <- 0.05

# SIMULATIONS [META-ANALYSES] ---------------------------------------------
## Leave-one-out cross-validation (LOOCV): --------------------------------

filenames_hr <- c("buffalo_hr_dur2months_dti1hour_50inds",
                  "buffalo_hr_dur4months_dti1hour_50inds",
                  "buffalo_hr_dur1year_dti1hour_50inds",
                  "buffalo_hr_dur3years_dti1hour_50inds",
                  "buffalo_hr_dur9years_dti1hour_50inds",
                  "gazelle_hr_dur2months_dti1hour_50inds",
                  "gazelle_hr_dur4months_dti1hour_50inds",
                  "gazelle_hr_dur1year_dti1hour_50inds",
                  "gazelle_hr_dur3years_dti1hour_50inds",
                  "gazelle_hr_dur9years_dti1hour_50inds")
files_raw <- .read_in_files(filenames_hr)
loocv_hr <- run_loocv(files_raw, filenames_hr, set_target = "hr")

filenames_sd <- c("buffalo_ctsd_dti4hours_dur2months_50inds",
                  "buffalo_ctsd_dti3hours_dur2months_50inds",
                  "buffalo_ctsd_dti2hours_dur2months_50inds",
                  "buffalo_ctsd_dti1hour_dur2months_50inds",
                  "buffalo_ctsd_dti30minutes_dur2months_50inds",
                  "gazelle_ctsd_dti4hours_dur2months_50inds",
                  "gazelle_ctsd_dti3hours_dur2months_50inds",
                  "gazelle_ctsd_dti2hours_dur2months_50inds",
                  "gazelle_ctsd_dti1hour_dur2months_50inds",
                  "gazelle_ctsd_dti30minutes_dur2months_50inds")
files_raw <- .read_in_files(filenames_sd)
loocv_sd <- run_loocv(files_raw, filenames_sd, set_target = "ctsd")

out_dt <- rbind(
  do.call(rbind, loocv_hr) |>
    dplyr::mutate(
      est = "km^2" %#% est,
      lci = "km^2" %#% lci,
      uci = "km^2" %#% uci,
      truth = "km^2" %#% truth),
  do.call(rbind, loocv_sd) |>
    dplyr::mutate(
      est = "km/day" %#% est,
      lci = "km/day" %#% lci,
      uci = "km/day" %#% uci,
      truth = "km/day" %#% truth)) |>
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |>
  dplyr::mutate(par = as.factor(par)) |>
  dplyr::mutate(overlaps = dplyr::between(
    error, -error_threshold, error_threshold)) |>
  dplyr::mutate(overlaps = factor(
    overlaps, levels = c("TRUE", "FALSE")))

nms <- .extract_sampling(c(filenames_hr,
                           filenames_sd), both_pars = TRUE)
out_dt$par <- factor(out_dt$par, levels = unique(nms))

saveRDS(out_dt, file = here::here("outputs", "dt_loocv_sims.rds"))

## Calculate correctness rate: --------------------------------------------

out_dt <- readRDS(here::here("outputs", "dt_loocv_sims.rds"))
out_dt |>
  dplyr::group_by(species, type, par) |>
  dplyr::mutate(correct_count = abs(error) < error_threshold) |>
  dplyr::summarize(
    sd_loocv = sd(abs(error), na.rm = TRUE) * 100,
    mean_loocv = mean(abs(error), na.rm = TRUE) * 100,
    n = n(),
    correct = sum(correct_count),
    correctness_rate = correct / n)
