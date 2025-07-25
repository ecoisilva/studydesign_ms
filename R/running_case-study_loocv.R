
source("global.R")

error_threshold <- 0.05

# CASE STUDY [META-ANALYSES] ----------------------------------------------
## Leave-one-out cross-validation (LOOCV): --------------------------------

filenames <- c("buffalo_hr_dur1year_dti15minutes_8inds",
               "buffalo_hr_dur6years_dti2hours_8inds",
               "buffalo_hr_dur4years_dti1day_20inds")
files_raw <- .read_in_files(filenames)
loocv_hr <- run_loocv(files_raw, filenames, set_target = "hr")

filenames <- c("buffalo_ctsd_dti15minutes_dur1year_8inds",
               "buffalo_ctsd_dti2hours_dur6years_8inds",
               "buffalo_ctsd_dti1day_dur4years_20inds")
files_raw <- .read_in_files(filenames)
loocv_sd <- run_loocv(files_raw, filenames, set_target = "ctsd")

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
      truth = "km/day" %#% truth)
) |>
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |>
  dplyr::mutate(
    dplyr::across(
      par, ~factor(., levels = c(
        "1 year 15 minutes",
        "6 years 2 hours",
        "4 years 1 day")))) |>
  dplyr::mutate(overlaps = dplyr::between(
    error, -error_threshold, error_threshold)) |>
  dplyr::mutate(overlaps = factor(
    overlaps, levels = c("TRUE", "FALSE"))) |>
  dplyr::distinct()

saveRDS(out_dt, file = here::here("outputs", "dt_loocv_case-study.rds"))

## Calculate correctness rate: --------------------------------------------

out_dt <- readRDS(here::here("outputs", "dt_loocv_case-study.rds"))

out_dt |>
  dplyr::group_by(type, par) |>
  dplyr::mutate(correct_count = abs(error) < error_threshold) |>
  dplyr::summarize(
    sd_loocv = sd(error) * 100,
    mean_loocv = mean(abs(error - 0)) * 100,
    n = n(),
    correct = sum(correct_count),
    correctness_rate = correct / n)
