
source("global.R")

img_buffalo <- readPNG(
  here::here("documentation", "ext_images", "african_buffalo.png"))

img_gazelle <- readPNG(
  here::here("documentation", "ext_images", "mongolian_gazelle.png"))

# SIMULATION OUTPUTS with RESAMPLING: -------------------------------------

## HOME RANGE -------------------------------------------------------------
### Buffalo ("short" position autocorrelation) ----------------------------

p <- p.resampled <- p.metrics <- NULL
filenames <- c("buffalo_hr_dur2months_dti1hour_50inds",
               "buffalo_hr_dur4months_dti1hour_50inds",
               "buffalo_hr_dur1year_dti1hour_50inds",
               "buffalo_hr_dur3years_dti1hour_50inds",
               "buffalo_hr_dur9years_dti1hour_50inds")
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_buffalo <- .combine_datasets(filenames,
                                 resampled = TRUE) |>
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_buffalo_summarized <- dat_buffalo |>
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |>
  dplyr::select(species, type, dur,  m,
                est, lci, uci, truth,
                error, error_lci, error_uci) |>
  dplyr::distinct() |>
  dplyr::group_by(species, type, dur, m) |>
  summarize_everything(alpha = alpha,
                       error_threshold = error_threshold) |>
  dplyr::ungroup()

dat_buffalo_mean <- dat_buffalo |>
  dplyr::group_by(species, type, dur) |>
  dplyr::slice_max(m) |>
  dplyr::group_by(species, type, dur, m) |>
  dplyr::mutate(
    x_pos = m, y_pos = error_lci,
    color = ifelse(abs(error) < error_threshold,
                   pal$sea, pal$dgr))

( p <- plotting_meta(
  dat_buffalo,
  data_summ = dat_buffalo_summarized,
  data_mean = dat_buffalo_mean,
  error_threshold = error_threshold,
  facet_var = "dur",
  adjust_y = 0.01,
  nudge_y = -0.05,
  img = img_buffalo,
  both = TRUE) )

ggplot2::ggsave(
  p, file = here::here(
    "figures", "supplementary", "hr_buffalo.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

( p.resampled <- plotting_meta_resampled(
  dat_buffalo,
  data_summ = dat_buffalo_summarized,
  data_mean = dat_buffalo_mean,
  error_threshold = error_threshold,
  facet_var = "dur",
  adjust_y = 0.01,
  nudge_y = -0.05,
  img = img_buffalo,
  both = TRUE) )

ggplot2::ggsave(
  p.resampled, file = here::here(
    "figures", "hr_buffalo_resampled.png"),
  bg = "white",
  height = 8, width = 10, 
  dpi = 300, device = "png")

### Gazelle ("long" position autocorrelation) -----------------------------

p <- p.resampled <- p.metrics <- NULL
filenames <- c("gazelle_hr_dur2months_dti1hour_50inds",
               "gazelle_hr_dur4months_dti1hour_50inds",
               "gazelle_hr_dur1year_dti1hour_50inds",
               "gazelle_hr_dur3years_dti1hour_50inds",
               "gazelle_hr_dur9years_dti1hour_50inds")
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_gazelle <- .combine_datasets(filenames,
                                 resampled = TRUE) |>
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_gazelle_summarized <- dat_gazelle |>
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |>
  dplyr::select(species, type, dur,  m,
                est, lci, uci, truth,
                error, error_lci, error_uci) |>
  dplyr::distinct() |>
  dplyr::group_by(species, type, dur, m) |>
  summarize_everything(alpha = alpha,
                       error_threshold = error_threshold) |>
  dplyr::ungroup()

dat_gazelle_mean <- dat_gazelle |>
  dplyr::group_by(species, type, dur) |>
  dplyr::slice_max(m) |>
  dplyr::group_by(species, type, dur, m) |>
  dplyr::mutate(
    x_pos = m, y_pos = error_lci,
    color = ifelse(abs(error) < error_threshold,
                   pal$sea, pal$dgr))

( p <- plotting_meta(
  dat_gazelle,
  dat_gazelle_summarized,
  data_mean = dat_gazelle_mean,
  error_threshold = error_threshold,
  facet_var = "dur",
  adjust_y = 0.01,
  nudge_y = -0.08,
  img = img_gazelle) )

ggplot2::ggsave(
  p, file = here::here(
    "figures", "supplementary", "hr_gazelle.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

( p.resampled <- plotting_meta_resampled(
  dat_gazelle,
  dat_gazelle_summarized,
  error_threshold = error_threshold,
  facet_var = "dur",
  img = img_gazelle) )

ggplot2::ggsave(
  p.resampled, file = here::here(
    "figures", "hr_gazelle_resampled.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

## SPEED & DISTANCE -------------------------------------------------------

### Buffalo ("short" velocity autocorrelation) ----------------------------

p <- p.resampled <- p.metrics <- NULL
filenames <- c("buffalo_ctsd_dti4hours_dur2months_50inds",
               "buffalo_ctsd_dti3hours_dur2months_50inds",
               "buffalo_ctsd_dti2hours_dur2months_50inds",
               "buffalo_ctsd_dti1hour_dur2months_50inds",
               "buffalo_ctsd_dti30minutes_dur2months_50inds")
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_buffalo <- .combine_datasets(filenames,
                                 resampled = TRUE) |>
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_buffalo <- estimate_neighbors(dat_buffalo, 
                                  x_col = "error",
                                  y_col = "m",
                                  adjust = 0.5)

dat_buffalo_summarized <- dat_buffalo |>
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |>
  dplyr::select(species, type, dti, m,
                est, lci, uci, truth,
                error, error_lci, error_uci) |>
  dplyr::distinct() |>
  dplyr::group_by(species, type, dti, m) |>
  summarize_everything(alpha = alpha,
                       error_threshold = error_threshold) |>
  dplyr::ungroup()

dat_buffalo_mean <- dat_buffalo |>
  dplyr::group_by(species, type, dti) |>
  dplyr::slice_max(m) |>
  dplyr::group_by(species, type, dti, m) |>
  dplyr::mutate(
    x_pos = m, y_pos = error_lci,
    color = ifelse(abs(error) < error_threshold,
                   pal$sea, pal$dgr))

( p <- plotting_meta(
  dat_buffalo,
  data_summ = dat_buffalo_summarized,
  data_mean = dat_buffalo_mean,
  error_threshold = error_threshold,
  facet_var = "dti",
  adjust_y = 0.003,
  nudge_y = -0.02,
  img = img_buffalo,
  both = TRUE) )

ggplot2::ggsave(
  p, file = here::here(
    "figures", "supplementary", "ctsd_buffalo.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

( p.resampled <- plotting_meta_resampled(
  dat_buffalo,
  dat_buffalo_summarized,
  data_mean = dat_buffalo_mean,
  error_threshold = error_threshold,
  facet_var = "dti",
  img = img_buffalo) )

ggplot2::ggsave(
  p.resampled, file = here::here(
    "figures", "ctsd_buffalo_resampled.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

### Gazelle ("long" velocity autocorrelation) -----------------------------

p.resampled <- NULL
filenames <- c("gazelle_ctsd_dti4hours_dur2months_50inds",
               "gazelle_ctsd_dti3hours_dur2months_50inds",
               "gazelle_ctsd_dti2hours_dur2months_50inds",
               "gazelle_ctsd_dti1hour_dur2months_50inds",
               "gazelle_ctsd_dti30minutes_dur2months_50inds")
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_gazelle <- .combine_datasets(filenames,
                                 resampled = TRUE) |>
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_gazelle_summarized <- dat_gazelle |>
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |>
  dplyr::select(species, type, dti,  m,
                est, lci, uci, truth,
                error, error_lci, error_uci) |>
  dplyr::distinct() |>
  dplyr::group_by(species, type, dti, m) |>
  summarize_everything(alpha = alpha,
                       error_threshold = error_threshold) |>
  dplyr::ungroup()

dat_gazelle_mean <- dat_gazelle |>
  dplyr::group_by(species, type, dti) |>
  dplyr::slice_max(m) |>
  dplyr::group_by(species, type, dti, m) |>
  dplyr::mutate(
    x_pos = m, y_pos = error_lci,
    color = ifelse(abs(error) < error_threshold,
                   pal$sea, pal$dgr))

( p <- plotting_meta(
  dat_gazelle,
  dat_gazelle_summarized,
  data_mean = dat_gazelle_mean,
  error_threshold = error_threshold,
  facet_var = "dti",
  adjust_y = 0.001,
  nudge_y = -0.015,
  img = img_gazelle) )

ggplot2::ggsave(
  p, file = here::here(
    "figures", "supplementary", "ctsd_gazelle.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

( p.resampled <- plotting_meta_resampled(
  dat_gazelle,
  dat_gazelle_summarized,
  data_mean = dat_gazelle_mean,
  error_threshold = error_threshold,
  facet_var = "dti",
  img = img_gazelle) )

ggplot2::ggsave(
  p.resampled, file = here::here(
    "figures", "ctsd_gazelle_resampled.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

