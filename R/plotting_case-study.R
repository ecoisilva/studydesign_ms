
source("global.R")

# CASE STUDY --------------------------------------------------------------

error_threshold <- 0.05

filenames <- c("buffalo_hr_dur1year_dti15minutes_8inds",
               "buffalo_hr_dur6years_dti2hours_8inds",
               "buffalo_hr_dur4years_dti1day_20inds")
files_raw <- .read_in_files(filenames)
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

nudge_y <- c(-0.05, -0.05, -0.05)
adjust_y <- 0.01

dat_hr <- .combine_datasets(filenames,
                            both_pars = TRUE,
                            resampled = TRUE)

filenames <- c("buffalo_ctsd_dti15minutes_dur1year_8inds",
               "buffalo_ctsd_dti2hours_dur6years_8inds",
               "buffalo_ctsd_dti1day_dur4years_20inds")
files_raw <- .read_in_files(filenames)
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_sd <- .combine_datasets(filenames,
                            both_pars = TRUE,
                            resampled = TRUE)

dat <- dplyr::full_join(dat_hr, dat_sd) |> 
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |> 
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_mean_simplified <- dat |>
  dplyr::group_by(species, type, par) |> 
  dplyr::slice_max(m) |> 
  dplyr::mutate(
    par = factor(par, levels = c("4 years 1 day",
                                 "6 years 2 hours",
                                 "1 year 15 minutes")))

p <- dat_mean_simplified |>
  ggplot2::ggplot(
    ggplot2::aes(x = error,
                 y = par)) +
  
  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.2,
    linetype = "solid") +
  ggplot2::geom_vline(
    xintercept = error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +
  ggplot2::geom_vline(
    xintercept = -error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +
  
  ggplot2::geom_linerange(
    ggplot2::aes(xmin = error_lci,
                 xmax = error_uci),
    show.legend = TRUE,
    color = "black",
    linewidth = 0.5) +
  ggplot2::geom_line(
    show.legend = TRUE,
    linewidth = 0.6) +
  ggplot2::geom_point(
    aes(color = overlaps,
        fill = overlaps),
    color = "black",
    show.legend = TRUE,
    shape = 21,
    size = 3) +
  
  ggh4x::facet_grid2(
    col = vars(type),
    scales = "free",
    space = "free",
    labeller = ggplot2::labeller(type = method_labels)) +
  
  ggplot2::labs(
    x = "Relative error (%)",
    y = "",
    color = paste0("Within error threshold (\u00B1",
                   error_threshold * 100, "%)?"),
    fill = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    shape = "Methods:") +
  
  ggplot2::scale_x_continuous(labels = scales::percent,
                              breaks = breaks_pretty()) +
  ggplot2::scale_y_discrete(labels = option_labels) +
  ggplot2::scale_fill_manual(
    values = pal_values, drop = FALSE,
    guide = ggplot2::guide_legend(
      override.aes = list(fill = pal_values))) +
  .theme()
p

ggplot2::ggsave(
  p, file = here::here("figures", "supplementary", "case-study.png"),
  bg = "white",
  height = 3, width = 9, 
  dpi = 300, device = "png")
