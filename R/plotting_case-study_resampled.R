
source("global.R")

# CASE STUDY with RESAMPLING: ---------------------------------------------

filenames <- c("buffalo_hr_dur1year_dti15minutes_8inds",
               "buffalo_hr_dur6years_dti2hours_8inds",
               "buffalo_hr_dur4years_dti1day_20inds")
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_hr <- .combine_datasets(filenames,
                            both_pars = TRUE,
                            resampled = TRUE)

filenames <- c("buffalo_ctsd_dti15minutes_dur1year_8inds",
               "buffalo_ctsd_dti2hours_dur6years_8inds",
               "buffalo_ctsd_dti1day_dur4years_20inds")
filenames <- paste0(filenames, "_", max_samples, "samples")
filenames <- paste0(filenames, "_", iter_step, "steps")

dat_sd <- .combine_datasets(filenames,
                            both_pars = TRUE,
                            resampled = TRUE)

dat <- dplyr::full_join(dat_hr, dat_sd) |> 
  dplyr::mutate(
    dplyr::across(type, ~factor(., levels = c("hr","ctsd")))) |> 
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat <- estimate_neighbors(dat, 
                          x_col = "error",
                          y_col = "m",
                          adjust = 0.5)

dat_one <- dplyr::filter(dat, sample == 1)
dat_true <- dat |> dplyr::filter(overlaps == TRUE)
dat_false <- dat |> dplyr::filter(overlaps == FALSE)

dat_summarized <- rbind(dat_hr, dat_sd) |> 
  dplyr::mutate(
    dplyr::across(
      type, ~factor(., levels = c("hr", "ctsd")))) |> 
  dplyr::select(species, type, par,  m,
                est, lci, uci, truth,
                error, error_lci, error_uci) |> 
  dplyr::distinct() |>
  dplyr::group_by(species, type, par, m) |> 
  summarize_everything(alpha = alpha,
                       error_threshold = error_threshold) |> 
  dplyr::mutate(
    is_top_row = ifelse(type == "hr", TRUE, FALSE)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    error_threshold = dplyr::case_when(
      type == "hr" ~ 0.05,
      type == "ctsd" ~ 0.05,
      TRUE ~ NA)) |> 
  dplyr::mutate(overlaps = dplyr::between(
    error_mean, -error_threshold, error_threshold)) |>
  dplyr::mutate(overlaps = factor(
    overlaps, levels = c("TRUE", "FALSE")))

dat_mean <- dat |>
  dplyr::group_by(species, type, par) |>
  dplyr::slice_max(m) |> 
  dplyr::group_by(species, type, par, m) |>
  dplyr::summarise(
    error_mean = mean(error, na.rm = TRUE),
    error_lci = calculate_ci(error, level = ci)$CI_low,
    error_uci = calculate_ci(error, level = ci)$CI_high,
    error_hdi_lci = bayestestR::ci(error, "HDI", ci = cri)$CI_low,
    error_hdi_uci = bayestestR::ci(error, "HDI", ci = cri)$CI_high) |> 
  dplyr::mutate(
    x_pos = m, y_pos = error_lci,
    color = ifelse(abs(error_mean) < error_threshold, pal$sea, pal$dgr))

p <- dat_summarized |>
  ggplot2::ggplot(
    ggplot2::aes(x = m,
                 y = error_mean,
                 color = overlaps,
                 fill = overlaps)) +
  
  ggh4x::facet_grid2(
    ggplot2::vars(type),
    ggplot2::vars(par),
    scales = "free",
    space = "free",
    independent = "y",
    labeller = ggplot2::labeller(
      par = option_labels,
      type = method_labels)) +
  
  ggplot2::geom_jitter(
    dat_false,
    mapping = ggplot2::aes(x = m,
                           y = error,
                           group = sample),
    position = ggplot2::position_dodge(width = 0.3),
    size = 1, shape = 21,
    color = pal_values_light[["FALSE"]],
    fill = pal_values_light[["FALSE"]]
  ) +
  
  ggplot2::geom_jitter(
    data = dat_true,
    mapping = ggplot2::aes(x = m,
                           y = error,
                           group = sample),
    position = ggplot2::position_dodge(width = 0.3),
    size = 1, shape = 21,
    color = pal_values_light[["TRUE"]],
    fill = pal_values_light[["TRUE"]]
  ) +
  
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.3,
    linetype = "solid") +
  ggplot2::geom_hline(
    data = subset(dat_summarized, is_top_row), 
    ggplot2::aes(yintercept = error_threshold),
    linewidth = 0.7,
    linetype = "dotted") +
  ggplot2::geom_hline(
    data = subset(dat_summarized, is_top_row), 
    ggplot2::aes(yintercept = -error_threshold),
    linewidth = 0.7,
    linetype = "dotted") +
  
  ggplot2::geom_hline(
    data = subset(dat_summarized, !is_top_row), 
    ggplot2::aes(yintercept = error_threshold),
    linewidth = 0.7,
    linetype = "dotted") +
  ggplot2::geom_hline(
    data = subset(dat_summarized, !is_top_row), 
    ggplot2::aes(yintercept = -error_threshold),
    linewidth = 0.7,
    linetype = "dotted") +
  
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = error_mean_lci,
                 ymax = error_mean_uci),
    show.legend = TRUE,
    position = ggplot2::position_dodge(width = 0.4),
    color = "black", linewidth = 0.4) +
  ggplot2::geom_point(
    show.legend = TRUE,
    position = ggplot2::position_dodge(width = 0.4),
    color = "black", shape = 23, size = 2) +

  ggplot2::labs(
    x = "<i>Population</i> sample size, <i>m</i>",
    y = "Relative error (%)") +
  
  ggplot2::scale_fill_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    breaks = c("TRUE", "FALSE"),
    values = pal_values, drop = FALSE,
    guide = ggplot2::guide_legend(
      override.aes = list(color = pal_values,
                          fill = pal_values,
                          size = 3),
      order = 1,
      label.vjust = 0.4,
      theme = ggplot2::theme(
        legend.key.width = unit(2, "lines"),
        legend.key.height = unit(0.5, "lines")))) +
  
  ggplot2::scale_shape_manual(values = c(16, 16)) +
  
  ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
  ggplot2::scale_y_continuous(labels = scales::percent,
                              breaks = breaks_pretty()) +
  
  .theme(ft_size = 12) +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(
      size = 14, margin = ggplot2::margin(b = 5)),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    legend.title = ggtext::element_markdown(
      family = "Roboto Condensed SemiBold",
      size = 10, margin = ggplot2::margin(r = 6, b = 0))) +
  ggplot2::guides(shape = "none", color = "none")
p

ggplot2::ggsave(
  p, file = here::here(
    "figures", "supplementary", "case-study_resampled.png"),
  bg = "white",
  height = 6, width = 8, 
  dpi = 300, device = "png")
