
source("global.R")

# CASE STUDY with LOOCV: --------------------------------------------------

error_threshold <- 0.05
out_dt <- readRDS(file = here::here("outputs", "dt_loocv_case-study.rds"))

p.loocv <- out_dt |>
  ggplot2::ggplot(
    ggplot2::aes(x = x,
                 y = error,
                 group = type,
                 color = overlaps)) +
  
  ggplot2::geom_hline(
    yintercept = error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +
  ggplot2::geom_hline(
    yintercept = -error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +
  
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.3,
    linetype = "solid") +
  
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = error_lci,
                 ymax = error_uci),
    show.legend = TRUE,
    color = "black",
    linewidth = 0.5) +
  
  ggplot2::geom_point(
    aes(color = overlaps,
        fill = overlaps),
    color = "black",
    show.legend = TRUE,
    shape = 21,
    size = 3) +
  
  ggh4x::facet_grid2(
    vars(type), 
    vars(par),
    scales = "free",
    space = "free",
    independent = "y",
    labeller = labeller(par = option_labels,
                        type = method_labels)) +
  
  ggplot2::labs(
    x = "LOOCV iteration",
    y = "Relative error (%)") +
  
  ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
  ggplot2::scale_y_continuous(labels = scales::percent,
                              breaks = breaks_pretty()) +
  ggplot2::scale_color_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    values = pal_values, drop = FALSE) +
  ggplot2::scale_fill_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    values = pal_values, drop = FALSE) +
  .theme()
p.loocv

ggplot2::ggsave(
  p.loocv, file = here::here(
    "figures", "supplementary", "case-study_loocv.png"),
  bg = "white",
  height = 6, width = 8, 
  dpi = 300, device = "png")

out_dt <- .prepare_sampling(out_dt, both_pars = TRUE)

p.loocv.summ <- out_dt |>
  dplyr::group_by(par) |>
  dplyr::summarize(
    error_mean = mean(error, na.rm = TRUE),
    error_lci = calculate_ci(error, level = 0.95)$CI_low,
    error_uci = calculate_ci(error, level = 0.95)$CI_high) |>
  dplyr::mutate(overlaps = dplyr::between(
    error_mean, -error_threshold, error_threshold)) |>
  dplyr::mutate(overlaps = factor(overlaps,
                                  levels = c("TRUE", "FALSE"))) |>

  ggplot2::ggplot(
    ggplot2::aes(x = par,
                 y = error_mean,
                 group = par,
                 color = overlaps)) +

  ggplot2::geom_hline(
    yintercept = error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +
  ggplot2::geom_hline(
    yintercept = -error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +

  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.3,
    linetype = "solid") +

  ggplot2::geom_linerange(
    ggplot2::aes(ymin = error_lci,
                 ymax = error_uci),
    show.legend = TRUE,
    color = "black",
    linewidth = 0.5) +

  ggplot2::geom_point(
    aes(color = overlaps,
        fill = overlaps),
    color = "black",
    show.legend = TRUE,
    shape = 21,
    size = 3) +

  ggplot2::labs(
    x = "LOOCV iteration",
    y = "Relative error (%)") +

  ggplot2::scale_y_continuous(labels = scales::percent,
                              breaks = breaks_pretty()) +
  ggplot2::scale_color_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    values = pal_values, drop = FALSE) +
  ggplot2::scale_fill_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    values = pal_values, drop = FALSE) +
  .theme()
p.loocv.summ

p.loocv.summ <- out_dt |>
  dplyr::group_by(par) |>
  dplyr::summarize(
    error_mean = mean(error, na.rm = TRUE),
    error_lci = calculate_ci(error, level = 0.95)$CI_low,
    error_uci = calculate_ci(error, level = 0.95)$CI_high) |>
  dplyr::mutate(overlaps = dplyr::between(
    error_mean, -error_threshold, error_threshold)) |>
  dplyr::mutate(overlaps = factor(overlaps,
                                  levels = c("TRUE", "FALSE"))) |>

  ggplot2::ggplot(
    ggplot2::aes(x = error_mean,
                 y = par,
                 group = par,
                 color = overlaps)) +

  ggplot2::geom_vline(
    xintercept = error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +
  ggplot2::geom_vline(
    xintercept = -error_threshold,
    alpha = 0.5, linetype = "dotted", linewidth = 0.6) +

  ggplot2::geom_vline(
    xintercept = 0,
    linewidth = 0.3,
    linetype = "solid") +

  ggplot2::geom_linerange(
    ggplot2::aes(xmin = error_lci,
                 xmax = error_uci),
    show.legend = TRUE,
    color = "black",
    linewidth = 0.5) +

  ggplot2::geom_point(
    aes(color = overlaps,
        fill = overlaps),
    color = "black",
    show.legend = TRUE,
    shape = 21,
    size = 3) +

  ggplot2::labs(
    x = "Relative error (%)",
    y = "") +

  ggplot2::scale_x_continuous(labels = scales::percent,
                              breaks = breaks_pretty()) +
  ggplot2::scale_color_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    values = pal_values, drop = FALSE) +
  ggplot2::scale_fill_manual(
    name = paste0("Within error threshold (\u00B1",
                  error_threshold * 100, "%)?"),
    values = pal_values, drop = FALSE) +
  .theme()
p.loocv.summ
