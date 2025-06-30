
source("global.R")

# SIMULATIONS OUTPUTS: ----------------------------------------------------
## HOME RANGE -------------------------------------------------------------

error_threshold <- 0.05

### Buffalo ("short" position autocorrelation) ----------------------------

filenames <- c("buffalo_hr_dur2months_dti1hour_50inds",
               "buffalo_hr_dur4months_dti1hour_50inds",
               "buffalo_hr_dur1year_dti1hour_50inds",
               "buffalo_hr_dur3years_dti1hour_50inds",
               "buffalo_hr_dur9years_dti1hour_50inds")

dat_buffalo <- .combine_datasets(filenames) |> 
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_buffalo_mean <- dat_buffalo |>
  dplyr::group_by(dur) |>
  dplyr::slice_max(m) |> 
  dplyr::summarise(
    error_mean = error,
    x_pos = m,
    y_pos = error_lci,
    color = ifelse(abs(error_mean) < error_threshold, pal$sea, pal$dgr))

( p1 <- dat_buffalo |>
    ggplot2::ggplot(
      ggplot2::aes(x = m,
                   y = error,
                   group = dur,
                   color = overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = error_threshold,
      alpha = 0.5,
      linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      alpha = 0.5,
      linetype = "dotted", linewidth = 0.4) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 2.5, alpha = .2) +
    ggplot2::geom_line(
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 0.6, alpha = 0.5) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.8),
      size = 1.7) +
    
    ggplot2::facet_wrap(
      . ~ factor(dur, levels = .extract_sampling(filenames)),
      nrow = 1) +
    
    ggrepel::geom_text_repel(
      data = dat_buffalo_mean,
      mapping = ggplot2::aes(
        x = x_pos,
        y = y_pos - 0.01,
        label = paste0(round(error_mean * 100, 1), "%")),
      color = dat_buffalo_mean$color,
      # box.padding = 0.01,
      # point.padding = 0,
      # force = 1,
      nudge_y = -0.08,
      # hjust = -1,
      arrow = ggplot2::arrow(
        angle = 25, length = unit(0.20, "cm"), type = "closed"),
      min.segment.length = 0,
      family = "Roboto Condensed", size = 4) +
    
    # rphylopic::geom_phylopic(
    #   data = data.frame(x = 20, y = -0.5, 
    #                     group = "9 years", dur = "9 years"),
    #   img = img_buffalo,
    #   aes(x = x, y = y),
    #   height = 0.15,
    #   colour = NA, fill = "grey80",
    #   show.legend = FALSE) +
    
    ggplot2::labs(
      title = "Sampling duration",
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?")) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    ggplot2::scale_color_manual(values = pal_values, drop = FALSE) +
    set_theme() )

ggplot2::ggsave(
  p1, file = here::here(
    "figures", "preliminary", "hr_error_buffalo.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

### Gazelle ("long" position autocorrelation) -----------------------------

filenames <- c("gazelle_hr_dur2months_dti1hour_50inds",
               "gazelle_hr_dur4months_dti1hour_50inds",
               "gazelle_hr_dur1year_dti1hour_50inds",
               "gazelle_hr_dur3years_dti1hour_50inds",
               "gazelle_hr_dur9years_dti1hour_50inds")

dat_gazelle <- .combine_datasets(filenames) |> 
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_gazelle_mean <- dat_gazelle |>
  dplyr::group_by(dur) |>
  dplyr::slice_max(m) |> 
  dplyr::summarise(
    error_mean = error,
    x_pos = m,
    y_pos = error_lci,
    color = ifelse(abs(error_mean) < error_threshold, pal$sea, pal$dgr))

( p2 <- dat_gazelle |>
    ggplot2::ggplot(
      ggplot2::aes(x = m,
                   y = error,
                   group = dur,
                   color = overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = error_threshold,
      alpha = 0.5,
      linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      alpha = 0.5,
      linetype = "dotted", linewidth = 0.4) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 2.5, alpha = .2) +
    ggplot2::geom_line(
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 0.6, alpha = 0.5) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.8),
      size = 1.7) +
    
    ggplot2::facet_grid(. ~ dur) +
    
    ggrepel::geom_text_repel(
      data = dat_gazelle_mean,
      mapping = ggplot2::aes(
        x = x_pos,
        y = y_pos - 0.01,
        label = paste0(round(error_mean * 100, 1), "%")),
      color = dat_gazelle_mean$color,
      nudge_y = c(-0.12, -0.12, -0.2, -0.15, -0.13),
      arrow = ggplot2::arrow(
        angle = 25, length = unit(0.20, "cm"), type = "closed"),
      min.segment.length = 0,
      family = "Roboto Condensed", size = 4) +
    
    ggplot2::labs(
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?")) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    ggplot2::scale_color_manual(values = pal_values, drop = FALSE) +
    set_theme() )

ggplot2::ggsave(
  p2, file = here::here(
    "figures", "preliminary", "hr_error_gazelle.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

## SPEED & DISTANCE -------------------------------------------------------

error_threshold <- 0.01

### Buffalo ("short" velocity autocorrelation) ----------------------------

filenames <- c("buffalo_ctsd_dti4hours_dur2months_50inds",
               "buffalo_ctsd_dti3hours_dur2months_50inds",
               "buffalo_ctsd_dti2hours_dur2months_50inds",
               "buffalo_ctsd_dti1hour_dur2months_50inds",
               "buffalo_ctsd_dti30minutes_dur2months_50inds")

dat_buffalo <- .combine_datasets(filenames) |> 
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_buffalo_mean <- dat_buffalo |>
  dplyr::group_by(dti) |>
  dplyr::slice_max(m) |> 
  dplyr::summarise(
    error_mean = error,
    x_pos = m,
    y_pos = error_lci,
    color = ifelse(abs(error_mean) < error_threshold, pal$sea, pal$dgr))

( p3 <- dat_buffalo |>
    ggplot2::ggplot(
      ggplot2::aes(x = m,
                   y = error,
                   group = dti,
                   color = overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = error_threshold,
      alpha = 0.5, linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      alpha = 0.5, linetype = "dotted", linewidth = 0.4) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3, linetype = "solid") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 2.5, alpha = .2) +
    ggplot2::geom_line(
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 0.6, alpha = 0.5) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.8),
      size = 1.7) +
    
    ggplot2::facet_wrap(
      . ~ factor(dti, levels = .extract_sampling(filenames)),
      nrow = 1) +
    
    ggrepel::geom_text_repel(
      data = dat_buffalo_mean,
      mapping = ggplot2::aes(
        x = x_pos,
        y = y_pos - 0.001,
        label = paste0(round(error_mean * 100, 1), "%")),
      color = dat_buffalo_mean$color,
      nudge_y = c(-0.06, -0.06, -0.05, -0.05, -0.05),
      arrow = ggplot2::arrow(
        angle = 25, length = unit(0.20, "cm"), type = "closed"),
      min.segment.length = 0,
      family = "Roboto Condensed", size = 4) +
    
    ggplot2::labs(
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?")) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    ggplot2::scale_color_manual(values = pal_values, drop = FALSE) +
    set_theme() )

ggplot2::ggsave(
  p3, file = here::here(
    "figures", "preliminary", "ctsd_error_buffalo.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

### Gazelle (long velocity autocorrelation) -------------------------------

filenames <- c("gazelle_ctsd_dti4hours_dur3months_50inds",
               "gazelle_ctsd_dti3hours_dur3months_50inds",
               "gazelle_ctsd_dti2hours_dur3months_50inds",
               "gazelle_ctsd_dti1hour_dur3months_50inds",
               "gazelle_ctsd_dti30minutes_dur3months_50inds")

dat_gazelle <- .combine_datasets(filenames) |> 
  dplyr::mutate(
    overlaps = dplyr::between(
      error, -error_threshold, error_threshold),
    overlaps = factor(overlaps, levels = c("TRUE", "FALSE")))

dat_gazelle_mean <- dat_gazelle |>
  dplyr::group_by(dti) |>
  dplyr::slice_max(m) |> 
  dplyr::summarise(
    error_mean = error,
    x_pos = m,
    y_pos = error_lci,
    color = ifelse(abs(error_mean) < error_threshold, pal$sea, pal$dgr))

( p4 <- dat_gazelle |>
    ggplot2::ggplot(
      ggplot2::aes(x = m,
                   y = error,
                   group = dti,
                   color = overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = error_threshold,
      alpha = 0.5,
      linetype = "dotted", linewidth = 0.4) +
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      alpha = 0.5,
      linetype = "dotted", linewidth = 0.4) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 2.75, alpha = .2) +
    ggplot2::geom_line(
      position = ggplot2::position_dodge(width = 0.8),
      linewidth = 0.6, alpha = 0.5) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.8),
      size = 1.7) +
    
    ggplot2::facet_wrap(
      . ~ factor(dti, levels = .extract_sampling(filenames)),
      nrow = 1) +
    
    ggrepel::geom_text_repel(
      data = dat_gazelle_mean,
      mapping = ggplot2::aes(
        x = x_pos,
        y = y_pos - 0.002,
        label = paste0(round(error_mean * 100, 1), "%")),
      color = dat_gazelle_mean$color,
      nudge_y = c(-0.02, -0.022, -0.017, -0.017, -0.017),
      arrow = ggplot2::arrow(
        angle = 25, length = unit(0.20, "cm"), type = "closed"),
      min.segment.length = 0,
      family = "Roboto Condensed", size = 4) +
    
    ggplot2::labs(
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?")) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    ggplot2::scale_color_manual(values = pal_values, drop = FALSE) +
    set_theme() )

ggplot2::ggsave(
  p4, file = here::here(
    "figures", "preliminary", "ctsd_error_gazelle.png"),
  bg = "white",
  height = 4.5, width = 12, 
  dpi = 300, device = "png")
