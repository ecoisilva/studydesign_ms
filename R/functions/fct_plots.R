
estimate_neighbors <- function(data,
                               method = "default",
                               x_col, y_col, 
                               adjust = 1, grid_size = 100) {
  
  if (method == "default") {
    
    # find an appropriate bandwidth (radius), pretty ad-hoc:
    xrange <- diff(range(data[[x_col]])) * adjust
    yrange <- diff(range(data[[y_col]])) * adjust
    r2 <- (xrange + yrange) / 70
    # message("r2: ", r2)
    
    # since x and y may be on different scales, we need a
    # factor to weight x and y distances accordingly:
    xy <- xrange / yrange
    
    # counting the number of neighbors around each point,
    # this will be used to color the points
    data$density <- NA
    data$density <- ggpointdensity:::count_neighbors(
      data[[x_col]], data[[y_col]], r2 = r2, xy = xy)
  }
  
  if (method == "kde2d") {
    
    finites <- is.finite(data[[x_col]]) & is.finite(data[[y_col]])
    ddata <- data[finites, ]
    
    h <- c(MASS::bandwidth.nrd(ddata[[x_col]]),
           MASS::bandwidth.nrd(ddata[[y_col]]))
    h <- pmax(sqrt(.Machine$double.eps), h)
    h <- h * adjust
    
    dens <- MASS::kde2d(ddata[[x_col]], ddata[[y_col]],
                        h = h, n = grid_size)
    
    ix <- findInterval(data[[x_col]], dens$x)
    iy <- findInterval(data[[y_col]], dens$y)
    ii <- cbind(ix, iy)
    
    data$density <- NA
    data$density[finites] <- dens$z[ii]
    data$density[!finites] <- min(dens$z, na.rm = TRUE)
    # data$n_neighbors <- data$density/max(data$density)
  }
  
  return(data)
}

center.position <- function(plot) {
  built <- ggplot_build(plot)
  
  x_range <- built$layout$panel_scales_x[[
    length(built$layout$panel_scales_x)]]$range$range
  y_range <- built$layout$panel_scales_y[[1]]$range$range
  
  xpos <- mean(x_range)
  ypos <- mean(y_range)
  
  return(data.frame(x = xpos, y = ypos, 
                    x_min = x_range[1],
                    x_max = x_range[2], 
                    y_min = y_range[1],
                    y_max = y_range[2]))
}

plotting_meta <- function(data,
                          data_summ,
                          data_mean,
                          error_threshold,
                          facet_var,
                          adjust_y,
                          nudge_y,
                          both = FALSE,
                          img = NULL) {
  
  if (facet_var == "dur") txt_title <- "Sampling duration"
  if (facet_var == "dti") txt_title <- "Sampling interval"
  
  facet_formula <- as.formula(paste(". ~", facet_var))
  
  facet_labels <- c(
    "resampled" = paste0("<b>Resampling</b>"),
    "original" = paste0(
      "<span style='color: #fa6b5a'><b>No</b></span> ",
      "<b>resampling</b>"))
  
  data_one <- dplyr::filter(data, sample == 1) |>
    dplyr::filter(m %% 2 == 0) # even rows only
  
  p <- data_one |>
    
    ggplot2::ggplot(
      ggplot2::aes(x = m,
                   y = error,
                   group = !!rlang::sym(facet_var),
                   shape = type,
                   color = overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::geom_hline(
      yintercept = error_threshold,
      linewidth = 0.4,
      linetype = "dotted") + 
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      linewidth = 0.4,
      linetype = "dotted") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      show.legend = TRUE,
      # position = ggplot2::position_dodge(width = 0.4),
      linewidth = 1.2, alpha = 0.3) +
    
    ggplot2::geom_point(
      # position = ggplot2::position_dodge(width = 0.4),
      show.legend = TRUE,
      size = 1.7) +
    
    ggplot2::facet_grid(facet_formula,
                        scales = "free_x",
                        space = "free_x",
                        labeller = ggplot2::labeller(
                          facet = facet_labels)) +
    
    ggrepel::geom_text_repel(
      data = data_mean,
      mapping = ggplot2::aes(
        x = m,
        y = y_pos - adjust_y,
        label = paste0(round(error * 100, 1), "%")
      ),
      # box.padding = 0.03,
      # point.padding = 0,
      # force = 1,
      hjust = -1,
      nudge_y = nudge_y,
      arrow = ggplot2::arrow(angle = 25,
                             length = unit(0.20, "cm"),
                             type = "closed"),
      min.segment.length = 0,
      color = data_mean$color,
      family = "Roboto Condensed",
      size = 4
    ) +
    
    ggplot2::labs(
      title = txt_title,
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?"),
      caption = paste0(
        "<span style='color: #595959'>Points and vertical lines",
        " represent a </span>",
        "<span style='color: #000000'>single </span>",
        "<span style='color: #595959'>random resample.</span>")
    ) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    ggplot2::scale_color_manual(values = pal_values,
                                drop = FALSE) +
    set_theme() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5)),
      plot.margin = unit(c(0, 0, 0.2, 0), "cm")) +
    ggplot2::guides(color = "none", shape = "none")
  
  if (!is.null(img)) {
    
    p <- p + patchwork::inset_element(
      p = grid::rasterGrob(img, interpolate = TRUE),
      top = 0.95,
      right = 0.999,
      bottom = 0.85,
      left = 0.92)
    
  }
  
  return(p)
}

plotting_meta_resampled <- function(data,
                                    data_summ,
                                    data_mean = NULL,
                                    error_threshold,
                                    facet_var,
                                    adjust_y = NULL,
                                    nudge_y = NULL,
                                    both = FALSE,
                                    img = NULL) {
  
  if (facet_var == "dur") txt_title <- "Sampling duration"
  if (facet_var == "dti") txt_title <- "Sampling interval"
  
  if (both) facet_formula <- as.formula(paste("facet ~", facet_var))
  else facet_formula <- as.formula(paste(". ~", facet_var))
  
  within_threshold <- data |>
    dplyr::group_by(type, !!rlang::sym(facet_var)) |>
    dplyr::summarise(
      m = max(data$m),
      perc = mean(abs(error) <= error_threshold) * 100)
  
  outside_threshold <- data |>
    dplyr::group_by(type, !!rlang::sym(facet_var)) |>
    dplyr::summarise(
      m = max(data$m),
      perc = mean(abs(error) > error_threshold) * 100)
  
  error_within_threshold <- ifelse(
    within_threshold$perc < 0.1,
    "< 0.01%",
    paste0(round(within_threshold$perc, 1), "%"))
  
  error_outside_threshold <- ifelse(
    outside_threshold$perc < 0.1,
    "< 0.01%",
    paste0(round(outside_threshold$perc, 1), "%"))
  
  data_one <- dplyr::filter(data, sample == 1) |>
    dplyr::filter(m %% 2 == 0)
  data_one$facet <- "original"
  data_summ$facet <- "resampled"
  
  data_true <- data |>dplyr::filter(overlaps == TRUE)
  data_false <- data |>dplyr::filter(overlaps == FALSE)
  
  facet_labels <- c(
    "resampled" = paste0("<b>Resampling</b>"),
    "original" = paste0(
      "<span style='color: #dd4b39'><b>No</b></span> ",
      "<b>resampling</b>"))
  
  p <- data_one |>
    dplyr::filter(m %% 2 == 0) |> # even rows only
    
    ggplot2::ggplot(
      ggplot2::aes(x = m,
                   y = error,
                   group = !!rlang::sym(facet_var),
                   shape = type,
                   color = overlaps)) +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      show.legend = TRUE,
      position = ggplot2::position_dodge(width = 0.4),
      linewidth = 1.2, alpha = 0.3) +
    
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4),
      show.legend = TRUE,
      size = 1.7) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::geom_hline(
      yintercept = error_threshold,
      linewidth = 0.4,
      linetype = "dotted") + 
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      linewidth = 0.4,
      linetype = "dotted") +
    
    ggplot2::facet_grid(facet_formula,
                        scales = "free_x",
                        space = "free_x",
                        labeller = ggplot2::labeller(
                          facet = facet_labels)) +
    
    ggrepel::geom_text_repel(
      data = data_mean,
      mapping = ggplot2::aes(
        x = m,
        y = y_pos - adjust_y,
        label = paste0(round(error * 100, 1), "%")
      ),
      hjust = -1,
      nudge_y = nudge_y,
      arrow = ggplot2::arrow(angle = 25,
                             length = unit(0.20, "cm"),
                             type = "closed"),
      min.segment.length = 0,
      color = data_mean$color,
      family = "Roboto Condensed",
      size = 4
    ) +
    
    ggplot2::labs(
      title = txt_title,
      x = NULL,
      y = "Relative error (%)",
      color = paste0("Within error threshold (\u00B1",
                     error_threshold * 100, "%)?")) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    ggplot2::scale_color_manual(values = pal_values,
                                drop = FALSE) +
    set_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5)),
      plot.margin = unit(c(0, 0, 0.2, 0), "cm")) +
    ggplot2::guides(color = "none", shape = "none")
  
  if (both)
    p.resampled <- data_summ |>
    dplyr::filter(m %% 2 == 0) |>
    
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = m,
                             y = error_mean,
                             color = overlaps,
                             fill = overlaps)) +
    
    ggplot2::geom_jitter(
      data_false,
      mapping = ggplot2::aes(x = m,
                             y = error,
                             group = sample),
      position = ggplot2::position_dodge(width = 0.3),
      size = 1, shape = 21,
      color = pal_values_light[["FALSE"]],
      fill = pal_values_light[["FALSE"]]
    ) +
    
    ggplot2::geom_jitter(
      data = data_true,
      mapping = ggplot2::aes(x = m,
                             y = error,
                             group = sample),
      position = ggplot2::position_dodge(width = 0.3),
      size = 1, shape = 21,
      color = pal_values_light[["TRUE"]],
      fill = pal_values_light[["TRUE"]]
    ) +
    
    # ggtext::geom_richtext(
    #   data = within_threshold,
    #   mapping = ggplot2::aes(
    #     x = m - 3, y = y_text,
    #     label = paste0("<span style='color: #009da0'>",
    #                    error_within_threshold,
    #                    "</span> of resamples<br>",
    #                    "**within** threshold")),
    #   color = "black", fill = NA, label.color = NA,
    #   hjust = 1, size = 3.5, family = "Roboto Condensed"
    # ) +
    
    # ggtext::geom_richtext(
    #   data = outside_threshold,
    #   mapping = ggplot2::aes(
    #     x = m - 3, y = y_text,
    #     label = paste0("<span style='color: #dd4b39'>",
    #                    error_outside_threshold,
    #                    "</span> of resamples<br>",
    #                    "**outside** of threshold")),
    #   color = "black", fill = NA, label.color = NA,
    #   hjust = 1, size = 3.5, family = "Roboto Condensed"
    # ) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::geom_hline(
      yintercept = error_threshold,
      linewidth = 0.4,
      linetype = "dotted") + 
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      linewidth = 0.4,
      linetype = "dotted") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_mean_lci,
                   ymax = error_mean_uci),
      show.legend = TRUE,
      position = ggplot2::position_dodge(width = 0.4),
      color = "black", linewidth = 0.4) +
    ggplot2::geom_point(
      ggplot2::aes(fill = overlaps),
      show.legend = TRUE,
      position = ggplot2::position_dodge(width = 0.4),
      color = "black", shape = 21, size = 1.3) +
    
    ggplot2::facet_grid(facet_formula,
                        # scales = "free_x",
                        # space = "free_x",
                        labeller = ggplot2::labeller(
                          facet = facet_labels)) +
    
    ggplot2::labs(
      title = NULL, # title = txt_title,
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)"
    ) +
    
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
        text.vjust = 4,
        label.vjust = 0.4,
        theme = ggplot2::theme(
          legend.key.width = unit(2, "lines"),
          legend.key.height = unit(0.5, "lines")))) +
    
    ggplot2::scale_shape_manual(values = c(16, 16)) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    
    set_theme(ft_size = 13) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5)),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      plot.margin = unit(c(0, 0, 0.2, 0), 'cm')) +
    ggplot2::guides(shape = "none", color = "none")
  
  else p.resampled <- data_summ |>
    dplyr::filter(m %% 2 == 0) |>
    
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = m,
                             y = error_mean,
                             color = overlaps,
                             fill = overlaps)) +
    
    ggplot2::geom_jitter(
      data_false,
      mapping = ggplot2::aes(x = m,
                             y = error,
                             group = sample),
      position = ggplot2::position_dodge(width = 0.3),
      size = 1, shape = 21,
      color = pal_values_light[["FALSE"]],
      fill = pal_values_light[["FALSE"]]
    ) +
    
    ggplot2::geom_jitter(
      data = data_true,
      mapping = ggplot2::aes(x = m,
                             y = error,
                             group = sample),
      position = ggplot2::position_dodge(width = 0.3),
      size = 1, shape = 21,
      color = pal_values_light[["TRUE"]],
      fill = pal_values_light[["TRUE"]]
    ) +
    
    # ggtext::geom_richtext(
    #   data = within_threshold,
    #   mapping = ggplot2::aes(
    #     x = m - 3, y = y_text,
    #     label = paste0("<span style='color: #009da0'>",
    #                    error_within_threshold,
    #                    "</span> of resamples<br>",
    #                    "**within** threshold")),
    #   color = "black", fill = NA, label.color = NA,
    #   hjust = 1, size = 3.5, family = "Roboto Condensed"
    # ) +
    
    # ggtext::geom_richtext(
    #   data = outside_threshold,
    #   mapping = ggplot2::aes(
    #     x = m - 3, y = y_text,
    #     label = paste0("<span style='color: #dd4b39'>",
    #                    error_outside_threshold,
    #                    "</span> of resamples<br>",
    #                    "**outside** of threshold")),
    #   color = "black", fill = NA, label.color = NA,
    #   hjust = 1, size = 3.5, family = "Roboto Condensed"
    # ) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::geom_hline(
      yintercept = error_threshold,
      linewidth = 0.7,
      linetype = "dotted") +
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      linewidth = 0.7,
      linetype = "dotted") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_mean_lci,
                   ymax = error_mean_uci),
      show.legend = TRUE,
      position = ggplot2::position_dodge(width = 0.4),
      color = "black", linewidth = 0.4) +
    ggplot2::geom_point(
      ggplot2::aes(fill = overlaps),
      show.legend = TRUE,
      position = ggplot2::position_dodge(width = 0.4),
      color = "black", shape = 21, size = 1.3) +
    
    ggplot2::facet_grid(facet_formula,
                        # scales = "free_x",
                        # space = "free_x",
                        labeller = ggplot2::labeller(
                          facet = facet_labels)) +
    
    ggplot2::labs(
      title = txt_title,
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Relative error (%)"
    ) +
    
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
        text.vjust = 4,
        label.vjust = 0.4,
        theme = ggplot2::theme(
          legend.key.width = unit(2, "lines"),
          legend.key.height = unit(0.5, "lines")))) +
    
    ggplot2::scale_shape_manual(values = c(16, 16)) +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    
    set_theme(ft_size = 12) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5))) +
    ggplot2::guides(shape = "none", color = "none")
  
  if (!is.null(img)) {
    
    if (both) p <- p + patchwork::inset_element(
      p = grid::rasterGrob(img, interpolate = TRUE),
      top = 0.95,
      right = 0.999,
      bottom = 0.85,
      left = 0.92)
    else p.resampled <- p.resampled + patchwork::inset_element(
      p = grid::rasterGrob(img, interpolate = TRUE),
      top = 0.95,
      right = 0.999,
      bottom = 0.85,
      left = 0.92)
  }
  
  if (both) return(p / p.resampled)
  else return(p.resampled)
}

plotting_metrics <- function(data,
                             facet_var,
                             error_threshold,
                             img = NULL) {
  
  # if (facet_var == "dur") txt_title <- "Sampling duration"
  # if (facet_var == "dti") txt_title <- "Sampling interval"
  
  facet_formula <- as.formula(paste(". ~", facet_var))
  
  plot_data <- data |>
    dplyr::group_by(species, type, !!rlang::sym(facet_var), m) |>
    dplyr::mutate(
      within = abs(error) <= error_threshold,
      overlap_start = pmax(error_lci, -error_threshold),
      overlap_end = pmin(error_uci, error_threshold),
      overlap_length = pmax(0, overlap_end - overlap_start),
      ci_length = error_uci - error_lci,
      coverage = overlap_length / ci_length,
      # covered = ifelse(lci <= truth & uci >= truth, 1, 0),
      ci_width = (uci - lci) / truth
    ) |>
    summarize(
      n = n(),
      within = mean(within, na.rm = TRUE),
      # within_lci = calculate_ci(within, level = 0.95)$CI_low,
      # within_uci = calculate_ci(within, level = 0.95)$CI_high,
      # coverage_probability = mean(coverage_probability, na.rm = TRUE),
      # coverage_probability = mean(covered),
      coverage = mean(coverage, na.rm = TRUE),
      # coverage_lci = calculate_ci(coverage, level = 0.95)$CI_low,
      # coverage_uci = calculate_ci(coverage, level = 0.95)$CI_high,
      
      ci_width = mean(ci_width, na.rm = TRUE)
    )
  
  data_crate <- plot_data |>
    dplyr::filter(within >= 1) |> # >= 1 - error_threshold
    dplyr::group_by(species, type, !!rlang::sym(facet_var)) |>
    dplyr::slice_min(m, with_ties = FALSE) |>
    dplyr::ungroup() |> 
    dplyr::mutate(x_pos = ifelse(m <= 10, m + 6, m - 6))
  
  data_cprob <- plot_data |>
    dplyr::filter(coverage >= 1) |>
    dplyr::group_by(species, type, !!rlang::sym(facet_var)) |>
    dplyr::slice_min(m, with_ties = FALSE) |>
    dplyr::ungroup() |> 
    dplyr::mutate(x_pos = ifelse(m <= 16, m + 6, m - 6))
  
  p1 <- plot_data |>
    dplyr::filter(m %% 2 == 0) |>
    
    ggplot2::ggplot(
      ggplot2::aes(x = m, 
                   y = within, 
                   color = within)) +
    
    ggplot2::geom_hline(yintercept = 1,
                        linewidth = 0.3,
                        linetype = "solid") +
    # ggplot2::geom_hline(yintercept = 1 - error_threshold,
    #                     linewidth = 0.7,
    #                     linetype = "dotted") +
    
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_point(size = 1) +
    
    # ggplot2::geom_linerange(
    #   ggplot2::aes(ymin = within_lci,
    #                ymax = within_uci),
    #   show.legend = TRUE,
    #   linewidth = 1.2, alpha = 0.3) +
    
    ggplot2::geom_vline(
      data = data_crate,
      ggplot2::aes(xintercept = m),
      color = pal$sea,
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    
    # ggplot2::geom_vline(
    #   data = data_cprob,
    #   ggplot2::aes(xintercept = m),
    #   color = "grey80",
    #   linewidth = 0.6,
    #   linetype = "dashed"
    # ) +
    
    ggtext::geom_richtext(
      data = data_crate,
      aes(x = x_pos,
          y = 0.2, 
          label = paste0("<i>m</i> = ", m)), 
      family = "Roboto Condensed",
      fill = NA, label.color = NA,
      vjust = 1.5, size = 3, color = pal$sea,
      inherit.aes = FALSE
    ) +
    
    ggplot2::facet_grid(facet_formula) +
    
    ggplot2::labs(
      # title = "Correctness rate",
      x = NULL, # "<i>Population</i> sample size, <i>m</i>",
      y = "Proportion within error threshold (%)"
    ) +
    ggplot2::scale_color_gradient2(
      name = "",
      low = pal$dgr,
      mid = pal$gld, 
      high = pal$sea,
      midpoint = 0.5,
      limits = c(0, 1), 
      labels = scales::percent_format()
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = scales::breaks_pretty(),
                                limits = c(0, 1)) +
    set_theme(ft_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5)),
      plot.margin = unit(c(0, 0, 0.2, 0), "cm")) +
    ggplot2::guides(color = "none")
  
  p2 <- plot_data |>
    dplyr::filter(m %% 2 == 0) |>
    
    ggplot2::ggplot(
      ggplot2::aes(x = m, 
                   y = coverage, 
                   color = coverage)) +
    
    ggplot2::geom_hline(yintercept = 1,
                        linewidth = 0.3,
                        linetype = "solid") +
    # ggplot2::geom_hline(yintercept = 1 - error_threshold,
    #                     linewidth = 0.7,
    #                     linetype = "dotted") +
    
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_point(size = 1) +
    
    # ggplot2::geom_linerange(
    #   ggplot2::aes(ymin = coverage_lci,
    #                ymax = coverage_uci),
    #   show.legend = TRUE,
    #   linewidth = 1.2, alpha = 0.3) +
    
    ggplot2::geom_vline(
      data = data_crate,
      ggplot2::aes(xintercept = m),
      color = "grey80",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    
    # ggplot2::geom_vline(
    #   data = data_cprob,
    #   ggplot2::aes(xintercept = m),
    #   color = pal$sea,
    #   linewidth = 0.6,
    #   linetype = "dashed"
    # ) +
    
    # ggtext::geom_richtext(
    #   data = data_cprob,
    #   aes(x = x_pos,
    #       y = 0.2, 
    #       label = paste0("<i>m</i> = ", m)), 
    #   family = "Roboto Condensed",
    #   fill = NA, label.color = NA,
    #   vjust = 1.5, size = 3, color = pal$sea,
    #   inherit.aes = FALSE
    # ) +
    
    ggplot2::facet_grid(facet_formula) +
    
    ggplot2::labs(
      # title = "Coverage probability",
      x = "<i>Population</i> sample size, <i>m</i>",
      y = "Proportion of CI overlap (%)"
    ) +
    ggplot2::scale_color_gradient2(
      name = "", 
      low = pal$dgr,
      mid = pal$gld,
      high = pal$sea,
      midpoint = 0.5,
      limits = c(0, 1),
      labels = scales::percent_format()
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = scales::breaks_pretty(),
                                limits = c(0, 1)) +
    set_theme(ft_size = 12) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5)),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      plot.margin = unit(c(0, 0, 0.2, 0), 'cm')) +
    ggplot2::guides(color = "none")
  
  # p3 <- plot_data |>
  #   dplyr::filter(m %% 2 == 0) |>
  #   
  #   ggplot2::ggplot(
  #     ggplot2::aes(x = m, 
  #                  y = ci_width, 
  #                  color = ci_width)) +
  #   
  #   ggplot2::geom_hline(yintercept = 0,
  #                       linewidth = 0.3,
  #                       linetype = "solid") +
  #   # ggplot2::geom_hline(yintercept = 1 - error_threshold,
  #   #                     linewidth = 0.7,
  #   #                     linetype = "dotted") +
  #   
  #   ggplot2::geom_line(linewidth = 0.4) +
  #   ggplot2::geom_point(size = 1) +
  #   
  #   # ggplot2::geom_vline(
  #   #   data = data_crate,
  #   #   ggplot2::aes(xintercept = m),
  #   #   color = "grey80",
  #   #   linewidth = 1, 
  #   #   linetype = "dotted"
  #   # ) +
  #   # 
  #   # ggplot2::geom_vline(
  #   #   data = data_cprob,
  #   #   ggplot2::aes(xintercept = m),
  #   #   color = pal$sea,
  #   #   linewidth = 1,
  #   #   linetype = "dotted"
  #   # ) +
  #   
  #   ggplot2::facet_grid(facet_formula) +
  #   
  #   ggplot2::labs(
  #     title = "CI width",
  #     x = "<i>Population</i> sample size, <i>m</i>",
  #     y = "Normalized CI width (%)"
  #   ) +
  #   
  #   ggplot2::scale_color_gradient2(
  #     name = "",
  #     low = pal$sea,
  #     mid = pal$gld,
  #     high = pal$dgr,
  #     midpoint = 0.5,
  #     limits = c(0, 1),
  #     labels = scales::percent_format()) +
  #   
  #   ggplot2::geom_vline(
  #     data = data_crate,
  #     ggplot2::aes(xintercept = m),
  #     color = "grey80",
  #     linewidth = 0.6,
  #     linetype = "dashed"
  #   ) +
  #   
  #   # ggplot2::geom_vline(
  #   #   data = data_cprob,
  #   #   ggplot2::aes(xintercept = m),
  #   #   color = "grey80",
  #   #   linewidth = 0.6,
  #   #   linetype = "dashed"
  #   # ) +
  #   
  #   ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
  #   ggplot2::scale_y_continuous(labels = scales::percent,
  #                               breaks = scales::breaks_pretty(),
  #                               limits = c(0, 1)) +
  #   set_theme(ft_size = 12) +
  #   ggplot2::theme(
  #     plot.title = ggtext::element_markdown(
  #       size = 14, margin = ggplot2::margin(b = 5)),
  #     legend.position = "none"
  #   )
  
  if (!is.null(img)) {
    
    p1 <- p1 + patchwork::inset_element(
      p = grid::rasterGrob(img, interpolate = TRUE),
      top = 0.95,
      right = 0.999,
      bottom = 0.85,
      left = 0.92)
    
  }
  
  # return(p1 / p2 / p3)
  return(p1 / p2)
}

plotting_loocv <- function(data,
                           error_threshold,
                           facet_var,
                           adjust_y,
                           nudge_y,
                           img = NULL) {
  
  par <- .extract_sampling(
    rownames(data), both_pars = FALSE)
  
  data$par <- par
  data$par <- factor(data$par, levels = unique(par))
  
  if (facet_var == "dur") txt_title <- "Sampling duration"
  if (facet_var == "dti") txt_title <- "Sampling interval"
  
  data_summ <- data |>
    dplyr::group_by(species, type, par) |>
    dplyr::mutate(correct_count = abs(error) < 0.05) |>
    dplyr::summarize(
      n = n(),
      correct = sum(correct_count),
      correctness_rate = correct / n)
  
  p <- data |>
    dplyr::filter(x %% 2 == 0) |>
    
    ggplot2::ggplot(
      ggplot2::aes(x = x,
                   y = error,
                   group = par,
                   color = overlaps)) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.3,
      linetype = "solid") +
    ggplot2::geom_hline(
      yintercept = error_threshold,
      linewidth = 0.5,
      linetype = "dotted") + 
    ggplot2::geom_hline(
      yintercept = -error_threshold,
      linewidth = 0.5,
      linetype = "dotted") +
    
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = error_lci,
                   ymax = error_uci),
      show.legend = TRUE,
      linewidth = 1.2, alpha = 0.3) +
    
    ggplot2::geom_point(
      show.legend = TRUE,
      size = 1.7) +
    
    ggplot2::facet_grid(
      . ~ par,
      scales = "free_x",
      space = "free_x") +
    
    # ggrepel::geom_text_repel(
    #   data = data_summ,
    #   mapping = ggplot2::aes(
    #     x = m - 1,
    #     y = error_mean - adjust_y,
    #     label = paste0(round(error_mean * 100, 1), "%")
    #   ),
    #   # box.padding = 0.03,
    #   # point.padding = 0,
    #   # force = 1,
    #   hjust = -1,
    #   nudge_y = nudge_y,
    #   arrow = ggplot2::arrow(angle = 25,
    #                          length = unit(0.20, "cm"),
    #                          type = "closed"),
    #   min.segment.length = 0,
    #   color = data_mean$color,
    #   family = "Roboto Condensed",
    #   size = 4
    # ) +
    
    ggplot2::labs(
      title = txt_title,
      x = "LOOCV iteration",
      y = "Relative error (%)") +
    
    ggplot2::scale_x_continuous(breaks = breaks_pretty()) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = breaks_pretty()) +
    
    ggplot2::scale_color_manual(
      name = paste0("Within error threshold (\u00B1",
                    error_threshold * 100, "%)?"),
      breaks = c("TRUE", "FALSE"),
      values = pal_values, drop = FALSE,
      guide = ggplot2::guide_legend(
        override.aes = list(color = pal_values,
                            fill = pal_values,
                            size = 2),
        order = 1,
        text.vjust = 4,
        label.vjust = 0.4,
        theme = ggplot2::theme(
          legend.key.width = unit(2, "lines"),
          legend.key.height = unit(0.5, "lines")))) +
    
    set_theme() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 14, margin = ggplot2::margin(b = 5)),
      plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  if (!is.null(img)) {
    p <- p + patchwork::inset_element(
      p = grid::rasterGrob(img, interpolate = TRUE),
      top = 0.18,
      bottom = 0.08,
      right = 0.98,
      left = 0.91)
    
  }
  
  else return(p)
}

set_theme <- function(ft_size = 13) {
  
  font <- font_title <- "Roboto Condensed"
  
  ggplot2::theme_minimal() %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(
        family = font, size = ft_size, colour = "grey5"),
      
      plot.title = ggtext::element_markdown(
        family = "Roboto Condensed SemiBold", 
        size = ft_size + 3, # face = 2,
        vjust = 1.2, hjust = 0, margin = margin(b = 2)),
      plot.subtitle = ggtext::element_markdown(
        family = font_title, 
        hjust = 0, margin = margin(b = 7)),
      plot.caption = ggtext::element_markdown(
        size = ft_size - 4, margin = margin(t = 3)),
      
      panel.grid.major = ggplot2::element_line(
        color = "grey92", linewidth = 0.2),
      panel.grid.minor = ggplot2::element_line(
        color = "grey92", linewidth = 0.2),
      panel.border = ggplot2::element_rect(
        color = "grey95", fill = NA, linewidth = 0.2),
      
      strip.background.x = element_rect(
        color = NA, fill = "grey90"),
      strip.background.y =  element_rect(
        color = NA, fill = NA),
      strip.text.x = element_markdown(),
      strip.text.y = element_markdown(angle = 90),
      
      axis.text = ggtext::element_markdown(
        colour = "#878787", size = ft_size - 5),
      axis.title = ggtext::element_markdown(
        family = "Roboto Condensed SemiBold", # face = 2,
        size = ft_size - 2),
      
      axis.title.x = ggtext::element_markdown(
        margin = margin(t = 5),
      ),
      axis.title.y = ggtext::element_markdown(
        margin = margin(r = 5), angle = 90
      ),
      
      legend.title = ggtext::element_markdown(
        family = "Roboto Condensed SemiBold", # face = 2,
        size = ft_size - 2 #,
        # margin = margin(r = 6, b = 14)
      ),
      legend.position = "bottom",
      plot.margin = ggplot2::unit(c(0.5, 0, 0.1, 0), "cm"))
}

