
source("global.R")

# PREPARE FILES: ----------------------------------------------------------

filenames <- list(
  buffalo_hr = c("buffalo_hr_dur2months_dti1hour_50inds",
                 "buffalo_hr_dur4months_dti1hour_50inds",
                 "buffalo_hr_dur1year_dti1hour_50inds",
                 "buffalo_hr_dur3years_dti1hour_50inds",
                 "buffalo_hr_dur9years_dti1hour_50inds"),
  
  buffalo_sd = c("buffalo_ctsd_dti4hours_dur2months_50inds",
                 "buffalo_ctsd_dti3hours_dur2months_50inds",
                 "buffalo_ctsd_dti2hours_dur2months_50inds",
                 "buffalo_ctsd_dti1hour_dur2months_50inds",
                 "buffalo_ctsd_dti30minutes_dur2months_50inds"),

  gazelle_hr = c("gazelle_hr_dur2months_dti1hour_50inds",
                 "gazelle_hr_dur4months_dti1hour_50inds",
                 "gazelle_hr_dur1year_dti1hour_50inds",
                 "gazelle_hr_dur3years_dti1hour_50inds",
                 "gazelle_hr_dur9years_dti1hour_50inds"),

  gazelle_sd = c("gazelle_ctsd_dti4hours_dur2months_50inds",
                 "gazelle_ctsd_dti3hours_dur2months_50inds",
                 "gazelle_ctsd_dti2hours_dur2months_50inds",
                 "gazelle_ctsd_dti1hour_dur2months_50inds",
                 "gazelle_ctsd_dti30minutes_dur2months_50inds")
)

rawfiles <- lapply(filenames, .read_in_files)

# PROCESS RAW OUTPUTS: ----------------------------------------------------

groups <- names(filenames)

for (group in groups) {
  message(group)
  
  for (f in seq_along(rawfiles[[group]])) {
    message(paste("--", f, "out of", length(rawfiles)))
    
    target <- strsplit(filenames[[group]][[f]], "_")[[1]][2]
    
    if (target == "hr") error_threshold <- 0.05
    else error_threshold <- 0.1
    
    ( start_time <- Sys.time() )
    out <- run_meta(rawfiles[[group]][[f]], 
                    set_target = target,
                    iter_step = iter_step)
    ( end_time <- Sys.time() - start_time )
    
    out$overlaps <- factor(
      dplyr::between(out$error, -error_threshold, error_threshold),
      levels = c(TRUE, FALSE)
    )
    
    p.optimal <- out |>
      ggplot2::ggplot(
        ggplot2::aes(x = m,
                     y = error,
                     group = type,
                     color = abs(error))) +

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
        show.legend = TRUE,
        linewidth = 2, alpha = 0.7) +
      ggplot2::geom_point(
        show.legend = TRUE,
        size = 2) +
      
      ggplot2::labs(
        x = "<i>Population</i> sample size, <i>m</i>",
        y = "Relative error (%)",
        color = "Relative error (%)") +
      
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty()) +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        breaks = scales::breaks_pretty(),
        limits = c(-1, 1)) +
      ggplot2::scale_color_gradientn(
        colors = pal_error(100),
        labels = scales::percent,
        limits = c(0, 1)) +
      set_theme() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.title = ggtext::element_markdown(
          family = "Roboto Condensed SemiBold",
          size = 10, margin = ggplot2::margin(r = 6, b = 14)),
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")) +
      ggplot2::guides(
        color = ggplot2::guide_colorbar(
          theme = ggplot2::theme(
            legend.key.height = grid::unit(0.25, "cm"),
            legend.key.width = grid::unit(4, "cm"),
            legend.ticks = element_blank())))
    print(p.optimal)
    
    filename <- filenames[[group]][[f]]
    
    ggplot2::ggsave(
      p.optimal,
      file = here::here("figures", "preliminary",
                        paste0(filename, ".png")),
      bg = "white",
      width = 6, height = 5, 
      dpi = 600, device = "png")
    
    # base::saveRDS(
    #   out, file = here::here("data", "processed",
    #                          paste0(filename, ".rds")))
    
    message("...done!")
  }
}
