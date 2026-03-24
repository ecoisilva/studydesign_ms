
summarize_everything <- function(data,
                                 alpha = 0.05,
                                 error_threshold = 0.05) {
  data |>
    dplyr::summarize(
      n = dplyr::n(),
      error_mean_lci = mean(error, na.rm = TRUE),
      error_mean_uci = mean(error, na.rm = TRUE),
      error_mean = mean(error, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      overlaps = dplyr::between(
        error_mean, -error_threshold, error_threshold),
      overlaps = factor(overlaps, levels = c("TRUE", "FALSE"))) |>
    ungroup()
}
