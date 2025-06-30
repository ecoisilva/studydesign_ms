
source("global.R")

img_buffalo <- readPNG(
  here::here("documentation", "ext_images", "african_buffalo.png"))

img_gazelle <- readPNG(
  here::here("documentation", "ext_images", "mongolian_gazelle.png"))

# -------------------------------------------------------------------------
# SIMULATION OUTPUTS with LOOCV: ------------------------------------------

error_threshold <- 0.05
out_dt <- readRDS(file = here::here("outputs", "dt_loocv_sims.rds"))
out_dt <- .prepare_sampling(out_dt, both_pars = TRUE)

p.overview <- out_dt |> 
  dplyr::group_by(species, type, par) |> 
  dplyr::mutate(
    correct_count = abs(error) < error_threshold
  ) |>
  dplyr::summarize(
    n = n(),
    correct = sum(correct_count),
    correctness_rate = correct / n
  ) |> 
  
  ggplot2::ggplot(
    ggplot2::aes(x = correctness_rate,
                 y = par,
                 group = par,
                 color = correctness_rate)) +
  
  ggh4x::facet_grid2(
    ggplot2::vars(type),
    ggplot2::vars(species),
    scales = "free",
    space = "free",
    labeller = ggplot2::labeller(
      species = species_labels,
      type = method_labels)) +
  
  ggplot2::geom_point(
    show.legend = TRUE,
    size = 3) +
  
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, 
                 xend = correctness_rate, 
                 y = par, yend = par,
                 color = correctness_rate),
    linetype = "dotted",
    linewidth = 0.5) +
  
  ggplot2::labs(
    x = "Correctness rate",
    y = "") +
  
  ggplot2::scale_x_continuous(labels = scales::percent,
                              breaks = scales::breaks_pretty()) +
  
  ggplot2::scale_color_gradient2(
    name = "",
    low = pal$dgr, mid = pal$gld, high = pal$sea,
    midpoint = 0.5,
    limits = c(0, 1),
    labels = scales::percent_format()) +
  set_theme() +
  ggplot2::guides(
    color = ggplot2::guide_colorbar(
      barheight = 0.5)) +
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 9))

ggplot2::ggsave(
  p.overview, file = here::here("figures",
                                "supplementary", 
                                "loocv_sims_correctness-rate.png"),
  bg = "white",
  height = 5, width = 8, 
  dpi = 300, device = "png")

## HOME RANGE -------------------------------------------------------------
### Buffalo ("short" position autocorrelation) ----------------------------

loocv_buffalo_hr <- out_dt |> 
  dplyr::filter(species == "buffalo") |> 
  dplyr::filter(type == "hr")

( p.loocv <- plotting_loocv(loocv_buffalo_hr,
                            error_threshold = error_threshold,
                            facet_var = "dur",
                            img = img_buffalo) )

ggplot2::ggsave(
  p.loocv, file = here::here("figures",
                             "supplementary", 
                             "loocv_hr_buffalo.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

### Gazelle ("long" position autocorrelation) -----------------------------

loocv_gazelle_hr <- out_dt |> 
  dplyr::filter(species == "gazelle") |> 
  dplyr::filter(type == "hr")

( p.loocv <- plotting_loocv(loocv_gazelle_hr,
                            error_threshold = error_threshold,
                            facet_var = "dur",
                            img = img_gazelle) )

ggplot2::ggsave(
  p.loocv, file = here::here("figures",
                             "supplementary", 
                             "loocv_hr_gazelle.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")


## SPEED & DISTANCE -------------------------------------------------------
### Buffalo ("short" velocity autocorrelation) ----------------------------

loocv_buffalo_sd <- out_dt |> 
  dplyr::filter(species == "buffalo") |> 
  dplyr::filter(type == "ctsd")

( p.loocv <- plotting_loocv(loocv_buffalo_sd,
                            error_threshold = error_threshold,
                            facet_var = "dti",
                            img = img_buffalo) )

ggplot2::ggsave(
  p.loocv, file = here::here("figures",
                             "supplementary", 
                             "loocv_ctsd_buffalo.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

##### Gazelle ("long" velocity autocorrelation) ----------------------------

loocv_gazelle_sd <- out_dt |> 
  dplyr::filter(species == "gazelle") |> 
  dplyr::filter(type == "ctsd")

( p.loocv <- plotting_loocv(loocv_gazelle_sd,
                            error_threshold = error_threshold,
                            facet_var = "dti",
                            img = img_gazelle) )

ggplot2::ggsave(
  p.loocv, file = here::here("figures",
                             "supplementary", 
                             "loocv_ctsd_gazelle.png"),
  bg = "white",
  height = 5, width = 10, 
  dpi = 300, device = "png")

