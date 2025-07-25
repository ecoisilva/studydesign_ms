
# Install and load all necessary packages: --------------------------------

packageList <- c("remotes",
                 "progress",
                 "dplyr",
                 "tidyr",
                 "data.table",
                 "stringr",
                 "lubridate",
                 "scales",
                 "units",
                 "extrafont",
                 "ggplot2",
                 "ggtext",
                 "ggpointdensity",
                 "ggnewscale",
                 "ggh4x",
                 "ggpubr",
                 "patchwork",
                 "png",
                 "grid",
                 "ctmm",
                 "hrbrthemes",
                 "viridis",
                 "bayestestR",
                 "combinat",
                 "purrr",
                 "here"
)

packageList_new <- packageList[!(
  packageList %in% installed.packages()[, "Package"])]
if (length(packageList_new)) install.packages(packageList_new)
sapply(packageList, library,
       character.only = TRUE, logical.return = TRUE)

if (!requireNamespace("movedesign", quietly = TRUE)) {
  remotes::install_github("ecoisilva/movedesign")
} else {
  library("movedesign")
}

# write.table(packageList, "packages_used.csv",
#             append = FALSE,
#             row.names	= FALSE,
#             col.names = FALSE,
#             sep = ",")

rm(packageList, packageList_new)

# Functions: --------------------------------------------------------------

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

quiet(sapply(list.files(path = here::here("R", "functions"),
                        pattern = ".R",
                        full.names = TRUE), source))

pal <- load_pal()

pal_error <- grDevices::colorRampPalette(c(
  "#1F3545", "#00585A", "#1F7677", "#2E9597", "#4EA36E",
  "#82A84D", "#EBB10C", "#F09F00", "#D67B00", "#C65906",
  "#BF470A", "#B7350E", "#A72012", "#971E10", "#871C0F"))

suppressMessages(extrafont::loadfonts(device = "win"))

# Pre-defined values: -----------------------------------------------------

# Confidence & credible intervals:
ci <- 0.95
cri <- 0.89
alpha <- 1 - ci

# Number of resamples:
max_samples <- 250

# Steps:
iter_step <- 1

# Error threshold:
error_threshold <- 0.05

# Palette for TRUE/FALSE values:
pal_values <- c("TRUE" = pal$sea, "FALSE" = pal$dgr)
pal_values_light <- c("TRUE" = pal$sea_l, "FALSE" = pal$dgr_l)

# Sampling schedule option labels:
option_labels <- c(
  "1 year 15 minutes" = paste0(
    "<span style='color: #000000'><b>Option 1</b></span><br>",
    "<span style='font-size:11px;'>",
    "1 year, every 15 minutes</span>"), 
  "6 years 2 hours" = paste0(
    "<span style='color: #000000'><b>Option 2</b></span><br>",
    "<span style='font-size:11px;'>",
    "6 years, every two hours</span>"), 
  "4 years 1 day" = paste0(
    "<span style='color: #000000'><b>Option 3</b></span><br>",
    "<span style='font-size:11px;'>",
    "4 years, every day</span>"))

# Method labels:
method_labels <- c(
  "hr" = paste0(
    "<b>Home range</b> ", "estimation"), 
  "ctsd" = paste0(
    "<b>Speed & distance</b> ", "estimation"))

# Species labels:
species_labels <- c(
  "buffalo" = paste0(
    "African <b>buffalo</b> (<em>Syncerus caffer</em>)"), 
  "gazelle" = paste0(
    "Mongolian <b>gazelle</b> (<em>Procapra gutturosa</em>)"))
