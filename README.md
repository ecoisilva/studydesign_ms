
# Too few, too many, or just right? Optimizing sample sizes for population-level inferences in animal tracking projects

[![Project Status:
Active](https://img.shields.io/badge/Status-active-%23009DA0)](https://www.repostatus.org/#active)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.16676569-%23009DA0)](https://doi.org/10.5281/zenodo.16676569)

## Table of contents

1.  [Introduction](#introduction)
2.  [Tutorials](#tutorials)
3.  [Directory structure](#directory-structure)
4.  [Scripts description](#scripts-description)
5.  [Citations](#citation)
6.  [License](#license)

## Introduction

This repository is a companion piece to the manuscript **“Too few, too
many, or just right? Optimizing sample sizes for population-level
inferences in animal tracking projects”**, currently available as a
preprint
[here](https://www.biorxiv.org/content/10.1101/2025.07.30.667390v2).

The main workflow presented in the manuscript provides a comprehensive
approach for optimizing sample sizes in animal tracking studies,
balancing *sampling duration*, *sampling interval*, and the *number of
sampled individuals* to ensure robust, unbiased population-level
inferences for home range, and speed & distance estimation. By
integrating robust, sampling-insensitive analytical methods and
accounting for uncertainty and logistical constraints (including fix
success, location error, device malfunctions, and individual variation),
this workflow guides researchers in designing effective studies or
evaluating existing data.

The workflow is implemented in the `movedesign` Shiny application and
`R` package, available on
[CRAN](https://cran.r-project.org/web/packages/movedesign/index.html)
and [GitHub](github.com/ecoisilva/movedesign).

## Tutorials

To install the stable version of `movedesign` from CRAN:

``` r
install.packages("movedesign")
```

To install the most recent development version directly from GitHub:

``` r
install.packages("remotes")
remotes::install_github("ecoisilva/movedesign")
```

#### Run Shiny application:

To launch `movedesign`, load the library and run the following command
in your `R` console:

``` r
library(movedesign)
movedesign::run_app()
```

A step-by-step tutorial for the Shiny interface is available
[here](https://ecoisilva.github.io/studydesign_ms/tutorial_pop.html).

#### Run in R console:

The full workflow can also be run directly in the R console. Users start
by fitting continuous-time movement models to empirical datasets, then
pass those models into `md_prepare()` to configure candidate designs
defined by three parameters: number of tagged individuals, sampling
duration, and sampling interval. Two contrasting designs are compared:
one short and fine-resolution, one long and coarse. By running
simulations with `md_run()`, previewing estimation error with
`md_plot_preview()` and `md_compare_preview()`, then scaling up to
multiple replicates with `md_replicate()` for stable inference. Finally,
users can compare and rank designs using `md_compare()`.

As an example, you can follow along the tutorial available
[here](https://ecoisilva.github.io/studydesign_ms/tutorial_pop_console.html).

<!-- A documented R script illustrating different workflows is also available [here](https://ecoisilva.github.io/studydesign_ms/documentation/R_script.R). -->

## Directory structure

The directory structure below provides all code required to reproduce
the data and figures from the manuscript. For guidance on applying the
workflow to your own data, refer to the tutorials in the
[Tutorials](#tutorials) section instead.

    studydesign_ms/
    │-- cluster/              # HPC bash files and scripts
    │-- data/                 # Data files
    │-- documentation/        # Tutorial and supplementary materials
    │-- figures/              # Generated figures
    │-- outputs/              # Processed outputs
    │-- R/                    # R scripts
    │-- renv/                 # `renv` package management directory
    │-- renv.lock             # Lock file for dependencies
    │-- global.R              # Global settings and objects
    │-- studydesign_ms.Rproj  # RStudio project file

## Scripts description

The :file_folder: **`cluster`** folder stores scripts for HPC jobs:

- `cluster/create_job_sh.R`: generates job submission scripts.
- `cluster/mean_hr.R`: runs simulations for mean home range.
- `cluster/mean_speed.R`: runs simulations for mean movement speed.
- `cluster/meta_resampled.R`: runs meta-analyses and resampling
  approach.

The :file_folder: **`R`** folder contains the other main scripts:

- `R/plotting_case-study.R`: generates output plots for a case study.
- `R/running_case-study_loocv.R`: runs LOOCV analyses.
- `R/plotting_case-study_loocv.R`: generates LOOCV output plots for a
  case study.
- `R/plotting_case-study_resampled.R`: generates resample plots for a
  case study.
- `R/plotting_sims_resampled.R`: generates resample plots for the
  simulations.

The :file_folder: **`R/functions`** subfolder contains helper functions
used throughout the project.

## Citations

If you use this workflow or the `movedesign` package in your work,
please cite:

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2025). Too few, too many, or just right? Optimizing sample
> sizes for population-level inferences in animal tracking projects.
> BioRxiv. DOI: <https://doi.org/10.1101/2025.07.30.667390>

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2023). movedesign: Shiny R app to evaluate sampling design for
> animal movement studies. Methods in Ecology and Evolution, 14(9),
> 2216–2225. DOI: <https://doi.org/10.1111/2041-210X.14153>

## License

[![CC BY
4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)

This work is licensed under a [Creative Commons Attribution 4.0
International License](http://creativecommons.org/licenses/by/4.0/).
