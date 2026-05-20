
# Too few, too many, or just right? Optimizing sample sizes for population-level inferences in animal tracking projects

<!-- [![DOI](https://zenodo.org/badge/X.svg)](https://doi.org/XX) -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Table of contents

1.  [Introduction](#introduction)
2.  [Citation](#citation)
3.  [Tutorials](#tutorials)
4.  [Directory structure](#directory-structure)
5.  [Scripts description](#scripts-description)
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

The workflow has been fully implemented in the `movedesign` Shiny
application and `R` package, available on
[CRAN](https://cran.r-project.org/web/packages/movedesign/index.html)
and [GitHub](github.com/ecoisilva/movedesign), allowing users to easily
test sampling strategies and assess the reliability of space-use and
movement metrics, ultimately promoting more rigorous and impactful
wildlife research and conservation.

## Tutorials

To install the stable version of `movedesign` from CRAN:

``` r
install.packages("movedesign")
```

#### Run Shiny application:

To launch `movedesign`, load the library and run the following command
in your `R` console:

``` r
library(movedesign)
movedesign::run_app()
```

<div class="callout-note">

A step-by-step tutorial for the Shiny interface is available
[here](https://ecoisilva.github.io/studydesign_ms/documentation/tutorial_pop.html).

</div>

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

``` mermaid
flowchart LR
    A[Fit movement models]
    --> B[Prepare candidate designs]

    B --> C[Run simulations]
    C --> D[Preview estimation error]
    D --> E[Replicate simulations]
    E --> F[Compare designs]
```

<div class="callout-note">

As an example, you can follow along the tutorial available
[here](https://ecoisilva.github.io/studydesign_ms/documentation/tutorial_pop_console.html).

</div>

<!-- A documented R script illustrating different workflows is also available [here](https://ecoisilva.github.io/studydesign_ms/documentation/R_script.R). -->

## Directory structure

The directory structure below provides all code required to reproduce
the data and figures from the manuscript. For guidance on applying the
workflow to your own data, refer to the tutorials in the
[Tutorials](#tutorials) section instead.

    studydesign_ms/
    │-- cluster/              # HPC bash files and scripts
    │-- data/                 # Data files
    │-- documentation/        # Tutorial and supplementary files
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

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2025). Too few, too many, or just right? Optimizing sample
> sizes for population-level inferences in animal tracking projects.
> BioRxiv <https://doi.org/10.1101/2025.07.30.667390>

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2023). movedesign: Shiny R app to evaluate sampling design for
> animal movement studies. Methods in Ecology and Evolution, 14(9),
> 2216–2225. DOI: <https://doi.org/10.1111/2041-210X.14153>

## License

[![CC BY
4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)
