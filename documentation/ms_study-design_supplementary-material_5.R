
# ─────────────────────────────────────────────────────────────────────────
# Supplementary Material 5. ───────────────────────────────────────────────

#   Author: Inês Silva           E-mail: i.simoes-silva @ hzdr.de
#   Last updated on: 2026-03-24, with R version: 4.5.2, movedesign 0.3.3
# 
#   Description:
#      R script implementing two workflows for study design evaluation
#      and optimization via the `movedesign` `R` package.
#            — WORKFLOW 1 compares two candidate designs;
#            — WORKFLOW 2 identifies optimal parameters automatically.
#   Assumption: range residency

# ── 0. Setup ─────────────────────────────────────────────────────────────

install.packages("remotes")
remotes::install_github("ecoisilva/movedesign")

# Load packages:
library(movedesign)
library(ctmm)

# load("inputs.rdata")
# load("out.rdata")
# load("out_optimize.rdata")

# ── 1. Data & models ─────────────────────────────────────────────────────

# Load buffalo tracking dataset bundled within `ctmm`:
data(buffalo) # named list of `telemetry` objects

# Fit movement models:
models <- fitting_models(buffalo, .parallel = FALSE)

# 2. WORKFLOW 1. Compare two pre-specified candidate designs ──────────────

# Target: population mean home range (HR) via meta-analysis
# Criterion: relative error across simulation replicates

# Designs:
#   Design A — 10 individuals · 1-month deployment · 5-day Δt
#   Design B — 6 individuals · 2-year deployment · 10-day Δt

start_time <- Sys.time()

# First candidate design:
input_hrA <- md_prepare(species = "buffalo",
                        data = buffalo,
                        models = models,
                        n_individuals = 10,
                        dur = list(value = 1, unit = "month"),
                        dti = list(value = 5, unit = "days"),
                        add_individual_variation = TRUE,
                        set_target = c("hr"),
                        which_meta = "mean",
                        parallel = TRUE)
summary(input_hrA)

# Second candidate design:
input_hrB <- md_prepare(species = "buffalo",
                        data = buffalo,
                        models = models,
                        n_individuals = 6,
                        dur = list(value = 2, unit = "years"),
                        dti = list(value = 10, unit = "days"),
                        add_individual_variation = TRUE,
                        set_target = c("hr"),
                        which_meta = "mean",
                        parallel = TRUE)
summary(input_hrB)

processed_hrA <- md_run(input_hrA)
processed_hrB <- md_run(input_hrB)

# Compare designs (preview):
out_preview <- md_compare_preview(list(processed_hrA,
                                       processed_hrB), 
                                  error_threshold = 0.05,
                                  n_resamples = 30)

# 2.1 Run multiples replicates ────────────────────────────────────────────

n_replicates <- 15
error_threshold <- 0.05

out_replicates_hrA <- md_replicate(input_hrA,
                                   n_replicates = n_replicates,
                                   error_threshold = error_threshold)
out_replicates_hrB <- md_replicate(input_hrB,
                                   n_replicates = n_replicates,
                                   error_threshold = error_threshold)

# Check convergence
md_check(out_replicates_hrA)
md_check(out_replicates_hrB)

md_plot_replicates(out_replicates_hrA)
md_plot_replicates(out_replicates_hrB)

# 2.2. Final comparison ───────────────────────────────────────────────────

out <- md_compare(list(out_replicates_hrA,
                       out_replicates_hrB))

( end_time <- Sys.time() - start_time )

# 3. WORKFLOW 2. Automated design optimization ────────────────────────────

# Searches for the minimum sampling duration and sampling interval
# that achieves a relative error below a specific error threshold.
# Duration (`dur`) and interval (`dti`) are omitted from `md_prepare()`
# so the function can search those dimensions freely.
# `n_individuals` here is the upper limit on the number of individuals
# that will be run (essentially, allowing for a maximum of 10 tags).

start_time <- Sys.time()

input_hr <- md_prepare(species = "buffalo",
                       data = buffalo,
                       models = models,
                       n_individuals = 10,
                       add_individual_variation = TRUE,
                       set_target = "hr",
                       which_meta = "mean",
                       parallel = TRUE)

# Iteratively evaluate candidate designs, up to 10 individuals:
out_optimize <- md_optimize(input_hr,
                            n_replicates = 15,
                            error_threshold = 0.25,
                            plot = TRUE)

# Performance summary:
summary(out_optimize, verbose = TRUE)
md_check(out_optimize) # for more details

# Plot relative error vs. population sample size:
md_plot_replicates(out_optimize)

# Plot final error distribution at m = 10:
md_plot(out_optimize)

( end_time <- Sys.time() - start_time )

# 4. Save outputs (grouped by workflow stage) ─────────────────────────────

# save(models,
#      input_hrA,
#      input_hrB,
#      input_hr,
#      file = "inputs.rdata")

# save(processed_hrA,
#      processed_hrB,
#      out_replicates_hrA,
#      out_replicates_hrB,
#      out,
#      file = "out.rdata")

# save(input_hr,
#      out_optimize,
#      file = "out_optimize.rdata")
