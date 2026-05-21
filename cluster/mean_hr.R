
shh <- suppressPackageStartupMessages

shh(library(here))
shh(library(progress))

shh(library(ctmm))
shh(library(data.table))
shh(library(stringr))
shh(library(dplyr))
shh(library(units))
shh(library(lubridate))

shh(library(parallel))
shh(library(doParallel))
shh(library(foreach))

shh(library(movedesign))

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

###########################################################################
######### R SCRIPT START ##################################################

args <- commandArgs(trailingOnly = TRUE)
get_species <- as.character(args[2])

rv <- list(add_var = as.logical(args[8]),
           grouped = FALSE,
           set_analysis = "hr",
           data_type = "selected",
           which_meta = "mean",
           seedList = list(),
           species = get_species)

# Sampling parameters:

individuals_no <- as.numeric(args[3])
rv$dur <- list(value = as.numeric(args[4]), unit = as.character(args[5]))
rv$dti <- list(value = as.numeric(args[6]), unit = as.character(args[7]))

# Set up parallel processing:

cores_no <- as.numeric(args[1])
cl <- parallel::makeForkCluster(cores_no)
doParallel::registerDoParallel(cl)

# Set folders: ------------------------------------------------------------

myfolders <- "/home/simoes48"
repo <- paste0(myfolders, "/studydesign_ms")

inputsfolder <- paste0(repo, "/data/")
outputfolder <- paste0(repo, "/outputs/")

# Scripts & functions: ----------------------------------------------------

sapply(list.files(path = paste0(repo, "/R/functions"),
                  pattern = ".R",
                  full.names = TRUE), source) |>
  quiet() |>
  suppressMessages() |>
  suppressWarnings()

# 0. Prepare inputs: ------------------------------------------------------

message("-------------------------------------------------")
start_total <- Sys.time()
message("- Start time: ", start_total)
message("- Adding individual variation? ", rv$add_var)
message("")

add_text <- ifelse(rv$add_var, "", "_novar")
filename_rds <- paste0(outputfolder,
                       get_species,
                       "_hr_",
                       "dur", rv$dur$value, rv$dur$unit, "_",
                       "dti", rv$dti$value, rv$dti$unit, "_",
                       individuals_no, "inds", add_text, ".rds")

message("- Filename:")
print(filename_rds)
file.exists(filename_rds)

# Get input species model fit:
INPUT <- get_ctmm_dataset(get_species)
datList <- INPUT$data
fitList <- INPUT$fit

rv$meanfitList <- list(mean(fitList))
names(rv$meanfitList) <- "All"

if (rv$add_var) {
  rv$sigma <- extract_pars(rv$meanfitList, "sigma")
  rv$tau_p <- extract_pars(rv$meanfitList, "position")
  rv$tau_v <- extract_pars(rv$meanfitList, "velocity")
  names(rv$tau_p) <- names(rv$tau_v) <- names(rv$sigma) <- "All"

} else {
  rv$sigma <- extract_pars(fitList, "sigma", meta = TRUE)
  rv$tau_p <- extract_pars(fitList, "position", meta = TRUE)
  rv$tau_v <- extract_pars(fitList, "velocity", meta = TRUE)
  names(rv$tau_p) <- names(rv$tau_v) <- names(rv$sigma) <- "All"
  
  rv$mu <- list(array(0, dim = 2, dimnames = list(c("x", "y"))))
  names(rv$mu) <- "All"
}

message("Simulating data...")
print(Sys.time())
rv$simList <- simulate_data(rv, individuals_no)
names(rv$simList) <- names(rv$seedList) <- paste0(seq_len(individuals_no))
print(Sys.time() - start_total)

gc()

message("Fitting models...")
print(Sys.time())
rv$simfitList <- fitting_model(rv$simList)
names(rv$simfitList) <- names(rv$simList)
print(Sys.time() - start_total)

gc()

saveRDS(list(sim = rv$simList,
             fit = rv$simfitList),
        file = here::here("data", "processed",
                          paste0(rv$species,
                                 "_dur",
                                 rv$dur$value, rv$dur$unit,
                                 "_dti",
                                 rv$dti$value, rv$dti$unit,
                                 "_",
                                 individuals_no, "inds",
                                 "_simfitList.rds")))

gc()

message("Estimating home range...")
rv$akdeList <- estimate_hr(rv)
print(Sys.time() - start_total)

message("Saving output to...")
print(filename_rds)
saveRDS(rv, file = filename_rds)

outputs_logs <- data.table::data.table(
  job_name = args[9],
  species = get_species,
  analysis = "hr",
  filename = paste0(get_species,
                    "_hr_",
                    "dur", rv$dur$value, rv$dur$unit, "_",
                    "dti", rv$dti$value, rv$dti$unit, "_",
                    individuals_no, "inds", add_text, ".rds"),
  dti_value = rv$dti$value,
  dti_unit = rv$dti$unit,
  dur_value = rv$dur$value,
  dur_unit = rv$dur$unit,
  inds = individuals_no,
  time_hrs = difftime(Sys.time(), start_total, units = 'hours')[[1]])

data.table::fwrite(
  outputs_logs,
  paste0(repo, "/cluster/logs.csv"),
  append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")

parallel::stopCluster(cl)


message("-- core computation completed! ------------------")
message("----------------------------- Total elapsed time:") 
print(difftime(Sys.time(), start_total))
message("-------------------------------------------------")

warnings()

######### R SCRIPT END ####################################################
###########################################################################
