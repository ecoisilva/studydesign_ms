
shh <- suppressPackageStartupMessages

shh(library(here))
shh(library(progress))

shh(library(ctmm))
shh(library(data.table))
shh(library(stringr))
shh(library(dplyr))
shh(library(units))
shh(library(lubridate))
shh(library(combinat))

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
max_samples <- as.numeric(args[1])
iter_step <- as.numeric(args[2])
filenames <- args[4:(3 + as.numeric(args[3]))]

# Set folders: ------------------------------------------------------------

myfolders <- "/home/simoes48"
repo <- paste0(myfolders, "/studydesign_ms")

outputfolder <- paste0(repo, "/outputs/")
scriptfolder <- paste0(repo, "/code/scripts")
sapply(list.files(path = scriptfolder,
                  pattern = ".R",
                  full.names = TRUE), source) |>
  quiet() |>
  suppressWarnings() |>
  suppressMessages()

# Functions: --------------------------------------------------------------

sapply(list.files(path = paste0(repo, "/R/functions"),
                  pattern = ".R",
                  full.names = TRUE), source) |>
  quiet() |>
  suppressWarnings() |>
  suppressMessages()

# Run meta-analyses (resampling): -----------------------------------------

message("-------------------------------------------------")
start_total <- Sys.time()
message("- Start time: ", start_total)
message("")

files_to_copy <- list()
for (f in seq_along(filenames)) {
  message(paste(f, "out of", length(filenames)))
  print("-- Filename:")
  
  out <- NULL
  input_filename <- filenames[[f]]
  input_file <- .read_in_files(input_filename)[[1]]
  print(paste("--", input_filename))
  
  target <- strsplit(input_filename, "_")[[1]][2]
  
  output_filename <- paste0(outputfolder,
                            "RESAMPLED_", input_filename,
                            "_", max_samples, "samples",
                            "_", iter_step, "steps.rds")
  files_to_copy[[f]] <- output_filename
  
  ( start_time <- Sys.time() )
  out <- run_meta_resamples(input_file,
                            set_target = target,
                            random = TRUE,
                            max_samples = max_samples,
                            iter_step = iter_step,
                            trace = TRUE)
  ( end_time <- Sys.time() - start_time )
  
  gc()
  saveRDS(out, file = output_filename)
  
}

message("-- core computation completed! ------------------")
message("----------------------------- Total elapsed time:") 
print(difftime(Sys.time(), start_total))
message("-------------------------------------------------")

warnings()

######### R SCRIPT END ####################################################
###########################################################################
