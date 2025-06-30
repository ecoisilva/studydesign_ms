
.check_for_inf_speed <- function(ctsd_list) {
  sapply(ctsd_list, function(x) {
    any(x$CI[, "est"] == "Inf")
  })
}


run_loocv <- function(obj, set_target = NULL, filenames,
                      .progress = TRUE) { 
  
  nms <- .extract_sampling(filenames, both_pars = TRUE)
  species <- lapply(filenames, \(x) strsplit(x, "_")[[1]][1])
  
  loocv_list <- list()
  for (f in seq_along(obj)) {
    
    if (.progress) message("\n", nms[[f]])
    loocv_list[[f]] <- run_meta_loocv(rv = obj[[f]], 
                                      set_target = set_target,
                                      .progress = .progress)
    
    if (!is.null(loocv_list[[f]])) loocv_list[[f]]$species <- species[[f]]
    if (!is.null(loocv_list[[f]])) loocv_list[[f]]$par <- nms[[f]]
  }
  
  return(setNames(loocv_list, filenames))
}

