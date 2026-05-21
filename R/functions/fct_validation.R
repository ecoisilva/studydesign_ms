
.check_for_inf_speed <- function(ctsd_list) {
  sapply(ctsd_list, function(x) {
    any(x$CI[, "est"] == "Inf")
  })
}

run_meta_loocv <- function(rv,
                           set_target = c("hr", "ctsd"),
                           subpop = FALSE,
                           trace = FALSE,
                           ...) {
  
  dots <- list(...)
  .only_max_m <- dots[[".only_max_m"]] %||% TRUE
  .max_m <- dots[[".max_m"]] %||% NULL
  .progress <- dots[[".progress"]] %||% FALSE
  .lists <- dots[[".lists"]] %||% NULL
  
  pb <- NULL
  dt_meta <- NULL
  
  if (inherits(rv, "reactivevalues")) {
    rv_list <- reactiveValuesToList(rv)
  } else { rv_list <- rv }
  
  out <- lapply(set_target, function(target) {
    
    if (target == "ctsd") {
      is_ctsd <- !.check_for_inf_speed(rv_list$ctsdList)
      simList <- rv_list$simList[is_ctsd]
      ctsdList <- rv_list$ctsdList[is_ctsd]
    } else {
      simList <- rv_list$simList
    }
    
    if (length(simList) == 0) return(NULL)
    
    n_total <- length(simList)
    if (n_total == 0) return(NULL)
    max_m <- if (is.null(.max_m)) n_total else min(.max_m, n_total)
    
    if (.progress) {
      pb <- txtProgressBar(min = 1, max = max_m, style = 3)
    }
    
    x <- 1
    for (x in seq_len(max_m)) {
      
      if (trace) message(sprintf("--- %d out of %d", x, max_m))
      
      sim_idx <- seq_len(max_m)
      tmp_file <- rlang::duplicate(rv_list, shallow = FALSE)
      
      tmp_file$seedList <- rv_list$seedList[sim_idx][-x]
      tmp_file$simList <- simList[sim_idx][-x]
      tmp_file$simfitList <- rv_list$simfitList[sim_idx][-x]
      if (target == "hr") 
        tmp_file$akdeList <- rv_list$akdeList[sim_idx][-x]
      if (target == "ctsd") 
        tmp_file$ctsdList <- ctsdList[sim_idx][-x]
      tmp_file$seedList <- rv_list$seedList[sim_idx][-x]
      
      if (target == "ctsd" && length(tmp_file$ctsdList) > 0) {
        tmp_file$ctsdList[sapply(tmp_file$ctsdList, is.null)] <- NULL
        
        new_i <- 0
        new_list <- list()
        for (i in seq_along(tmp_file$ctsdList)) {
          if (tmp_file$ctsdList[[i]]$CI[, "est"] != "Inf") {
            new_i <- new_i + 1
            new_list[[new_i]] <- tmp_file$ctsdList[[i]]
          }
        }
        
        if (length(new_list) == 0) new_list <- NULL
        
        tmp_file$ctsdList <- new_list
        tmp_file$ctsdList[sapply(tmp_file$ctsdList, is.null)] <- NULL
        
      } # end of if (target == "ctsd" &&
      #              length(tmp_file$ctsdList) > 0)
      
      tmp_dt <- NULL
      tmp_dt <- run_meta(tmp_file,
                         set_target = target,
                         .only_max_m = TRUE,
                         .automate_seq = TRUE)
      
      if (nrow(tmp_dt) > 0) {
        tmp_dt$x <- x
        if (is.null(dt_meta)) {
          dt_meta <- tmp_dt
        } else {
          dt_meta <- rbind(dt_meta, tmp_dt)
        }
      }
      if (.progress) {
        setTxtProgressBar(pb, x)
      }
      
    } # end of [x] loop (individuals)
    
    return(dt_meta)
    
  }) # end of [set_target] lapply
  
  if (.progress && !is.null(pb)) close(pb)
  
  return(dplyr::distinct(do.call(rbind, out)))
  
}


run_loocv <- function(obj, set_target = NULL, filenames,
                      .progress = TRUE) { 
  
  nms <- .extract_sampling(filenames, both_pars = TRUE)
  species <- lapply(filenames, \(x) strsplit(x, "_")[[1]][1])
  
  loocv_list <- list()
  for (f in seq_along(obj)) {
    
    obj[[f]]$add_ind_var <- TRUE
    if (.progress) message("\n", nms[[f]])
    loocv_list[[f]] <- run_meta_loocv(rv = obj[[f]], 
                                      set_target = set_target,
                                      .progress = .progress)
    
    if (!is.null(loocv_list[[f]])) loocv_list[[f]]$species <- species[[f]]
    if (!is.null(loocv_list[[f]])) loocv_list[[f]]$par <- nms[[f]]
  }
  
  return(setNames(loocv_list, filenames))
}

