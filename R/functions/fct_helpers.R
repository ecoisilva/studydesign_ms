
"%!in%" <- Negate("%in%")

load_pal <- function() {
  
  # out <- list(sea_l = "#7fcecf",
  #             sea = "#08bbbf",
  #             dgr_l = "#eea59c",
  #             dgr = "#fa6b5a")
  
  out <- list(sea_l = "#9cd6d6",
              dgr_l = "#f2c3bd",
              sea = "#009da0",
              dgr = "#dd4b39",
              gld = "#ffb300")
  
  return(out)
}


calculate_ci <- function(data, level = 0.95) {
  
  alpha <- 1 - (1 - level)/2
  
  dof <- length(data) - 1
  margin <- qt(alpha, df = dof) *
    sd(data, na.rm = TRUE)/sqrt(length(data))
  
  out <- data.frame(
    CI = level,
    CI_low = mean(data, na.rm = TRUE) - margin,
    CI_high = mean(data, na.rm = TRUE) + margin)
  
  return(out)
}

.read_in_files <- function(filenames,
                           folder = "data/simulated") {
  
  out <- list()
  for (i in seq_along(filenames)) {
    out[[i]] <- readRDS(
      here::here(folder, paste0(filenames[[i]], ".rds")))
  }
  
  return(out)
}

.extract_sampling <- function(filenames,
                              both_pars = FALSE) {
  
  extract_from_filename <- function(filename, prefix, time_units) {
    
    out <- stringr::str_extract(
      filename, paste0(prefix, "\\d+(", time_units, ")"))
    out <- gsub(prefix, "", out)
    out <- gsub(paste0("(?<=\\d)(", time_units, ")"),
                " \\1", out, perl = TRUE)
    return(out)
  }
  
  out <- c()
  for (f in seq_along(filenames)) {
    
    analysis <- strsplit(filenames[[f]], "_")[[1]][2]
    
    tmp1_dur <- "dur"
    tmp2_dur <- "years|year|months|month|days|day"
    tmp1_dti <- "dti"
    tmp2_dti <- "days|day|hours|hour|minutes|minute"
    
    if (both_pars) {
      
      dur_out <- extract_from_filename(filenames[[f]],
                                       tmp1_dur, tmp2_dur)
      dti_out <- extract_from_filename(filenames[[f]],
                                       tmp1_dti, tmp2_dti)
      out <- c(out, paste(dur_out, dti_out))
      
    } else {
      
      if (analysis == "hr") {
        tmp1 <- tmp1_dur
        tmp2 <- tmp2_dur
      }
      
      if (analysis == "ctsd") {
        tmp1 <- tmp1_dti
        tmp2 <- tmp2_dti
      }
      
      out <- c(out, extract_from_filename(filenames[[f]],
                                          tmp1, tmp2))
    }
  }
  
  return(out)
}

.prepare_sampling <- function(data, both_pars = FALSE) {
  
  par <- .extract_sampling(
    rownames(data), both_pars = both_pars)
  
  data$par <- par
  data$par <- factor(data$par, levels = unique(par))
  return(data)
}

.combine_datasets <- function(filenames,
                              error_threshold = 0.05,
                              both_pars = FALSE,
                              resampled = FALSE) {
  
  species <- strsplit(filenames[[1]], "_")[[1]][1]
  analysis <- strsplit(filenames[[1]], "_")[[1]][2]
  sampling_parameter <- .extract_sampling(filenames,
                                          both_pars = both_pars)
  
  out <- NULL
  
  if (resampled) {
    
    for (i in seq_along(filenames)) {
      
      is_var <- !grepl("novar", filenames[[i]])
      
      if (resampled)
        filepath <- here("outputs",
                         paste0("RESAMPLED_",
                                filenames[i], ".rds"))
      
      tmp <- readRDS(filepath)
      tmp$var <- is_var
      
      if (both_pars) {
        tmp$par <- sampling_parameter[i]
      } else {
        if (analysis == "hr")
          tmp$dur <- sampling_parameter[i]
        if (analysis == "ctsd")
          tmp$dti <- sampling_parameter[i]
      }
      
      if (is.null(out)) {
        out <- tmp
      } else {
        out <- dplyr::full_join(out, tmp)
      }
    }
    
  } else {
    
    for (i in seq_along(filenames)) {
      
      is_var <- !grepl("novar", filenames[[i]])
      filepath <- here("data",
                       "processed",
                       paste0(filenames[i], ".rds"))
      tmp <- readRDS(filepath)
      tmp$var <- is_var
      
      if (both_pars) {
        tmp$par <- sampling_parameter[i]
      } else {
        if (analysis == "hr")
          tmp$dur <- sampling_parameter[i]
        if (analysis == "ctsd")
          tmp$dti <- sampling_parameter[i]
      }
      
      if (is.null(out)) {
        out <- tmp
      } else {
        out <- dplyr::full_join(out, tmp)
      }
    }
  }
  
  if (both_pars)
    out$par <- factor(out$par, levels = sampling_parameter)
  if (!both_pars && analysis == "hr") 
    out$dur <- factor(out$dur, levels = sampling_parameter)
  if (!both_pars && analysis == "ctsd") 
    out$dti <- factor(out$dti, levels = sampling_parameter)
  
  out <- out |> 
    dplyr::mutate(overlaps = dplyr::between(
      est, -error_threshold, error_threshold)) |> 
    dplyr::mutate(overlaps = factor(out$overlaps,
                                    levels = c("TRUE", "FALSE"))) |> 
    dplyr::mutate(species = species)
  return(out)
}

extract_units <- function(input, name = NULL) {
  
  if (length(input) == 0) return(NULL)
  
  tryCatch(
    expr = {
      string <- gsub(
        "\\(([^()]+)\\)", "\\1",
        stringr::str_extract_all(input,
                                 "\\(([^()]+)\\)")[[1]])
      return(string)
      
    }, error = function(e) return(NULL))
}

extract_pars <- function(
    obj,
    name = c("position", "velocity", "sigma", "speed"),
    si_units = FALSE,
    meta = FALSE) {
  
  name <- match.arg(name)
  
  unit <- NA
  out <- NULL
  if (missing(obj)) 
    stop("`obj` argument not provided.")
  if (class(obj)[1] != "list" && class(obj[[1]])[1] != "ctmm") {
    obj <- list(obj)
  }
  
  if (name == "position" || name == "velocity")
    var <- paste("tau", name) else
      if (name == "sigma") var <- "area" else
        if (name == "speed") var <- name
  
  if (meta && length(obj) > 1) {
    movedesign:::.capture_meta(obj, 
                  variable = var,
                  units = !si_units, 
                  verbose = FALSE, 
                  plot = FALSE) -> out
    if (is.null(out)) return(NULL)
    
    unit <- extract_units(rownames(out$meta)[1])
    tmp <- c(out$meta[1, 1],
             out$meta[1, 2],
             out$meta[1, 3])
    if (name == "sigma") tmp <- tmp / -2 / log(0.05) / pi
    
    return(list(data.frame(value = tmp, unit = unit,
                           row.names = c("low", "est", "high"))))
  }
  
  out <- list()
  out <- lapply(seq_along(obj), function(x) {
    
    sum.obj <- summary(obj[[x]], units = !si_units)
    nms.obj <- rownames(sum.obj$CI)
    
    if (var == "area") {
      tmp <- sum.obj$CI[grep(var, nms.obj), ]
      unit <- extract_units(nms.obj[grep(var, nms.obj)])
      
      if (!is.null(nrow(tmp))) 
        if (nrow(tmp) > 1)
          tmp <- subset(tmp, !grepl("^CoV", row.names(tmp)))[1,]
      
      tmp <- data.frame(value = tmp / -2 / log(0.05) / pi,
                        unit = unit)
      
      if (!si_units) tmp <- fix_unit(tmp, convert = TRUE)
      
      return(data.frame(tmp,
                        row.names = c("low", "est", "high")))
    }
    
    # Special cases of movement processes:
    
    tmp_name <- name
    tmp <- sum.obj$CI[grep(name, nms.obj), ]
    unit <- extract_units(nms.obj[grep(name, nms.obj)])
    
    if (length(obj[[x]]$tau) == 2 &&
        all(obj[[x]]$tau[1] == obj[[x]]$tau[2])) {
      
      # (OUΩ and OUf):
      
      tmp_name <- ifelse(any(grepl("decay", nms.obj)), 
                         "decay", "\u03C4")
      tmp <- sum.obj$CI[grep(tmp_name, nms.obj), ]
      unit <- extract_units(nms.obj[grep(tmp_name, nms.obj)])
    }
    
    if (!is.null(nrow(tmp))) 
      if (nrow(tmp) > 1)
        tmp <- subset(tmp, !grepl("^CoV", row.names(tmp)))[1,]
    unit <- extract_units(nms.obj[grep(tmp_name, nms.obj)])
    
    if (length(tmp) == 0) return(NULL)
    if (si_units && !all(is.na(tmp))) tmp <- unit %#% tmp
    
    return(data.frame(value = tmp, unit = unit,
                      row.names = c("low", "est", "high")))
  })
  
  names(out) <- names(obj)
  out[sapply(out, is.null)] <- NULL
  if (length(out) == 0) return(NULL)
  
  return(out)
}
