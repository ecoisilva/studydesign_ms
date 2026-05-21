
get_ctmm_dataset <- function(x = "buffalo", id = NULL) {
  utils::data(list = x, package = "ctmm")
  
  if (x == "buffalo") out_dataset <- buffalo
  if (x == "gazelle") out_dataset <- gazelle
  
  simfitList <- readRDS(
    here::here("data", "processed", paste0(x, "_fitList.rds")))
  
  data <- out_dataset
  guess <- fit <- list()
  
  if (is.null(id)) {
    for (i in seq_along(data)) {
      guess[[i]] <- ctmm::ctmm.guess(data[[i]], interactive = FALSE)
      fit[[i]] <- simfitList[[i]][[1]]
    }
  } else {
    data <- data[[id]]
    guess <- ctmm::ctmm.guess(data, interactive = FALSE)
    fit <- simfitList[[id]][[1]]
  }
  
  names(fit) <- names(guess) <- names(data)
  
  return(list(data = data,
              guess = guess,
              fit = fit))
}

generate_seed <- function(seed_list = NULL) {
  
  set.seed(NULL)
  get_random <- function(n) {
    round(stats::runif(n, min = 1, max = 999999), 0)
  }
  
  out <- get_random(1)
  if (!is.null(seed_list))
    while ((out %in% seed_list) && ((out + 1) %in% seed_list)) 
      out <- get_random(1)
  return(out)
}

simulate_data <- function(rv, num_sims) {
  
  tmpList <- list()
  for (i in seq_len(num_sims)) {
    
    rv$seedList[[i]] <<- rv$seed0 <- generate_seed()
    sim <- movedesign:::simulating_data(rv)[[1]]
    tmpList[[i]] <- sim
  }
  
  return(tmpList)
}
