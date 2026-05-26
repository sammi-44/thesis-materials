prob_higher_cmplxons <- function(mu_cmplxons, sd_baseline, 
                                 mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  cmplxons_draws <- rnorm(n_sim, mu_cmplxons, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(cmplxons_draws > test_draws)
    results$p_lower[i]  <- mean(cmplxons_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_cmplxons <- prob_higher_cmplxons(
  mu_cmplxons = 106.418,      
  sd_baseline = 2,        # baseline standard deviation
  mus = c(101.145, 100.221, 98.634, 94.997),  #other constraint means
  sds = rep(2, 4)         # sds repeated
)

print(result_cmplxons)

#constraints ranked 2nd

prob_higher_onsidv <- function(mu_onsidv, sd_baseline, 
                                 mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  onsidv_draws <- rnorm(n_sim, mu_onsidv, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(onsidv_draws > test_draws)
    results$p_lower[i]  <- mean(onsidv_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_onsidv <- prob_higher_onsidv(
  mu_onsidv = 101.145,      
  sd_baseline = 2,        # baseline SD
  mus = c(100.221, 98.634, 94.997),  # other means
  sds = rep(2, 3)         # standard deviation repeated
)

print(result_onsidv)

#constraint ranked 3rd

prob_higher_onsplos <- function(mu_onsplos, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  onsplos_draws <- rnorm(n_sim, mu_onsplos, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(onsplos_draws > test_draws)
    results$p_lower[i]  <- mean(onsplos_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_onsplos <- prob_higher_onsplos(
  mu_onsplos = 100.221,      
  sd_baseline = 2,        # baseline SD
  mus = c(98.634, 94.997),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_onsplos)

#constraints ranked 4th

prob_higher_maxseg <- function(mu_maxseg, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  maxseg_draws <- rnorm(n_sim, mu_maxseg, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(maxseg_draws > test_draws)
    results$p_lower[i]  <- mean(maxseg_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_maxseg <- prob_higher_maxseg(
  mu_maxseg = 98.634,      
  sd_baseline = 2,        # baseline SD
  mus = c(94.997),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_maxseg)

print(result_cmplxons)
print(result_onsidv)
print(result_onsplos)
print(result_maxseg)