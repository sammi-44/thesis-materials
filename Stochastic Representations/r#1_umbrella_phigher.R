prob_higher_rtcmplxons <- function(mu_rtcmplxons, sd_baseline, 
                                 mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  rtcmplxons_draws <- rnorm(n_sim, mu_rtcmplxons, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(rtcmplxons_draws > test_draws)
    results$p_lower[i]  <- mean(rtcmplxons_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_rtcmplxons <- prob_higher_rtcmplxons(
  mu_rtcmplxons = 105.895,      
  sd_baseline = 2,        # baseline standard deviation
  mus = c(100.068, 99.932, 96.107, 94.037, 92.103),  #other constraint means
  sds = rep(2, 5)         # sds repeated
)

print(result_rtcmplxons)

#constraints ranked 2nd

prob_higher_idplace <- function(mu_idplace, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idplace_draws <- rnorm(n_sim, mu_idplace, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idplace_draws > test_draws)
    results$p_lower[i]  <- mean(idplace_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_idplace <- prob_higher_idplace(
  mu_idplace = 100.068,      
  sd_baseline = 2,        # baseline SD
  mus = c(99.932, 96.107, 94.037, 92.103),  # other means
  sds = rep(2, 4)         # standard deviation repeated
)

print(result_idplace)

#constraint ranked 3rd

prob_higher_parsesyll <- function(mu_parsesyll, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  parsesyll_draws <- rnorm(n_sim, mu_parsesyll, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(parsesyll_draws > test_draws)
    results$p_lower[i]  <- mean(parsesyll_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_parsesyll <- prob_higher_parsesyll(
  mu_parsesyll = 99.932,      
  sd_baseline = 2,        # baseline SD
  mus = c(96.107, 94.037, 92.103),  # other means
  sds = rep(2, 3)         # standard deviation repeated
)

print(result_parsesyll)

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
  mu_maxseg = 96.107,
  sd_baseline = 2,        # baseline SD
  mus = c(94.037, 92.103),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_maxseg)

#constraints ranked 5th

prob_higher_idvoice <- function(mu_idvoice, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idvoice_draws <- rnorm(n_sim, mu_idvoice, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idvoice_draws > test_draws)
    results$p_lower[i]  <- mean(idvoice_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_idvoice <- prob_higher_idvoice(
  mu_idvoice = 94.037,     
  sd_baseline = 2,        # baseline SD
  mus = c(92.103),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_idvoice)

print(result_rtcmplxons)
print(result_idplace)
print(result_parsesyll)
print(result_maxseg)
print(result_idvoice)