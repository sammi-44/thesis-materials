#constraint ranked 1st

prob_higher_idplacecons <- function(mu_idplacecons, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idplacecons_draws <- rnorm(n_sim, mu_idplacecons, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idplacecons_draws > test_draws)
    results$p_lower[i]  <- mean(idplacecons_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_idplacecons <- prob_higher_idplacecons(
  mu_idplacecons = 101.240,       
  sd_baseline = 2,        # baseline SD
  mus = c(100.000, 98.760),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_idplacecons)

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
  mu_maxseg = 100.000,      
  sd_baseline = 2,        # baseline SD
  mus = c(98.760),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_maxseg)

print(result_idplacecons)
print(result_maxseg)