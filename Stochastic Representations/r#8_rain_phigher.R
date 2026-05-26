#constraint ranked 1st

prob_higher_idconsv <- function(mu_idconsv, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idconsv_draws <- rnorm(n_sim, mu_idconsv, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idconsv_draws > test_draws)
    results$p_lower[i]  <- mean(idconsv_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_idconsv <- prob_higher_idconsv(
  mu_idconsv = 101.632,       
  sd_baseline = 2,        # baseline SD
  mus = c(100.000, 98.368),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_idconsv)

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
  mus = c(98.368),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_maxseg)

print(result_idconsv)
print(result_maxseg)