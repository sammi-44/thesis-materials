prob_higher_b <- function(mu_b, sd_baseline, 
                                 mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  b_draws <- rnorm(n_sim, mu_b, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(b_draws > test_draws)
    results$p_lower[i]  <- mean(b_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_b <- prob_higher_b(
  mu_b = 104.320,      
  sd_baseline = 2,        # baseline standard deviation
  mus = c(103.062, 96.938, 96.342, 96.276),  #other constraint means
  sds = rep(2, 4)         # sds repeated
)

print(result_b)

#constraints ranked 2nd

prob_higher_rhotic <- function(mu_rhotic, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  rhotic_draws <- rnorm(n_sim, mu_rhotic, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(rhotic_draws > test_draws)
    results$p_lower[i]  <- mean(rhotic_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_rhotic <- prob_higher_rhotic(
  mu_rhotic = 103.062,      
  sd_baseline = 2,        # baseline SD
  mus = c(96.938, 96.342, 96.276),  # other means
  sds = rep(2, 3)         # standard deviation repeated
)

print(result_rhotic)

#constraint ranked 3rd

prob_higher_idcons <- function(mu_idcons, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idcons_draws <- rnorm(n_sim, mu_idcons, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idcons_draws > test_draws)
    results$p_lower[i]  <- mean(idcons_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_idcons <- prob_higher_idcons(
  mu_idcons = 96.938,       
  sd_baseline = 2,        # baseline SD
  mus = c(96.342, 96.276),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_idcons)

#constraints ranked 4th

prob_higher_idonsplace <- function(mu_idonsplace, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # Ensure same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idonsplace_draws <- rnorm(n_sim, mu_idonsplace, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idonsplace_draws > test_draws)
    results$p_lower[i]  <- mean(idonsplace_draws < test_draws)
  }
  
  results
}

set.seed(2)  # For reproducible results

result_idonsplace <- prob_higher_idonsplace(
  mu_idonsplace = 96.342,      
  sd_baseline = 2,        # baseline SD
  mus = c(96.276),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_idonsplace)

print(result_b)
print(result_rhotic)
print(result_idcons)
print(result_idonsplace)