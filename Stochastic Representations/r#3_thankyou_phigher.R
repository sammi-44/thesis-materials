prob_higher_onsfric <- function(mu_onsfric, sd_baseline, 
                                   mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  onsfric_draws <- rnorm(n_sim, mu_onsfric, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(onsfric_draws > test_draws)
    results$p_lower[i]  <- mean(onsfric_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_onsfric <- prob_higher_onsfric(
  mu_onsfric = 112.719,      
  sd_baseline = 2,        # baseline standard deviation
  mus = c(103.233, 102.255, 100.000, 98.848, 89.880),  #other constraint means
  sds = rep(2, 5)         # sds repeated
)

print(result_onsfric)

#constraints ranked 2nd

prob_higher_maxseg <- function(mu_maxseg, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # to make same length
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

set.seed(2)  

result_maxseg <- prob_higher_maxseg(
  mu_maxseg = 103.233,      
  sd_baseline = 2,        # baseline SD
  mus = c(102.255, 100.000, 98.848, 89.880),  # other means
  sds = rep(2, 4)         # standard deviation repeated
)

print(result_maxseg)

#constraint ranked 3rd

prob_higher_cmplx <- function(mu_cmplx, sd_baseline, 
                                  mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  cmplx_draws <- rnorm(n_sim, mu_cmplx, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(cmplx_draws > test_draws)
    results$p_lower[i]  <- mean(cmplx_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_cmplx <- prob_higher_cmplx(
  mu_cmplx = 102.255,        
  sd_baseline = 2,        # baseline SD
  mus = c(100.000, 98.848, 89.880),  # other means
  sds = rep(2, 3)         # standard deviation repeated
)

print(result_cmplx)

#constraints ranked 4th

prob_higher_idonscoda <- function(mu_idonscoda, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idonscoda_draws <- rnorm(n_sim, mu_idonscoda, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idonscoda_draws > test_draws)
    results$p_lower[i]  <- mean(idonscoda_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_idonscoda <- prob_higher_idonscoda(
  mu_idonscoda = 100.000, 
  sd_baseline = 2,        # baseline SD
  mus = c(98.848, 89.880),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_idonscoda)

#constraints ranked 5th

prob_higher_onsgnocoda <- function(mu_onsgnocoda, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  onsgnocoda_draws <- rnorm(n_sim, mu_onsgnocoda, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(onsgnocoda_draws > test_draws)
    results$p_lower[i]  <- mean(onsgnocoda_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_onsgnocoda <- prob_higher_onsgnocoda(
  mu_onsgnocoda = 98.848,     
  sd_baseline = 2,        # baseline SD
  mus = c(89.880),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_onsgnocoda)


print(result_onsfric)
print(result_maxseg)
print(result_cmplx)
print(result_idonscoda)
print(result_onsgnocoda)