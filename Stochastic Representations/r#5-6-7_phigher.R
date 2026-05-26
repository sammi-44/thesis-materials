prob_higher_liquid <- function(mu_liquid, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  liquid_draws <- rnorm(n_sim, mu_liquid, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(liquid_draws > test_draws)
    results$p_lower[i]  <- mean(liquid_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_liquid <- prob_higher_liquid(
  mu_liquid = 107.259,      
  sd_baseline = 2,        # baseline standard deviation
  mus = c(104.985, 100.409, 99.591, 97.937, 97.191, 95.438, 93.463, 78.101, -181.454, -189.967),  #other constraint means
  sds = rep(2, 10)         # sds repeated
)

print(result_liquid)

#constraints ranked 2nd

prob_higher_corant <- function(mu_corant, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  corant_draws <- rnorm(n_sim, mu_corant, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(corant_draws > test_draws)
    results$p_lower[i]  <- mean(corant_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_corant <- prob_higher_corant(
  mu_corant = 104.985,      
  sd_baseline = 2,        # baseline SD
  mus = c(100.409, 99.591, 97.937, 97.191, 95.438, 93.463, 78.101, -181.454, -189.967),  # other means
  sds = rep(2, 9)         # standard deviation repeated
)

print(result_corant)

#constraint ranked 3rd

prob_higher_idaff <- function(mu_idaff, sd_baseline, 
                              mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idaff_draws <- rnorm(n_sim, mu_idaff, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idaff_draws > test_draws)
    results$p_lower[i]  <- mean(idaff_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_idaff <- prob_higher_idaff(
  mu_idaff = 100.409,        
  sd_baseline = 2,        # baseline SD
  mus = c(99.591, 97.937, 97.191, 95.438, 93.463, 78.101, -181.454, -189.967),  # other means
  sds = rep(2, 8)         # standard deviation repeated
)

print(result_idaff)

#constraints ranked 4th

prob_higher_noaff <- function(mu_noaff, sd_baseline, 
                                  mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  noaff_draws <- rnorm(n_sim, mu_noaff, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(noaff_draws > test_draws)
    results$p_lower[i]  <- mean(noaff_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_noaff <- prob_higher_noaff(
  mu_noaff = 99.591, 
  sd_baseline = 2,        # baseline SD
  mus = c(97.937, 97.191, 95.438, 93.463, 78.101, -181.454, -189.967),  # other means
  sds = rep(2, 7)         # standard deviation repeated
)

print(result_noaff)

#constraints ranked 5th

prob_higher_unif <- function(mu_unif, sd_baseline, 
                                   mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  unif_draws <- rnorm(n_sim, mu_unif, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(unif_draws > test_draws)
    results$p_lower[i]  <- mean(unif_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_unif <- prob_higher_unif(
  mu_unif = 97.937,     
  sd_baseline = 2,        # baseline SD
  mus = c(97.191, 95.438, 93.463, 78.101, -181.454, -189.967),  # other means
  sds = rep(2, 6)         # standard deviation repeated
)

print(result_unif)

#ranked 6th

prob_higher_complex <- function(mu_complex, sd_baseline, 
                                mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  complex_draws <- rnorm(n_sim, mu_complex, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(complex_draws > test_draws)
    results$p_lower[i]  <- mean(complex_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_complex <- prob_higher_complex(
  mu_complex = 97.191,      
  sd_baseline = 2,        # baseline standard deviation
  mus = c(95.438, 93.463, 78.101, -181.454, -189.967),  #other constraint means
  sds = rep(2, 5)         # sds repeated
)

print(result_complex)

#constraints ranked 7th

prob_higher_idsib <- function(mu_idsib, sd_baseline, 
                               mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idsib_draws <- rnorm(n_sim, mu_idsib, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idsib_draws > test_draws)
    results$p_lower[i]  <- mean(idsib_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_idsib <- prob_higher_idsib(
  mu_idsib = 95.438,      
  sd_baseline = 2,        # baseline SD
  mus = c(93.463, 78.101, -181.454, -189.967),  # other means
  sds = rep(2, 4)         # standard deviation repeated
)

print(result_idsib)

#constraint ranked 8th

prob_higher_idcont <- function(mu_idcont, sd_baseline, 
                              mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idcont_draws <- rnorm(n_sim, mu_idcont, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idcont_draws > test_draws)
    results$p_lower[i]  <- mean(idcont_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_idcont <- prob_higher_idcont(
  mu_idcont = 93.463,        
  sd_baseline = 2,        # baseline SD
  mus = c(78.101, -181.454, -189.967),  # other means
  sds = rep(2, 3)         # standard deviation repeated
)

print(result_idcont)

#constraints ranked 9th

prob_higher_idson <- function(mu_idson, sd_baseline, 
                                  mus, sds, n_sim = 100000) {
  # to make same length
  if (length(mus) != length(sds)) {
    stop("mus and sds must be same length")
  }
  
  results <- data.frame(mu = mus, sd = sds, 
                        p_higher = numeric(length(mus)),
                        p_lower = numeric(length(mus)))
  
  idson_draws <- rnorm(n_sim, mu_idson, sd_baseline)
  
  for (i in seq_along(mus)) {
    test_draws <- rnorm(n_sim, mus[i], sds[i])
    
    results$p_higher[i] <- mean(idson_draws > test_draws)
    results$p_lower[i]  <- mean(idson_draws < test_draws)
  }
  
  results
}

set.seed(2)  

result_idson <- prob_higher_idson(
  mu_idson = 78.101, 
  sd_baseline = 2,        # baseline SD
  mus = c(-181.454, -189.967),  # other means
  sds = rep(2, 2)         # standard deviation repeated
)

print(result_idson)

#constraints ranked 10th

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
  mu_maxseg = -181.454,     
  sd_baseline = 2,        # baseline SD
  mus = c(-189.967),  # other means
  sds = rep(2, 1)         # standard deviation repeated
)

print(result_maxseg)


print(result_liquid)
print(result_corant)
print(result_idaff)
print(result_noaff)
print(result_unif)
print(result_complex)
print(result_idsib)
print(result_idcont)
print(result_idson)
print(result_maxseg)