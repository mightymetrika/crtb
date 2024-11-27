## code to prepare `pow_2var_cor_n15_wide_sim` dataset goes here
system.time(
  pow_2var_cor_n15_wide_sim <- crtb_lm_sim(n = 15, sim_iter =2000, B = 2000,
                                      beta_gen = function(){
                                        return(
                                          list(1, #beta0
                                               1, #beta1
                                               2) #beta2
                                        )
                                      },
                                      gen_ivs = function(n){
                                        mu1 <- mu2 <- 0 # means
                                        s1 <- 5 # sd var1
                                        s2 <- 3 # sd var2
                                        correl <- 0.8 # correlation
                                        Sigma <- matrix(c(s1^2, s1*s2*correl, s1*s2*correl, s2^2), 2, 2) # var-cov
                                        bndat <- MASS::mvrnorm(n, mu = c(mu1, mu2), Sigma = Sigma) # generate bivariate normal data
                                        X1 <- bndat[,1] # extract X1
                                        X2 <- bndat[,2] # extract X2
                                        epsilon <- stats::rnorm(n) # error term
                                        return(list(X1 = X1, X2 = X2, epsilon = epsilon))
                                      },
                                      .formula = "Y ~ X1 + X2",
                                      tie_thresh = 0.0,
                                      alpha = 0.05,
                                      progress = FALSE)
)


usethis::use_data(pow_2var_cor_n15_wide_sim, overwrite = TRUE)
