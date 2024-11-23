## code to prepare `type1_2var_cor_n30_wide_skew_sim` dataset goes here
library(sn)
system.time(
  type1_2var_cor_n30_wide_skew_sim <- crtb_lm_sim(n = 30, sim_iter = 2000, B = 2000,
                                                  beta_gen = function(){
                                                    return(
                                                      list(1, #beta0
                                                           0, #beta1
                                                           2) #beta2
                                                    )
                                                  },
                                                  gen_ivs = function(n){
                                                    mu1 <- mu2 <- 0 # means
                                                    s1 <- 5 # sd var1
                                                    s2 <- 3 # sd var2
                                                    Sk1 <- 0.85 # marginal skewness
                                                    Sk2 <- 0.35 # marginal skewness
                                                    correl <- 0.8 # correlation
                                                    Sigma <- matrix(c(s1^2, s1*s2*correl, s1*s2*correl, s2^2), 2, 2) # var-cov
                                                    cpM <- list(mean=c(mu1,mu2), var.cov=Sigma, gamma1=c(Sk1, Sk2)) # mean vector, variance matrix, marginal skewness:
                                                    dpM <- sn::cp2dp(cpM, family="SN")
                                                    bsndat <- sn::rmsn(n, dp=dpM)
                                                    X1 <- bsndat[,1] # extract X1
                                                    X2 <- bsndat[,2] # extract X2
                                                    epsilon <- stats::rnorm(n)
                                                    return(list(X1 = X1, X2 = X2, epsilon = epsilon))
                                                  },
                                                  .formula = "Y ~ X1 + X2",
                                                  tie_thresh = 0.0,
                                                  alpha = 0.05,
                                                  progress = FALSE)
)

usethis::use_data(type1_2var_cor_n30_wide_skew_sim, overwrite = TRUE)
