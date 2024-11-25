## code to prepare `pow_2var_uncor_n30_sim` dataset goes here
system.time(
  pow_2var_uncor_n30_sim <- crtb_lm_sim(n = 30, sim_iter = 2000, B = 2000,
                                        beta_gen = function(){
                                          return(
                                            list(1, #beta0
                                                 1, #beta1
                                                 2) #beta2
                                          )
                                        },
                                        gen_ivs = function(n){
                                          X1 <- stats::rnorm(n, 0, 1)
                                          X2 <- stats::rnorm(n, 0, 3)
                                          epsilon <- stats::rnorm(n)
                                          return(list(X1 = X1, X2 = X2, epsilon = epsilon))
                                        },
                                        .formula = "Y ~ X1 + X2",
                                        tie_thresh = 0.0,
                                        alpha = 0.05,
                                        progress = FALSE)
)

usethis::use_data(pow_2var_uncor_n30_sim, overwrite = TRUE)
