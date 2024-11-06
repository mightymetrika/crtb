## code to prepare `type1_2var_uncor_n15_sim` dataset goes here
Sys.time()
type1_2var_uncor_n15_sim <- crtb_lm_sim(n = 15, sim_iter = 10000, B = 2000,
                                        beta_gen = function(){
                                          return(
                                            list(1, #beta0
                                                 0, #beta1
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
Sys.time()

usethis::use_data(type1_2var_uncor_n15_sim, overwrite = TRUE)
