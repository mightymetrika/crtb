## code to prepare `type1_2var_uncor_n75_wide_het` dataset goes here
system.time(
  type1_2var_uncor_n75_wide_het <- crtb_lm_sim(n = 75, sim_iter = 2000, B = 2000,
                                          beta_gen = function(){
                                            return(
                                              list(1, #beta0
                                                   0, #beta1
                                                   2) #beta2
                                            )
                                          },
                                          gen_ivs = function(n){
                                            X1 <- stats::rnorm(n, mean = 0, sd = 5)
                                            X2 <- stats::rnorm(n, 0, 3)

                                            #setup heteroskedastic errors
                                            grps = 5
                                            base_sd = 10
                                            sd_step = 5
                                            sequences <- split(1:n, ceiling((1:n)/(n/grps)))
                                            epsilon <- vector(mode = "numeric", length = n)

                                            # Generate increasing SDs for each group
                                            sds <- seq(from = base_sd, by = sd_step, length.out = grps)

                                            # Loop through groups to assign errors
                                            for(i in 1:grps) {
                                              group_X1 <- abs(X1[sequences[[i]]])
                                              epsilon[sequences[[i]]] <- stats::rnorm(n/grps, 0, group_X1*sds[i])
                                            }
                                            return(list(X1 = X1, X2 = X2, epsilon = epsilon))
                                          },
                                          .formula = "Y ~ X1 + X2",
                                          tie_thresh = 0.0,
                                          alpha = 0.05,
                                          progress = FALSE)
)

usethis::use_data(type1_2var_uncor_n75_wide_het, overwrite = TRUE)
