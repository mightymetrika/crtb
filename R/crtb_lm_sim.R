crtb_lm_sim <- function(n = 100, # Sample size
                        sim_iter = 1000, # Number of simulation iterations
                        B = 1000, # Number of bootstrap resamples
                        beta0 = 1, beta1 = 2, beta2 = 3, # True coefficients
                        x1 = \(n)stats::rnorm(n,0,2)){

  B_crtb <- B / 2  # Number of crtb resamples (since crtb returns two datasets per iteration)

  # Storage for results
  results <- list(
    bootstrap = list(
      beta1_estimates = matrix(0, nrow = sim_iter, ncol = B),
      beta1_SE = numeric(sim_iter),
      beta1_bias = numeric(sim_iter),
      beta1_CI_low = numeric(sim_iter),
      beta1_CI_high = numeric(sim_iter),
      beta1_coverage = numeric(sim_iter)
    ),
    crtb = list(
      beta1_estimates = matrix(0, nrow = sim_iter, ncol = B),
      beta1_SE = numeric(sim_iter),
      beta1_bias = numeric(sim_iter),
      beta1_CI_low = numeric(sim_iter),
      beta1_CI_high = numeric(sim_iter),
      beta1_coverage = numeric(sim_iter)
    ),
    original_beta1 = numeric(sim_iter)
  )

  # Simulation loop
  for (sim in 1:sim_iter) {
    # Generate data
    X1 <- x1(n)
    # X2 <- 0.3 * X1 + sqrt(1 - 0.3^2) * stats::rnorm(n)  # Low correlation with X1
    X2 <- 0.6 * X1 + sqrt(1 - 0.3^2) * stats::rnorm(n)  # Low correlation with X1
    epsilon <- stats::rnorm(n)
    Y <- beta0 + beta1 * X1 + beta2 * X2 + epsilon

    data <- data.frame(Y = Y, X1 = X1, X2 = X2)

    # Fit the linear regression model
    model <- stats::lm(Y ~ X1 + X2, data = data)
    beta1_hat <- stats::coef(model)['X1']
    results$original_beta1[sim] <- beta1_hat

    # Standard bootstrap
    beta1_bootstrap <- numeric(B)
    for (b in 1:B) {
      indices <- sample(1:n, size = n, replace = TRUE)
      data_bootstrap <- data[indices, ]
      model_bootstrap <- stats::lm(Y ~ X1 + X2, data = data_bootstrap)
      beta1_bootstrap[b] <- stats::coef(model_bootstrap)['X1']
    }
    # Compute bootstrap statistics
    beta1_SE_bootstrap <- stats::sd(beta1_bootstrap)
    beta1_bias_bootstrap <- mean(beta1_bootstrap) - beta1

    # 95% confidence interval
    beta1_CI_low_bootstrap <- beta1_hat - stats::qnorm(0.975) * beta1_SE_bootstrap
    beta1_CI_high_bootstrap <- beta1_hat + stats::qnorm(0.975) * beta1_SE_bootstrap

    beta1_coverage_bootstrap <- (beta1_CI_low_bootstrap <= beta1) & (beta1_CI_high_bootstrap >= beta1)

    # Store bootstrap results
    results$bootstrap$beta1_estimates[sim, ] <- beta1_bootstrap
    results$bootstrap$beta1_SE[sim] <- beta1_SE_bootstrap
    results$bootstrap$beta1_bias[sim] <- beta1_bias_bootstrap
    results$bootstrap$beta1_CI_low[sim] <- beta1_CI_low_bootstrap
    results$bootstrap$beta1_CI_high[sim] <- beta1_CI_high_bootstrap
    results$bootstrap$beta1_coverage[sim] <- beta1_coverage_bootstrap

    # CRTB
    beta1_crtb <- numeric(B)
    b_crtb_counter <- 1
    for (b in 1:(B_crtb)) {
      indices <- crtb(1:n)
      if (is.null(indices)) {
        # If resampled_data is NULL, skip this iteration
        next
      }
      # Original resample
      ordat <- data[indices$ordat, ]
      model_crtb_ordat <- stats::lm(Y ~ X1 + X2, data = ordat)
      beta1_crtb[b_crtb_counter] <- stats::coef(model_crtb_ordat)['X1']
      b_crtb_counter <- b_crtb_counter + 1

      # Complementary resample
      crdat <- data[indices$crdat, ]
      model_crtb_crdat <- stats::lm(Y ~ X1 + X2, data = crdat)
      beta1_crtb[b_crtb_counter] <- stats::coef(model_crtb_crdat)['X1']
      b_crtb_counter <- b_crtb_counter + 1
    }
    # Adjust the length of beta1_crtb if necessary
    if (b_crtb_counter <= B) {
      beta1_crtb <- beta1_crtb[1:(b_crtb_counter - 1)]
    }
    # Compute crtb statistics
    beta1_SE_crtb <- stats::sd(beta1_crtb)
    beta1_bias_crtb <- mean(beta1_crtb) - beta1

    # 95% confidence interval
    beta1_CI_low_crtb <- beta1_hat - stats::qnorm(0.975) * beta1_SE_crtb
    beta1_CI_high_crtb <- beta1_hat + stats::qnorm(0.975) * beta1_SE_crtb

    beta1_coverage_crtb <- (beta1_CI_low_crtb <= beta1) & (beta1_CI_high_crtb >= beta1)

    # Store crtb results
    # Pad with NA if necessary
    if (length(beta1_crtb) < B) {
      beta1_crtb <- c(beta1_crtb, rep(NA, B - length(beta1_crtb)))
    }
    results$crtb$beta1_estimates[sim, ] <- beta1_crtb
    results$crtb$beta1_SE[sim] <- beta1_SE_crtb
    results$crtb$beta1_bias[sim] <- beta1_bias_crtb
    results$crtb$beta1_CI_low[sim] <- beta1_CI_low_crtb
    results$crtb$beta1_CI_high[sim] <- beta1_CI_high_crtb
    results$crtb$beta1_coverage[sim] <- beta1_coverage_crtb

    # Optional: print progress
    if (sim %% 100 == 0) {
      cat("Completed crtb simulation", sim, "out of", sim_iter, "\n")
    }
  }

  # Compute average performance metrics
  bootstrap_bias_mean <- mean(results$bootstrap$beta1_bias, na.rm = TRUE)
  crtb_bias_mean <- mean(results$crtb$beta1_bias, na.rm = TRUE)

  bootstrap_SE_mean <- mean(results$bootstrap$beta1_SE, na.rm = TRUE)
  crtb_SE_mean <- mean(results$crtb$beta1_SE, na.rm = TRUE)

  bootstrap_coverage_mean <- mean(results$bootstrap$beta1_coverage, na.rm = TRUE)
  crtb_coverage_mean <- mean(results$crtb$beta1_coverage, na.rm = TRUE)

  bootstrap_CI_width_mean <- mean(results$bootstrap$beta1_CI_high - results$bootstrap$beta1_CI_low, na.rm = TRUE)
  crtb_CI_width_mean <- mean(results$crtb$beta1_CI_high - results$crtb$beta1_CI_low, na.rm = TRUE)

  # Output the results
  cat("Bootstrap Results:\n")
  cat("Average Bias:", bootstrap_bias_mean, "\n")
  cat("Average SE:", bootstrap_SE_mean, "\n")
  cat("Coverage Probability:", bootstrap_coverage_mean, "\n")
  cat("Average CI Width:", bootstrap_CI_width_mean, "\n")

  cat("\nCRTB Results:\n")
  cat("Average Bias:", crtb_bias_mean, "\n")
  cat("Average SE:", crtb_SE_mean, "\n")
  cat("Coverage Probability:", crtb_coverage_mean, "\n")
  cat("Average CI Width:", crtb_CI_width_mean, "\n")

}
