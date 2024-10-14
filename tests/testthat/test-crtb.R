test_that("crtb works with one group as a data frame", {

  dat <- data.frame(obs = rpois(6,5))
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE) |> suppressWarnings()

  if (is.null(out)){
    # handle NULL due to too many ties
    dat <- NULL
  } else {
    # overwrite out to ensure we can use out whether or not too many ties
    out <- out$crdat
  }

  expect_equal(length(dat), length(out))
  expect_equal(names(dat), names(out))
})

test_that("crtb works with one group as a vector", {

  dat <- rpois(6,5)
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(length(dat), length(out))
  expect_null(names(out))
})

test_that("crtb works with rowwise with multiple groups", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  out <- crtb(dat, rowwise = TRUE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  rm(out,dat)


  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  out <- crtb(dat, rowwise = TRUE, replace = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))

})

test_that("crtb works with colwise with multiple groups", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  out <- crtb(dat, rowwise = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  rm(out, dat)

  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  out <- crtb(dat, rowwise = FALSE, replace = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
})


test_that("crtb works with pooled = FALSE", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(30,5),
                    obs2 = rpois(30,5),
                    obs3 = rbinom(30,10, 0.5))

  out <- crtb(dat, pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  rm(out, dat)

  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(30,5),
                    obs2 = rpois(30,5),
                    obs3 = rbinom(30,10, 0.5))

  out <- crtb(dat, pooled = FALSE, replace = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))

})

test_that("crtb works with custom sample_fun", {

  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  # Define a sample function that returns the data in reverse order
  sample_fun <- function(x) {
    return(rev(x))
  }

  # Run resampling with sample_fun
  out <- crtb(dat, rowwise = TRUE, sample_fun = sample_fun) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))

  # # Additional checks to ensure sample_fun was applied
  # expect_false(identical(dat, out$crdat))
})

test_that("crtb works with custom sample_fun and pooled = FALSE", {

  dat <- data.frame(obs1 = rpois(30,5),
                    obs2 = rpois(30,5),
                    obs3 = rbinom(30,10, 0.5))

  # Define a sample function that returns the data in reverse order
  sample_fun <- function(x) {
    return(rev(x))
  }

  # Run resampling with sample_fun
  out <- crtb(dat, rowwise = TRUE, sample_fun = sample_fun, pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))

  # # Additional checks to ensure sample_fun was applied
  # expect_false(identical(dat, out$crdat))
})


# test_that("crtb errors with invalid sample_fun", {
#
#   dat <- data.frame(obs1 = rpois(6,5),
#                     obs2 = rpois(6,5),
#                     obs3 = rbinom(6,10, 0.5))
#
#   # sample_fun is not a function
#   expect_error(crtb(dat, rowwise = TRUE, sample_fun = 123), "sample_fun must be a function")
#
#   # sample_fun returns wrong length
#   sample_fun <- function(x) {
#     return(sample(x, size = length(x)-1))
#   }
#   expect_error(crtb(dat, rowwise = TRUE, sample_fun = sample_fun), "sample_fun must return a vector of the same length as its input")
# })

test_that("crtb works with odd number of observations and one group as a data frame", {

  dat <- data.frame(obs = rpois(21,5))
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(length(dat), length(out))
  expect_equal(names(dat), names(out))
})

test_that("crtb works with odd number of observations and one group as a vector", {

  dat <- rpois(7,5)
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(length(dat), length(out))
  expect_null(names(out))
})

test_that("crtb works with odd number of observations and rowwise = TRUE", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,5),
                    obs3 = rbinom(7,10, 0.5))

  out <- crtb(dat, rowwise = TRUE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  rm(out)


  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,5),
                    obs3 = rbinom(7,10, 0.5))

  out <- crtb(dat, rowwise = TRUE, replace = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))

})

test_that("crtb works with odd number of observations and pooled = FALSE", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,5),
                    obs3 = rbinom(7,10, 0.5))

  out <- crtb(dat, pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    # handle NULL due to too many ties
    dat <- NULL
  } else {
    # overwrite out to ensure we can use out whether or not too many ties
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  rm(out, dat)

  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,5),
                    obs3 = rbinom(7,10, 0.5))


  out <- crtb(dat, pooled = FALSE, replace = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))

})



# # Set seed for reproducibility
# set.seed(123)
#
# # Simulation parameters
# n <- 100        # Sample size
# sim_iter <- 1000  # Number of simulation iterations
# B <- 200        # Number of bootstrap resamples
# B_crtb <- B / 2  # Number of crtb resamples (since crtb returns two datasets per iteration)
#
# # True coefficients
# beta0 <- 1
# beta1 <- 2  # Target variable
# beta2 <- 3
#
# # Storage for results
# results <- list(
#   bootstrap = list(
#     beta1_estimates = matrix(0, nrow = sim_iter, ncol = B),
#     beta1_SE = numeric(sim_iter),
#     beta1_bias = numeric(sim_iter),
#     beta1_CI_low = numeric(sim_iter),
#     beta1_CI_high = numeric(sim_iter),
#     beta1_coverage = numeric(sim_iter)
#   ),
#   crtb = list(
#     beta1_estimates = matrix(0, nrow = sim_iter, ncol = B),
#     beta1_SE = numeric(sim_iter),
#     beta1_bias = numeric(sim_iter),
#     beta1_CI_low = numeric(sim_iter),
#     beta1_CI_high = numeric(sim_iter),
#     beta1_coverage = numeric(sim_iter)
#   ),
#   original_beta1 = numeric(sim_iter)
# )
#
# # Simulation loop
# for (sim in 1:sim_iter) {
#   # Generate data
#   X1 <- stats::rnorm(n)
#   X2 <- 0.3 * X1 + sqrt(1 - 0.3^2) * stats::rnorm(n)  # Low correlation with X1
#   epsilon <- stats::rnorm(n)
#   Y <- beta0 + beta1 * X1 + beta2 * X2 + epsilon
#
#   data <- data.frame(Y = Y, X1 = X1, X2 = X2)
#
#   # Fit the linear regression model
#   model <- stats::lm(Y ~ X1 + X2, data = data)
#   beta1_hat <- stats::coef(model)['X1']
#   results$original_beta1[sim] <- beta1_hat
#
#   # Standard bootstrap
#   beta1_bootstrap <- numeric(B)
#   for (b in 1:B) {
#     indices <- sample(1:n, size = n, replace = TRUE)
#     data_bootstrap <- data[indices, ]
#     model_bootstrap <- stats::lm(Y ~ X1 + X2, data = data_bootstrap)
#     beta1_bootstrap[b] <- stats::coef(model_bootstrap)['X1']
#   }
#   # Compute bootstrap statistics
#   beta1_SE_bootstrap <- sd(beta1_bootstrap)
#   beta1_bias_bootstrap <- mean(beta1_bootstrap) - beta1
#
#   # 95% confidence interval
#   beta1_CI_low_bootstrap <- beta1_hat - stats::qnorm(0.975) * beta1_SE_bootstrap
#   beta1_CI_high_bootstrap <- beta1_hat + stats::qnorm(0.975) * beta1_SE_bootstrap
#
#   beta1_coverage_bootstrap <- (beta1_CI_low_bootstrap <= beta1) & (beta1_CI_high_bootstrap >= beta1)
#
#   # Store bootstrap results
#   results$bootstrap$beta1_estimates[sim, ] <- beta1_bootstrap
#   results$bootstrap$beta1_SE[sim] <- beta1_SE_bootstrap
#   results$bootstrap$beta1_bias[sim] <- beta1_bias_bootstrap
#   results$bootstrap$beta1_CI_low[sim] <- beta1_CI_low_bootstrap
#   results$bootstrap$beta1_CI_high[sim] <- beta1_CI_high_bootstrap
#   results$bootstrap$beta1_coverage[sim] <- beta1_coverage_bootstrap
#
#   # CRTB
#   beta1_crtb <- numeric(B)
#   b_crtb_counter <- 1
#   for (b in 1:(B_crtb)) {
#     resampled_data <- crtb(data)
#     if (is.null(resampled_data)) {
#       # If resampled_data is NULL, skip this iteration
#       next
#     }
#     # Original resample
#     resampled_data$ordat[] <- sapply(resampled_data$ordat, as.numeric)
#     model_crtb_ordat <- stats::lm(Y ~ X1 + X2, data = resampled_data$ordat)
#     beta1_crtb[b_crtb_counter] <- stats::coef(model_crtb_ordat)['X1']
#     b_crtb_counter <- b_crtb_counter + 1
#
#     # Complementary resample
#     resampled_data$crdat[] <- sapply(resampled_data$crdat, as.numeric)
#     model_crtb_crdat <- stats::lm(Y ~ X1 + X2, data = resampled_data$crdat)
#     beta1_crtb[b_crtb_counter] <- stats::coef(model_crtb_crdat)['X1']
#     b_crtb_counter <- b_crtb_counter + 1
#   }
#   # Adjust the length of beta1_crtb if necessary
#   if (b_crtb_counter <= B) {
#     beta1_crtb <- beta1_crtb[1:(b_crtb_counter - 1)]
#   }
#   # Compute crtb statistics
#   beta1_SE_crtb <- stats::sd(beta1_crtb)
#   beta1_bias_crtb <- mean(beta1_crtb) - beta1
#
#   # 95% confidence interval
#   beta1_CI_low_crtb <- beta1_hat - stats::qnorm(0.975) * beta1_SE_crtb
#   beta1_CI_high_crtb <- beta1_hat + stats::qnorm(0.975) * beta1_SE_crtb
#
#   beta1_coverage_crtb <- (beta1_CI_low_crtb <= beta1) & (beta1_CI_high_crtb >= beta1)
#
#   # Store crtb results
#   # Pad with NA if necessary
#   if (length(beta1_crtb) < B) {
#     beta1_crtb <- c(beta1_crtb, rep(NA, B - length(beta1_crtb)))
#   }
#   results$crtb$beta1_estimates[sim, ] <- beta1_crtb
#   results$crtb$beta1_SE[sim] <- beta1_SE_crtb
#   results$crtb$beta1_bias[sim] <- beta1_bias_crtb
#   results$crtb$beta1_CI_low[sim] <- beta1_CI_low_crtb
#   results$crtb$beta1_CI_high[sim] <- beta1_CI_high_crtb
#   results$crtb$beta1_coverage[sim] <- beta1_coverage_crtb
#
#   # Optional: print progress
#   if (sim %% 100 == 0) {
#     cat("Completed simulation", sim, "out of", sim_iter, "\n")
#   }
# }
#
# # Compute average performance metrics
# bootstrap_bias_mean <- mean(results$bootstrap$beta1_bias, na.rm = TRUE)
# crtb_bias_mean <- mean(results$crtb$beta1_bias, na.rm = TRUE)
#
# bootstrap_SE_mean <- mean(results$bootstrap$beta1_SE, na.rm = TRUE)
# crtb_SE_mean <- mean(results$crtb$beta1_SE, na.rm = TRUE)
#
# bootstrap_coverage_mean <- mean(results$bootstrap$beta1_coverage, na.rm = TRUE)
# crtb_coverage_mean <- mean(results$crtb$beta1_coverage, na.rm = TRUE)
#
# bootstrap_CI_width_mean <- mean(results$bootstrap$beta1_CI_high - results$bootstrap$beta1_CI_low, na.rm = TRUE)
# crtb_CI_width_mean <- mean(results$crtb$beta1_CI_high - results$crtb$beta1_CI_low, na.rm = TRUE)
#
# # Output the results
# cat("Bootstrap Results:\n")
# cat("Average Bias:", bootstrap_bias_mean, "\n")
# cat("Average SE:", bootstrap_SE_mean, "\n")
# cat("Coverage Probability:", bootstrap_coverage_mean, "\n")
# cat("Average CI Width:", bootstrap_CI_width_mean, "\n")
#
# cat("\nCRTB Results:\n")
# cat("Average Bias:", crtb_bias_mean, "\n")
# cat("Average SE:", crtb_SE_mean, "\n")
# cat("Coverage Probability:", crtb_coverage_mean, "\n")
# cat("Average CI Width:", crtb_CI_width_mean, "\n")
