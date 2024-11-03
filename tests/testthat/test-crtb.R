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

test_that("crtb works with grp_var for pooled resampling with one value column", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, grp_var = "tx") |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))
  rm(out,dat)


  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, rowwise = TRUE, replace = FALSE, grp_var = "tx") |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))

})


test_that("crtb works with grp_var for pooled resampling with two value columns", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,9),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, grp_var = "tx") |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))
  rm(out,dat)


  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,9),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, rowwise = TRUE, replace = FALSE, grp_var = "tx") |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))

})

test_that("crtb works with grp_var for non-pooled resampling with one value column", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, grp_var = "tx", pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))
  rm(out,dat)


  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, rowwise = TRUE, replace = FALSE, grp_var = "tx",
              pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))

})

test_that("crtb works with grp_var for non-pooled resampling with two value columns", {

  # run resampling with replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,9),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, grp_var = "tx", pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))
  rm(out,dat)


  # run resampling without replacement
  dat <- data.frame(obs1 = rpois(7,5),
                    obs2 = rpois(7,9),
                    tx = c("T", "T", "T", "T", "C", "C", "C"))

  out <- crtb(dat, rowwise = TRUE, replace = FALSE, grp_var = "tx",
              pooled = FALSE) |> suppressWarnings()

  if (is.null(out)){
    dat <- NULL
  } else {
    out <- out$crdat
  }

  expect_equal(nrow(dat), nrow(out))
  expect_equal(names(dat), names(out))
  expect_equal(table(dat$tx), table(out$tx))

})
