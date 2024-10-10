test_that("crtb works with one group as a data frame", {

  dat <- data.frame(obs = rpois(6,5))
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE)

  expect_equal(length(dat), length(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")
})

test_that("crtb works with one group as a vector", {

  dat <- rpois(6,5)
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE)

  expect_equal(length(dat), length(out$crdat))
  expect_null(names(out$crdat))
  expect_vector(out$crdat, size = 6)
})

test_that("crtb works with rowwise with multiple groups", {

  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  # run resampling with replacement
  out <- crtb(dat, rowwise = TRUE)

  expect_equal(nrow(dat), nrow(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")
  rm(out)


  # run resampling without replacement
  out <- crtb(dat, rowwise = TRUE, replace = FALSE)

  expect_equal(nrow(dat), nrow(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")

})

test_that("crtb works with colwise with multiple groups", {

  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  # run resampling with replacement
  out <- crtb(dat, rowwise = FALSE)

  expect_equal(nrow(dat), nrow(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")
  rm(out)

  # run resampling without replacement
  out <- crtb(dat, rowwise = FALSE, replace = FALSE)

  expect_equal(nrow(dat), nrow(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")
})


test_that("crtb works with pooled = FALSE", {

  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))

  # run resampling with replacement
  out <- crtb(dat, pooled = FALSE)

  expect_equal(nrow(dat), nrow(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")
  rm(out)

  # run resampling without replacement
  out <- crtb(dat, pooled = FALSE, replace = FALSE)

  expect_equal(nrow(dat), nrow(out$crdat))
  expect_equal(names(dat), names(out$crdat))
  expect_s3_class(out$crdat, "data.frame")

})
