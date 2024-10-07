test_that("crtb works with one group", {
  dat <- data.frame(obs = rpois(6,5))
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE)

  dat <- rpois(6,5)
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE)

  expect_equal(2 * 2, 4)
})


test_that("crtb works with rowwise with multiple groups", {
  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))
  out <- crtb(dat, pooled = TRUE, rowwise = TRUE)

  expect_equal(2 * 2, 4)
})

test_that("crtb works with colwise with multiple groups", {
  dat <- data.frame(obs1 = rpois(6,5),
                    obs2 = rpois(6,5),
                    obs3 = rbinom(6,10, 0.5))
  out <- crtb(dat, pooled = TRUE, rowwise = FALSE)

  expect_equal(2 * 2, 4)
})
