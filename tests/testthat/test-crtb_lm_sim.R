test_that("crtb_lm_sim works", {
  simres <- crtb_lm_sim(sim_iter = 2, B = 2, tie_thresh = 0) |> suppressWarnings()
  expect_s3_class(simres, "data.frame")
})
