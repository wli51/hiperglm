test_that("logistic.log.likelihood.gradient against numerical gradient",
          {
            n_obs <- 32
            n_pred <- 4
            data <-
              simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
            design <- data$design
            outcome <- data$outcome

            start_c <- rep(1, n_pred)

            analytical_gradient <-
              logistic.log.likelihood.gradient(start_c, design, outcome)
            numerical_gradient <-
              approx_grad(
                func = function(x)
                  logistic.log.likelihood(coef = x,
                                          x = design,
                                          y = outcome),
                x = start_c
              )

            expect_true(
              are_all_close(
                analytical_gradient,
                numerical_gradient,
                abs_tol = 1e-3,
                rel_tol = 1e-3
              )
            )
          })


test_that("take.one.newton.step QR option agrees with LU ", {
  n_obs <- 32
  n_pred <- 4
  data <-
    simulate_data(n_obs, n_pred, model = 'logit', seed = 1928)
  design <- data$design
  outcome <- data$outcome
  start_c <- rep(0, n_pred)

  lu_out <- take.one.newton.step(design, outcome, start_c, "LU")
  qr_out <- take.one.newton.step(design, outcome, start_c, "QR")

  expect_true(
    are_all_close(
      lu_out,
      qr_out,
      abs_tol = 1e-6,
      rel_tol = 1e-6
    )
  )
})

test_that("take.one.newton.step RcppQR option agrees with QR", {
  n_obs <- 32
  n_pred <- 4
  data <-
    simulate_data(n_obs, n_pred, model = 'logit', seed = 1928)
  design <- data$design
  outcome <- data$outcome
  start_c <- rep(0, n_pred)

  qr_out <- take.one.newton.step(design, outcome, start_c, "QR")
  RcppQR_out <- take.one.newton.step(design, outcome, start_c, "RcppQR")

  expect_true(
    are_all_close(
      qr_out,
      RcppQR_out,
      abs_tol = 1e-6,
      rel_tol = 1e-6
    )
  )
})
