test_that("linear.log.likelihood.gradient against numerical gradient", {
  n_obs <- 32
  n_pred <- 4
  data <-
    simulate_data(n_obs, n_pred, model = 'linear', seed = 2883)
  design <- data$design
  outcome <- data$outcome

  start_c <- rep(1, n_pred)

  analytical_gradient <-
    linear.log.likelihood.gradient(start_c, design, outcome, noise_var = 1)
  numerical_gradient <-
    approx_grad(
      func = function(x)
        linear.log.likelihood(
          coef = x,
          x = design,
          y = outcome,
          noise_var = 1
        ),
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
