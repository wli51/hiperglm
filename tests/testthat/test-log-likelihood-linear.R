test_that("log_likelihood_linear_gradient against numerical gradient", {

  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 2883)
  design <- data$design; outcome <- data$outcome

  random_c <- rnorm(n_pred)

  analytical_gradient <- log_likelihood_gradient_linear(random_c, design, outcome, noise_var=1)
  # numerical_gradient <- approx_grad(log_likelihood_linear, random_c, design, outcome, noise_var=1)
  numerical_gradient <-
    approx_grad(func = function(x) log_likelihood_linear(coef=x, x=design, y=outcome, noise_var=1),
                x=random_c)

  expect_true(are_all_close(
    analytical_gradient, numerical_gradient, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
