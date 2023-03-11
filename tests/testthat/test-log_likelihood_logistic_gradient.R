test_that("log_likelihood_logistic_gradient against numerical gradient",
          {
            n_obs <- 32
            n_pred <- 4
            data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
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
