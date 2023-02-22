test_that("are_all_close_correct_true", {

  # 1) correctly returns TRUE

  abs_tol = 1e-6
  rel_tol = 1e-6

  v = rnorm(5)
  # artificially add tolerable error both relative to the base vector and in
  # absolute term
  w = v + sample(c(1,0,0,0,0))*abs_tol*1e-1

  expect_true(are_all_close(
    v, w, abs_tol = abs_tol, rel_tol = rel_tol
  ))
})

test_that("are_all_close_rel_false", {

  # 2) correctly returns FALSE because the relative error is above rel_tol

  abs_tol = 1e-6
  rel_tol = 1e-6

  # artificially scale down the base vector and add error term below the
  # absolute tolerance but bigger than the base vector
  v = rnorm(5)*rel_tol*1e-2
  w = v + sample(c(1,0,0,0,0))*rel_tol*1e-1

  expect_true(!are_all_close(
    v, w, abs_tol = abs_tol, rel_tol = rel_tol
  ))
})

test_that("are_all_close_abs_false", {

  # 3) correctly returns FALSE because the absolute error is above abs_tol

  abs_tol = 1e-6
  rel_tol = 1e-6

  # artificially scale up the base vector so relative error won't be a problem
  # so we may add an error term that is above the absolute tolerance but still
  # small relative to the base vector
  v = rnorm(5)*1e6
  w = v + sample(c(1,0,0,0,0))*abs_tol*1e2

  expect_true(!are_all_close(
    v, w, abs_tol = abs_tol, rel_tol = rel_tol
  ))
})
