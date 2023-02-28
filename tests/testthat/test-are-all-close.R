test_that("are_all_close_correct_true", {

  abs_tol = 1e-6
  rel_tol = 1e-6

  v = rep(1e0, 5)

  w = v + sample(c(1,0,0,0,0))*abs_tol*1e-1

  expect_true(are_all_close(
    v, w, abs_tol = abs_tol, rel_tol = rel_tol
  ))
})

test_that("are_all_close_rel_false", {
  abs_tol = 1e-6
  rel_tol = 1e-6

  v = rep(1e0, 5)*rel_tol*1e-2
  w = v + sample(c(1,0,0,0,0))*rel_tol*1e-1

  expect_true(!are_all_close(
    v, w, abs_tol = abs_tol, rel_tol = rel_tol
  ))
})

test_that("are_all_close_abs_false", {

  abs_tol = 1e-6
  rel_tol = 1e-6

  v = rep(1e0, 5)*1e6
  w = v + sample(c(1,0,0,0,0))*abs_tol*1e2

  expect_true(!are_all_close(
    v, w, abs_tol = abs_tol, rel_tol = rel_tol
  ))
})
