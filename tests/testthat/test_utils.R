
test_that("rebasing constants causes error", {
  expect_error(rebase.y(rep(1, 100), runif(100)), "y1 values constant when attempting rebase.")
  expect_error(rebase.y(runif(100), rep(1, 100)), "y2 values constant when attempting rebase.")
})

