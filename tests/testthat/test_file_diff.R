test_that("file diff 2 different files returns TRUE", {
  set.sodd.options(
    data.dir = "~/sodd.data/",
    verbosity = 0
  )
  expect_silent(dload.league.season("E0", "1920", quiet = TRUE))
  expect_silent(dload.league.season("E0", "2021", quiet = TRUE))
  expect_equal(
    check.file.diff(
      paste0(get.sodd.data.dir(), "1920/E0.csv"),
      paste0(get.sodd.data.dir(), "2021/E0.csv")
    ),
    TRUE
  )
})
test_that("file diff same file returns FALSE", {
  expect_equal(
    check.file.diff(
      paste0(get.sodd.data.dir(), "2021/E0.csv"),
      paste0(get.sodd.data.dir(), "2021/E0.csv")
    ),
    FALSE
  )
})
test_that("file diff same file dloaded twice returns FALSE", {
  expect_silent(dload.league.season("E0", "2021", quiet = TRUE))
  tmpfile0 <- tempfile()
  file.copy(paste0(get.sodd.data.dir(), "2021/E0.csv"), tmpfile0)
  expect_silent(dload.league.season("E0", "2021", quiet = TRUE))
  tmpfile1 <- tempfile()
  file.copy(paste0(get.sodd.data.dir(), "2021/E0.csv"), tmpfile1)
  expect_equal(
    (tmpfile0 == tmpfile1),
    FALSE
  )
  expect_equal(file.exists(tmpfile0), TRUE)
  expect_equal(file.exists(tmpfile1), TRUE)
  expect_equal(check.file.diff(tmpfile0, tmpfile1), FALSE)
})
