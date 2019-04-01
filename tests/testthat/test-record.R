testthat::context("See if we can record the screen")

testthat::test_that("Simple recording", {
  out = start_screen_record(audio = FALSE)
  Sys.sleep(4)
  res = end_screen_record(out$pid)
  testthat::expect_true(file.exists(out$outfile))
  fz = file.size(out$outfile)
  testthat::expect_true(fz > 50000)


})
