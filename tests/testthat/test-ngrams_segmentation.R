library(testthat)
library(NUSS)

test_that("igrepl matches patterns in string correctly", {
  expect_equal(
    NUSS::igrepl(c("today",
                   "b.* fox",
                   "jumps over",
                   "vigorous"),
                 "The quick brown fox jumps over the lazy dog", FALSE),
    c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(
    NUSS::igrepl(c("today",
                   "brown fox",
                   "jumps over",
                   "vigorous"),
                 "The quick brown fox jumps over the lazy dog", TRUE),
    c(FALSE, TRUE, TRUE, FALSE))
})
