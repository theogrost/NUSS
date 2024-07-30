library(testthat)
library(NUSS)

udict_texts <- c("this is science",
                 "science is #fascinatingthing",
                 "this is a scientific approach",
                 "science is everywhere",
                 "the beauty of science")

udict  <- data.frame(
  to_search = c("is", "science", "this", "a", "approach", "beauty", "everywhere", "of", "scientific", "the"),
  to_replace = c("is", "science", "this", "a", "approach", "beauty", "everywhere", "of", "scientific", "the"),
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  points = c(4, 4, 2, 1, 1, 1, 1, 1, 1, 1)
)

test_that("unigram_dictionary works correctly", {
  expect_equal(
    NUSS::unigram_dictionary(udict_texts),
    udict)
})
