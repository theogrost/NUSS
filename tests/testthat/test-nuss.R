library(testthat)
library(NUSS)

seqs <- c("thisisscience", "thisisthis")

bdict_texts <- c("this is science",
                 "science is #fascinatingthing",
                 "this is a scientific approach",
                 "science is everywhere",
                 "the beauty of science")

result  <- data.frame(
  sequence = c( "thisisscience", "thisisthis" ),
  segmented = c( "this is science", "this is this" ),
  words.number = c(3, 3),
  points = c(1, 8),
  score = c(0.111111111111111, 0.888888888888889),
  to.second = as.numeric(c(NA, NA)),
  type = c("ngrams_segmentation", "unigram_sequence_segmentation")
)

test_that("nuss works correctly", {
  expect_equal(
    NUSS::nuss(seqs, bdict_texts),
    result)
})
