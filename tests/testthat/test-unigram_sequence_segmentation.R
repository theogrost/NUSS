library(testthat)
library(NUSS)

dictionary <- data.frame(
  to_search = c("this",
                "is",
                "science",
                "unnecessary",
                "word",
                "example",
                "test",
                "data",
                "analysis",
                "sci",
                "ence",
                "2",
                "0",
                "1",
                "4"),
  to_replace = c("this",
                "is",
                "science",
                "unnecessary",
                "word",
                "example",
                "test",
                "data",
                "analysis",
                "sci",
                "ence",
                "2",
                "0",
                "1",
                "4"),
  id = 1:15,
  points = c(10, 10, 30, 10, 10, 10, 10, 10, 10, 0, 0, 0, 0, 0, 0),
  stringsAsFactors = FALSE
)

test_that("unigram_sequence_segmentation works correctly", {
  # Test case 1: Basic functionality
  result <- unigram_sequence_segmentation("thisisscience",
                                          dictionary)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(result$segmented[1], "this is science")

  # Test case 2: With simplify = FALSE and omit_zero = FALSE
  result <- unigram_sequence_segmentation("thisisscience2024",
                                          dictionary,
                                          simplify = FALSE,
                                          omit_zero = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(result$segmented[1], "this is science 2 0 2 4")

  # Test case 3: Return type 'all'
  result <- unigram_sequence_segmentation("thisisscience",
                                          dictionary,
                                          retrieve = "all")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 1)

  # Test case 4: Return type 'first-shortest'
  result <- unigram_sequence_segmentation("thisisscience",
                                          dictionary,
                                          retrieve = "first-shortest")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Test case 5: Return type 'most-pointed'
  result <- unigram_sequence_segmentation("thisisscience",
                                          dictionary,
                                          retrieve = "most-pointed")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Test case 6: Return type 'most-scored'
  result <- unigram_sequence_segmentation("thisisscience",
                                          dictionary,
                                          retrieve = "most-scored")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Test case 7: Return empty row
  result <- unigram_sequence_segmentation("ThisIsScience",
                                          dictionary,
                                          retrieve = "most-scored")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
