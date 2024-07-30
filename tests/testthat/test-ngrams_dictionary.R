library(testthat)
library(NUSS)

ndict_texts <- c("this is science",
           "science is #fascinatingthing",
           "this is a scientific approach",
           "science is everywhere",
           "the beauty of science")

ndict  <- data.frame(
  to_search = c("is", "science", "scienceis", "this", "thisis", "a", "approach", "ascientific", "ascientificapproach", "beauty", "beautyof", "beautyofscience", "everywhere", "isa", "isascientific", "isascientificapproach", "iseverywhere", "isscience", "of", "ofscience", "scienceiseverywhere", "scientific", "scientificapproach", "the", "thebeauty", "thebeautyof", "thebeautyofscience", "thisisa", "thisisascientific", "thisisascientificapproach", "thisisscience"),
  to_replace = c("is", "science", "science is", "this", "this is", "a", "approach", "a scientific", "a scientific approach", "beauty", "beauty of", "beauty of science", "everywhere", "is a", "is a scientific", "is a scientific approach", "is everywhere", "is science", "of", "of science", "science is everywhere", "scientific", "scientific approach", "the", "the beauty", "the beauty of", "the beauty of science", "this is a", "this is a scientific", "this is a scientific approach", "this is science"),
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
  points = c(4, 4, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

test_that("ngrams_dictionary works correctly", {
  expect_equal(
    NUSS::ngrams_dictionary(ndict_texts),
    ndict)
})
