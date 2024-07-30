#' Create unigram dictionary
#'
#' \code{unigram_dictionary} returns the data.frame containing dictionary for
#' \link{unigram_sequence_segmentation}.
#'
#' @param texts character vector, these are the texts used to create ngrams
#' dictionary. Case-sensitive.
#' @param points_filter numeric, sets the minimal number of points (occurrences)
#' of an unigram to be included in the dictionary.
#'
#' @return The output always will be data.frame with 4 columns: 1) to_search,
#' 2) to_replace, 3) id, 4) points.
#' @examples
#' texts <- c("this is science",
#'            "science is #fascinatingthing",
#'            "this is a scientific approach",
#'            "science is everywhere",
#'            "the beauty of science")
#' unigram_dictionary(texts)
#'
#' @export
unigram_dictionary <- function(texts,
                               points_filter = 1) {
  return(ngrams_dictionary(texts,
                           clean = TRUE,
                           ngram_min = 1,
                           ngram_max = 1,
                           points_filter = points_filter))
}
