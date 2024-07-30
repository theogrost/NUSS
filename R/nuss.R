#' Mixed N-Grams and Unigram Sequence Segmentation (NUSS) function
#'
#' \code{nuss} returns the data.frame containing
#' hashtag, its segmented version, ids of dictionary words,
#' number of words it have taken to segment the hashtag,
#' total number of points, and computed score.
#'
#' This function is an arbitrary combination of \link{ngrams_dictionary},
#' \link{unigram_dictionary}, \link{ngrams_segmentation},
#' \link{unigram_sequence_segmentation}, created to easily segment short texts
#' based on text corpus.
#'
#' @param sequences character vector, sequence to be segmented,
#' (e.g., hashtag) or without it. Case-insensitive.
#' @param texts character vector, these are the texts used to create n-grams
#' and unigram dictionary. Case-insensitive.
#'
#' @return The output always will be data.frame with sequences, that were
#'   \cr **The output is not in the input order. If needed, use
#'   \link[base]{lapply}**
#'
#' @examples
#' texts <- c("this is science",
#'            "science is #fascinatingthing",
#'            "this is a scientific approach",
#'            "science is everywhere",
#'            "the beauty of science")
#' nuss(c("thisisscience", "scienceisscience"), texts)
#'
#' @importFrom magrittr "%>%"
#' @export
nuss <- function(sequences,
                 texts) {
  sequences <- tolower(sequences)
  texts <- tolower(texts)
  ndict <- NUSS::ngrams_dictionary(texts)
  udict <- NUSS::unigram_dictionary(texts)
  ngrams_segmented <- NUSS::ngrams_segmentation(sequences,
                                                ndict) %>%
    dplyr::select("sequence",
                  "segmented",
                  "words.number",
                  "points",
                  "score",
                  "to.second") %>%
    dplyr::mutate(type = "ngrams_segmentation")
  remaining_sequences <- sequences[!(sequences %in% ngrams_segmented$sequence)]

  unigram_sequence_segmented <- NUSS::unigram_sequence_segmentation(
    remaining_sequences,
    udict
    ) %>%
    dplyr::select("sequence",
                  "segmented",
                  "words.number",
                  "points",
                  "score",
                  "to.second") %>%
    dplyr::mutate(type = "unigram_sequence_segmentation")

  result <- rbind(ngrams_segmented, unigram_sequence_segmented)
  return(result)
}
