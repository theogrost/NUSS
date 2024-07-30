#' Segmenting sequences with unigrams
#'
#' \code{unigram_sequence_segmentation} segments input sequence into possible segmented
#' text based on unigram sequence segmentation approach.
#'
#' This function is not intendend for long strings segmentation -
#'   70 characters should be considered too long
#'   and may take hours to complete. 15 characters takes about 0.02s,
#'   30 characters about 0.03s.
#'
#' @param sequences character vector, sequence to be segmented
#' (e.g., hashtag). Case-sensitive.
#' @param dictionary data.frame, containing ids, words to search, words to use
#' for segmentation, and their points. See details.
#' @param retrieve character vector of length 1, the type of the result
#' data.frame to be returned: 'all', 'first-shortest', 'most-pointed' or
#' 'most-scored'. See value section.
#' @param simplify logical, if adjacent numbers should be merged into one,
#'   and underscores removed. See simplification section.
#' @param omit_zero logical, if words with 0 points should be omitted
#'   from word count. See simplification section.
#' @param score_formula character vector of length 1, with formula
#'   to calculate score.
#' @section unigram_dictionary:
#' Dictionary has to be data.frame with four named columns: 1) to_search,
#' 2) to_replace, 3) id, 4) points.\cr
#'   'to_search' should be column of type character, containing unigram to
#'   look for. Word case might be used.\cr
#'   'to_replace' should be column of type character, containing word that
#'   should be used for creating segmentation vector, if 'to_search' matches
#'   text. \cr
#'   'id' should be column of type numeric, containing id of unigram.\cr
#'   'points' should be column of type numeric, containing number of points
#'   for the word - the higher, the better. Unigrams with 0 points might be
#'   removed from the wordcount with omit_zero argument.
#' @section Simplification:
#'   Two arguments are possible for simplification:\cr
#'   * simplify - removes spaces between numbers and removes underscores,\cr
#'   * omit_zero - removes ids of 0-pointed unigrams,
#'   and omits them in the word count.\cr
#'   By deafult segmented sequence will be simplified,
#'   and numbers and underscores will be removed from word count
#'   for score computing, since they are neutral as they are necessary.
#' @return The output always will be data.frame. If \code{retrieve='all'}
#'   is used, then the return will include all possible segmentation
#'   of the given sequence.\cr
#'   If \code{retrieve='first-shortest'} is used, the first of the shortest
#'   segmentations (with respect to the order of word's appearance
#'   in the dictionary, 1 row).\cr
#'   If \code{retrieve='most-pointed'} is used, segmentation with most total
#'   points is returned (1 row).\cr
#'   If \code{retrieve='most-scored'} is used, segmentation with the highest
#'   score calculated as
#'   \cr \eqn{score = points / words.number ^ 2} (or as specified by the user).
#'   \cr **The output is not in the input order. If needed, use
#'   \link[base]{lapply}**
#'
#' @examples
#' # With custom dictionary
#' texts <- c("this is science",
#'            "science is #fascinatingthing",
#'            "this is a scientific approach",
#'            "science is everywhere",
#'            "the beauty of science")
#' udict <- unigram_dictionary(texts)
#' unigram_sequence_segmentation('thisisscience', udict)
#'
#' # With built-in dictionary (English, only lowercase)
#' unigram_sequence_segmentation('thisisscience')
#' unigram_sequence_segmentation('thisisscience2024')
#' unigram_sequence_segmentation('thisisscience2024', simplify=FALSE, omit_zero=FALSE)
#'
#' @importFrom magrittr "%>%"
#' @export
unigram_sequence_segmentation <- function(
    sequences,
    dictionary = NUSS::base_dictionary,
    retrieve = "most-scored",
    simplify = TRUE,
    omit_zero = TRUE,
    score_formula = "points / words.number ^ 2") {
  result <- internal_unigram_sequence_segmentation(
    sequences,
    dictionary$to_search,
    dictionary$to_replace,
    dictionary$id,
    dictionary$points,
    omit_zero
    )
  result <- sequences_list_as_df(result)
  if (nrow(result) > 0) {
    result$score <- with(result, eval(parse(text = score_formula)))
  }
  if (simplify) {
    result$segmented <- gsub("(?<=\\d) (?=\\d)",
                             "",
                             result$segmented,
                             perl = TRUE)
    result$segmented <- gsub(" _ ",
                             " ",
                             result$segmented,
                             fixed = TRUE)
  }

  result <- result %>%
    dplyr::group_by(.data$sequence) %>%
    dplyr::mutate(to.second = .data$score / sort(.data$score, TRUE)[2]) %>%
    dplyr::ungroup()

  result <- switch(
    retrieve,
    "all" = result,
    "first-shortest" = result %>%
      dplyr::group_by(.data$sequence) %>%
      dplyr::slice(which.min(.data$words.number)) %>%
      dplyr::ungroup(),
    "most-pointed" = result %>%
      dplyr::group_by(.data$sequence) %>%
      dplyr::slice(which.max(.data$points)) %>%
      dplyr::ungroup(),
    "most-scored" = result %>%
      dplyr::group_by(.data$sequence) %>%
      dplyr::slice(which.max(.data$score)) %>%
      dplyr::ungroup()
  )

  result <- as.data.frame(result)

  return(result)
}
