#' Segmenting hashtag with unigrams
#'
#' \code{unigram_sequence_segmentation} returns the data.frame containing hashtag,
#'   its segmented version, ids of dictionary words,
#'   number of words it have taken to segment the hashtag,
#'   total number of points, and computed score.
#'
#' This function is not intendend for long strings segmentation -
#'   140 characters should be considered too long
#'   and may take hours to complete. 15 characters takes about 0.02s,
#'   30 characters about 0.03s.
#'
#' @param sequences character vector, sequence to be segmented, either with '#' (e.g., hashtag)
#'   or without it.
#' @param dictionary data.frame, containing ids, words to search, words to use for segmentation, and their points. See details.
#' @param retrieve character vector of length 1, the type of the result data.frame to be returned:
#'   'all', 'first-shortest', 'most-pointed' or 'most-scored'.
#'   See value section.
#' @param simplify logical, if adjacent numbers should be merged into one,
#'   and underscores removed. See simplification section.
#' @param omit_zero logical, if words with 0 points should be omitted
#'   from word count. See simplification section.
#' @param score_formula character vector of length 1, with formula
#'   to calculate score.
#' @section Dictionary:
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
#'   By deafult segmented hashtag will be simplified,
#'   and numbers and underscores will be removed from word count
#'   for score computing, since they are neutral as they are necessary.
#' @return The output always will be data.frame. If \code{retrieve='all'}
#'   is used, then the return will include all possible segmentation
#'   of the given hashtag.\cr
#'   If \code{retrieve='first-shortest'} is used, the first of the shortest
#'   segmentations (with respect to the order of word's appearance
#'   in the dictionary, 1 row).\cr
#'   If \code{retrieve='most-pointed'} is used, segmentation with most total
#'   points is returned (1 row).\cr
#'   If \code{retrieve='most-scored'} is used, segmentation with the highest score
#'   calculated as
#'   \cr \eqn{score = points / words.number ^ 2} (or as specified by the user).
#' @examples
#'   unigram_sequence_segmentation('#thisisscience')
#'   unigram_sequence_segmentation('#this_is_science')
#'   unigram_sequence_segmentation('#thisisscience2020')
#'   unigram_sequence_segmentation('#thisisscience2020', simplify=FALSE, omit_zero=FALSE)
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
  sequences_list <- internal_unigram_sequence_segmentation(
    sequences,
    dictionary$to_search,
    dictionary$to_replace,
    dictionary$id,
    dictionary$points,
    omit_zero
    )
  sequences_df <- sequences_list_as_df(sequences_list)
  if (nrow(sequences_df) > 0) {
    sequences_df$score <- with(sequences_df,
                               eval(parse(text = score_formula)))
  }
  if (simplify) {
    sequences_df$segmented <- gsub("(?<=\\d) (?=\\d)",
                                   "",
                                   sequences_df$segmented,
                                   perl = TRUE)
    sequences_df$segmented <- gsub(" _ ",
                                   " ",
                                   sequences_df$segmented,
                                   fixed = TRUE)
  }

  sequences_df <- dplyr::mutate(dplyr::group_by(sequences_df, sequence),
                                to.second = .data$score / sort(.data$score,
                                                               TRUE)[2])

  sequences_df <- switch(
    retrieve,
    "all" = sequences_df,
    "first-shortest" = dplyr::slice(dplyr::group_by(sequences_df,
                                                    .data$sequence),
                                    which.min(.data$words.number)),
    "most-pointed" = dplyr::slice(dplyr::group_by(sequences_df, .data$sequence),
                                  which.max(.data$points)),
    "most-scored" = dplyr::slice(dplyr::group_by(sequences_df, .data$sequence),
                                 which.max(.data$score))
  )

  sequences_df <- as.data.frame(sequences_df)
  return(sequences_df)
}
