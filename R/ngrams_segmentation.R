#' Segmenting hashtag with n-grams.
#'
#' \code{ngrams_segmentation} returns the data.frame containing hashtag,
#'   its segmented version,
#'   number of words it have taken to segment the hashtag,
#'   total number of points, and computed score.
#'
#' @param sequences character vector, sequence to be segmented, either with '#' (e.g., hashtag)
#'   or without it.
#' @param texts data.frame, containing ids, words to search, words to use
#' for segmentation, and their points. See details.
#' @param clean character vector of length 1, the type of the result
#' data.frame to be returned:
#'  'all', 'first-shortest', 'most-pointed' or 'most-scored'.
#'  See value section.
#' @param ngram_min logical, if adjacent numbers should be merged into one,
#'   and underscores removed. See simplification section.
#' @param ngram_max logical, if words with 0 points should be omitted
#'   from word count. See simplification section.
#' @param retrieve character vector of length 1, with formula
#'   to calculate score.
#' @param simplify
#' @param omit_zero
#' @param score_formula
#'
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
#'   If \code{retrieve='most-scored'} is used, segmentation with the highest
#'   score calculated as \cr \eqn{score = points / words.number ^ 2}
#' @examples
#'   unigram_sequence_segmentation('#thisisscience')
#'   unigram_sequence_segmentation('#this_is_science')
#'   unigram_sequence_segmentation('#thisisscience2020')
#'   unigram_sequence_segmentation('#thisisscience2020', simplify=FALSE,
#'   omit_zero=FALSE)
#'
#' @export
ngrams_segmentation <- function(sequences,
                                texts,
                                clean = TRUE,
                                ngram_min = 1,
                                ngram_max = 5,
                                retrieve = "most-scored",
                                simplify = TRUE,
                                omit_zero = TRUE,
                                score_formula = "points / words.number ^ 2") {
  if(clean) {
    texts <- clean_texts(texts[!is.na(texts)&texts!=''])
  }
  lowercase_sequences <- gsub("^#", "", tolower(sequences))
  it_train <- text2vec::itoken(texts,
                               preprocessor = tolower,
                               tokenizer = text2vec::word_tokenizer,
                               ids = seq_along(texts),
                               progressbar = FALSE)
  texts_vocab <- text2vec::create_vocabulary(
    it_train,
    ngram = c(ngram_min = ngram_min,
              ngram_max = ngram_max))[, 1:2]
  ngrams_df <- data.frame(sequence = gsub('_', '', texts_vocab[, 1]),
                          segmented = gsub('_', ' ', texts_vocab[, 1]),
                          words.number = NA,
                          points = texts_vocab[, 2],
                          score = NA,
                          to.second = NA,
                          stringsAsFactors = FALSE)
  ngrams_df <- ngrams_df[order(-ngrams_df$points), ]
  ngrams_df <- ngrams_df[ngrams_df$sequence %in% lowercase_sequences, ]
  ngrams_df$words.number <- stringr::str_count(ngrams_df$segmented, ' ') + 1
  if (nrow(ngrams_df) > 0) {
    ngrams_df$score <- with(ngrams_df,
                            eval(parse(text = score_formula)))
  }
  if (simplify) {
    ngrams_df$segmented <- gsub("(?<=\\d) (?=\\d)",
                                "",
                                ngrams_df$segmented,
                                perl = TRUE)
    ngrams_df$segmented <- gsub(" _ ",
                                " ",
                                ngrams_df$segmented,
                                fixed = TRUE)
  }

  ngrams_df <- dplyr::mutate(dplyr::group_by(ngrams_df, sequence),
                             to.second = .data$score / sort(.data$score,
                                                            TRUE)[2])

  ngrams_df <- switch(
    retrieve,
    "all" = ngrams_df,
    "first-shortest" = dplyr::slice(dplyr::group_by(ngrams_df,
                                                    .data$sequence),
                                    which.min(.data$words.number)),
    "most-pointed" = dplyr::slice(dplyr::group_by(ngrams_df, .data$sequence),
                                  which.max(.data$points)),
    "most-scored" = dplyr::slice(dplyr::group_by(ngrams_df, .data$sequence),
                                 which.max(.data$score))
  )

  ngrams_df <- as.data.frame(ngrams_df)
  # if(simplify) {
  #   ngrams_df$segmented <- gsub("(?: [0-9]+)\\K (?=[0-9]+ )", "",
  #                               ngrams_df$segmented, perl = TRUE)
  # }
  return(ngrams_df)
}
