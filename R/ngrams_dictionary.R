#' Create n-grams dictionary
#'
#' \code{ngrams_dictionary} returns the data.frame containing dictionary for
#' \link{ngrams_segmentation}.
#'
#' @param texts character vector, these are the texts used to create n-grams
#' dictionary. Case-sensitive.
#' @param clean logical, indicating if the texts should be cleaned before
#' creating n-grams dictionary.
#' @param ngram_min numeric, sets the minimum number of words in creating
#' the dictionary.
#' @param ngram_max numeric, sets the maximum number of words in creating
#' the dictionary.
#' @param points_filter numeric, sets the minimal number of points (occurrences)
#' of an n-gram to be included in the dictionary.
#'
#' @return The output always will be data.frame with 4 columns: 1) to_search,
#' 2) to_replace, 3) id, 4) points.
#' @examples
#' texts <- c("this is science",
#'            "science is #fascinatingthing",
#'            "this is a scientific approach",
#'            "science is everywhere",
#'            "the beauty of science")
#' ngrams_dictionary(texts)
#' ngrams_dictionary(texts,
#'                   clean = FALSE)
#' ngrams_dictionary(texts,
#'                   clean = TRUE,
#'                   ngram_min = 2,
#'                   ngram_max = 2)
#'
#' @export
ngrams_dictionary <- function(texts,
                              clean = TRUE,
                              ngram_min = 1,
                              ngram_max = 5,
                              points_filter = 1) {
  if(clean) {
    texts <- clean_texts(texts[!is.na(texts) & texts!=''])
  }
  it_train <- text2vec::itoken(texts,
                               preprocessor = identity,
                               tokenizer = text2vec::word_tokenizer,
                               id = seq_along(texts),
                               progressbar = FALSE)
  texts_vocab <- text2vec::create_vocabulary(
    it_train,
    ngram = c(ngram_min = ngram_min,
              ngram_max = ngram_max))[, 1:2]
  ngrams_df <- data.frame(to_search = gsub("[^a-zA-Z0-9]",
                                           '',
                                           texts_vocab[, 1]),
                          to_replace = gsub('_',
                                            ' ',
                                            texts_vocab[, 1]),
                          id = NA,
                          points = texts_vocab[, 2])
  ngrams_df <- ngrams_df[ngrams_df$points >= points_filter,]
  ngrams_df <- ngrams_df[order(-ngrams_df$points, ngrams_df$to_search),]
  ngrams_df$id = seq_len(nrow(ngrams_df))
  rownames(ngrams_df) <- ngrams_df$id
  return(ngrams_df)
}
