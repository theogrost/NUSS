#' @importFrom utils "globalVariables"
globalVariables(c(".data"))
sequences_list_as_df <- function(texts_list) {
  if(length(texts_list[[1]])>0) {
    texts_df <- data.frame(sequence = texts_list[["text"]],
                           stringsAsFactors = FALSE)
    texts_df$segmented <- sapply(texts_list[["segmented"]],
                                 paste0,
                                 collapse = " ")
    texts_df$ids <- I(lapply(texts_list[["ids"]], function(e) e))
    texts_df$words.number <- sapply(texts_list[["ids"]], length)
    texts_df$words.length <- I(lapply(strsplit(texts_df$segmented, " "),
                                      function(e) nchar(e)))
    texts_df$points <- sapply(texts_list[["points"]], sum)
  } else {
    cnames <-  c("sequence",
                 "segmented",
                 "ids",
                 "words.number",
                 "words.length",
                 "points",
                 "score",
                 "to.second")
    texts_df <- data.frame(matrix(ncol = length(cnames), nrow = 0))
    colnames(texts_df) <- cnames
  }
  return(texts_df)
}

clean_texts <- function(texts) {
  texts <- textclean::replace_non_ascii(texts)
  texts <- tolower(texts)
  texts <- remove_urls(texts)
  texts <- remove_hashtags(texts)
  texts <- remove_users(texts)
  texts <- remove_emojis(texts)
  return(texts)
}

remove_urls <- function(texts) {
  texts <- gsub(" ?(f|ht)(tp)(s?)(://)[^ ]*", '', texts, perl = T)
  return(texts)
}

remove_hashtags <- function(texts) {
  texts <- gsub('#[a-zA-Z_0-9]+', '', texts, perl = T)
  return(texts)
}

remove_users <- function(texts) {
  texts <- gsub('@[a-zA-Z_0-9]+', '', texts, perl = T)
  return(texts)
}

remove_emojis <- function(texts) {
  texts <- gsub('\\p{So}|\\p{Cn}', '', texts, perl=T)
  return(texts)
}

re_uppercase <- function(lower, upper, ignore = c(" ", "-", "_")) {
  lower_split <- strsplit(lower, "", fixed = TRUE)
  upper_split <- strsplit(upper, "", fixed = TRUE)
  result <- sapply(seq_along(lower), function(i){
    cu_lo <- lower_split[[i]]
    cu_hi <- upper_split[[i]]
    cu_up <- grepl("[[:upper:]]", cu_hi[!cu_hi %in% ignore], perl = TRUE)
    if(any(cu_up) & !is.na(lower[i])) {
      cu_lo[!cu_lo%in%ignore][cu_up] <- toupper(cu_lo[!cu_lo%in%ignore][cu_up])
      return(paste0(cu_lo, collapse = ""))
    } else {
      return(lower[i])
    }
  })
  return(result)
}
