
#' Retrieve count of title words
#'
#' @param wos: the dataframe of references
#' @keywords authors
#' @export
#' @importFrom dplyr anti_join 
#' @importFrom tidytext unnest_tokens
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @examples
#' title_words()
 
title_words  <- function(wos) { 
    data('tidytext::stop_words')
    title_words  <- wos %>% select(TI, AU, PY) %>%
        unnest_tokens(word, TI) %>%
        mutate(word = str_extract(word, "[a-zA-Z']+")) %>% 
        anti_join(stop_words) %>%
        drop_na(word) %>% 
        count(word, sort=TRUE)
    return(title_words)
}

#' Count of title bigrams
#'
#' @param wos: the dataframe of references
#' @keywords title
#' @export
#' @import stringr
#' @examples
#' title_bigrams()

title_bigrams  <- function(wos) {
    title_bigrams  <-  wos %>% select(TI) %>%
        unnest_tokens(bigram, TI,  token='ngrams', n =2 )
    return(title_bigrams)
}
 
#' Title bigrams separated into two columns
#'
#' @param wos: the dataframe of references
#' @keywords title
#' @export
#' @import dplyr
#' @examples
#' title_bigrams_separated()

title_bigrams_separated  <- function(wos) {
    title_bigrams_separated  <-  title_bigrams(wos) %>%
        separate(bigram, c('word1', 'word2'), sep=' ')
    return(title_bigrams_separated)
}

#' Title bigrams separated and filtered for stopwords
#'
#' @param wos: the dataframe of references
#' @keywords title
#' @export
#' @import tidytext
#' @examples
#' title_bigrams_filtered()

title_bigrams_filtered  <- function(wos) {
    data('stop_words')
    bigrams_filtered <- title_bigrams_separated(wos) %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    return(bigrams_filtered)
}

#' Title bigrams sorted by frequency
#'
#' @param wos: the dataframe of references
#' @keywords title; bigram
#' @export
#' @examples
#' title_bigrams_count()

title_bigrams_count <- function(wos) {
    bigram_count  <-  title_bigrams_filtered(wos) %>% count(word1, word2, sort = TRUE)
    return(bigram_count )
}


