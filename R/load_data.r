library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(bibliometrix)

#' Load tsv file from Web of Science
#'
#' This function assumes that all the files have been joined, and all quote marks removed
#' @param file: the tsv of WoS references
#' @keywords tsv 
#' @export
#' @examples
#' load_data()
 
load_data  <- function(file) {
    wos = readr::read_delim(file, trim_ws = TRUE, quote='"', delim='\t', col_names = TRUE)
    wos  <<-  unique(wos)
    return(wos)
}

#' Number of publications by year
#' @param wos: the dataframe of references
#' @keywords years
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @examples
#' year_count()

year_count  <- function(wos) {
    yrs  <- wos %>% select(PY) %>% count(PY, sort=TRUE) %>% arrange(PY)
    return(yrs)
}

#' Display top n first authors
#'
#' @param wos: the dataframe of references
#' @param n: the number of authors to return
#' @keywords authors
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr transmute
#' @importFrom tidyr separate
#' @importFrom tidyr gather
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_lower
#' @examples
#' top_authors()
 
top_authors  <- function(wos, n = 30) {
    authors_top  <- wos %>% select(AU, PY) %>%
        separate(AU, into = paste('au', 1:10, sep='_'),
                 sep=';', extra = 'drop', fill='right') %>%
        gather(key=AU, value = author, au_1:au_10, na.rm = TRUE) %>%
        transmute(author = str_trim(str_to_lower(author))) %>%  
        count(author, sort=TRUE)
    return(authors_top)
}

#' Retrieve count of title words
#'
#' @param wos: the dataframe of references
#' @keywords authors
#' @export
#' @importFrom dplyr anti_join 
#' @importFrom tidytext unnest_tokens
#' @import stringr
#' @examples
#' title_words()
 
title_words  <- function(wos) { 
    data('stop_words')
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

#' keywords from WoS DE field separated
#'
#' The function filters only for journal articles
#' @param wos: the dataframe of references
#' @keywords keyword
#' @export
#' @examples
#' keywords()

keywords  <- function(wos) {
    wos_keys  <- wos %>% select(TI,DT, PY,  DE) %>%
        filter('Article' %in% DT) %>% 
        separate(DE, into = paste('key', 1:10, sep='_'), sep=';', extra = 'drop', fill='right') %>%
        gather(key=key, value= keywd, key_1:key_10) %>%
        transform(keywd = str_trim(str_to_lower(keywd))) %>%  
        drop_na()
    return(wos_keys)
}


#' Count of top keywords
#'
#' @param wos: the dataframe of references
#' @keywords keyword
#' @export
#' @examples
#' keyword_count()

keyword_count  <- function(wos){
    key_count  <- keywords(wos) %>% select(PY, keywd) %>%
        count(keywd, sort=TRUE)
    return(key_count)
}

#' Top keyword counts
#'
#' @param wos: the dataframe of references
#' @param n: the number of keywords
#' @keywords keyword
#' @export
#' @examples
#' keyword_count_top()

keyword_count_top  <- function(wos,n=5){
    wos_key_count  <- keywords(wos) %>%
        select(TI, PY, keywd) %>%
        group_by(PY) %>% 
        count(keywd,  sort = TRUE)  %>%
        top_n(5) %>%
        filter(n > 1) %>%
        arrange(PY)
    return(wos_key_count)
}


#' Plot top keyword counts
#'
#' @param wos_key_count: the dataframe with keyword counts
#' @keywords keyword
#' @export
#' @examples
#' plot_keyword_count_top()

 plot_keyword_count_top  <- function(wos_key_count){
    ggplot(wos_key_count, aes(x=PY, y=n)) +
        geom_col() +
        facet_wrap(~keywd)
 }


#' Aggregate cited references 
#'
#' @param wos: the dataframe of references
#' @keywords citations
#' @export
#' @examples
#' cited_references_gather()

cited_references_gather  <- function(wos) {
    wos_refs  <- wos %>% select(TI,DT, PY,CR) %>%
        separate(CR, into = paste('ref', 1:40, sep='_'), sep=';', extra = 'drop', fill='right') %>%
        gather(key=r, value= ref, ref_1:ref_40) %>%
        transform(ref = str_trim(str_to_lower(ref))) %>%  
        drop_na()
    return(wos_refs)
}

#' Counted cited references
#'
#' @param wos: the dataframe of references
#' @keywords citations
#' @export
#' @examples
#' cited_reference_count()

cited_reference_count  <-  function(wos) {
    wos_refs_count  <- cited_references_gather %>% select(PY, ref) %>%
        group_by(PY) %>%
        count(ref, sort=TRUE) %>%
        top_n(20) %>%
        filter(n > 5) %>%
        arrange(PY)
    return(wos_refs_count)
}

