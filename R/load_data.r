library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
data('stop_words')

#' Load tsv file from Web of Science
#'
#' This function assumes that all the files have been joined, and all quote marks removed
#' @param file: the tsv of WoS references
#' @keywords 
#' @export
#' @examples
#' load_data()
 
load_data  <- function(file) {
    data('stop_words')
    wos = read_delim(file, trim_ws = TRUE, quote='"', delim='\t', col_names = TRUE)
    wos  <<-  unique(wos)
    return(wos)
}

#' Display top n first authors
#'
#' This function assumes that the author field only contains 1 author 
#' @param wos: the dataframe of references
#' @param n: the number of authors to return
#' @keywords authors
#' @export
#' @examples
#' top_authors()
 
top_authors  <- function(wos, n = 30) {
    wos %>% select(PY) %>% count(PY, sort=TRUE)
    authors_top  <- wos %>% select(AU, PY) %>%
        count(AU, sort=TRUE) %>%
        print(n=30)
    return(authors_top)
}


#' Retrieve count of title words
#'
#' @param wos: the dataframe of references
#' @keywords authors
#' @export
#' @examples
#' title_words()
 
title_words  <- function(wos) { 
    title_words  <- wos %>% select(TI, AU, PY) %>%
        unnest_tokens(word, TI) %>%
        mutate(word = str_extract(word, "[a-zA-Z']+")) %>% 
        anti_join(stop_words) %>%
        drop_na(word) %>% 
        count(word, sort=TRUE)
    return(title_words)
}

title_bigrams  <- function(wos) {
    title_bigrams  <-  wos %>% select(TI) %>%
        unnest_tokens(bigram, TI,  token='ngrams', n =2 )
    return(title_bigrams)
}

title_bigrams_separated  <- function(wos) {
    title_bigrams_separated  <-  title_bigrams(wos) %>%
        separate(bigram, c('word1', 'word2'), sep=' ')
    return(title_bigrams_separated)
}

title_bigrams_filtered  <- function(wos) {
    bigrams_filtered <- title_bigrams_separated(wos) %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    return(bigrams_filtered)
}

title_bigrams_count <- function(wos) {
    bigram_count  <-  bigrams_filtered(wos) %>% count(word1, word2, sort = TRUE)
    return(bigram_count )
}

## looking at keywords

wos_keys  <- wos %>% select(TI,DT, PY,  DE) %>%
    filter('Article' %in% DT) %>% 
    separate(DE, into = paste('key', 1:10, sep='_'), sep=';', extra = 'drop', fill='right') %>%
    gather(key=key, value= keywd, key_1:key_10) %>%
    transform(keywd = str_trim(str_to_lower(keywd))) %>%  
    drop_na()


wos_keys %>% select(PY, keywd) %>%
    filter(PY == '2012') %>%
    count(keywd, sort=TRUE)

wos_key_count  <- wos_keys %>%
    select(TI, PY, keywd) %>%
    group_by(PY) %>% 
    count(keywd,  sort = TRUE)  %>%
    top_n(5) %>%
    filter(n > 1) %>%
    arrange(PY)
wos_key_count

# Source: local data frame [148 x 3]
# Groups: PY [17]
# 
#       PY                           keywd     n
#    <int>                           <chr> <int>
# 1   2001                        internet     6
# 2   2001                           space     2
# 3   2002                        internet     7
# 4   2002                           china     3
# 5   2002                   communication     3
# 6   2002                   interactivity     3
# 7   2002                       new media     3
# 8   2003                        internet     7
# 9   2003                   internet cafe     3
# 10  2003 computer-mediated communication     2
# # ... with 138 more rows

View(wos_key_count)

ggplot(wos_key_count, aes(x=PY, y=n)) +
    geom_col() +
    facet_wrap(~keywd)

## cited references

wos_refs  <- wos %>% select(TI,DT, PY,CR) %>%
    separate(CR, into = paste('ref', 1:40, sep='_'), sep=';', extra = 'drop', fill='right') %>%
    gather(key=r, value= ref, ref_1:ref_40) %>%
    transform(ref = str_trim(str_to_lower(ref))) %>%  
    drop_na()
head(wos_refs)

wos_refs_count  <- wos_refs %>% select(PY, ref) %>%
    group_by(PY) %>%
    count(ref, sort=TRUE) %>%
    top_n(20) %>%
    filter(n > 5) %>%
    arrange(PY)
wos_refs_count

View(wos_refs_count)
