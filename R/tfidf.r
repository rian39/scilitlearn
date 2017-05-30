#' Frequencies of all words in abstracts
#'
#' @param wos: the dataframe of references
#' @param field: the field to count
#' @keywords words
#' @export
#' @import dplyr
#' @import tidytext
#' @examples
#' words_all()

words_all  <- function(wos, field='AB'){
    data('stop_words')
    wos_words <- wos %>% select(get(field), TI)  %>%
      unnest_tokens(word, AB) %>%
        anti_join(stop_words) %>%
      count(TI, word, sort = TRUE) %>%
      ungroup()

    total_words <- wos_words %>% 
      group_by(TI) %>% 
      summarize(total = sum(n))

    wos_words <- left_join(wos_words, total_words)
    return(wos_words)
}

#' Ranked frequencies of all words in abstracts
#'
#' @param wos: the dataframe of references
#' @param field: the field to count
#' @param plot: whether to plot the ranked frequencies 
#' @keywords words
#' @export
#' @import dplyr
#' @import tidytext
#' @import ggplot2
#' @examples
#' words_all_ranked_frequencies()

words_all_ranked_frequencies  <- function(wos, field = 'AB', plot = FALSE) {

    freq_by_rank <- words_all(wos, field) %>% 
      group_by(TI) %>% 
      mutate(rank = row_number(), 
         `term frequency` = n/total)

    if (plot == TRUE) {
        library(ggplot2)
        freq_by_rank %>% 
          ggplot(aes(rank, `term frequency`, color=TI)) + 
          geom_line(size = 0.5, alpha = 0.5) + 
          scale_x_log10() +
          scale_y_log10() + 
          theme(legend.position="none")
    }

    return(freq_by_rank)
}


#' Term Frequency-Inverse document frequency  of all words 
#'
#' @param wos:_words: the dataframe of word counts for each document
#' @keywords tfidf
#' @export
#' @import dplyr
#' @import tidytext
#' @examples
#' tfidf()

tfidf  <-  function(wos_words) {

    wos_words <-  wos_words %>%
      bind_tf_idf(word, TI, n) %>%
      select( -total) %>%
      arrange(desc(tf_idf))

    return(wos_words)
} 

#' Plot Term Frequency-Inverse document frequency  of all words 
#'
#' @param wos:_words: the dataframe of word counts for each document
#' @param terms: the number of terms to plot
#' @keywords tfidf
#' @export
#' @import dplyr
#' @import ggplot2
#' @import tidytext
#' @examples
#' tfidf_plot()

tfidf_plot  <- function(wos_words, terms = 20) {
    plot_wos  <-  wos_words %>%
        mutate(word = factor(word, levels = rev(unique(word))))

    ggplot(plot_wos[1:terms,], aes(word, tf_idf, fill = TI)) +
        geom_col() +
        labs(x = NULL, y = 'tf_idf') +
        coord_flip()
}

#' Count term frequency/inverse document frequencies
#' @param wos: the dataframe of references
#' @param field: field to construct tfidf matrix for
#' @param extra_stopwords: any extra stopwords to exclude
#' @keywords tfidf
#' @export
#' @import dplyr
#' @import tidytext
#' @examples
#' field_tfidf()

extra_stopwords = ''
field ='AB'

field_tfidf <- function(wos, field = 'AB',  extra_stopwords = '') {

    my_stopwords  <- construct_stopword_list(extra_stopwords)

    tf_idf <- wos %>% select(UT, get(field))  %>% 
        mutate(id = UT) %>%
        na.omit() %>%
        unnest_tokens(word, get(field)) %>%
        anti_join(my_stopwords) %>%
        count(id, word, sort = TRUE) %>%
        ungroup() %>%
        bind_tf_idf(word, id, n)

    return(tf_idf)
}
