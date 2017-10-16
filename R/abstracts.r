#' Add tot he stopword list 
#' @param wos: the dataframe of references
#' @param extra_stopwords: any extra stopwords to exclude
#' @keywords stopwords
#' @export

#' @import dplyr
#' @import tidytext
#' @examples
#' construct_stopword_list()

construct_stopword_list  <- function(extra_stopwords) { data(tidytext::stop_words)

    my_stopwords <- bind_rows(stop_words,
                              data_frame(word = c('communication', 'social', 'media',
                                                  'study', 'article', 'research',
                                                  'analysis', 'explores', 'scholars',
                                                  'paper', 'investigates',
                                                  'analyzes', 'examines', 'argue', 'argues', 
                                                  'analyses', extra_stopwords),
                                         lexicon= 'custom'))

    cat('adding stopwords: ', extra_stopwords, '\n')
    return(my_stopwords)
}

#' Topic models for abstracts
#' @param wos: the dataframe of references
#' @param extra_stopwords: any extra stopwords to exclude
#' @param topic_count: the numbers of topics; default is 20
#' @keywords abstracts
#' @export
#' @import dplyr
#' @import tidytext
#' @import topicmodels
#' @examples
#' abstract_topics()

abstract_topics  <- function(wos, topic_count = 20, extra_stopwords = '') {
    my_stopwords  <- construct_stopword_list(extra_stopwords)
    ab  <- wos %>% select(AB, UT) %>%
        mutate(id = UT) %>%
        na.omit() %>%
        unnest_tokens(word, AB) %>%
        anti_join(my_stopwords)

    word_count  <-  ab %>%
       anti_join(my_stopwords) %>%
       count(id, word, sort=TRUE) %>%
       ungroup()

    cat('constructing document term matrix ...\n')
    ab_dtm  <-  word_count %>%
       cast_dtm(id, word, n)
    
    cat('constructing topics models ...')

    ab_lda <- LDA(ab_dtm, k = topic_count, control = list(seed = 1234))
    tidy_lda  <- tidy(ab_lda)
    return(tidy_lda)
}


#' Plot topic models for abstracts
#' @param tidy_lda: a tidytext lda object
#' @keywords abstracts
#' @export
#' @import dplyr
#' @import tidytext
#' @import topicmodels
#' @import ggplot2
#' @examples
#' abstract_topics_plot()

abstract_topics_plot  <- function(tidy_lda){

    top_terms <- tidy_lda %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)

    top_terms

    top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      group_by(topic, term) %>%    
      arrange(desc(beta)) %>%  
      ungroup() %>%
      mutate(term = factor(paste(term, topic, sep = "__"), 
                           levels = rev(paste(term, topic, sep = "__")))) %>%
      ggplot(aes(term, beta, fill = as.factor(topic))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
      labs(title = "Top 10 terms in each LDA topic",
           x = NULL, y = expression(beta)) +
      facet_wrap(~ topic, ncol = 4, scales = "free")

}



