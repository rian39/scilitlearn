library(tidytext)
library(ggplot2)
library(topicmodels)
library(dplyr)

#wos = load_data('~/R/scilitlearn/sample.tsv')

#' Topic models for abstracts
#' @param wos: the dataframe of references
#' @keywords abstracts
#' @export
#' @import dplyr
#' @import tidytext
#' @import topicmodels
#' @examples
#' abstract_topics()

abstract_topics  <- function(wos, topic_count = 20) {
    data(stop_words)

    my_stopwords <- bind_rows(stop_words,
                              data_frame(word = c('communication', 'social', 'media',
                                                  'study', 'article', 'research',
                                                  'analysis', 'explores', 'scholars',
                                                  'paper'),
                                         lexicon= 'custom'))

   ab  <- wos %>% select(AB, ID) %>%
        mutate(id = ID) %>%
       unnest_tokens(word, AB) %>%
       anti_join(my_stopwords)

   word_count  <-  ab %>%
       anti_join(my_stopwords) %>%
       count(id, word, sort=TRUE) %>%
       ungroup()

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

abstract_tfidf  <- function(wos) {

        ab_tf_idf <- ab  %>% 
          count(id, word, sort = TRUE) %>%
          ungroup() %>%
          bind_tf_idf(word, id, n)

    ab_tf_idf
    abstract_topics(wos)
}
