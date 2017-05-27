#' Frequencies of all words in abstracts
#'
#' @param wos: the dataframe of references
#' @param term: the terms to search for a vector; this can be a regex
#' @keywords words
#' @export
#' @import dplyr
#' @import tidytext
#' @examples
#' words_all()

words_all  <- function(wos, field='AB'){
    data('stop_words')
    wos_words <- wos %>% select(AB, TI)  %>%
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
#' @param plot: whether to plot the ranked frequencies 
#' @keywords words
#' @export
#' @import dplyr
#' @import tidytext
#' @import ggplot2
#' @examples
#' words_all_ranked_frequencies()

words_all_ranked_frequencies  <- function(wos, plot = FALSE) {

    freq_by_rank <- words_all(wos) %>% 
      group_by(TI) %>% 
      mutate(rank = row_number(), 
         `term frequency` = n/total)

    if (plot == TRUE) {
        library(ggplot2)
        freq_by_rank
        freq_by_rank %>% 
          ggplot(aes(rank, `term frequency`, color=TI)) + 
          geom_line(size = 0.5, alpha = 0.5) + 
          scale_x_log10() +
          scale_y_log10() + 
          theme(legend.position="none")
    }

    return(freq_by_rank)
}

