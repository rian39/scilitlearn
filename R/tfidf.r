
library(dplyr)
library(janeaustenr)
library(tidytext)

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

View(words_all(wos))

freq_by_rank <- wos_words %>% 
  group_by(TI) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

library(ggplot2)
freq_by_rank
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color=TI)) + 
  geom_line(size = 0.5, alpha = 0.5) + 
  scale_x_log10() +
  scale_y_log10() + 
  theme(legend.position="none")


rank_subset <- freq_by_rank %>% 
  filter(rank < 50,
         rank > 5)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = TI)) + 
  geom_abline(intercept = -1.46, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 0.5, alpha = 0.5) + 
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position="none")
