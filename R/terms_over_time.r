#' Plot occurrence of terms from TI, DE or some other field
#' over time. 
#'
#' @param wos: the dataframe of references
#' @param field: individual field to use
#' @param combine: combine all relevant fields
#' @param plt: whether to plot of not
#' @param terms_to_plot: the top or lowest n terms
#' @keywords time
#' @export
#' @import dplyr
#' @import lubridate
#' @import tidyverse
#' @import purrr
#' @import tidyr
#' @import broom
#' @import stringr
#' @import tidytext
#' @examples
#' terms_over_time()

terms_over_time  <-  function(wos, field = 'TI', combine = TRUE, plt = TRUE, terms_to_plot = 16) {

    #field='TI'
    # join all relevant fields into TI
    if (combine)
        wos  <- wos %>% mutate(TI = tolower(paste(TI, AB, DE)))

    field_words <- wos %>%
      arrange(desc(TC))

    if (field == 'TI') {
          field_words  <- field_words %>%
              distinct(TI, .keep_all = TRUE) %>%
              unnest_tokens(word, TI, drop = FALSE) %>%
              distinct(UT, word, .keep_all = TRUE)
    }

    if (field == 'AB') {
          field_words  <- field_words %>%
              distinct(AB, .keep_all = TRUE) %>%
              unnest_tokens(word, AB, drop = FALSE) %>%
              distinct(UT, word, .keep_all = TRUE)
    }

    if (field == 'DE') {
          field_words  <- field_words %>%
              distinct(DE, .keep_all = TRUE) %>%
              unnest_tokens(word, DE, drop = FALSE) %>%
              distinct(UT, word, .keep_all = TRUE)
    }

    field_words  <-  field_words %>%
      anti_join(stop_words, by = "word") %>%
      filter(str_detect(word, "[^\\d]")) %>%
      group_by(word) %>%
      mutate(word_total = n()) %>%
      ungroup()

    word_counts <- field_words %>%
      count(word, sort = TRUE)

    wos_per_month  <- wos %>%
      group_by(date) %>%
      summarize(month_total = n())

    word_month_counts <- field_words %>%
      filter(word_total >= 5) %>%
      count(word, date) %>%
      complete(word, date, fill = list(n = 0)) %>%
      inner_join(wos_per_month, by = "date") %>%
      mutate(percent = n / month_total) %>%
      mutate(year = year(date) + yday(date) / 365)

    mod <- ~ glm(cbind(n, month_total - n) ~ year, ., family = "binomial")

    slopes <- word_month_counts %>%
      nest(-word) %>%
      mutate(model = map(data, mod)) %>%
      unnest(map(model, tidy)) %>%
      filter(term == "year") %>%
      arrange(desc(estimate))

    words_to_plot  <- slopes %>% top_n(terms_to_plot, wt = estimate) %>% select(word)
    plot_data  <- word_month_counts %>% filter(word %in% words_to_plot$word)

    if (plt) {
        g  <- ggplot(plot_data, aes(x=date, y=percent)) +
          geom_line(aes(color = word), show.legend = FALSE) +
          expand_limits(y = 0) +
          facet_wrap(~ word, scales = "free_y")
      return(g)
    } 

  return(word_month_counts)
}

#' Plot  terms from TI, DE or some other field that have peakdd
#' over time. 
#'
#' @param wos: the dataframe of references
#' @param field: individual field to use
#' @param combine: combine all relevant fields
#' @param terms_to_plot: the top or lowest n terms
#' @keywords time
#' @export
#' @import dplyr
#' @import lubridate
#' @import tidyverse
#' @import purrr
#' @import tidyr
#' @import broom
#' @import stringr
#' @import tidytext
#' @examples
#' peaked_terms_monthly()

peaked_terms_monthly  <-  function(wos, field = 'TI', combine = FALSE, terms_to_plot = 16) {
        library(splines)

    word_month_counts  <-  terms_over_time(wos, field, combine, plt = FALSE,  terms_to_plot) 
    mod2 <- ~ glm(cbind(n, month_total - n) ~ ns(year, 4), ., family = "binomial")

    # Fit a cubic spline to each shape
    spline_predictions <- word_month_counts %>%
      mutate(year = as.integer(as.Date(date)) / 365) %>%
      nest(-word) %>%
      mutate(model = map(data, mod2)) %>%
      unnest(map2(model, data, augment, type.predict = "response"))

    # Find the terms with the highest peak / average ratio
    peak_per_month <- spline_predictions %>%
      group_by(word) %>%
      mutate(average = mean(.fitted)) %>%
      top_n(1, .fitted) %>%
      ungroup() %>%
      mutate(ratio = .fitted / average) %>%
      filter(date != min(date), date != max(date)) %>%
      top_n(16, ratio)

    peak_per_month %>%
      select(word, peak = date) %>%
      inner_join(spline_predictions, by = "word") %>%
      mutate(word = reorder(word, peak)) %>%
      ggplot(aes(date, percent)) +
      geom_line(aes(color = word), show.legend = FALSE) +
      geom_line(aes(y = .fitted), lty = 2) +
      facet_wrap(~ word, scales = "free_y") +
      #scale_y_continuous(labels = percent_format()) +
      expand_limits(y = 0) +
      labs(x = "Year",
           y = "Percent of titles containing this term",
           title = "16 words that peaked then declined",
           subtitle = "Spline fit (df = 4) shown.\nSelected based on the peak of the spline divided by the overall average; ordered by peak month.")

}

#peak_terms_monthly(wos)
