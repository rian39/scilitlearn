#' keywords from WoS DE field separated
#'
#' The function filters only for journal articles
#' @param wos: the dataframe of references
#' @keywords keyword
#' @export
#' @import dplyr
#' @examples
#' keywords()

keywords  <- function(wos, with_id = FALSE){
    wos_keys  <- wos %>% select(PY, DT, DE, id=UT) %>%
        filter('Article' %in% DT) %>% 
        separate(DE, into = paste('key', 1:10, sep='_'),
                 sep=';', extra = 'drop', fill='right') %>%
        gather(key=key,  value= keywd, key_1:key_10) %>%
        transform(keywd = str_trim(str_to_lower(keywd))) %>%  
        group_by(id) %>%
        drop_na() %>%
        select(id, keywd, PY)

    return(wos_keys)
}


#' Count of top keywords
#'
#' @param wos: the dataframe of references
#' @keywords keyword
#' @import dplyr
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
#' The keyword counts are grouped by publication year. When keywords are tied in their raking, they are all returned. 
#' @param wos: the dataframe of references
#' @param top_n: the number of keywords
#' @keywords keyword
#' @export
#' @import dplyr
#' @examples
#' keyword_count_top()

keyword_count_top  <- function(wos,top_n=5){
    wos_key_count  <- keywords(wos) %>%
        select(PY, keywd) %>%
        group_by(PY) %>% 
        count(keywd,  sort = TRUE)  %>%
        top_n(top_n) %>%
        filter(n > 1) %>%
        arrange(PY)
    return(wos_key_count)
}


#' Plot top keyword counts
#'
#' @param wos_key_count: the dataframe with keyword counts
#' @param top_n: the number of keywords
#' @keywords keyword
#' @export
#' @import ggplot2
#' @examples
#' keyword_count_top_plot()

 keyword_count_top_plot  <- function(wos, top_n = 5){
    wos_key_count  <-  keyword_count_top(wos, top_n)
    ggplot(wos_key_count, aes(x=PY, y=n)) +
        geom_col() +
        facet_wrap(~ keywd)
 }

