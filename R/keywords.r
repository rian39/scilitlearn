

#' keywords from WoS DE field separated
#'
#' The function filters only for journal articles
#' @param wos: the dataframe of references
#' @keywords keyword
#' @export
#' @import dplyr
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
#' @param wos: the dataframe of references
#' @param n: the number of keywords
#' @keywords keyword
#' @export
#' @import dplyr
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
#' @import ggplot2
#' @examples
#' plot_keyword_count_top()

 plot_keyword_count_top  <- function(wos_key_count){
    ggplot(wos_key_count, aes(x=PY, y=n)) +
        geom_col() +
        facet_wrap(~keywd)
 }

