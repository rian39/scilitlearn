
#' Display top n first authors
#'
#' @param wos: the dataframe of references
#' @param n: the number of authors to return
#' @keywords authors
#' @export
#' @importFrom tidyr gather
#' @import stringr 
#' @import dplyr
#' @examples
#' authors_top()
 
authors_top <- function(wos, n = 30) {
    authors_top  <- wos %>% select(AU, PY) %>%
        separate(AU, into = paste('au', 1:10, sep='_'),
                 sep=';', extra = 'drop', fill='right') %>%
        gather(key=AU, value = author, au_1:au_10, na.rm = TRUE) %>%
        transmute(author = str_trim(str_to_lower(author))) %>%  
        count(author, sort=TRUE)
    return(authors_top)
}
