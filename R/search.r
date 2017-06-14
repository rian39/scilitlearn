
#' Search title, abstract and keywords for terms
#'
#' The search is run across all relevant fields 
#' @param wos: the dataframe of references
#' @param term: the terms to search for a vector; this can be a regex
#' @keywords search
#' @param view: show in Viewer
#' @export
#' @import dplyr
#' @examples
#' search_title_abstract_keywords(wos, terms)


#library(scilitlearn)
#library(dplyr)
#wos  <- load_data('../sample.tsv')
#terms = c('digital divide', 'Google')
#terms = 'facebook|google|twitter|network.*'
#res

search_title_abstract_keywords  <- function(wos, terms, view = FALSE) {

    terms = paste(terms,  collapse='|')
    res  <-  wos %>%
        filter(grepl(TI, pattern =  terms, ignore.case = TRUE),
               grepl(AB, pattern =  terms, ignore.case = TRUE),
               grepl(DE, pattern =  terms, ignore.case = TRUE)) 
    if (view) {
        View(res)
    }
    return(res)
}
