
#' Search title, abstract and keywords for terms
#'
#' The search is run across all relevant fields 
#' @param wos: the dataframe of references
#' @param term: the terms to search for a vector; this can be a regex
#' @param view: show in Viewer
#' @param format_as_bibtex: return a single BibTex citation string [@A_2010; @B_2012]
#' @keywords search
#' @export
#' @import dplyr
#' @examples
#' search_term()


#library(scilitlearn)
#library(dplyr)
#wos  <- load_data('../sample.tsv')
#terms = c('digital divide', 'Google')
#terms = 'facebook|google|twitter|network.*'
#res

search_term  <- function(wos, terms, view = FALSE, format_as_bibtex = FALSE) {

    terms = paste(terms,  collapse='|')
    res  <-  wos %>%
        filter(grepl(TI, pattern =  terms, ignore.case = TRUE),
               grepl(AB, pattern =  terms, ignore.case = TRUE),
               grepl(DE, pattern =  terms, ignore.case = TRUE)) %>%
        arrange(PY,AU)
    if (view) {
        View(res)
    }
    if (format_as_bibtex) {
       return(latex_format(res, single_ref = TRUE)) 
    } else {
        return(res)
    }
}

