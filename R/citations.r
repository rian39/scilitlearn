#' List cited references in narrow form 
#' The key for this data frame is title and publication year 
#' @param wos: the dataframe of references
#' @keywords citations
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @examples
#' cited_references_gather()

cited_references_gather  <- function(wos) {
    wos_refs  <- wos %>% select(TI,DT, PY,CR) %>%
        separate(CR, into = paste('ref', 1:40, sep='_'), sep=';', extra = 'drop', fill='right') %>%
        gather(key=r, value= ref, ref_1:ref_40) %>%
        transform(ref = str_trim(str_to_lower(ref))) %>%  
        drop_na()
    return(wos_refs)
}

#' Count cited references
#' Returns the n top cited references 
#' @param wos: the dataframe of references
#' @param n: the number to return
#' @param year_grouped whether to group by year
#' @keywords citations
#' @export
#' @import dplyr
#' @examples
#' cited_reference_count()

cited_reference_count  <-  function(wos, n = 20, year_grouped = FALSE) {
    if (year_grouped) {
        wos_refs_count  <- cited_references_gather(wos) %>% select(PY, ref) %>%
            group_by(PY) %>%
            count(ref, sort=TRUE) %>%
            top_n(n) %>%
            arrange(PY)
    } else {
        wos_refs_count  <- cited_references_gather(wos) %>% select(PY, ref) %>%
            group_by(PY) %>%
            count(ref, sort=TRUE) %>%
            arrange(PY) %>%
            ungroup()
    }
    return(wos_refs_count)
}

