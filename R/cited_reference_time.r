
#' Plot top cited references over time 
#' @param wos: the dataframe of references
#' @param n the number of cited references to plot
#' @keywords citations
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import ggplot2
#' @examples
#' cited_references_time()

cited_references_time  <-  function(wos, topn = 20) {
    t = cited_reference_count(wos, year_grouped = FALSE)  
    tc = t %>% group_by(ref) %>% mutate(cu = cumsum(n))
    tct  <- tc %>%  group_by(PY) %>%
        top_n(n=10)

    ggplot(tct, aes(x=as.Date(paste0(PY,'-01-01')), y=n)) +
        geom_col() +
        facet_wrap(~ref) +
        scale_x_date()

}
