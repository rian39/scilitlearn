
#' discplines from WoS WC field separated
#'
#' The function filters only for journal articles
#' @param wos: the dataframe of references
#' @keywords discipline
#' @export
#' @import dplyr
#' @examples
#' keywords()

discplines  <- function(wos, with_id = FALSE){
    wos_keys  <- wos %>% select(PY, DT, WC, id=UT) %>%
        filter('Article' %in% DT) %>% 
        separate(WC, into = paste('disc', 1:3, sep='_'),
                 sep=';', extra = 'drop', fill='right') %>%
        gather(key=disc,  value= keywd, key_1:key_10) %>%
        transform(disc = str_trim(str_to_lower(disc))) %>%  
        group_by(id) %>%
        drop_na() %>%
        select(id, WC , PY)

    return(wos_keys)
}

