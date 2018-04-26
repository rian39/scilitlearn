
#' discplines from WoS WC field separated
#'
#' The function filters only for journal articles
#' @param wos: the dataframe of references
#' @keywords discipline
#' @export
#' @import dplyr
#' @examples
#' disciplines()

disciplines  <- function(wos, with_id = FALSE){
    wos_keys  <- wos %>% select(PY,  WC, id=UT) %>%
        separate(WC, into = paste('disc', 1:3, sep='_'),
                 sep=';', extra = 'drop', fill='right') %>%
        gather(key=disc,  value= keywd, disc_1:disc_3) %>%
        drop_na() 
    wos_keys  <- wos_keys %>% mutate(keywd = str_replace_all(keywd, '"', ''))
    return(wos_keys)
}
