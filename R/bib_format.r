
#' Return any set of WoS references as latex reference
#'
#' e.g. [@Derrida_2001a].
#' TODO: function that deals with van/von/de and other double barrel names 
#' TODO: multiple references for the same year. 
#' @param wos: the dataframe of references
#' @export
#' @import dplyr
#' @import stringr
#' @examples
#' latex_format(wos)

#library(scilitlearn)
#library(dplyr)
#wos  <- load_data('../sample.tsv')
#wos  <- wos[1:4,]
#View(wos)
#library(stringr)

latex_format  <- function(wos) {
    wos  <- wos %>% mutate(ref = str_extract(gsub(x=AF,"'", '') , pattern='\\w+')) %>%
        mutate(ref = paste('[@', ref, '_', PY, ']', sep=''))
    return(wos$ref)
}

#' Convert all WoS isi csv in directory to bibtex a file
#' @keywords bibtex
#' @export
#' @param wos_dir:  the directory of references
#' @param out_file:  the bib file produced
#' convert_to_bib()

convert_to_bib  <-  function(wos_dir = '.',  out_file = 'out.bib') {
      path <- paste(system.file(package="scilitlearn"), "wos_to_bib.py", sep="/")
    command = paste('python', path, wos_dir, out_file)
    system(command)

}
