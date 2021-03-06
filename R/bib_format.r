
#' Return any set of WoS references as latex reference
#'
#' e.g. [@Derrida_2001a].
#' TODO: function that deals with van/von/de and other double barrel names 
#' TODO: multiple references for the same year. 
#' @param wos: the dataframe of references
#' @param single_ref: return as a single bibtex reference .e.g [@A_2012; B_2013]
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

latex_format  <- function(wos, single_ref = FALSE) {
    wos  <-  wos %>% mutate(ref = str_replace(string = str_extract(gsub(AF, pattern= ' ', replacement= ''),
                                     pattern =  '^(.+?),'), pattern=',', replacement=''))
    #wos$ref
    #wos  <- wos %>% mutate(ref = str_extract(gsub(x=AF,"'", '') , pattern='\\w+'))
    if (single_ref) {
            wos  <-  wos %>% mutate(ref = paste('[', paste('@', ref, '_', PY,
                                               collapse = '; ', sep=''), ']', sep=''))
        return(wos$ref[1])
    } else {
            wos  <-  wos %>% mutate(ref = paste('[@', ref, '_', PY, ']', sep='' ))
        return(wos$ref)
    }
}

#' Convert all WoS isi csv in directory to bibtex a file
#' @keywords bibtex
#' @export
#' @param wos_dir:  the directory of references
#' @param out_file:  the bib file produced
#' convert_file_to_bib()

convert_file_to_bib  <-  function(wos_dir = '.',  out_file = 'out.bib') {
      path <- paste(system.file(package="scilitlearn"), "wos_to_bib.py", sep="/")
    command = paste('python', path, wos_dir, out_file)
    system(command)

}

#' Convert all WoS records to biblatex and save as file
#' @keywords bibtex
#' @export
#' @param wos:  the references
#' @param out_file:  the bib file produced
#' convert_refs_to_bib()

convert_refs_to_bib  <-  function(wos,  out_file = 'out.bib') {
    wos_dir ='.'
    write.table(wos, file='wos.tsv', sep = '\t', row.names = FALSE)
    path <- paste(system.file(package="scilitlearn"), "wos_to_bib.py", sep="/")
    command = paste('python', path, wos_dir, out_file)
    system(command)
    system('rm wos.tsv')
}
