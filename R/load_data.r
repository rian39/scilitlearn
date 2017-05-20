#' Load tsv file from Web of Science
#'
#' This function assumes that all the files have been joined, and all quote marks removed
#' @param file: the tsv of WoS references
#' @keywords tsv 
#' @export
#' @examples
#' load_data()
 
load_data  <- function(file) {
    wos = readr::read_delim(file, trim_ws = TRUE, quote='"', delim='\t', col_names = TRUE)
    wos  <-  unique(wos)
    cat('loaded ', nrow(wos), ' references\n')
    print(wos %>% select(DT) %>% count(DT, sort = TRUE))
    return(wos)
}

#' Number of publications by year
#' @param wos: the dataframe of references
#' @keywords years
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @examples
#' year_count()

year_count  <- function(wos) {
    yrs  <- wos %>% select(PY) %>% count(PY, sort=TRUE) %>% arrange(PY)
    return(yrs)
}
