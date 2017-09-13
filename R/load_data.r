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
    # add column for date

    wos  <- wos %>% 
        mutate(date = as.Date(paste(1, tolower(PD), PY, sep=' '),
                              format = '%d %b %Y'))
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


#' Load directory of WoS records
#'
#' This function assumes that all the files have been zipped and all quote marks removed
#' @param dir: the directory of files
#' @keywords directory, WoS files 
#' @export
#' @examples
#' load_zip()

load_directory <- function(dir) {
    setwd(dir)   
    filenames  <- dir()
    d  <- lapply(filenames, load_data)
    df  <- do.call(rbind, d)
    setwd('..')
    return(df)
}

#' Load zipfile of WoS records
#'
#' This function assumes that all the files have been zipped and all quote marks removed
#' @param zipfile: the collection of files
#' @keywords zip 
#' @export
#' @examples
#' load_zip()

load_zip  <-  function(zipfile) {
    unzip(zipfile, exdir = 'tmp') 
    df  <- load_directory('tmp')
    #file.remove(dir())     
    return(df)
}

