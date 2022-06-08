#' Retrieve a listing of available files
#' 
#' @export
#' @param path character the data path
#' @return character vector of zero or more files
list_years <- function(path = get_path()){
  list.files(path, full.names = TRUE)
}

#' Construct a path to locally stored data 
#' 
#' @export
#' @param ... path segments as a character vector
#' @param root character, the root path
#' @return a constructed file path
get_path <- function(...,
                     root = rappdirs::user_data_dir("columbine")){
  file.path(root[1], ...)
}

#' Retrieve the data url for a given year
#' 
#' @export
#' @param year integer, one or more 4 digit years 2013-present
#' @param base_url character, https://massshootingtracker.site/ base data url 
#' @return the full url to a year's data
get_data_url <- function(year = 2022,
                    base_url = 'https://mass-shooting-tracker-data.s3.us-east-2.amazonaws.com'){
  file.path(base_url,
            sprintf("%0.4i-data.json", year))
}


#' Retrieve the current year
#' 
#' @export
#' @return integer year
current_year <- function(){
  as.integer(format(Sys.Date(), "%Y"))
}

#' Create a sequence of years, 2013 to present
#' 
#' @export
#' @param start integer, the first year in the sequence
#' @param end the last year in the sequence
#' @return integer sequence of years
seq_years <- function(start = 2013, end = current_year()){
  seq(from = start[1], to = end[1])
}


