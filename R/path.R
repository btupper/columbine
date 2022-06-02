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
years <- function(start = 2013, end = current_year()){
  seq(from = start[1], to = end[1])
}


#' Fetch the data for one or more yea
#' rs as a tibble
#' 
#' @export
#' @param year integer one or more years 2013-present
#' @param save_data logical, if TRUE, then save the data
#' @param save_path character the path to save to
#' @param geocode logical if TRUE geocode the locations with lon, lat
#' @return tibble
fetch_data <- function(year = current_year(),
                       save_data = FALSE,
                       savepath = ".",
                       geocode = TRUE){
  
  urls <- get_data_url(year)
  names(urls) <- year
  
  lapply(names(urls),
    function(nm){
      x <- jsonlite::fromJSON(urls[nm]) |>
        dplyr::as_tibble() |>
        dplyr::mutate(date = as.Date(.data$date),
                      killed = as.numeric(.data$killed),
                      wounded = as.numeric(.data$wounded))
      if (geocode){
        x <- tidygeocoder::geocode(x, city =city, state = state, method = 'osm') |>
          dplyr::rename(lon = "long")
      }
      if (save_data){
        filename = file.path(save_path[1], sprintf("mass-shootings-%s.rds", nm))
        saveRDS(x, filename)
      }
      x
    }) |>
    dplyr::bind_rows() 
}