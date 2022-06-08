#' Geocode (determine lon,lat in this case) for one or more locations
#' 
#' @export
#' @param x tibble as returned by \code{\link{fetch_data}}
#' @return an updated version of the input with lon and lat assigned best geocodes
geo_code <- function(x){
  
  drop_annotation <- function(x, pattern = "(", fixed = TRUE){
    ix <- regexpr(pattern, x, fixed = fixed)
    substring(x, 1, ix-1)
  }
  cleaned_city = drop_annotation(x$city)
  x <- dplyr::mutate(x, cleaned = cleaned_city) |>
    dplyr::select(-dplyr::any_of(c("lat", "long", "lng", "lon")))
  tidygeocoder::geocode(x, city = cleaned, state = state, method = 'osm') |>
    dplyr::rename(lon = "long") |>
    dplyr::select(-dplyr::any_of("cleaned"))
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
                       save_path = get_path(),
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
             x <- geo_code(x)
           }
           if (save_data){
             if (!dir.exists(save_path[1])) ok <- dir.create(save_path[1], recursive = TRUE)
             filename = file.path(save_path[1], sprintf("mass-shootings-%s.rds", nm))
             saveRDS(x, filename)
           }
           x
         }) |>
    dplyr::bind_rows() 
}

#' Read one or more years of data
#'
#' @export
#' @param years one or more years to read
#' @param path character, the data path
#' @param keep character, one or maore variables to keep, the default includes
#'   "date", "killed", "wounded", "city", "state", "lat", "lon".  Specify "all"
#'   to retrieve all fields.  Not compatible with \code{form = "sf"}.
#' @param form character, one of 'data.frame' or 'sf'
#' @return data frame, possibly sf POINT table
read_data <- function(years = seq_years(),
                      path = get_path(),
                      keep = c("date", "killed", "wounded", "city", "state", "lat", "lon"),
                      form = c("data.frame", "sf")[1]){
  
  ff <- list_years(path = path)
  
  read_one <- function(filename){
    readRDS(filename)
  }
  
  x <- lapply(ff, read_one) |>
    dplyr::bind_rows()
  
  if (!("all" %in% keep))   x <- dplyr::select(x, dplyr::any_of(keep))
  
  if (!("all" %in% keep) && tolower(form[1]) == 'sf'){
    x <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }
  
  x
}