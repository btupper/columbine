#' Geocode (determine lon,lat in this case) for one or more locations
#' 
#' @export
#' @param x tibble as returned by \code{\link{fetch_data}}
#' @return an updated version of the input with lon and lat assigned best geocodes
geo_code <- function(x){
  
  drop_annotation <- function(x, 
                              pattern = '[(|/)]' , 
                              fixed = FALSE){
                              
    ix <- regexpr(pattern, x, fixed = fixed)
    iy <- ix > 0
    x[iy] = substring(x[iy], 1, ix[iy]-1)
    
    x <- gsub("Co.", "", x, fixed = TRUE)
    
    trimws(x)
  }
  
  cleaned_city = drop_annotation(x$city)
  x <- dplyr::mutate(x, cleaned = cleaned_city) |>
    dplyr::select(-dplyr::any_of(c("lat", "long", "lng", "lon")))
  tidygeocoder::geocode(x, city = cleaned, state = state, method = 'osm') |>
    dplyr::rename(lon = "long") |>
    dplyr::select(-dplyr::any_of("cleaned"))
}


#' Fetch the MST data for one or more years as a tibble
#' 
#' @export
#' @param year integer one or more years 2013-present
#' @param save_data logical, if TRUE, then save the data
#' @param save_path character the path to save to
#' @param geocode logical if TRUE geocode the locations with lon, lat
#' @return tibble
fetch_mst <- function(year = current_year(),
                       save_data = TRUE,
                       save_path = get_path(),
                       geocode = TRUE){
  
  urls <- get_mst_url(year)
  names(urls) <- year
  
  lapply(names(urls),
         function(nm){
           x <- jsonlite::fromJSON(urls[nm]) |>
             dplyr::as_tibble() |>
             dplyr::mutate(date = as.Date(.data$date),
                           killed = as.numeric(.data$killed),
                           wounded = as.numeric(.data$wounded)) |>
             dplyr::mutate(ID = sprintf("%s-%0.5i", 
                                        format(.data$date, "%Y-%m-%d"), 
                                        seq_len(dplyr::n())),
                           .before = 1)
           
           if (geocode){
             # first we geocode
             x <- geo_code(x)
             # now we copy to a sf (dropping where missing locations
             y <- as_sf(x) |>
               add_county() |>
               add_congress() |>
               as_dataframe() |>
               dplyr::select(dplyr::all_of(c("ID", "county", "congress")))
             # and join back
             x <- dplyr::left_join(x, y, by = "ID")
             
           }
           
           if (save_data){
             if (!dir.exists(save_path[1])) ok <- dir.create(save_path[1], recursive = TRUE)
             filename = file.path(save_path[1], sprintf("mst-%s.rds", nm))
             saveRDS(x, filename)
           }
           x
         }) |>
    dplyr::bind_rows() 
}

#' Convert to sf
#' 
#' @param x tibble
#' @return sf
as_sf <- function(x){
  dplyr::filter(x, !is.na(.data$lon) | !is.na(.data$lat)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
}

#' Convert to data frame (tibble)
#' 
#' @param x sf object
#' @return tibble
as_dataframe <- function(x){
  xy <- sf::st_coordinates(x) |>
    dplyr::as_tibble() |>
    rlang::set_names(c("lon", "lat"))
  sf::st_drop_geometry(x) |>
    dplyr::bind_cols(xy)
}


#' Add county geoid to a table
#' 
#' @param x sf table
#' @param ... other arguments for \code{\link[USAboundaries]{us_counties}}
#' @return the input table with county added
add_county <- function(x, ...){
  counties <- USAboundaries::us_counties(resolution = "high")
  ix <- sf::st_intersects(x, counties)
  x$county <- sapply(seq_along(ix), 
    function(i) {
      geoid <- if (length(ix[[i]]) > 0){
        ix[[i]][[1]]
      } else {
        NA
      }
      geoid
  })
  x
}

#' Add congress id to a table
#' 
#' @param x table
#' @param ... other arguments for \code{\link[USAboundaries]{us_congress}}
#' @return the inout table with congress added
add_congress <- function(x, ...){
  congress <- USAboundaries::us_congressional(resolution = "high")
  ix <- sf::st_intersects(x, congress)
  x$congress <- sapply(seq_along(ix), 
                     function(i) {
                       geoid <- if (length(ix[[i]]) > 0){
                         ix[[i]][[1]]
                       } else {
                         NA
                       }
                       geoid
                     })
  x
}


#' Read one or more years of MST data
#'
#' @export
#' @param years one or more years to read, by default all available
#' @param path character, the data path
#' @param keep character, one or maore variables to keep, the default includes
#'   "date", "killed", "wounded", "city", "county_id", "congress_id", "state", "lon", "lat".
#'    Specify "all" to retrieve all fields.  
#' @param form character, one of 'data.frame' or 'sf'
#' @param complete logical, if TRUE remove records without lon,lat.  Ignored if
#'   \code{form} is 'sf' in which case only complete cases of lon and lat are permitted.
#' @param crop sfc or sf object defining a bounding box, ignored if \code{form} is not
#' 'sf'.  Set to NULL to skip cropping.
#' @return data frame, possibly sf POINT table
read_mst <- function(years = seq_years(),
                     path = get_path(),
                     keep = c("date", "killed", "wounded", "city", 
                              "county", "congress", "state", "lat", "lon"),
                     form = c("data.frame", "sf")[2],
                     crop = sf::st_as_sfc(sf::st_bbox(c(xmin = -180, 
                                                         ymin = 0, 
                                                         xmax = 0, 
                                                         ymax = 80),
                                                       crs = 4326)),
                      complete = tolower(form[1]) == "sf"){
  
  ff <- list_mst(years = years, path = path)
  
  read_one <- function(filename){
    readRDS(filename)
  }
  
  x <- lapply(ff, read_one) |>
    dplyr::bind_rows()
  
  if (!("all" %in% keep))   x <- dplyr::select(x, dplyr::any_of(keep))
  
  if (!("all" %in% keep) && tolower(form[1]) == 'sf'){
    x <- dplyr::filter(x, !is.na(.data$lon) | !is.na(.data$lat)) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
    if (!is.null(crop)) x <- sf::st_crop(x, crop)
  } else {
    if (complete) x <- dplyr::filter(x, !is.na(.data$lon) & !is.na(.data$lat))
  }
  
  x
}