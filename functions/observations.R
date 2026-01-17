#' Read raw OBIS data and then filter it
#' 
#' @param scientificname chr, the name of the species to read
#' @param minimum_year num, the earliest year of observation to accept or 
#'   set to NULL to skip
#' @param temporal_filter num vector, defines range of observations
#' @param remove_missing chr vector, column names to remove from observations list
#' @param spatial_filter logical, apply spatial mask 
#' @return a filtered table of observations

read_observations = function(scientificname = "Clupea harengus",
                                 minimum_year = 1970,
                                 temporal_filter = c(1983, 2013),
                                 remove_missing = c("eventDate", "individualCount"),
                                 spatial_filter = TRUE,
                                 ...){
  
  # read in raw OBIS data
  x = read_obis(scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb))
  
  # set a minimum year for observations
  if (!is.null(minimum_year)) {
    x <- dplyr::filter(x, year >= minimum_year)
  }
  
  # set further temporal constraints to target data
  if (!is.null(temporal_filter)) {
    x <- dplyr::filter(
      x,
      year >= temporal_filter[1],
      year <= temporal_filter[2]
    )
  }
  
  # remove observations with missing variables
  #' missing dates
  if ("eventDate" %in% remove_missing) {
    x <- dplyr::filter(x, !is.na(eventDate))
  }
  
  #' missing observation counts
  if ("individualCount" %in% remove_missing) {
    x <- dplyr::filter(x, !is.na(individualCount))
  }
  
  # spatial filter
  if (spatial_filter) {
    db = brickman_database() |>
      filter(scenario == "STATIC", var == "mask")
    mask = read_brickman(db)
    hitOrMiss = extract_brickman(mask, x)
    x = x |>
      filter(!is.na(hitOrMiss$value))
  }
  
  return(x)
}