read_observations = function(scientificname = "Clupea harengus",
                             minimum_year = 1970, 
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = read_obis(scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb))
  
  # eventDate must exist! Otherwise I can't say how variables affect it in time
  x = x |>
    dplyr::filter(!is.na(eventDate))
  
  # individual count must exist
  x = x |>
    dplyr::filter(!is.na(individualCount))
  
  # if the user provided a non-NULL filter by year (caleb: I don't fully understand how this works)
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  # spatial filter
  db = brickman_database() |>
    filter(scenario == "STATIC", var == "mask")
  mask = read_brickman(db)
  hitOrMiss = extract_brickman(mask, x)
  x = x |>
    filter(!is.na(hitOrMiss$value))
  
  return(x)
}
