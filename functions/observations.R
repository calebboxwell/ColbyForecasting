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
  
  # temporal filter - to fine tune the range
  obs = obs |>
    filter(year >= 1983 & year <= 2013)
  dim(obs)
  
  # plot observation counts per year
  ggplot(data = obs,
         mapping = aes(x = year)) + 
    geom_bar() + 
    geom_vline(xintercept = c(1983, 2013), linetype = "dashed") + 
    labs(title = "Counts per year")
  
  # plot counts per month
  obs = obs |>
    mutate(month = factor(month, levels = month.abb))
  
  ggplot(data = obs,
         mapping = aes(x = month)) + 
    geom_bar() + 
    labs(title = "Counts per month")
  
  # plot points on mask of the Gulf of Maine
  db = brickman_database() |>
    filter(scenario == "STATIC", var == "mask")
  mask = read_brickman(db)
  plot(mask, breaks = "equal", axes = TRUE, reset = FALSE)
  plot(st_geometry(obs), pch = ".", add = TRUE)
  
  return(x)
}
