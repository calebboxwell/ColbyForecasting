# Assignment Script - Assignment 2

# setup R
source("setup.R")

# load data
coast = read_coastline()
obs = read_observations(scientificname = "Clupea harengus")
db_mask = brickman_database() |>
  filter(scenario == "STATIC", 
         var == "mask"
         )
mask = read_brickman(db_mask)
db_covariates = brickman_database() |>
  filter(scenario == "PRESENT",
         interval == "mon",
         var %in% c("SST", "Tbtm", "SSS")
         )
covariates <- read_brickman(db_covariates)

# thin the observations to reduce sample bias
thinned_obs = sapply(month.abb,
                     function(mon){
                       temp_x = obs |>
                         filter(month == mon)
                       if(nrow(temp_x) == 0)
                         return(NULL)
                       thin_by_cell(obs |> filter(month == mon), mask)
                     }, simplify = FALSE) |>
  dplyr::bind_rows() 

thinned_counts = count(st_drop_geometry(thinned_obs), month)

# create a bias map to influence where we sample
bias_map = rasterize_point_density(obs, mask) # using original observation data

# create a bunch of background points
all_counts = count(st_drop_geometry(obs), month) # keep track of total number of counts
nback_avg = mean(all_counts$n) |>
  round()

# large table of thinned observations and background points
obsbkg = sapply(month.abb,
                function(mon){ 
                  temp_x = thinned_obs |> filter(month == mon)
                  if(nrow(temp_x) == 0)
                    return(NULL)
                  sample_background(thinned_obs |> filter(month == mon), # <- just this month
                                    bias_map,
                                    method = "bias",  # <-- it needs to know it's a bias map
                                    return_pres = TRUE, # <-- give me the obs back, too
                                    n = nback_avg) |>   # <-- how many points
                    mutate(month = mon, .before = 1)
                }, simplify = FALSE) |>
  bind_rows() |>
  mutate(month = factor(month, levels = month.abb))

# bind covariates to the large table
obsbkg_covariates <- extract_brickman(
  covariates,
  obsbkg,
  form = "wide"
)

# subsetting the large table to get one observation and background point per month
subset <- obsbkg_covariates |>
  dplyr::group_by(month, class) |>
  dplyr::slice_sample(n = 1)

# view data
subset

# plot to visualize the location of points
ggplot() +
  geom_sf(data = subset,
          mapping = aes(col = class),
          alpha =  1, shape = "circle small", size = 3.5) +
  geom_sf(data = coast, col = "orange")  + 
  labs(x = "Longitude", y = "Latitude", title = "Background and Presence of Clupea harengus") +   
  theme_bw() +  # make a white background
  scale_fill_okabe_ito() + # colorblind friendly
  facet_wrap(~month)