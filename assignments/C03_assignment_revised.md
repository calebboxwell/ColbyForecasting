---
title: "C03_assignment"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Species

For my backup species I have chosen Harbor seals (*Phoca vitulina*) because they are a predator of Atlantic herring (*Clupea harengus*). There also have observations for each month, which is something that the Atlantic herring don't have in January, June, and December. Finally, while the total number of filtered harbor seal observations is less than that of Atlantic herring, the seal's distribution across the Gulf of Maine is decent.

## Code

This code sets the working directory and the source. After running the following code chunk, I run 'fetch_obis(SPECIES)' in the console to download the species data from Obis.
```{r source_setup, warning = FALSE}
source("/home/cjboxw26/ColbyForecasting/setup.R")
SPECIES = "Phoca vitulina"
```

After setting up R and downloading the data, I read in all historic harbor seal observations using the observations.R function.
```{r call observations.R for Harbor seals}
obs = read_obis(SPECIES)
obs = read_observations(
  scientificname = "Phoca vitulina"
  )
```

The next step is to load the shoreline mask and covariates from the brickman database. The purpose of the mask is to ensure that all observations are coming from the Gulf of Maine, and the covariates will eventually be used to predict what harbor seal distribution looks like in 2050.

```{r presence and background with covariates}
# load data
coast = read_coastline()
obs = read_observations(scientificname = "Phoca vitulina")
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
```

Next, the observations are thinned and a bias map is created to mitigate any spatial biases present in the data. Background points are used to characterize locations without any harbor seal observations.
```{r thinning}
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
```

Finally, we take the presence and background points and merge them with the covariate brickman data. We can also draw out a random sample, selecting one presence and background point per month.
```{r thinned table with covariates, warning=FALSE}
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
```

While the subsetted table is the main output we are after, we can also generate several plots. The following figures show the bias map of all observations and the locations of the randomly selected presence/background points respectively.
```{r plots}
# bias map
ggplot() +
  geom_stars(data = bias_map, aes(fill = count)) +
  scale_fill_viridis_b(na.value = "transparent") +
  geom_sf(data = coast, col = "orange") + 
  labs(x = "Longitude", y = "Latitude", title = "Bias map using all observations")

# plot to visualize the location of points
ggplot() +
  geom_sf(data = subset,
          mapping = aes(col = class),
          alpha =  1, shape = "circle small", size = 3.5) +
  geom_sf(data = coast, col = "orange")  + 
  labs(x = "Longitude", y = "Latitude", title = "Background and Presence of Phoca vitulina") +   
  theme_bw() +  # make a white background
  scale_fill_okabe_ito() + # colorblind friendly
  facet_wrap(~month)
```
