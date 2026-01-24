C03_assignment
================

## Species

For my backup species I have chosen Harbor seals (*Phoca vitulina*)
because they are a predator of Atlantic herring (*Clupea harengus*).
There also have observations for each month, which is something that the
Atlantic herring don’t have in January, June, and December. Finally,
while the total number of filtered harbor seal observations is less than
that of Atlantic herring, the seal’s distribution across the Gulf of
Maine is decent.

## Code

This code sets the working directory and the source. After running the
following code chunk, I run ‘fetch_obis(SPECIES)’ in the console to
download the species data from Obis.

``` r
source("/home/cjboxw26/ColbyForecasting/setup.R")
SPECIES = "Phoca vitulina"
```

After setting up R and downloading the data, I read in all historic
harbor seal observations using the observations.R function.

``` r
obs = read_obis(SPECIES)
obs = read_observations(
  scientificname = "Phoca vitulina"
  )
```

The next step is to load the shoreline mask and covariates from the
brickman database. The purpose of the mask is to ensure that all
observations are coming from the Gulf of Maine, and the covariates will
eventually be used to predict what harbor seal distribution looks like
in 2050.

``` r
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

Next, the observations are thinned and a bias map is created to mitigate
any spatial biases present in the data. Background points are used to
characterize locations without any harbor seal observations.

``` r
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

Finally, we take the presence and background points and merge them with
the covariate brickman data. We can also draw out a random sample,
selecting one presence and background point per month.

``` r
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

    ## Simple feature collection with 24 features and 6 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -73.16415 ymin: 41.18308 xmax: -66.33626 ymax: 44.47363
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 24 × 7
    ## # Groups:   month, class [24]
    ##    .id   month class        SSS   SST  Tbtm             geometry
    ##    <chr> <fct> <fct>      <dbl> <dbl> <dbl>          <POINT [°]>
    ##  1 p0003 Jan   presence    30.8  4.03  5.22   (-66.4167 44.1333)
    ##  2 p0280 Jan   background  29.3  3.89  4.90 (-72.17699 41.26535)
    ##  3 p0299 Feb   presence    30.8  2.03  2.35 (-70.45215 42.04122)
    ##  4 p0348 Feb   background  30.8  2.39  5.41 (-66.33626 44.39137)
    ##  5 p0604 Mar   presence    30.9  1.57  1.63 (-68.59151 44.17164)
    ##  6 p0948 Mar   background  28.5  2.94  3.79 (-72.09472 41.18308)
    ##  7 p0960 Apr   presence    29.5  6.03  4.98 (-71.56917 41.21833)
    ##  8 p1241 Apr   background  23.8  6.32  6.52 (-73.16415 41.18308)
    ##  9 p1279 May   presence    30.0  7.83  6.32  (-69.5881 43.79954)
    ## 10 p1400 May   background  29.5  7.78  6.30 (-67.81701 44.47363)
    ## # ℹ 14 more rows

While the subsetted table is the main output we are after, we can also
generate several plots. The following figures show the bias map of all
observations and the locations of the randomly selected
presence/background points respectively.

``` r
# bias map
ggplot() +
  geom_stars(data = bias_map, aes(fill = count)) +
  scale_fill_viridis_b(na.value = "transparent") +
  geom_sf(data = coast, col = "orange") + 
  labs(x = "Longitude", y = "Latitude", title = "Bias map using all observations")
```
![](assignments/C03_assignment_files/figure-gfm/plots-1.png)

``` r
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
![](assignments/C03_assignment_files/figure-gfm/plots-2.png)
