# Assignment Script - Assignment 1
#
# Assignment:
# Use the menu option `File > New File > R Script` to create a blank file. 
# Save the file (even though it is empty) in the "assignment" directory as "assignment_script_1.R".  
# Use this file to build a script that meets the following challenge.  
# Note that the existing file, "assignment_script_0.R" is already there as an example.
# 
# Use the [Brickman tutorial](https://github.com/BigelowLab/ColbyForecasting/wiki/Brickman) to extract data from the 
# location of Buoy M01 for RCP4.5 2055. Make a plot of `SST` (y-axis) as a function of `month` (x-axis).  
# Here's one possible outcome.

# Assignment Script:
# always start with setting things up
source("setup.R")

# load filtered data
buoys = gom_buoys() |>
  filter(id == "M01")

db <- brickman_database() |>
  filter(
    scenario == "RCP45",
    year == 2055,
    interval == "mon"
  )

# read the covariates
covars = read_brickman(db)

# extract data at the buoy location
x = extract_brickman(covars, buoys, form = "wide")

# fix month order
x = x |>
  mutate(month = factor(month, levels = month.abb))

# plot!
ggplot(data = x,
       mapping = aes(x = month, y = SST)) +
  geom_point() + 
  labs(x = NULL,
       y = "Sea Surface Temperature (C)", 
       title = "RCP4.5 (2055) SST at buoy M01")

# save image
ggsave("images/RCP4.5_2055_SST.png")