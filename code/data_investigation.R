# data_investigation.R
# This script investigates what is in the final clean dataset


# Setup -------------------------------------------------------------------

# Libraries used in this script
source("code/functions.R")


# Load data ---------------------------------------------------------------

# Load all clean data
if(!exists("clean_all")) load("data/analyses/clean_all.RData")


# Visuals -----------------------------------------------------------------

# Legacy summaries
# NB: There are isues in this code that prevent it for running for everything
# doParallel::registerDoParallel(cores = 12)
# plyr::l_ply(long_driver_names$driver, review_summary_plot, .parallel = TRUE, summary_list = clean_all, analyse = TRUE)

# Category level summary
clean_all |> 
  mutate(year = year(date)) |> 
  filter(year <= 2023, year >= 1900) |> 
  summarise(annual_count = n(), .by = c(category, year)) |> 
  ggplot(aes(x = year, y = annual_count)) +
  geom_col(aes(fill = category)) +
  scale_x_continuous(expand = c(0, 0))

# Driver level summary
## Count of data points per driver per year
clean_all |> 
  mutate(year = year(date)) |> 
  filter(year <= 2023, year >= 1900) |> 
  summarise(annual_count = n(), .by = c(category, driver, year)) |> 
  ggplot(aes(x = year, y = annual_count)) +
  geom_col(aes(fill = driver)) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~category)
## Count of citations per driver per year
clean_all |> 
  mutate(year = year(date)) |> filter(year <= 2023, year >= 1900) |> 
  dplyr::select(citation, year, category, driver) |> distinct() |> 
  summarise(citation_count = n(), .by = c(citation, category, driver, year)) |> 
  ggplot(aes(x = year, y = citation_count)) + geom_col() +
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~driver)

# Variable level summary
## Count of different variables per year
clean_all |> 
  mutate(year = year(date)) |> filter(year <= 2023, year >= 1900) |> 
  dplyr::select(year, category, driver, variable) |> distinct() |> 
  summarise(var_count = n(), .by = c(category, driver, year)) |> 
  ggplot(aes(x = year, y = var_count)) + geom_col() +
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~category)

# Scatterplot with lm per site per driver

# Scatterplot with each dot showing a unique citation with y = n() x = year()
clean_all |> 
  mutate(year = year(date)) |> 
  filter(year <= 2023, year >= 1900) |> 
  summarise(annual_count = n(), .by = c(citation, category, year)) |> 
  ggplot(aes(x = year, y = annual_count)) +
  geom_point(aes(colour = category)) +
  scale_x_continuous(expand = c(0, 0))
clean_all |> 
  mutate(year = year(date)) |> 
  filter(year <= 2023, year >= 1900) |> 
  summarise(annual_count = n(), .by = c(citation, driver, year)) |> 
  ggplot(aes(x = year, y = annual_count)) +
  geom_point(aes(colour = driver)) +
  scale_x_continuous(expand = c(0, 0))

# Horizontal boxplots of temperature etc. by depth with box colour showing annual trend

# List of variable names. Perhaps time series or scatter plot.

# Barplot time series of complete monthly daily data. But how for depth? 

