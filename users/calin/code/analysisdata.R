# code/analysisdata.R
# analysis of data during first week


# Set up ------------------------------------------------------------------

library(tidyverse)
load(file = "users/calin/data/arctic_data.RData")
site_data <-  read_rds(file = "users/calin/data/filtered_data.Rds") %>% 
  dplyr::select(-embargo)


# Analysis ----------------------------------------------------------------

unique(arctic_data$variable)
colnames(arctic_data)


test1 <- arctic_data %>% 
  rbind(site_data) %>% 
  filter(grepl("calanus|glaucous gull|ice",variable)) %>% 
  # dplyr::select(type:value) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  dplyr::select(c(year, month, variable, value)) %>% 
  pivot_wider(names_from = variable, values_from = value, values_fn = mean) %>% 
  ggplot(aes(x = `glaucous gull colony count [n]`, y = `sea ice cover [proportion]`)) +
  geom_point() +
  geom_smooth(method = "lm")

test1
