# code/test1.R
# 1r code pour tester rstudio


# Set up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)


# Data --------------------------------------------------------------------

mtcars <- mtcars


# Figures -----------------------------------------------------------------

plot1 <- ggplot(data = mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

plot2 <- ggplot(data=mtcars, aes(x = as.factor(cyl), y = drat)) +
  geom_boxplot(aes( fill = as.factor(cyl))) +
  labs(x = "cylindre", y = "rear", title = "torque by cylindre", 
       subtitle = "i like dogs", caption = "made by eva-carmen")

plot3 <- ggarrange(plot1, plot2,labels = c("a)", "b)"),align = "hv" )

ggsave("figures/firstplots.png", plot = plot3, width = 15)


# Stats -------------------------------------------------------------------

summary(lm(mpg~wt, data = mtcars)) 

