
# Load Libraries and Data
library(tidyverse)
library(modelr)
library(broom)
library(knitr)

gdpsim <- read.csv("gdpsim.csv")

view(gdpsim)

# Part 1
growth <- rnorm(150, mean = 2.4, sd = 0.7)

gdpsim <- cbind(gdpsim, growth)

# Part 2
gdpsim <- mutate(gdpsim, GDPpc_2020 = GDPpc_1950 * (1 + (growth/100))^70)

# Part 3
gdpsim <- mutate(gdpsim, logGDP_1950 = log(GDPpc_1950))

ggplot(gdpsim, mapping = aes(x = logGDP_1950, y = growth)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

mod1 <- lm(growth ~ logGDP_1950, 
              data = gdpsim)

kable(glance(mod1))

### I would not conclude that there is any convergence. In fact, the relationship between initial GDP and average growth rate appears to be if anything positive, implying divergence. The p-value is below 0.05, although knowing these numbers are random, this is coincidence. 


# Part 4
gdpsim_med <- filter(gdpsim, GDPpc_2020 > median(GDPpc_2020))

ggplot(gdpsim_med, mapping = aes(x = logGDP_1950, y = growth)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

mod2 <- lm(growth ~ logGDP_1950, 
           data = gdpsim_med)

kable(glance(mod2))

### Now, the relationship has flipped, and lower initial GDP implies higher growth, and so convergence, although this relationship is very weak, with a p-value over 0.05. 

# Part 5
gdpsim_quart <- filter(gdpsim, GDPpc_2020 > quantile(GDPpc_2020, 0.75))

ggplot(gdpsim_quart, mapping = aes(x = logGDP_1950, y = growth)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

mod3 <- lm(growth ~ logGDP_1950, 
           data = gdpsim_quart)

kable(glance(mod3))

### In this model, the relationship remains positive and becomes both stronger, low initial GDP implying an even higher growth rate, and more significant, with a p-value under 0.01. 

# Part 6

### Knowing these numbers are random, it is odd that we find seemingly significant support for convergence in the last model. This occurs because among countries that ended up rich, the ones who started poorer had to have grown faster to catch up and end up in the data set. 
