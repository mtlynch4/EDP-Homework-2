
# Load Libraries and Data
library(tidyverse)
library(modelr)
library(broom)
library(knitr)

gdpreal <- read.csv("gdpreal.csv")

# Part 1

## Tidy data
gdpreal <- slice(gdpreal, 1:4)
  
gdpreal <- gather(gdpreal, 5:64, key = "year", value = "growth")

colnames(gdpreal)[1:2] <- c("aggregate", "aggregate_code")

gdpreal <- select(gdpreal, 1, 5, 6)

gdpreal$year <- substr(gdpreal$year, 2, 5)

gdpreal$year <- as.Date(gdpreal$year, format = "%Y")
gdpreal$growth <- as.double(gdpreal$growth)

## Plot data
ggplot(gdpreal, mapping = aes(x = year, y = growth, color = aggregate)) +
  geom_smooth(se = FALSE)

# Part 2

## Load Data

solow <- read.csv("solow.csv")


## Tidy Data
solow <- select(solow, -2, -4)

solow <- slice(solow, 1:736)

colnames(solow) <- c("country", "series", "value")

solow <- spread(solow, series, value)

colnames(solow) <- c("country", "sav_net_all", "sav_net", "gdp_pc", "cap_form", "dom_sav", "sav_gdp", "sav_gni", "invest")

solow$sav_net_all <- as.numeric(as.character(solow$sav_net_all))
solow$sav_net <- as.numeric(as.character(solow$sav_net))
solow$gdp_pc <- as.numeric(as.character(solow$gdp_pc))
solow$cap_form <- as.numeric(as.character(solow$cap_form))
solow$dom_sav <- as.numeric(as.character(solow$dom_sav))
solow$sav_gdp <- as.numeric(as.character(solow$sav_gdp))
solow$sav_gni <- as.numeric(as.character(solow$sav_gni))
solow$invest <- as.numeric(as.character(solow$invest))


## Plot Data

### Do this one. Net investment in nonfinancial assets and gross savings

ggplot(solow, mapping = aes(x = sav_gdp, y = invest)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE)

solow_mod_1 <- lm(invest ~ sav_gdp,
                data = solow)

tidy(solow_mod_1)


### Oooh. replace investment with Gross Capital Formation and things change 

ggplot(solow, mapping = aes(x = sav_gdp, y = cap_form)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE)

solow_mod_2 <- lm(cap_form ~ sav_gdp,
                data = solow)

tidy(solow_mod_2)



view(solow)


# Part 3
solow <- mutate(solow, log_gdp_pc = log(gdp_pc))

ggplot(solow, mapping = aes(x = sav_gdp, y = log_gdp_pc)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE)

solow_mod_3 <- lm(sav_gdp ~ log_gdp_pc,
                data = solow)

tidy(solow_mod_3)
