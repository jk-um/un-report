library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

# piping %>% feed in first argument
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

# find the mean population and the most recent year
gapminder_data %>% summarize(averagePopulation = mean(pop), recentYear = max(year))

# filter selects specific rows
# (single) = for assinging value, == for specifying value 
# also can use >, <, >= ect

gapminder_data %>% filter(year == 2007) %>% summarize(averageLifeExp = mean(lifeExp))

# find the average GDP per capita for the first year
gapminder_data %>% summarize(firstYear = min(year))
gapminder_data %>% filter(year == 1952) %>% summarize(averageGDPperCapita = mean(gdpPercap))
                                                      
# group_by to get results on groups
gapminder_data %>%
  group_by(year) %>%
  summarize(averageLifeExp = mean(lifeExp))

# find mean life expectancies for each continent
gapminder_data %>%
  group_by(continent) %>%
  summarize(averageLifeExp = mean(lifeExp))

# mutate - add more columns
gapminder_data %>%
  mutate(gdp = gdpPercap * pop)

# add a column of population in millions
gapminder_data %>%
  mutate(popInMil = pop / 1000000)

# select - select the columns to keep
gapminder_data %>%
  select(year, pop)

# drop the column continent
gapminder_data %>%
  select(-continent)

# save dataset by assigning
newData <- gapminder_data

# create a dataset with country, continent, year, and lifeExp columns
newData <- gapminder_data %>% 
  select(-pop, -gdpPercap)
View(newData)

# arrange() - arrange rows
# arrange(year)

# long vs wide dataset
# convert using pivot_longer, and pivot_wider
gapminder_data <- read_csv("data/gapminder_data.csv")
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# rename() - rename columns

# create a new dataset with data from the Americas and year 2007 and drop columns year and continent
gapminder_data <- read_csv("data/gapminder_data.csv")
newData <- gapminder_data %>% 
  filter(continent == "Americas", year == 2007) %>%
  select(-continent, -year)

gapminder_data <- read_csv("data/gapminder_data.csv") %>%
  filter(continent == "Americas", year == 2007) %>%
  select(-continent, -year)
