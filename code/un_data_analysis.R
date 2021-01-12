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

# skip if want to skip first few rows while reading in
read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

# goal: get emmision data close to 2007 to be joined with the GDP data to check the correlation between the life expectancy and emission
# select country, year, series, and value
co2_emmissions <- read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  # modify column values
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))


View(co2_emmissions)

# join datasets
# inner_join(), outer_join()
inner_join(gapminder_data, co2_emmissions, by = "country")

# anti_join() shows rows that are in the first dataset and not in the second dataset
anti_join(gapminder_data, co2_emmissions, by = "country") # found some countries named differently. Need to clear these first

# PR treated as part of US in the UN data. Emission needs to be added to the US
gapminder_data <- read_csv("data/gapminder_data.csv") %>%
  filter(continent == "Americas", year == 2007) %>%
  select(-continent, -year) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop)) # order matters here as pop used in the later calculations

anti_join(gapminder_data, co2_emmissions, by = "country")
gapminder_co2 <- inner_join(gapminder_data, co2_emmissions, by = "country")
View(gapminder_co2)


# mutate and if-else for data cleaning
# |, &&, !
gap_co2_region <- gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" |
                            country == "United States" |
                            country == "Mexico", "north", "south"))
View(gap_co2_region)

# scatter plot of gdp vs emissions and color it by region
ggplot(data = gap_co2_region) +
  aes(x = gdpPercap, y = per_capita_emissions, color = region) +
  labs(x = "GDP Per Capita", y = "CO2 emissions") +
  geom_point()

