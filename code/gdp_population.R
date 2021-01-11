# load tidyverse packages
library(tidyverse)

# read in data
# object <- function(argument)
# naming: must start with a letter and no spaces are allowed
gapminder_1997 <- read_csv("data/gapminder_1997.csv")

# make a plot
# argument is an object so no quotation marks
# layering with +
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita ($)")  + # both single quote and double quote would work
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (Years)") +
  labs(title = "Do people in wealthy countries live longer?") +
  geom_point() +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent)


# different color palettes
RColorBrewer::display.brewer.all()

# new data: gapminder_data
gapminder_data <- read_csv("data/gapminder_data.csv") # View(gapminder_data) in the console will open a tab
gapminder_data
dim(gapminder_data)


ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent, group = country) + # group used to force line connecting data from same country
  geom_line()

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp, fill = continent) +
  labs(x = "Continent", y = "Life Expectancy") +
#  geom_boxplot()
  #geom_violin() + # layers are not transparent so violin will cover jitter if order flipped. Can counter with alpha and pop
  #geom_jitter()
  geom_jitter(aes(size = pop)) +
#  geom_violin(alpha = 0.5, fill = 'khaki3')
  geom_violin(alpha = 0.5)

# colors
sample(colors(), size = 10)

# univariate plots
ggplot(data = gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# saving plots
ggsave("figure/last_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp, fill = continent) +
  labs(x = "Continent", y = "Life Expectancy") +
  geom_jitter(aes(size = pop)) +
  geom_violin(alpha = 0.5)

violin_plot <- violin_plot + theme_bw() 
violin_plot

ggsave("figure/lifeExp_violin_plot.pdf", plot = violin_plot, width = 6, height = 4)

# Faceting plots
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

# practice saving a plot as "my_awesome_plot.jpg"
hist_plot <- ggplot(data = gapminder_1997) +
  aes(x = lifeExp) +
  labs(x = "Continent", y = "Life Expectancy") +
  geom_histogram()
ggsave("figure/my_awesome_plot.jpg", plot = hist_plot, width = 6, height = 4)
