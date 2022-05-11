# Introduction to the tidyverse by DataCamp (David Robinson) 

# 1. TIDYVERSE FUNCTIONS ####
# Load ####
# Load the tidyverse and gapminder packages
pacman::p_load(tidyverse, gapminder)

# Functions ####
# Useful tidyverse functions

# Conflicts between tidyverse and other packages
tidyverse_conflicts()

# List all tidyverse dependencies
tidyverse_deps()

# Get tidyverse logo, using ASCII or unicode characters
tidyverse_logo() 

# List all tidyverse packages
tidyverse_packages()

# Update tidyverse packages
tidyverse_update()

# 2. DATA WRANGLING ####
# Look at the gapminder dataset
class(gapminder)
gapminder
range(gapminder$year)

# Filter ####

# Filter the gapminder dataset for the year 1957
gapminder %>% 
  filter(year == 1957)

# Filter for China in 2002
gapminder %>% 
  filter(country == "China", year == 2002)

# Arrange ####

# Sort in ascending order of lifeExp
gapminder %>% 
  arrange(lifeExp)

# Sort in descending order of lifeExp
gapminder %>% 
  arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
gapminder %>% 
  filter (year == 1957) %>% 
  arrange(desc(pop))

# Mutate ####

# Use mutate to change lifeExp to be in months
gapminder %>% 
  mutate(lifeExp = lifeExp*12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>% 
  mutate(lifeExpMonths = lifeExp*12)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>% 
  filter(year == 2007) %>% 
  mutate(lifeExpMonths = lifeExp*12) %>% 
  arrange(desc(lifeExpMonths))

# 3. DATA VISUALIZATION ####

# Create gapminder_1952
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Scatter plot ####
# Create a scatter plot with pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x=pop, y=gdpPercap)) +
  geom_point()

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()


# Log scales ####

# Change the existing scatter plot to put the x-axis (representing population) on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10()

# Create a scatter plot with population (pop) on the x-axis and GDP per capita (gdpPercap) on the y-axis. Put both the x- and y- axes on a log scale.

ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


# Adding size and color ####
# Create a scatter plot with population (pop) on the x-axis, life expectancy (lifeExp) on the y-axis, and with continent (continent) represented by the color of the points. Put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, 
                           color = continent)) +
                           geom_point() +
                           scale_x_log10()
  
# Modify the scatter plot so that the size of the points represents each country's GDP per capita (gdpPercap)
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, 
                           color = continent, size=gdpPercap)) +
                           geom_point() +
                           scale_x_log10()

# Faceting ####

# Create a scatter plot of gapminder_1952 with the x-axis representing population (pop), the y-axis representing life expectancy (lifeExp), and faceted to have one subplot per continent (continent). Put the x-axis on a log scale.
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~year)

# 4. GROUPING AND SUMMARIZING ####
# Summarize ####

# Summarize to find the median life expectancy
gapminder %>%
  summarize(medianLifeExp = median(lifeExp))

# Filter for the year 1957, then use the median() function within a summarize() to calculate the median life expectancy into a column called medianLifeExp.
gapminder %>% 
  filter(year == 1957) %>% 
  summarise(medianLifeExp = median(lifeExp))

# Find both the median life expectancy (lifeExp) and the maximum GDP per capita (gdpPercap) in the year 1957, calling them medianLifeExp and maxGdpPercap respectively. You can use the max() function to find the maximum.
gapminder %>% 
  filter(year == 1957) %>% 
  summarise(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))

# Group_by ####

# Find the median life expectancy (lifeExp) and maximum GDP per capita (gdpPercap) within each year, saving them into medianLifeExp and maxGdpPercap, respectively.
gapminder %>% 
  group_by(year) %>% 
  summarise(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))

# Filter the gapminder data for the year 1957. Then find the median life expectancy (lifeExp) and maximum GDP per capita (gdpPercap) within each continent, saving them into medianLifeExp and maxGdpPercap, respectively.

gapminder %>% 
  filter(year == 1957) %>% 
  group_by(continent) %>% 
  summarise(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))


# Find the median life expectancy (lifeExp) and maximum GDP per capita (gdpPercap) within each combination of continent and year, saving them into medianLifeExp and maxGdpPercap, respectively.
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(medianLifeExp=median(lifeExp), maxGdpPercap=max(gdpPercap))

# Use the by_year dataset to create a scatter plot showing the change of median life expectancy over time, with year on the x-axis and medianLifeExp on the y-axis. Be sure to add expand_limits(y = 0) to make sure the plot's y-axis includes zero.
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

ggplot(by_year, aes(x=year, y=medianLifeExp)) +
  geom_point() +
  expand_limits(y=0)


# Summarize the gapminder dataset by continent and year, finding the median GDP per capita (gdpPercap) within each and putting it into a column called medianGdpPercap. Use the assignment operator <- to save this summarized data as by_year_continent.
by_year_continent <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(medianGdpPercap = median(gdpPercap))

# Create a scatter plot showing the change in medianGdpPercap by continent over time. Use color to distinguish between continents, and be sure to add expand_limits(y = 0) so that the y-axis starts at zero.
ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, 
                              color=continent)) + geom_point() +
                              expand_limits(y=0)


# Filter the gapminder dataset for the year 2007, then summarize the median GDP per capita and the median life expectancy within each continent, into columns called medianLifeExp and medianGdpPercap. Save this as by_continent_2007.

by_continent_2007 <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(medianGdpPercap = median(gdpPercap),
            medianLifeExp = median(lifeExp))

# Use the by_continent_2007 data to create a scatterplot comparing these summary statistics for continents in 2007, putting the median GDP per capita on the x-axis to the median life expectancy on the y-axis. Color the scatter plot by continent. 
ggplot(by_continent_2007, aes(x=medianGdpPercap, 
                              y=medianLifeExp, 
                              color=continent)) +
                              geom_point()
  
# 5. TYPES OF VISUALIZATIONS ####
# Line plot ####

# Use group_by() and summarize() to find the median GDP per capita within each year, calling the output column medianGdpPercap. Use the assignment operator <- to save it to a dataset called by_year.

by_year <- gapminder %>% 
  group_by(year) %>% 
  summarize(medianGdpPercap = median(gdpPercap))

# Use the by_year dataset to create a line plot showing the change in median GDP per capita over time. Be sure to use expand_limits(y = 0) to include 0 on the y-axis.
ggplot(by_year, aes(x=year, y=medianGdpPercap)) +
  geom_line() +
  expand_limits(y=0)

# Use group_by() and summarize() to find the median GDP per capita within each year and continent, calling the output column medianGdpPercap. Use the assignment operator <- to save it to a dataset called by_year_continent.
by_year_continent <- gapminder %>% 
  group_by(year, continent) %>% 
  summarise(medianGdpPercap=median(gdpPercap))

# Use the by_year_continent dataset to create a line plot showing the change in median GDP per capita over time, with color representing continent. Be sure to use expand_limits(y = 0) to include 0 on the y-axis.
ggplot(by_year_continent, aes(x=year, y=medianGdpPercap, color=continent)) +
  geom_line() +
  expand_limits(y=0)

# Barplot ####
# Use group_by() and summarize() to find the median GDP per capita within each continent in the year 1952, calling the output column medianGdpPercap. Use the assignment operator <- to save it to a dataset called by_continent.
by_continent <- gapminder %>% 
  filter(year==1952) %>% 
  group_by(continent) %>% 
  summarize(medianGdpPercap=median(gdpPercap))

# Use the by_continent dataset to create a bar plot showing the median GDP per capita in each continent.
ggplot(by_continent, aes(x=continent, y=medianGdpPercap)) +
  geom_col()

# Filter for observations in the Oceania continent in the year 1952. Save this as oceania_1952.
oceania_1952 <- gapminder %>% 
  filter(year == 1952, continent=="Oceania") 
# Use the oceania_1952 dataset to create a bar plot, with country on the x-axis and gdpPercap on the y-axis.
ggplot(oceania_1952, aes(x=country, y=gdpPercap))+
  geom_col()

# Histogram ####
# Use the gapminder_1952 dataset to create a histogram of country population (pop_by_mil) in the year 1952. Inside the histogram geom, set the number of bins to 50.

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

ggplot(gapminder_1952,aes(pop_by_mil))+
  geom_histogram(bins = 50)


# Use the gapminder_1952 dataset to create a histogram of country population (pop) in the year 1952, putting the x-axis on a log scale with scale_x_log10().
gapminder %>%
  filter(year == 1952) %>% 
  ggplot(aes(pop))+
  geom_histogram(bins = 50)+
  scale_x_log10()

# Boxplot ####
# Use the gapminder_1952 dataset (code is provided) to create a boxplot comparing GDP per capita (gdpPercap) among continents. Put the y-axis on a log scale with scale_y_log10().
gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000) %>% 
  ggplot(aes(x=continent, y=gdpPercap))+
  geom_boxplot()+
  scale_y_log10()
# Add a title to the graph: Comparing GDP per capita across continents. Use a search engine, such as Google or Bing, to learn how to do so.
gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000) %>% 
  ggplot(aes(x=continent, y=gdpPercap))+
  geom_boxplot()+
  scale_y_log10()+
  ggtitle("Comparing GDP per capita across continents")
