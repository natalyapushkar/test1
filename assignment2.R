
#not to be included in quarto file importing liraries
install.packages("tidyverse") 
install.packages("plotly")


library(tidyverse)
library(plotly)
library(countrycode)


#joining tables, create an object from multiple data files

#importing data


unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

#joining the tables using unicef inicators and metadata only for more efficient computation and reduction of file sizes

#Unicef_join <- full_join(unicef_indicator_1, unicef_indicator_2)
#unicef_metadata_join1 <- full_join(unicef_indicator_1, unicef_metadata)
#unicef_metadata_join2 <- full_join(unicef_indicator_2, unicef_metadata)

#making the map#making the join_by()map
map_world <- map_data("world")



#map 1 join shape of countries with the data file
#new data object map_world and data_join
#no country variable in map_world so we assign country to region
#since data_join is too big Unicef_indicator1 is used to show the OBS_value of each region

map_data_join <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))

ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) + 
  geom_polygon()


#time series visualisations using countries and Population, total



timeseries_plot_1 <- unicef_metadata %>%
  ggplot() +
  aes(year,unicef_metadata[["Population, total"]] , color = country) + 
  geom_line() +
  labs(
    x = "Year",
    y = "Population, total",
    title = "Population growth 1960-2022"
  )
ggplotly(timeseries_plot_1)



#scatterplot

ggplot(unicef_metadata) + 
  aes(unicef_metadata[["GDP per capita (constant 2015 US$)"]], unicef_metadata[["Life expectancy at birth, total (years)"]], color = country, size = unicef_metadata[["Population, total"]]) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~ year) +
  scale_x_continuous(limits = c(0, 50000),
                     breaks = c(20000, 40000)) +
  labs(
    x = "GDP per capita (constant 2015 US$)",
    y = "Life expectancy at birth, total (years)",
    title = "Evolution of the relationship between GDP per capita and Life expectancy at birth between 1960 and 2021"
      )

#Bar Chart
unicef_metadata %>%
  group_by(country, year) %>%
  summarise(m_lifeexp = mean(unicef_metadata[["Life expectancy at birth, total (years)"]], na.rm = TRUE)) %>%
  ggplot() + 
  aes(country, m_lifeexp, fill = country) + 
  geom_col() + 
  facet_wrap(~ year) +
  labs(
    x = "Country",
    y = "Mean Life Expectancy",
    title = "Evolution of the life expectancy between 1960 and 2021")






