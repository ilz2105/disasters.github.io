

library(flexdashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(rvest)

# read in data
ave_temp = read_csv("https://www.ncdc.noaa.gov/cag/statewide/mapping/110-tavg.csv", skip = 3) %>% 
  janitor::clean_names() %>% 
  separate(date, into = c("year", "month"), sep = 4) %>%
  mutate(
    year = as.numeric(year), 
    state_code = setNames(state.abb, state.name)[location]) %>% 
  filter(year >= 1953 & year <= 2018) %>% 
  group_by(location, year) %>% 
  mutate(
    mean_temp = mean(value)) %>% 
  select(state_code, location, year, mean_temp) %>% 
  distinct()

precip = read_csv("https://www.ncdc.noaa.gov/cag/statewide/mapping/110-pcp.csv", skip = 3) %>% 
  janitor::clean_names() %>% 
  separate(date, into = c("year", "month"), sep = 4) %>%
  mutate(
    year = as.numeric(year), 
    state_code = setNames(state.abb, state.name)[location]) %>% 
  filter(year >= 1953 & year <= 2018) %>% 
  group_by(location, year) %>% 
  mutate(
    total_precip = sum(value)) %>% 
  select(state_code, location, year, total_precip) %>% 
  distinct()

tmax = read_csv("https://www.ncdc.noaa.gov/cag/statewide/mapping/110-tmax.csv", skip = 3) %>% 
  janitor::clean_names() %>% 
  separate(date, into = c("year", "month"), sep = 4) %>%
  mutate(
    year = as.numeric(year), 
    state_code = setNames(state.abb, state.name)[location]) %>% 
  filter(year >= 1953 & year <= 2018) %>% 
  group_by(location, year) %>% 
  mutate(
    max_temp = max(value)) %>% 
  select(state_code, location, year, max_temp) %>% 
  distinct()

tmin = read_csv("https://www.ncdc.noaa.gov/cag/statewide/mapping/110-tmin.csv", skip = 3)  %>% 
  janitor::clean_names() %>% 
  separate(date, into = c("year", "month"), sep = 4) %>%
  mutate(
    year = as.numeric(year), 
    state_code = setNames(state.abb, state.name)[location]) %>% 
  filter(year >= 1953 & year <= 2018) %>% 
  group_by(location, year) %>% 
  mutate(
    min_temp = min(value)) %>% 
  select(state_code, location, year, min_temp) %>% 
  distinct()

disasters = read_csv("./data/DisasterDeclarationsSummaries2.csv") %>% 
  janitor::clean_names() %>% 
  mutate(disaster = factor(incident_type)) %>% 
  rename(
    year = fy_declared) %>% 
  filter(year >= 1953 & year <= 2018) %>% 
  count(state, year, disaster) %>%
  group_by(year, state) %>% 
  mutate(
    n_state = sum(n)
  ) %>% 
  group_by(year) %>% 
  mutate(
    n_total = sum(n)
  ) %>% 
  group_by(year, disaster) %>% 
  mutate(
    n_type = sum(n)
  ) %>% 
  rename("n_disaster_state" = "n") 

final_data = 
  inner_join(ave_temp, precip, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>% 
  inner_join(tmax, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>% 
  inner_join(tmin, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>% 
  full_join(disasters, by = c("year" = "year", "state_code" = "state"))

map_data = 
  final_data %>%
  ungroup(location)%>%
  select(state_code, year, mean_temp, max_temp, min_temp, total_precip, n_state) %>%
  rename(mtotal_precip = total_precip) %>%
  rename(mn_state = n_state) %>%
  pivot_longer(cols = (starts_with("m")), names_to = "variable_type",
               names_repair = "check_unique", values_to = "statistic",
               values_ptypes = list()) %>%
  drop_na() %>%
  mutate(
    variable_type = 
      recode(variable_type,
             "mean_temp" = "Yearly Mean Temperature",
             "max_temp" = "Yearly Max Temperature",
             "min_temp" = "Yearly Min Temperature",
             "mtotal_precip" = "Yearly Total Precipitation",
             "mn_state" = "Yearly Total of All Disasters"
      )
  )

variable_type <- (map_data %>% ungroup(year) %>% distinct(variable_type) %>% pull())
year <- c(1953:2019)

ui = fluidPage(

titlePanel("Interactive Map"), 

sidebarLayout(
  sidebarPanel(
    # year select box 
    selectInput("year_1", label = h3("Choose year"),
            choices = as.list(year),
            selected = 2001),# variable select box
    selectInput("variable_type", label = h3("Choose variable"),
            choices = variable_type, 
            selected = "mean_temp")),
    mainPanel(
      plotlyOutput("map")
    )
    )
)



server = function(input, output){
  state_abb <- state.name
names(state_abb) <- state.abb
output$map=renderPlotly({
  # map plot settings.
  geo1 <- list(
    scope = "usa",
    projection = list(type = "state"),
    showlakes = TRUE,
    lakecolor = toRGB("white")
  )
  # display values using color intensity -- This should also be good to go if previous code chunk works.
  map_data %>% 
    filter(variable_type == input$variable_type, year == input$year_1) %>% 
    plot_geo(locationmode = "USA-states") %>% 
    add_trace(
      z = ~statistic,
      locations = ~state_code,
      color = ~statistic,
      colors = "Reds",
      # add text
      text = ~paste(variable_type, state_abb[state_code], sep = "-")
    ) %>%
    layout(
      geo = geo1,
      title = "US Map - State Statistics",
      legend = list(x = 100, y = 0.5)
    )
})}

### Description

# This map allows users to select different years (1953-2019), demonstrating the changes in variables over time and visualize differences across the country. The map includes the following variables:

# * **Yearly Total of All Disasters**: Darker areas represent higher counts of disaster declarations, hovering over each state will display the counts. Blank states means there were no disasters declared or no data available in that state for that year.

# * **Yearly Mean Temperature**: Darker areas represent a higher average temperature in that state across all 12 months of the year that was summarized. Hovering over each state will display the average temperature. Blank states means there is no data available in that state for that year.

# * **Yearly Min Temperature**: Darker areas represent a higher minimum temperature in that state across all 12 months of the year that was summarized. Hovering over each state will display the minimum temperature. Blank states means there is no data available in that state for that year.

# * **Yearly Max Temperature**: Darker areas represent a higher maximum temperature in that state across all 12 months of the year that was summarized. Hovering over each state will display the maximum temperature. Blank states means there is no data available in that state for that year.

# * **Yearly Total Precipitation**: Darker areas represent a higher total precipitation in that state across all 12 months of the year that was summarized. Hovering over each state will display the total precipitation. Blank states means there is no data available in that state for that year.


shinyApp(ui = ui, server = server)

