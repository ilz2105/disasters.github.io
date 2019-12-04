# Loading packages
library(flexdashboard)
library(tidyverse)
library(viridis)
library(plotly)
library(rvest)
library(shiny)
library(rsconnect)

knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
    fig.height = 6,
    out.width = "90%"
)
options(
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
theme_set(theme_minimal() + theme(legend.position = "bottom"))


# Loading and tidying data
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

# Joined data from 5 data sets
final_data = 
    inner_join(ave_temp, precip, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>% 
    inner_join(tmax, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>% 
    inner_join(tmin, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>% 
    full_join(disasters, by = c("year" = "year", "state_code" = "state")) %>% 
    rename ("state" = "location")

state_name = 
    final_data %>% 
    filter(state != "NA") %>% 
    distinct(state) %>% 
    pull(state)

disaster_name = 
    final_data %>% 
    filter(disaster != "NA") %>% 
    distinct(disaster) %>% 
    pull(disaster)


ui = fluidPage(
    
    # Application title
    titlePanel("Examining Natural Disasters and Climate Change by US State"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("state_choice", "Select State", choices = state_name),
            
            selectInput("disaster_choice", "Select Disaster Type", choices = disaster_name),
            
            sliderInput("year_range", "Choose year range", min = 1950, max = 2019, value = c(1950, 2019))
        ),
        
        mainPanel(
            plotlyOutput("count_styr"),
            plotlyOutput("count_styr_avgtemp"),
            plotlyOutput("avgtempchange")
        )
    )
)


server = function(input, output) {
    
    output$count_styr = 
        renderPlotly({
            final_data %>% 
                filter(
                    state == input[["state_choice"]], 
                    disaster == input[["disaster_choice"]],
                    year %in% input$year_range[1]:input$year_range[2]) %>%
                rename(
                    "year" = "Year",
                     "Count of Disaster Type by State and Year" = "n_disaster_state") %>% 
                plot_ly(x = ~year, y = ~n_disaster_state, color = ~disaster, type = "bar", 
                        name = "Count of Disaster Type by State and Year Over Time")
        })
    
    output$count_styr_avgtemp = 
        renderPlotly({
            final_data %>%
                filter(
                    state == input[["state_choice"]], 
                    disaster == input[["disaster_choice"]],
                    n_state != "NA",
                    mean_temp != "NA") %>%
                group_by(state, year) %>%
                mutate(text_label = str_c("Number of Disasters: ", n_state, '\nAverage Temperature: ', mean_temp)) %>% 
                rename(
                    "Average Temperature (degrees F)" = "mean_temp",
                    "Count of Total Disasters By State and Year" = "n_state") %>% 
                 plot_ly(
                    x = ~n_state, y = ~mean_temp, type = "scatter", mode = "markers",
                    alpha = 0.5, color = ~state, text = ~text_label,
                    name = "Count of Total Disasters By State and Year vs. Average Temperature (degrees F)")
        })
    
    output$avgtempchange =
        renderPlotly({
            final_data %>% 
                filter(
                    state == input[["state_choice"]],
                    year %in% input$year_range[1]:input$year_range[2]) %>%
                distinct() %>%
                mutate(text_label = str_c('Average Temperature: ', mean_temp, '\nMinimum Temperature: ', min_temp, '\nMaximum Temperature: ', max_temp)) %>%
                rename(
                    "Year" = "year",
                    "Average Temperature (degrees F)" = "mean_temp") %>% 
                plot_ly(x = ~Year, y = ~`Average Temperature`, color = ~state, 
                        type = "bar", text = ~text_label, 
                        name = "Change in Average Temperature (degrees F) Over Time in State") %>% 
                
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
