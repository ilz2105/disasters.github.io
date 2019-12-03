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


ui = fluidPage(
    
    # Application title
    titlePanel("Examining Natural Disasters and Climate Change by US State"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            state_name = 
                final_data %>% 
                filter(state != "NA") %>% 
                distinct(state) %>% 
                pull(state),
            
            selectInput("state_choice", "Select State", choices = state_name),
            
            
            disaster_name = 
                final_data %>% 
                filter(disaster != "NA") %>% 
                distinct(disaster) %>% 
                pull(disaster),
            
            selectInput("disaster_choice", "Select Disaster Type", choices = disaster_name),
            
            
            sliderInput("year_range", "Choose year range", min = 1950, max = 2019, value = c(1950, 2019))
        ),
        
        mainPanel(
            plotOutput("count_styr"),
            plotOutput("count_styr_avgtemp"),
            plotOutput("avgtempchange")
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
                plot_ly(x = ~year, y = ~n_disaster_state, color = ~disaster, type = "bar")
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
                plot_ly(
                    x = ~n_state, y = ~mean_temp, type = "scatter", mode = "markers",
                    alpha = 0.5, color = ~state, text = ~text_label)
        })
    
    output$avgtempchange =
        renderPlotly({
            final_data %>% 
                filter(
                    state == input[["state_choice"]],
                    year %in% input$year_range[1]:input$year_range[2]) %>%
                distinct() %>%
                mutate(text_label = str_c('Average Temperature: ', mean_temp, '\nMinimum Temperature: ', min_temp, '\nMaximum Temperature: ', max_temp)) %>% 
                plot_ly(x = ~year, y = ~mean_temp, color = ~state, type = "bar", text = ~text_label)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
