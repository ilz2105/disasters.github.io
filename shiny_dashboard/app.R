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
    distinct() %>%
    ungroup()

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
    distinct() %>%
    ungroup()

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
    distinct() %>%
    ungroup()

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
    distinct() %>%
    ungroup()

disasters = read_csv("./data/DisasterDeclarationsSummaries2.csv") %>%
    janitor::clean_names() %>%
    mutate(disaster = factor(incident_type)) %>%
    rename(
        year = fy_declared) %>%
    filter(year >= 1953 & year <= 2018) %>%
    count(state, year, disaster) %>%
    ungroup()

# Joined data from 5 data sets
final_data =
    inner_join(ave_temp, precip, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>%
    inner_join(tmax, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>%
    inner_join(tmin, by = c("location" = "location", "year" = "year", "state_code" = "state_code")) %>%
    full_join(disasters, by = c("year" = "year", "state_code" = "state")) %>%
    mutate(n = replace_na(n, 0)) %>%
    group_by(year, location) %>%
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
    rename("n_disaster_state" = "n") %>%
    mutate(
        region = case_when(
            location %in% c(
                "Alabama","Arkansas","Delaware","Florida","Georgia"
                ,"Kentucky","Louisiana","Maryland","Mississippi",
                "Oklahoma","North Carolina","South Carolina",
                "Tennessee","Texas","Virginia","West Virginia") ~ "southeast",
            location %in% c("Connecticut","Maine","New Hampshire","Massachusetts",
                            "New Jersey","New York","Pennsylvania","Rhode Island","Vermont") ~ 'northeast',
            location %in% c("Alaska",
                            "Arizona","California","Colorado","Hawaii","Idaho",
                            "Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
            ~  'west',
            location %in% c("Illinois",
                            "Indiana","Iowa","Kansas","Michigan","Missouri",
                            "Minnesota","Nebraska","North Dakota","Ohio","South Dakota", "Wisconsin") ~ 'midwest')) %>% 
    rename ("state" = "location")

regional_data = final_data %>%
    group_by(year, region) %>%
    mutate(
        ave_temp = mean(mean_temp),
        sum_precip = sum(total_precip),
        count_region = sum(n_disaster_state)
    ) %>%
    select(region, year, ave_temp, sum_precip, count_region) %>%
    filter(region != "NA") %>%
    distinct() %>%
    ungroup()

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
            
            helpText("On this interactive page, you can examine patterns of natural disasters and 
                     average temperatures in a certain US state between the years 1953-2018. 
                     Average temperature is used as a proxy for climate change in this instance. 
                     You can select a specific state and natural disaster of interest using the dropdown menus. 
                     You can also customize the year range to the period that you are interested in using the
                     slider."),
            
            # Select state
            selectInput("state_choice", "Select State", choices = state_name),
            
            # Select natural disaster type
            selectInput("disaster_choice", "Select Disaster Type", choices = disaster_name),
            
            # Select year range
            sliderInput("year_range", "Choose year range", min = 1953, max = 2018, value = c(1953, 2018))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Trends of Disaster Type", plotlyOutput("count_styr")),
                tabPanel("Trends of Disasters & Average Temperature", plotlyOutput("count_styr_avgtemp")),
                tabPanel("Trends of Average Temperatures", plotlyOutput("avgtempchange")))
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
                plot_ly(x = ~year, y = ~n_disaster_state, color = ~disaster, type = "bar") %>% 
                layout(
                    title = "Count of Disaster Type by State and Year Over Time",
                    xaxis = list(title = "Year",
                                 zeroline = TRUE),
                    yaxis = list(title = "Count of Disaster Type by State and Year",
                                 zeroline = TRUE))
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
                mutate(text_label = str_c("Number of Disaster Events: ", n_state, 
                                          "\nAverage Temperature: ", round(mean_temp, digits = 1), " F")) %>% 
                plot_ly(
                    x = ~n_state, y = ~mean_temp, type = "scatter", mode = "markers",
                    alpha = 0.5, color = ~state, text = ~text_label) %>% 
                layout(
                    title = "Count of Total Disasters By State and Year vs. Average Temperature (degrees F)",
                    xaxis = list(title = "Count of Total Disasters By State and Year", 
                                  zeroline = TRUE),
                    yaxis = list(title = "Average Temperature (degrees F)",
                                 zeroline = TRUE))
        })
    
    
    
    
    output$avgtempchange =
        renderPlotly({
            final_data %>% 
                filter(
                    state == input[["state_choice"]],
                    year %in% input$year_range[1]:input$year_range[2]) %>%
                distinct() %>%
                mutate(text_label = str_c("Average Temperature: ", round(mean_temp, digits = 1), " F", 
                                          "\nMinimum Temperature: ", round(min_temp, digits = 1), " F", 
                                          "\nMaximum Temperature: ", round(max_temp, digits = 1), " F")) %>%
                plot_ly(x = ~year, y = ~mean_temp, color = ~state, 
                        type = "bar", text = ~text_label) %>% 
                layout(
                    title = "Change in Average Temperature (degrees F) Over Time",
                    xaxis = list(title = "Year",
                                 zezroline = TRUE), 
                    yaxis = list(title = "Average Temperature (degrees F)",
                                 zeroline = TRUE)) 
                
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
