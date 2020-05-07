
## COVID-2019 interactive mapping tool

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# load required packages
library(tidyverse)
library("ggmap")
library(maptools)
library(maps)
library(ggplot2)
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# update data with automated script
# source("Dataupdate.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
countries = read_csv("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/countries_codes_and_coordinates.csv")[,-1]

### MAP FUNCTIONS ###
# function to draw maps.
distmap = function(cv_cases, plot_date) {
  plot_df = subset(cv_cases, date == plot_date)
  mapWorld <- borders("world", colour="grey50", fill="lightgoldenrodyellow") # create a layer of borders
  mp <- ggplot() + mapWorld + 
    geom_point(aes(x = plot_df$longitude, y = plot_df$latitude, col = "tomato", size = plot_df$outcome))
  mp
}


# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 1th confirmed case", "Day of 1th death"), plot_start_date) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, 
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 1th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case1>0)
    g = ggplot(cv_cases, aes(x = days_since_case1, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_case1, "\n", region, ": ",new_outcome)))+
      xlab("Days since 1th confirmed case")
  }
  
  if (start_point=="Day of 1th death") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_death1, "\n", region, ": ",new_outcome))) +
      xlim(c(plot_start_date,current_date+1)) +
      xlab("Days since 1th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 1th confirmed case", "Day of 1th death"), plot_start_date) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,current_date+1)) + xlab("Date")
  }
  
  if (start_point=="Day of 1th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case1>0)
    g = ggplot(cv_cases, aes(x = days_since_case1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case1,"\n", region, ": ",outcome))) +
      xlab("Days since 1th confirmed case")
  }
  
  if (start_point=="Day of 1th death") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death1,"\n", region, ": ",outcome))) +
      xlab("Days since 1th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 1th confirmed case", "Day of 1th death"), plot_start_date)  {
  if (start_point=="Date"){
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 1th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case1>0)
    g = ggplot(cv_cases, aes(x = days_since_case1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case1, "\n", region, ": ",outcome))) +
      xlab("Days since 1th confirmed case")
  }
  
  if (start_point=="Day of 1th death") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death1, "\n", region, ": ",outcome))) +
      xlab("Days since 1th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(as.character(current_date)),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = inner_join(cv_cases, countries %>% select(-jhu_ID), by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$deathsper100k = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/100000),1),nsmall = 1))
cv_cases$newdeathsper100k = as.numeric(format(round(cv_cases$new_deaths/(cv_cases$population/100000),1),nsmall = 1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 1th case and 1th death
cv_cases$days_since_case1 = cv_cases$days_since_death1 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case1[country_db$cases>=1] = 1:sum(country_db$cases>=1)
  country_db$days_since_death1[country_db$deaths>=1] = 1:sum(country_db$deaths>=1)
  cv_cases$days_since_case1[cv_cases$country==country_name] = country_db$days_since_case1
  cv_cases$days_since_death1[cv_cases$country==country_name] = country_db$days_since_death1
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_1000 = subset(cv_today, cases>=100)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case1, days_since_death1)), "input_data/coronavirus_today.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case1 = cv_cases_global$days_since_death1 = 1:nrow(cv_cases_global)
cv_cases_global$per100k = as.numeric(format(round(cv_cases_global$cases/80000,2), nsmall = 1))
cv_cases_global$deathper100k = as.numeric(format(round(cv_cases_global$deaths/80000,2), nsmall = 1))
cv_cases_global$newper100k = as.numeric(format(round(cv_cases_global$new_cases/80000,2),nsmall = 1))
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")


### SHINY UI ###
ui <- fluidPage(
  tabPanel("COVID-19 Mapper",
           
           sidebarLayout(
             sidebarPanel(
               h3(textOutput("reactive_case_count"), align = "right"),
               h4(textOutput("reactive_death_count"), align = "right"),
               span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
               span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
               h6(textOutput("clean_date_reactive"), align = "right"),
               h6(textOutput("reactive_country_count"), align = "right"),
               plotOutput("epi_curve", height="130px", width="100%"),
               plotOutput("cumulative_plot", height="130px", width="100%"),
               
               pickerInput("data_select", "Type:",
                           choices = c("Cumulative Cases", "New Cases", "Active Cases", "Deaths", "New Deaths", "Recovered", "New Recovered", "Cases per 100k", "Death per 100k"),
                           selected = c("Cumulative Cases"),
                           multiple = F),
               
               sliderInput("plot_date",
                           label = h5("Select mapping date"),
                           min = as.Date(cv_min_date,"%Y-%m-%d"),
                           max = as.Date(current_date,"%Y-%m-%d"),
                           value = as.Date(current_date),
                           timeFormat = "%d %b", 
                           animate=animationOptions(interval = 3000, loop = FALSE))
               
             ),
             mainPanel(plotlyOutput("mymap"))
           )
  ),
  tabPanel("Region Plots",
           sidebarLayout(
             sidebarPanel(
               
               pickerInput("level_select", "Level:",   
                           choices = c("Global", "Country"), 
                           selected = c("Country"),
                           multiple = FALSE),
               
               pickerInput("region_select", "Country/Region:",   
                           choices = as.character(cv_today_1000[order(-cv_today_1000$cases),]$country), 
                           options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                           selected = cv_today_1000$country,
                           multiple = TRUE), 
               
               pickerInput("outcome_select", "Outcome:",   
                           choices = c("Deaths per 100,000", "Cases per 100,000", "Cases (total)", "Deaths (total)"), 
                           selected = c("Cases (total)"),
                           multiple = FALSE),
               
               pickerInput("start_date", "Plotting start date:",   
                           choices = c("Date", "Day of 1th confirmed case", "Day of 1th death"), 
                           options = list(`actions-box` = TRUE),
                           selected = "Date",
                           multiple = FALSE), 
               
               sliderInput("minimum_date",
                           "Start Date:",
                           min = as.Date(cv_min_date,"%Y-%m-%d"),
                           max = as.Date(current_date,"%Y-%m-%d"),
                           value=as.Date(cv_min_date),
                           timeFormat="%d %b"),
               
               "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                 tabPanel("New", plotlyOutput("country_plot")),
                 tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
               )
             )
           )
  ),
)

### SHINY SERVER ###
server = function(input, output, session){
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
    # reactive = cv_cases %>% filter(date == "2020-04-25")
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  ####
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
  })
  
  output$reactive_active_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_1000[order(-cv_today_1000$cases),]$country), 
                        selected = cv_today_1000$country)
    }
  }, ignoreInit = TRUE)
  
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    if (input$outcome_select=="Cases (total)") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths (total)") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    if (input$outcome_select=="Cases per 100,000") { 
      db$outcome = db$per100k 
      db$new_outcome = db$newper100k 
    }
    
    if (input$outcome_select=="Deaths per 100,000") { 
      db$outcome = db$deathsper100k 
      db$new_outcome = db$newdeathsper100k 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db(), start_point=input$start_date, input$minimum_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date, input$minimum_date)
  })
  
  map_reactive_db = reactive({
    db = cv_cases
    if (input$data_select == "Cumulative Cases"){
      db$outcome = db$cases
    }
    
    if (input$data_select == "New Cases"){
      db$outcome = db$new_cases
    }
    
    if (input$data_select == "Active Cases"){
      db$outcome = db$active_cases
    }
    
    if (input$data_select == "Deaths"){
      db$outcome = db$deaths
    }
    
    if (input$data_select == "New Deaths"){
      db$outcome = db$new_deaths
    }
    
    if (input$data_select == "Recovered"){
      db$outcome = db$recovered
    }
    
    if (input$data_select == "New Recovered"){
      db$outcome = db$new_recovered
    }
    
    if (input$data_select == "Cases per 100k"){
      db$outcome = db$per100k
    }
    
    if (input$data_select == "Death per 100k"){
      db$outcome = db$deathper100k
    }
    
    db
  })
  
  output$mymap = renderPlotly(
    {distmap(map_reactive_db(), plot_date = input$plot_date)
  })
  
}


### RUN APP ###
shinyApp(ui, server)
