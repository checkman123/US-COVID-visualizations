library(tidyverse)
library(openintro)
library(albersusa)
library(plotly)
library(shiny)
library(lubridate)
uscovid <- read.csv("https://data.cdc.gov/resource/9mfq-cb36.csv?$limit=2000000")

#seperate date and time for the api call one
Map <- uscovid %>%
  separate(submission_date, c("date", "time"), sep = "T") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

Histogram <- Map %>%
  mutate(tempDate = date) %>%
  separate(tempDate, c("year", "month", "day")) %>%
  mutate(day = as.integer(day), month = as.integer(month), year = as.integer(year))

#Set NA to 0
Map[is.na(Map)] <- 0

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

covidmap <- function(myvar, mydate) {
  #select data on my variable for a single date to create choropleth map
  my_stat <- enquo(myvar)
  
  if(myvar == "tot_death") {myvar <- "Cumulative Deaths"}
  if(myvar == "tot_cases") {myvar <- "Cumulative Cases"}
  if(myvar == "new_case") {myvar <- "Positive Cases"}
  if(myvar == "new_death") {myvar <- "Positive Deaths"}
  
  covid_date <- Map %>%
    filter(date == mydate) %>%
    select(state, plot_stat = !!my_stat)  
  
  us_states <- usa_sf("laea")
  
  mapdata <- left_join(us_states, covid_date, by = c("iso_3166_2" = "state"))
  
  mapdata <- mapdata %>% 
    mutate(text = paste("<b>State: </b>", name, "\n",
          "<b>", myvar ,":</b> ", format(plot_stat,big.mark=",",
                                         scientific=FALSE), sep=""))
  
  m <- ggplot(mapdata) +
    geom_sf(aes(fill = plot_stat, text = text)) +
    scale_fill_continuous(myvar, low="yellow", high="red", labels = scales::comma) +
    my_map_theme()
  
  ggplotly(m, tooltip = "text") %>%
    style(hoveron = "fill")
}

covidHist <- function(varChoices, varYear, varMonth) {
  my_stat <- enquo(varChoices)
  
  if(varChoices == "new_case") {
    varTitle <- "Positive Cases"
    
    covid_date_hist <- Histogram %>%
      filter(year == varYear, month == varMonth) %>%
      group_by(year, month, day, date) %>%
      summarize(count = sum(new_case),
                plot_stat = !!my_stat) %>%
      mutate(text = paste("<b>Day: ", day ,"</b>\nCount: ", 
                          format(count,big.mark=",",scientific=FALSE), sep="")) 
  } else {
    varTitle <- "Positive Deaths"
    
    covid_date_hist <- Histogram %>%
      filter(year == varYear, month == varMonth) %>%
      group_by(year, month, day, date) %>%
      summarize(count = sum(new_death),
                plot_stat = !!my_stat) %>%
      mutate(text = paste("<b>Day: ", day ,"</b>\nCount: ", 
                          format(count,big.mark=",",scientific=FALSE), sep="")) 
  }
  
  x_axis_labels <- min(1):max(31)
  

  
  h <-   ggplot(covid_date_hist, aes(x=day, y=count, text = text)) +
    geom_col(color="darkblue", fill="lightblue",position = "dodge")  +
    scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels, limits = c(1,31)) +
    scale_y_continuous(varTitle, labels = scales::comma, breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  
  ggplotly(h, tooltip = "text") %>%
    style(hoverlabel = list(bgcolor = "white"))
}

#Shiny

map_list <- list("Cumulative Deaths" = "tot_death",
                 "Cumulative Cases" = "tot_cases",
                 "Positive Cases" = "new_case",
                 "Positive Deaths" = "new_death"
                 )

hist_list <- list("Positive Cases" = "new_case",
                  "Positive Death" = "new_death")

hist_year_list <- list("2020" = "2020", 
                       "2021" = "2021", 
                       "2022" = "2022")

hist_month_list <- list("January" = "1",
                        "February" = "2",
                        "March" = "3",
                        "April" = "4",
                        "May" = "5",
                        "June" = "6",
                        "July" = "7",
                        "August" = "8",
                        "September" = "9",
                        "October" = "10",
                        "November" = "11",
                        "December" = "12")

ui <- fluidPage(
  # Application title
  titlePanel("US COVID"),
  tabsetPanel(id = "tab",
              tabPanel(title = "US Covid Choropleth Map", value = "Map",
                       sidebarPanel(selectInput("myvar",
                                                "Select a variable to display:",
                                                choices = map_list,
                                                selected = "tot_death")),
                       sidebarPanel(sliderInput("mydate",
                                                "Select a date to display:",
                                                min = as.Date("2020-01-22", "%Y-%m-%d"),
                                                max = as.Date("2022-03-15", "%Y-%m-%d"),
                                                value = as.Date("2022-03-15", "%Y-%m-%d"))
                       )),
              tabPanel(title = "US Covid Histogram", value = "Histogram",
                       sidebarPanel(selectInput("varChoices",
                                                "Select a variable to display:",
                                                choices = hist_list,
                                                selected = "new_case")),
                       sidebarPanel(selectInput("varYear",
                                                "Select a year to display:",
                                                choices = hist_year_list,
                                                selected = "2022")),
                       sidebarPanel(selectInput("varMonth",
                                                "Select a month to display:",
                                                choices = hist_month_list,
                                                selected = "1"))
              )
  ),
  # Show our plot
  mainPanel( 
    h3(textOutput("TitleText")),
    plotlyOutput("statPlot")
  )
)
# Define server logic required to create the graph
server <- function(input, output) {
  output$TitleText <- renderText({
    if(input$tab == "Map") {
      varText <- input$myvar
      if(varText == "tot_death") {varText <- "Cumulative Deaths"}
      if(varText == "tot_cases") {varText <- "Cumulative Cases"}
      if(varText == "new_case") {varText <- "Positive Cases"}
      if(varText == "new_death") {varText <- "Positive Deaths"}
    }else{
      varText <- input$varChoices
      if(varText == "new_case") {varText <- "Positive Cases"}
      if(varText == "new_death") {varText <- "Positive Deaths"}
    }
    
    paste(varText, " Over Time")
  })
  
  output$statPlot <- renderPlotly({
    if(input$tab == "Map"){
      covidmap(input$myvar,input$mydate)
    } 
    else {
      covidHist(input$varChoices,input$varYear,input$varMonth)
    }
  })
}

shinyApp(ui = ui, server = server)
