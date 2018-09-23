library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(countrycode)
library(readr)
library(readxl)
library(DT)

pdf(NULL)

origdata <- read_csv("laurel-world-happiness-report-data/data/data_behind_table_2_1_whr_2017.csv")
money.wide <- read_excel("Download-GDPPCconstant-USD-countries.xls", 
                         skip = 2)
money.long <- melt(money.wide, id.vars = c("CountryID","Country"))

origdata <- mutate(origdata, year = as.character(year))
origdata <- left_join(origdata, money.long, by = c("country" = "Country", "year" = "variable"))
origdata <- mutate(origdata, gdp = value)
  
happiness.load <- origdata %>%
  mutate(confidence_in_gov = confidence_in_national_government, 
         Gini_Income = gini_of_household_income_reported_in_gallup_by_wp5_year, 
         Gini_Average = gini_index_world_bank_estimate_average_2000_13,
         continent = countrycode(sourcevar = country, origin = "country.name",destination = "continent"),
         region = countrycode(sourcevar = country, origin = "country.name", destination = "region"),
         continent = as.character(ifelse(country == "Kosovo", "Europe", as.character(continent))),
         region = as.factor(ifelse(country == "Kosovo", "Southern Europe", as.character(region))),
         year_date = as.Date(ISOdate(year, 1, 1))) %>%
  select(country:perceptions_of_corruption, Gini_Income, Gini_Average, gini_index_world_bank_estimate, confidence_in_gov, continent, region, gdp, year_date)





# ggplot(twenty16, aes(x = reorder(country, -life_ladder), y = life_ladder, fill = continent)) + geom_bar(stat = "identity")

# ggplot(twenty16, aes(x = reorder(continent, -life_ladder), life_ladder, fill = continent)) + stat_summary(fun.y = "mean", geom = "bar")



# Building a header
header <- dashboardHeader(title = "Global Happiness Dashboard",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "This text is larger than you can see when you click. But: Who's happy? Who's miserable?", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 10, color = "green",
                                                text =  "Figure out: does money correlate with happiness?")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Owen",
                                         message = HTML("Thankyou, esteemed user, for using this app. Reach out to ostevens@andrew.cmu.edu for more information"),
                                         icon = icon("exclamation-circle"))
                          )
)



#Assign dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    selectInput("continentSelect",
                "Continent:",
                choices = c(sort(unique(happiness.load$continent))),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Americas")),
    # Year
    sliderInput(inputId = "yearSelect",
                label = "Year (2005-2016):",
                min = min(happiness$year),
                max = max(happiness$year),
                value = max(happiness$year),step = 1,round = T, sep = ''),
    menuItem("Scatterplots", tabName = "scatter",badgeLabel = "new", badgeColor = "green")
  )
)

#Body!
body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("Happiness",width = 6),
            valueBoxOutput("GDP")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("GDP", plotlyOutput("plot_gdp"),plotlyOutput("regionalgdp")),
                   tabPanel("Happiness", plotlyOutput("plot_happiness"), plotlyOutput("regionalhap")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Country Stats", DT::dataTableOutput("table"), width = 12))
  ),
  tabItem("scatter",
          fluidPage(
            inputPanel(
              # Select Y
              selectInput("y",
                          "Y Axis:",
                          #choices = c(choices = str_to_title(str_replace_all(names, "_"," ")) = colnames(happiness)),
                          choices = colnames(happiness.load)[c(-(1:2))],
                          selected = "life_ladder"),
              selectInput("x",
                          "X Axis:",
                          choices = colnames(happiness.load)[c(-(1:2))],
                          selected = "gdp")
            ),
            fluidRow(
              plotlyOutput("scatterplot")
            )))))

# Server section
ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  # Reactive input data
  hInput <- reactive({
    happiness <- happiness.load %>%
      # Year Filter
      filter(year == input$yearSelect)
    # Continent Filter
    if (length(input$continentSelect) > 0 ) {
      happiness <- subset(happiness, continent %in% input$continentSelect)
    }
    return(happiness)
  })
  # Reactive melted data
  mhInput <- reactive({
    mhInput() %>%
      melt(c("country", input$x, input$y, "continent"))
  })
  
  
  output$scatterplot <- renderPlotly({
    ggplot(hInput(), aes_string(x = input$x, y = input$y, color = "continent")) +
               geom_point() +
               theme(legend.position = "top")
    
                
              
              
  })
    
  
  
  
  # plot showing happiness by country
  output$plot_happiness <- renderPlotly({
    dat <- hInput()
    ggplot(data = dat, aes(x = reorder(country, -life_ladder), y = life_ladder, fill = continent), text =      paste("country:", country)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(y = "Happiness (out of 10)", x = "Country", title = "Avg. Happiness by Country")
    })
  
  #plot showing gdp by country
  output$plot_gdp <- renderPlotly({
    data <- hInput()
    ggplot(data = data, aes(x = reorder(country, gdp), y = gdp, fill = continent)) + 
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "GDP per capita", x = "Country", title = "GDP by Country")
  })
  

    # GDP broken down by regino
    output$regionalgdp <- renderPlotly({
      data <- hInput()
      ggplot(data, aes(x = reorder(region, -gdp),y = gdp, fill = continent)) +
        stat_summary(fun.y = "mean", geom = "bar") +
        labs(y = "Average Happiness", x = "Region", title = "Avg. GDP by Region") +
        guides(fill = FALSE)
})
    
    # Happiness broken down by region
    output$regionalhap <- renderPlotly({
      data <- hInput()
      ggplot(data, aes(x = reorder(region, -life_ladder), life_ladder, fill = continent)) +
        stat_summary(fun.y = "mean", geom = "bar") +
        labs(y = "Avg GDP per capita", x = "Region", title = "Avg. Happiness by Region") +
        guides(fill = FALSE)
    })

  
  # Data table of countries
  output$table <- DT::renderDataTable({
    tabledisp <- hInput()
    datatable(select(hInput(), country, year, happiness = life_ladder, gdp, social_support:perceptions_of_corruption, gini = gini_index_world_bank_estimate),  options = list(scrollX = TRUE)) %>%
      formatCurrency(4, '$') %>%
      formatRound(c(3,5), 3) %>%
      formatRound('healthy_life_expectancy_at_birth', 1) %>%
      formatPercentage('freedom_to_make_life_choices', 2)
    })
  
  #happiness info box
  output$Happiness <- renderInfoBox({
    h <- hInput()
    happiest <- h[which.max(h$life_ladder), 'country']
    infoBox("Happiest country",value = happiest, subtitle = paste('Out of', nrow(h), "countries"), icon = icon("smile-o"), color = "purple")
  })
  
  #GDP info box
  output$GDP <- renderValueBox({
    h <- hInput()
    num <- round(mean(h$gdp, na.rm = T), 2)
    valueBox(subtitle = "Avg GDP Per Capita", value = num, icon("sort-numeric-asc"), color = "green")
  })
}  




# Run the application 
shinyApp(ui = ui, server = server)
  
  

      
      

    









# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#    
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2] 
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#       
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
