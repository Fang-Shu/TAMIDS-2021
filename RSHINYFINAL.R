#NEWST VERSIONe
library(shiny)
library(tidyverse)
library(rstanarm)
library(shinyWidgets)
library(tidymodels)
library(shinythemes)
library(gt)
library(gtsummary)
library(png)

plot_1 = readRDS("newplot_1.RDS")
contribution_votes = readRDS("newcontribution_votes.RDS")
votes = readRDS("newvotes.RDS")

ui <- navbarPage(
  "US Senate Elections Campaign Financing",
  theme = shinytheme("flatly"),
  
  tabPanel("Trends by Party",
           fluidPage(
             titlePanel("Senate campaign spendings by year"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Scroll down and Select a state"),
                 
                 selectInput("state",
                             label = "State:",
                             choices = state.name,
                             selected = "Alabama"
                 )
               ),
               

               mainPanel(column(12, fluidRow(plotOutput("disbursement_win")
               )
               ),
               column(12, fluidRow(plotOutput("national_trend")
               )
               ),

               )
             )
           )
  ),

  
  tabPanel("Votes vs Spending",
           fluidPage(theme = shinytheme("united"),
             titlePanel("Relationship between Campaign Expenditures and Percent of Votes"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 helpText("Scroll down and choose a year"),
                 sliderTextInput("year_2",

                                 label = "Year:",
                                 choices = c("1980", "1982", "1984", 
                                             "1986", "1988", "1990", 
                                             "1992", "1994", "1996", 
                                             "1998", "2000", "2002", 
                                             "2004", "2006", "2008", 
                                             "2010", "2012", "2014",
                                             "2016", "2018") ,
                                 selected = 1980)
               ),
               

               mainPanel(
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                      plotOutput("linear"),
                                      plotOutput("loess")
                 )
                 ),
                 #h2(""),
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                      plotOutput("lin_reg"), 
                                      gt_output("lin_reg_metrics")
                 )       
                 ),

               )),
           )
  ),
  tabPanel("Introduction",
           titlePanel("About"),
           h3("Confident Outliers"),
           #img(src = "senate.jpg", height = 140, width = 400, alt="somethingg went wrong"),
           p("This is a R shiny dashboard that visualizes the role of financing in senate elections. \n
           Users will be able to visualize the amount that winning campaigns outpent their components through 1980 to 2018 in each of the 50 states.
           Users can also select a year and visualize the relationship between the votes received in percentages and the amount of campaign spending.
               "),
           h4("Observations"),
           p("We may see that for the majority of the elections the winning campaign will outspend the losing campaign. There is not a significant
             relationship between the proportion of votes a person receives and the amount of money spent. However, it is evident that both parties
             are seeing an increase in funding throughout the past years, and democrats tend to spend more than republicans.")
))

server <- function(input, output) {
  
  output$disbursement_win <- renderPlot({
    plot_1 %>%  
      
      filter(state.x == input$state) %>%
      ggplot(aes(x = year, y = diff/1000, color = party)) +
      geom_point(size = 5) +
      
      scale_color_manual(breaks = c("DEM", "REP", "IND"),
                         values = c("DEM" = "blue", "REP" = "red", "IND" = "green"), 
                         labels = c("Democrat", "Republican", "Independent")) +
      labs(title = "How Much Winning Campaigns Outspent Opponents",
           x = "Year",
           y = "Spending Difference (in Millions)",
           color = "Party") +
      scale_x_discrete(breaks = c(1980, 1982, 1984, 1986, 1988, 1990, 
                                  1992, 1994, 1996, 1998, 2000, 2002, 
                                  2004, 2006, 2008, 2010, 2012, 2014, 
                                  2016, 2018)) +
      theme_bw() +
      
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 15), 
            axis.text = element_text(size = 15), 
            axis.title = element_text(size = 15),
            plot.title = element_text(hjust = .5))
  })
  
  output$national_trend <- renderPlot({
    contribution_votes %>% 
      mutate(year = as.numeric(year)) %>% 
      filter(party == "DEM" | party == "REP") %>% 
      ggplot(aes(x = year, y = disbursements/1000, color = party)) + 
      geom_point(alpha = .3) +
      geom_smooth() +
      
      scale_color_manual(values = c("DEM" = "blue", "REP" = "red"), 
                         labels = c("Democrat", "Republican")) +
      labs(title = "Campaign Spending by year",
           x = "Year",
           y = "Spendings (in Millions)",
           color = "Party") +
      theme_classic() +
      theme(text = element_text(size = 15), 
            axis.text = element_text(size = 15), 
            axis.title = element_text(size = 15),
            plot.title = element_text(hjust = .5)) +
      scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2018))
  })
  
  output$linear <- renderPlot({
    data <- votes %>% 
      filter(year == input$year_2)
    model_votes <- stan_glm(data = data,
                            vote_percentage ~ disbursements,
                            family = gaussian(),
                            refresh = 0)

    data %>% 
      ggplot(aes(x = disbursements, y = vote_percentage)) +
      geom_point() +
      theme_classic() +
      labs(title = "Votes Received vs Spending",
           x = "Spending (in Thousands)",
           y = "Votes Received (Percentage") +
      theme(text = element_text(size = 15), 
            axis.text = element_text(size = 15), 
            axis.title = element_text(size = 15),
            plot.title = element_text(hjust = .5)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1, 
                                                         accuracy = 1L)) +
      scale_x_continuous(labels = scales::comma_format())
  })
}
shinyApp(ui = ui, server = server)