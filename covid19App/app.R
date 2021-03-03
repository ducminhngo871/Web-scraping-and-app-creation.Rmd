library(shiny)
library(tidyverse)
library(ggthemes)      # for more themes (including theme_map())
library(gplots)        # for col2hex() function

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(
  selectInput("state", 
              "State", 
              choices = list("Washington", "Illinois", "California", "Arizona",
                             "Massachusetts", "Wisconsin", "Texas", "Nebraska",
                             "Utah", "Oregon", "Florida", "New York",
                             "Rhode Island", "Georgia", "New Hampshire", "North Carolina",
                             "New Jersey", "Colorado", "Maryland", "Nevada", 
                             "Tennessee", "Hawaii", "Indiana","Kentucky",
                             "Minnesota", "Oklahoma", "Pennsylvania","South Carolina",
                             "District of Columbia", "Kansas", "Missouri","Vermont", 
                             "Virginia","Connecticut", "Iowa","Louisiana",
                             "Ohio","Michigan","South Dakota","Arkansas",
                             "Delaware","Mississippi","New Mexico","North Dakota",
                             "Wyoming","Alaska","Maine","Alabama",
                             "Idaho","Montana","Puerto Rico","Virgin Islands",
                             "Guam","West Virginia","Northern Mariana Islands"
                             ),
              selected = list("Washington"),
              multiple = TRUE),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid19 %>% 
      filter(cases >= 20, 
             state %in% input$state) %>% 
      ggplot(aes(x = date, y = cases)) +
      geom_line(aes(color = state)) + 
      labs(title = "Changes in COVID-19 Cases in the US", 
           x = "", 
           y = "", 
           color = "") + 
      scale_y_log10() + 
      facet_wrap(vars(state)) + 
      theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
            legend.position = "none") + 
      theme_minimal()
  })
}
shinyApp(ui = ui, server = server)