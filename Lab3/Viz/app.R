library(shiny)
library(ggplot2)
nj <- read.csv("http://andrewpbray.github.io/data/crime-train.csv", na = c("", "NA", "?"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Crime Data Plots"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("predictor",
                     "Predictor:",
                     choices = colnames(nj)[-127])
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      ggplot(nj, aes_string(x = input$predictor, y = "ViolentCrimesPerPop")) +
        geom_point()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

