#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Styling Download Button"),
  sidebarLayout(
    sidebarPanel(
      h4("Default CSS styling"),
      # default styling
      downloadButton('downloadData1', label= 'Download 1'),
      tags$hr(),

      h4("Styling using tags()"),
      downloadButton("download", label="Download with default CSS"),

      # Using the class parameter in download button and tags() to define the style
      downloadButton("download0", label="Download with style", class = "butt"),
      tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: white;}")), # background color and font color

      downloadButton("download1", label="Download with style", class = "butt1"),
      # style font family as well in addition to background and font color
      tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;} .butt1{font-family: Courier New}")),


      downloadButton("download2", label="Download with style", class = "butt2"),
      # making the font italics this time
      tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}"))

    ),
    mainPanel()
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application
shinyApp(ui = ui, server = server)

