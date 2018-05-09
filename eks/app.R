#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## Only run examples in interactive R sessions
  ui <- fluidPage(
    downloadLink("downloadData", "Download")
  )

  server <- function(input, output) {
    # Our dataset
    data <- mtcars

    output$downloadData <- downloadHandler(
      filename = paste("data-", Sys.Date(), ".csv", sep=""),
      content = function(filename) {
        write.csv(data, file=fileename)
      }
    )
  }

  shinyApp(ui, server)
