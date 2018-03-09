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
ui <- shinyUI(basicPage(
  textInput('firstname', 'First name', value = 'Jimmy'),
  textInput('lastname', 'Last name', value = 'John'),
  downloadButton(outputId = 'mndRapp', label = 'Månedsrapport til nedlasting')
))



library(knitr)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mndRapp = downloadHandler(
      filename = 'myreport.pdf',

      content = function(file) {
        out = knit2pdf('C:/ResultattjenesteGIT/Nakke/inst/NakkeMndRapp.Rnw', encoding = 'UTF-8', clean = TRUE)
        file.rename(out, file) # move pdf to file for downloading
      },

      contentType = 'application/pdf'
    )

}

# Run the application
shinyApp(ui = ui, server = server)

