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

   # Application title
   titlePanel("Testing testing, Nakke"),

   # Velge sykehus og vise antall
   sidebarLayout(
      sidebarPanel(
        # Input: Slider for the number of bins ----
        sliderInput(inputId = "bins", label = "Antall søyler:",
                    min = 1, max = 25, value = 10),
        dateRangeInput(inputId = 'datovalg', start = "2012-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMann", label="Kjønn:",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                    ),
        sliderInput(inputId="alder", label = "Alder", min = 0,
                    max = 130, value = c(0, 130)
        ),
        selectInput(inputId = "myelopati", label="Myelopati:",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBak", label="Tilgang: ",
                                choices = c("Alle"=0, "Fremre"=1, "Bakre"=2))
        #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
         #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
                    ),


      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
         )
      )
   )

#SPØRSMÅL:
#Hvordan kan man finne ut hvordan inputverdiene er lagret og under hvilket navn (ved flere verdier)


# Define server logic required to draw a histogram
server <- function(input, output) {


  library(Nakke)
  load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data

  output$distPlot <- renderPlot({
    #indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
    #indKj <- if (input$erMann %in% 0:1) {which(RegData$ErMann == input$erMann)} else {indKj <- 1:dim(RegData)[1]}
    #indDato <- which(RegData$InnDato >= as.POSIXlt(input$datovalg) & RegData$InnDato <= as.POSIXlt(input$datovalg))
    print(input$fremBak)
    Utvalg <- NakkeLibUtvalg(RegData=RegData,
                             datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                             erMann = as.numeric(input$erMann),
                             minald = as.numeric(input$alder[1]),
                             maxald = as.numeric(input$alder[2]),
                             myelopati = as.numeric(input$myelopati),
                             fremBak = as.numeric(input$fremBak) )
    RegData <- Utvalg$RegData
    x <- as.numeric(RegData$BMI) #[indKj] #[indDato]
    bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlim = c(10, 50), #ceiling(max(RegData$Alder/10)*10)),
         xlab = "BMI",
         main = "BMIfordeling")

  })

}

# Run the application
shinyApp(ui = ui, server = server)

