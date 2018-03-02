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
        conditionalPanel(
          'input.ark === "Tabeller"',
          helpText("Her kommer det kanskje noen brukervalg")
          ),
        conditionalPanel(
          'input.ark === "Kvalitetsindikatorer"',
          helpText("Her kommer det kanskje noen brukervalg")
        ),

        conditionalPanel( #Denne skal bare vises for figursamlinger
          'input.ark === "Figursamlinger"',
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
        )
      ), #sidebarPanel




      # Vise det vi har valgt...
      mainPanel(
        tabsetPanel(
          id='ark',
          tabPanel("Tabeller", tableOutput("tabeller")),
          tabPanel("Kvalitetsindikatorer", verbatimTextOutput("kvalInd")),
          tabPanel("Figursamlinger", plotOutput("figurer"))
        )
      ) #mainPanel
   ) #xxrLayout
) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required to draw a histogram
server <- function(input, output) {

  library(Nakke)
  #load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data
  dato <- '2018-03-02'
  fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
  RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  RegData <- NakkePreprosess(RegData = RegData)
  DagNaa <- Sys.Date()
  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))


  output$tabeller <- renderTable({
    tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
    rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle avdelinger:'
    colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
    xtable::xtable(tabAvdAarN, digits=0, align=c('l', rep('r', ncol(tabAvdAarN))), rownames = T,
                   caption='Antall registreringer per år og avdeling, siste 5 år.')},
    rownames = T, digits=0
    )

  #output$oversikt <- renderText()

  output$figurer <- renderPlot({
    #indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
    #indKj <- if (input$erMann %in% 0:1) {which(RegData$ErMann == input$erMann)} else {indKj <- 1:dim(RegData)[1]}
    #indDato <- which(RegData$InnDato >= as.POSIXlt(input$datovalg) & RegData$InnDato <= as.POSIXlt(input$datovalg))
    #print(input$fremBak)
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

