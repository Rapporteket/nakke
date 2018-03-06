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
        dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                       label = "Velg sluttdato", language="nb"),
        helpText("Denne endrer bare tabellen med 12-månedersoversikt"),
        selectInput(inputId = "status", label="Ferdigstilt/kladd (legeskjema):",
                    choices = c("Ikke valgt"=2, "Ferdigstilt"=1, "Kladd"=0))
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

        tabPanel("Tabeller",
                 h2("Antall registreringer per måned og avdeling"),
                 tableOutput("tabAvdMnd12"),
                 h2("Registrerte skjema ved hver avdeling for valgte 12 måneder"),
                 tableOutput("tabAvdSkjema12"),
                 h2("Antall registreringer per år og avdeling, siste 5 år"),
                 tableOutput("tabAvdNAar5")),
        tabPanel("Kvalitetsindikatorer", verbatimTextOutput("kvalInd")),
        tabPanel("Figursamlinger", plotOutput("figurer"))
      )
    ) #mainPanel
  ) #xxrLayout
) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required to draw a histogram
server <- function(input, output) {

  library(Nakke)
  library(lubridate)
  #load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data
  dato <- '2018-03-02'
  fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
  RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')

  RegData <- NakkePreprosess(RegData = RegData)
  datoTil <- as.POSIXlt(Sys.Date())
  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
  # Nye variable:
  RegData$Mnd <- RegData$InnDato$mon +1
  RegData$Kvartal <- ceiling(RegData$Mnd/3)
  RegData$Halvaar <- ceiling(RegData$Mnd/6)
  aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')

  #SkjemaRekkeflg #1-pasientskjema, 2-legeskjema, 3- Oppf. 3mnd, 4 - Oppf. 12mnd
  fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
  SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  SkjemaData <- SkjemaData[SkjemaData$SkjemaStatus>-1, ]
  SkjemaData$InnDato <- as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d")
  SkjemaData$Aar <- 1900 + strptime(SkjemaData$InnDato, format="%Y")$year
  SkjemaData$Mnd <- as.yearmon(SkjemaData$InnDato)


#Felles reaktive tabeller
#   reactive({
#   SkjemaData <- SkjemaData[which(SkjemaData$SkjemaStatus == input$status), ]
#   SkjemaData12mnd <- SkjemaData[as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d") > as.POSIXlt(datoFra12), ]
#
# })

  output$tabAvdMnd12 <- renderTable({

    if (input$status %in% 0:1) {SkjemaData12mnd <- SkjemaData12mnd[which(SkjemaData12mnd$SkjemaStatus == input$status), ]}
#Flyttes til overvåkning
    datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))
    SkjemaData12mnd <- SkjemaData[SkjemaData$InnDato < as.POSIXlt(input$datoTil)
                                  & SkjemaData$InnDato > as.POSIXlt(datoFra12), ]

    tabAvdSiste12mnd <- addmargins(table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, c('Sykehusnavn', 'Mnd')]))
    colnames(tabAvdSiste12mnd) <- substring(colnames(tabAvdSiste12mnd),1,3)
    xtable::xtable(tabAvdSiste12mnd)
  },
  rownames = TRUE, digits=0 #, align = c('l', rep('r', ncol(tabAvdSiste12mnd)))
  )



    #Velge ferdigstillelse og tidsintervall. Se Rygg
  output$tabAvdSkjema12 <- renderTable({

    #Flyttes til overvåkning
    datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))
    SkjemaData12mnd <- SkjemaData[SkjemaData$InnDato < as.POSIXlt(input$datoTil)
                                  & SkjemaData$InnDato > as.POSIXlt(datoFra12), ]
    LegeSkjema <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, 'Sykehusnavn'])
    PasientSkjema <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==1, 'Sykehusnavn'])
    Oppf3mnd <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==3, 'Sykehusnavn'])
    Oppf12mnd <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==4, 'Sykehusnavn'])

    tabAvd12MndNskjemaDum <- cbind(
      Lege = LegeSkjema,
      Pasient = PasientSkjema,
      'Oppfølging 3 mnd.' = Oppf3mnd,
      'Oppfølging 12 mnd.' = Oppf12mnd)

    tabAvd12MndNskjemaDum <- addmargins(tabAvd12MndNskjemaDum, margin=1)

    tabAvd12MndNskjema <- cbind(
      tabAvd12MndNskjemaDum[ ,1:2],
      'Pasient (%)' =  sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Pasient']/tabAvd12MndNskjemaDum[,'Lege']*100, 1),
      'Oppfølging 3 mnd.' = tabAvd12MndNskjemaDum[ ,3],
      'Oppfølging 3 mnd. (%)' = sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Oppfølging 3 mnd.']/tabAvd12MndNskjemaDum[,'Lege']*100, '%'),
      'Oppfølging 12 mnd.' = tabAvd12MndNskjemaDum[ ,4],
      'Oppfølging 12 mnd. (%)' =  sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Oppfølging 12 mnd.']/tabAvd12MndNskjemaDum[,'Lege']*100, 1)
      )
    #sprintf('%1.3f'
    xtable::xtable(tabAvd12MndNskjema)
  },
  rownames = T, align = 'r' #c('l', rep('r', ncol(tabAvd12MndNskjema)))
    ) #digits=1,


  output$tabAvdNAar5 <- renderTable({

    tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
    rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle avdelinger:'
    colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
    xtable::xtable(tabAvdAarN)
    #xtable::xtable(tabAvdAarN)
  },
  rownames = T, digits=0)




  output$figurer <- renderPlot({
    #indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
    #indKj <- if (input$erMann %in% 0:1) {which(RegData$ErMann == input$erMann)} else {indKj <- 1:dim(RegData)[1]}
    #indDato <- which(RegData$InnDato >= as.POSIXlt(input$datovalg) & RegData$InnDato <= as.POSIXlt(input$datovalg))
    #print(input$fremBak)
    Utvalg <- NakkeUtvalgEnh(RegData=RegData,
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

