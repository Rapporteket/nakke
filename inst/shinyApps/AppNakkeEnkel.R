#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Hvert "conditionalPanel" er området til venstre med rullegardinmenyer, spesifiseres for hver enkelt side.
#I "tabPanel" viser man fram figurer/tabeller
#I "server"-delen gjøres alle beregninger og legges i "output"

library(shiny)
library(knitr)

# Define UI for application that draws figures
ui <- fluidPage( #"Hoved"Layout for alt som vises på skjermen

  # Application title
  titlePanel("TEST, enkel versjon basert på fiktive Nakkedata"),

  # Velge sykehus og vise antall
	fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
      conditionalPanel( #Ønsker ulike valgmuligheter for ulike faner/ark
        'input.ark == "Tabeller"',
        dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                  label = "Velg sluttdato", language="nb"),
        #helpText("Denne endrer bare tabellen med 12-månedersoversikt"),
        selectInput(inputId = "status", label="Ferdigstilt/kladd (legeskjema), kun tab. med månedsoversikt:",
                    choices = c("Ikke valgt"=2, "Ferdigstilt"=1, "Kladd"=0))
      ),

      conditionalPanel( #Denne skal bare vises for figursamlinger
        'input.ark == "Fordelinger"',
        #Rullegardinmeny for valgtVar:
        selectInput(inputId = "valgtVar", label="Velg variabel",
                    choices = c('Alder' = 'Alder', 'Antall nivå operert' = 'AntallNivaaOpr',
                                'Antibiotika' = 'Antibiotika', 'Arbeidstaus før operasjon' = 'ArbeidstausPreOp','Arbeidstaus 3 mnd. etter' = 'Arbeidstaus3mnd',
                                'Arbeidstaus 12 mnd. etter' = 'Arbeidstaus12mnd', 'ASA-grad' = 'ASAgrad',
                                'BMI' = 'BMI', 'Angst (EQ5D) før operasjon' = 'EqAngstPreOp',
                                'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                                'Fornoydhet med behandlinga, 3 mnd. etter' = 'FornoydBeh3mnd',
                                'Fornoydhet med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd' ,
                                'Komorbiditet' = 'Komorbiditet',
                                'Komplikasjoner, pas.rapp. 3 mnd. etter' = 'Kompl3mnd',
                                'Kompllikasjoner ved operasjon' = 'KomplOpr',
                                'Liggedøgn, postoperativt' = 'LiggeDognPostop',
                                'Liggedøgn, totalt' = 'LiggeDognTotalt',
                                'Morsmål' = 'Morsmal',
                                'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                                'Uforetrygdet før operasjon' = 'UforetrygdPreOp',
                                'Utdanning' = 'Utdanning') #c('Alder'='Alder', "Ant. nivå operert" = 'AntallNivaaOpr')
        ),
        dateRangeInput(inputId = 'datovalg', start = "2017-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMann", label="Kjønn",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
        ),
        sliderInput(inputId="alder", label = "Alder", min = 0,
                    max = 110, value = c(0, 110)
        ),
        selectInput(inputId = "myelopati", label="Myelopati",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBak", label="Tilgang ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
        selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
        )
        #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
        #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
      ),

      conditionalPanel( #
        'input.ark == "Andeler"',
        #'input.ark === "Fordelinger" || input.ark === "Andeler" ',
       selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                     choices = c('Alder' = 'Alder',
                     'Andre sykdommer' = 'AndreRelSykdommer',
                     'Antibiotika' = 'Antibiotika',
                     'BMI' = 'BMI',
                     'Komplikasjoner, pasientrapportert 3 mnd. etter' = 'EnhverKompl3mnd',
                     'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                     'Fornøydhet med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd',
                     'Misfornøyd, 3 mnd. etter' = 'Misfor3mnd',
                     'Komplikasjon, dyp infeksjon, 3 mnd. etter' = 'KomplinfekDyp3mnd',
                     'Operasjonsindikasjon, myelopati' = 'OprIndikMyelopati',
                     'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                     'Symptomvariaghet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                     'Søkt uføretrygd før operasjon' = 'UforetrygdPreOp',
                     'Utdanning' = 'Utdanning')
        ),
        dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMannAndelGrVar", label="Kjønn",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
        ),
        sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                    max = 110, value = c(0, 110)
        ),
        selectInput(inputId = "myelopatiAndelGrVar", label="Myelopati",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBakAndelGrVar", label="Tilgang ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
       br(),
       p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
       selectInput(inputId = 'enhetsUtvalgAndelTid', label='Egen enhet og/eller landet',
                   choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
       ),
       selectInput(inputId = "tidsenhetAndelTid", label="Velg tidsenhet",
                   choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                   'Kvartal'='Kvartal', 'Måned'='Mnd')))

      )
	), #sidebarPanel/kolonna til venstre




    # Vise det vi har valgt...
    column(width = 7, #mainPanel(
      tabsetPanel( #
        id='ark',

         tabPanel("Tabeller",
                 h2("Antall registreringer per måned og avdeling"),
                 tableOutput("tabAvdMnd12"),
                 br(),
                 h2("Ferdigstilte skjema ved hver avdeling for valgte 12 måneder"),
                 p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                 tableOutput("tabAvdSkjema12"),
                 br(),
                 h2("Antall registreringer per år og avdeling, siste 5 år"),
                 tableOutput("tabAvdNAar5")
        ),
        tabPanel("Fordelinger",
                 h3("Fordeling av valgt variabel"),
                 h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
                 br(),
                 br(),
                 plotOutput("fordelinger")),
        tabPanel("Andeler",
                 h2("Sykehusvise andeler og utvikling over tid for valgt variabel"),
                 h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
                 br(),
                 br(),
                 plotOutput("andelerGrVar"),
                 plotOutput("andelTid"))
      )
#    )
	) #mainPanel
  ) #xxrLayout
) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required to draw a histogram
server <- function(input, output) {

  library(Nakke)
  library(lubridate)
  library(zoo)
  system.file('inst/NakkeMndRapp.Rnw', package='Nakke')
  #load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data
  dato <- '2018-03-16'
  fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
  #RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  #Funker:
  data('NakkeRegDataSyn', package = 'Nakke')
  #try(data(package = "Nakke"))

  RegData <- NakkePreprosess(RegData = RegData)
  datoTil <- as.POSIXlt(Sys.Date())
  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
  # Nye variable:
  RegData$Mnd <- RegData$InnDato$mon +1
  RegData$Kvartal <- ceiling(RegData$Mnd/3)
  RegData$Halvaar <- ceiling(RegData$Mnd/6)
  aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
  reshIDdummy <- 601161

  #SkjemaRekkeflg #1-pasientskjema, 2-legeskjema, 3- Oppf. 3mnd, 4 - Oppf. 12mnd
  fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
  data('SkjemaDataSyn', package = 'Nakke')
  #SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  SkjemaData <- SkjemaData[SkjemaData$SkjemaStatus > -1, ]
  SkjemaData$InnDato <- as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d")
  SkjemaData$Aar <- 1900 + strptime(SkjemaData$InnDato, format="%Y")$year
  SkjemaData$Mnd <- as.yearmon(SkjemaData$InnDato)
  SkjemaData$Sykehusnavn <- as.factor(SkjemaData$Sykehusnavn)

  output$tabAvdMnd12 <- renderTable({
    datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))
    SkjemaData12mnd <- SkjemaData[SkjemaData$InnDato < as.POSIXlt(input$datoTil)
                                  & SkjemaData$InnDato > as.POSIXlt(datoFra12), ]
    if (as.numeric(input$status) %in% 0:1) {SkjemaData12mnd <-
      SkjemaData12mnd[which(SkjemaData12mnd$SkjemaStatus == as.numeric(input$status)), ]
    }
    #Flyttes til overvåkning
    tabAvdSiste12mnd <- addmargins(table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, c('Sykehusnavn', 'Mnd')]))
    colnames(tabAvdSiste12mnd) <- substring(colnames(tabAvdSiste12mnd),1,3)
    xtable::xtable(tabAvdSiste12mnd)
  },
  rownames = TRUE, digits=0 #, align = c('l', rep('r', ncol(tabAvdSiste12mnd)))
  )



  #Velge ferdigstillelse og tidsintervall.
  output$tabAvdSkjema12 <- renderTable({
    SkjemaDataFerdig <- SkjemaData[SkjemaData$SkjemaStatus ==1, ]
    #Flyttes til overvåkning
    datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))

    #datoFra12 <- '2017-03-01'
    #SkjemaData12mnd <- SkjemaDataFerdig[SkjemaDataFerdig$InnDato < as.POSIXlt('2018-04-30')
    SkjemaData12mnd <- SkjemaDataFerdig[SkjemaDataFerdig$InnDato < as.POSIXlt(input$datoTil)
                                        & SkjemaDataFerdig$InnDato > as.POSIXlt(datoFra12), ]
    LegeSkjema <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, 'Sykehusnavn'])
    PasientSkjema <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==1, 'Sykehusnavn'])
    Oppf3mnd <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==3, 'Sykehusnavn'])
    Oppf12mnd <- table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==4, 'Sykehusnavn'])

    tabAvd12MndNskjemaDum <- cbind(
      Lege = LegeSkjema,
      Pasient = PasientSkjema,
      'Oppf3mnd' = Oppf3mnd,
      'Oppf12mnd' = Oppf12mnd)

    tabAvd12MndNskjemaDum <- addmargins(tabAvd12MndNskjemaDum, margin=1)

    tabAvd12MndNskjema <- cbind(
      tabAvd12MndNskjemaDum[ ,1:2],
      'Pasient (%)' =  sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Pasient']/tabAvd12MndNskjemaDum[,'Lege']*100, 1),
      'Oppfølging 3 mnd.' = tabAvd12MndNskjemaDum[ ,3],
      'Oppfølging 3 mnd. (%)' = sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Oppf3mnd']/tabAvd12MndNskjemaDum[,'Lege']*100, '%'),
      'Oppfølging 12 mnd.' = tabAvd12MndNskjemaDum[ ,4],
      'Oppfølging 12 mnd. (%)' =  sprintf('%1.1f', tabAvd12MndNskjemaDum[,'Oppf12mnd']/tabAvd12MndNskjemaDum[,'Lege']*100, 1)
    )
    #sprintf('%1.3f'
    xtable::xtable(tabAvd12MndNskjema,  align = c('l', rep('r', ncol(tabAvd12MndNskjema))),
                   caption= paste0('Tidsperiode: ', as.POSIXlt(datoFra12), 'til', as.POSIXlt(input$datoTil)))
  },
  rownames = T, align= 'r' #
  ) #digits=1,


  output$tabAvdNAar5 <- renderTable({

    tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
    rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle avdelinger:'
    colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
    xtable::xtable(tabAvdAarN)
    #xtable::xtable(tabAvdAarN)
  },
  rownames = T, digits=0)


  output$fordelinger <- renderPlot({

    NakkeFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                    reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                    datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                    erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                    fremBak = as.numeric(input$fremBak))
  })


  output$andelerGrVar <- renderPlot({

    NakkeFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                    reshID=reshIDdummy,
                    datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                    minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                    erMann=as.numeric(input$erMannAndelGrVar), myelopati = as.numeric(input$myelopatiAndelGrVar),
                    fremBak = as.numeric(input$fremBakAndelGrVar))
  })

  output$andelTid <- renderPlot({

    NakkeFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                         reshID=reshIDdummy,
                         datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                         minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                         erMann=as.numeric(input$erMannAndelGrVar),
                         myelopati = as.numeric(input$myelopatiAndelGrVar),
                         fremBak = as.numeric(input$fremBakAndelGrVar),
                     tidsenhet = input$tidsenhetAndelTid,
                     enhetsUtvalg = input$enhetsUtvalgAndelTid)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

