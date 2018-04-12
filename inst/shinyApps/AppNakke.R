#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(shinyBS) # Additional Bootstrap Controls

# ui <- shinyUI(basicPage(
#   downloadButton('report')
# ))
#
# server <- function(input, output) {
#   output$report = downloadHandler(
#     filename = 'MndRapp.pdf',
#     content = function(file) {
#       out = knit2pdf('C:/ResultattjenesteGIT/Nakke/inst/NakkeMndRapp.Rnw', encoding = 'UTF-8', clean = TRUE)
#       file.rename(out, file) # move pdf to file for downloading
#     },
#     contentType = 'application/pdf'
#   )
#
# }


# Define UI for application that draws figures
ui <- fluidPage( #"Hoved"Layout for alt som vises på skjermen

  # Application title
  titlePanel("Testing testing, Nakke"),

  # Velge sykehus og vise antall
  #sidebarLayout( #Definerer overordnet layout med en sidekolonne og ett hovedpanel.
    #sidebarPanel( #Området som viser "valgbokser"
	fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
      conditionalPanel( #Ønsker ulike valgmuligheter for ulike faner/ark
        'input.ark == "Tabeller"',
        dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                  label = "Velg sluttdato", language="nb"),
        #helpText("Denne endrer bare tabellen med 12-månedersoversikt"),
        selectInput(inputId = "status", label="Ferdigstilt/kladd (legeskjema), kun tab. med månedsoversikt:",
                    choices = c("Ikke valgt"=2, "Ferdigstilt"=1, "Kladd"=0))
      ),

      conditionalPanel(
        'input.ark == "Viktigste resultater"',
        selectInput(inputId = "valgtVarKvalInd", label="Velg variabel",
                    choices = c('Komplikasjon, stemme' = 'KomplStemme3mnd',
                                'Komplikasjon, svelging' = 'KomplSvelging3mnd')),
        dateInput(inputId = "datoFraKvalInd", label='Velg startdato', value = "2017-01-01"),
        selectInput(inputId = "tidsenhetKvalInd", label="Velg tidsenhet",
                    choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                'Kvartal'='Kvartal', 'Måned'='Mnd'))),
        selectInput(inputId = "myelopatiKvalInd", label="Myelopati:",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBakKvalInd", label="Tilgang: ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
        selectInput(inputId = 'enhetsUtvalgKvalInd', label='Egen enhet og/eller landet',
                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
        )
      ),

      conditionalPanel( #Denne skal bare vises for figursamlinger
        'input.ark == "Fordelinger"',
        #'input.ark === "Fordelinger" || input.ark === "Sykehusvise andeler" ',

        selectInput(inputId = "valgtVar", label="Velg variabel",
                    choices = c('Alder' = 'Alder', 'Antall nivå operert' = 'AntallNivaaOpr', 'Antibiotika' = 'Antibiotika' ,
                                'ArbeidstausPreOp' = 'ArbeidstausPreOp','Arbeidstaus3mnd' = 'Arbeidstaus3mnd',
                                'Arbeidstaus12mnd' = 'Arbeidstaus12mnd', 'ASAgrad' = 'ASAgrad', 'BMI' = 'BMI',
                                'EqAngstPreOp' = 'EqAngstPreOp', 'ErstatningPreOp' = 'ErstatningPreOp',
                                'FornoydBeh3mnd' = 'FornoydBeh3mnd', 'FornoydBeh12mnd' = 'FornoydBeh12mnd' ,
                                'Komorbiditet' = 'Komorbiditet', 'Kompl3mnd' = 'Kompl3mnd', 'KomplOpr' = 'KomplOpr',
                                'LiggeDognPostop' = 'LiggeDognPostop', 'LiggeDognTotalt' = 'LiggeDognTotalt',
                                'Morsmal' = 'Morsmal', 'NytteOpr3mnd' = 'NytteOpr3mnd', 'NytteOpr12mnd' = 'NytteOpr12mnd',
                                'OperasjonsKategori' = 'OperasjonsKategori', 'OprIndik' = 'OprIndik',
                                'OprIndikPareseGrad' = 'OprIndikPareseGrad', 'OprIndikMyelopati' = 'OprIndikMyelopati',
                                'OprIndikSmerter' = 'OprIndikSmerter', 'Radiologi' = 'Radiologi', 'Roker' = 'Roker',
                                'Snuser' = 'Snuser', 'SivilStatus' = 'SivilStatus', 'Sårdren' = 'Saardren',
                                'SmertestillBrukPreOp' = 'SmertestillBrukPreOp', 'SymptVarighetArmer' = 'SymptVarighetArmer',
                                'SymptVarighetNakkeHode' = 'SymptVarighetNakkeHode', 'TidlOpr' = 'TidlOpr',
                                'TidlOprAntall' = 'TidlOprAntall', 'UforetrygdPreOp' = 'UforetrygdPreOp',
                                'Utdanning' = 'Utdanning') #c('Alder'='Alder', "Ant. nivå operert" = 'AntallNivaaOpr')
        ),
        dateRangeInput(inputId = 'datovalg', start = "2017-01-01", end = Sys.Date(),
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
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
        selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
        )
        #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
        #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
      ),

      conditionalPanel( #
        'input.ark == "Sykehusvise andeler"',
        #'input.ark === "Fordelinger" || input.ark === "Sykehusvise andeler" ',
       selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                     choices = c('Alder' = 'Alder', 'AndreRelSykdommer' = 'AndreRelSykdommer',
                     'Antibiotika' = 'Antibiotika', 'ArbeidstausPreOp' = 'ArbeidstausPreOp',
                     'Arbeidstaus3mnd' = 'Arbeidstaus3mnd', 'Arbeidstaus12mnd' = 'Arbeidstaus12mnd',
                     'ASAgrad' = 'ASAgrad', 'BMI' = 'BMI',
                     'EnhverKompl3mnd' = 'EnhverKompl3mnd', 'ErstatningPreOp' = 'ErstatningPreOp',
                     'FornoydBeh3mnd' = 'FornoydBeh3mnd', 'FornoydBeh12mnd' = 'FornoydBeh12mnd',
                     'Misfor3mnd' = 'Misfor3mnd', 'Misfor12mnd' = 'Misfor12mnd',
                     'KomplinfekDyp3mnd' = 'KomplinfekDyp3mnd', 'KomplinfekOverfl3mnd' = 'KomplinfekOverfl3mnd',
                     'KomplStemme3mnd' = 'KomplStemme3mnd', 'KomplSvelging3mnd' = 'KomplSvelging3mnd',
                     'NDIendr12mnd30pst' = 'NDIendr12mnd30pst', 'NytteOpr3mnd' = 'NytteOpr3mnd',
                     'NytteOpr12mnd' = 'NytteOpr12mnd', 'NRSsmerteArmEndr12mnd' = 'NRSsmerteArmEndr12mnd',
                     'Verre3mnd' = 'Verre3mnd', 'Verre12mnd' = 'Verre12mnd',
                     'OprIndikMyelopati' = 'OprIndikMyelopati', 'Roker' = 'Roker',
                     'Saardren' = 'Saardren', 'SmertestillPreOp' = 'SmertestillPreOp',
                     'SymptVarighetArmer' = 'SymptVarighetArmer', 'SymptVarighetNakkeHode' = 'SymptVarighetNakkeHode',
                     'UforetrygdPreOp' = 'UforetrygdPreOp', 'Utdanning' = 'Utdanning')
        ),
        dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
                       label = "TidsperiodeAndelGrVar", separator="t.o.m.", language="nb"),
        selectInput(inputId = "erMannAndelGrVar", label="Kjønn:",
                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
        ),
        sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                    max = 130, value = c(0, 130)
        ),
        selectInput(inputId = "myelopatiAndelGrVar", label="Myelopati:",
                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
        selectInput(inputId = "fremBakAndelGrVar", label="Tilgang: ",
                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2))
      )

    ), #sidebarPanel/kolonna til høyre




    # Vise det vi har valgt...
    column(width = 7, #mainPanel(
      tabsetPanel( #
        id='ark',

        tabPanel("Viktigste resultater",
                 #fluidRow(
                   #column(width=5,
                 h2("Månedsrapport:"), #),
                   #column(width=2,
                 downloadButton(outputId = 'mndRapp', label='Last ned månedsrapport (tar litt tid)', class = "butt"),
                 tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                   #)),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 h2("Kvalitetsindikatorer", align='center' ),
                 br(),
                 h3(em("Utvikling over tid")),
                 plotOutput("kvalIndFig1"),
                 br(),
                 h3(em("Sykehusvise resultater")),
                 plotOutput("kvalIndFig2")),
        tabPanel("Tabeller",
                 h2("Antall registreringer per måned og avdeling"),
                 tableOutput("tabAvdMnd12"),
                 br(),
                 h2("Ferdigstilte skjema ved hver avdeling for valgte 12 måneder"),
                 tableOutput("tabAvdSkjema12"),
                 br(),
                 h2("Antall registreringer per år og avdeling, siste 5 år"),
                 tableOutput("tabAvdNAar5")
        ),
        tabPanel("Fordelinger",
                 h3("Figur som viser fordeling av valgt variabel"),
                 h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
                 br(),
                 plotOutput("fordelinger")),
        tabPanel("Sykehusvise andeler",
                 h2("Figuren viser sykehusvise andeler for valgt variabel"),
                 h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
                 h4(' '),
                 h4(' '),
                 plotOutput("andelerGrVar"))
      )
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
  reshID <- 601161

  #SkjemaRekkeflg #1-pasientskjema, 2-legeskjema, 3- Oppf. 3mnd, 4 - Oppf. 12mnd
  fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
  data('SkjemaDataSyn', package = 'Nakke')
  #SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  SkjemaData <- SkjemaData[SkjemaData$SkjemaStatus > -1, ]
  SkjemaData$InnDato <- as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d")
  SkjemaData$Aar <- 1900 + strptime(SkjemaData$InnDato, format="%Y")$year
  SkjemaData$Mnd <- as.yearmon(SkjemaData$InnDato)
  SkjemaData$Sykehusnavn <- as.factor(SkjemaData$Sykehusnavn)

  reshID <- 601161



  #Felles reaktive tabeller
  #   reactive({
  #   SkjemaData <- SkjemaData[which(SkjemaData$SkjemaStatus == input$status), ]
  #   SkjemaData12mnd <- SkjemaData[as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d") > as.POSIXlt(datoFra12), ]
  #
  # })

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
    xtable::xtable(tabAvd12MndNskjema,  align = c('l', rep('r', ncol(tabAvd12MndNskjema))))
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

#output$tekstDash <- c('Figurer med kvalitetsindikatorer',
#                      'hente ned månedsrapport'),
  output$mndRapp = downloadHandler(
     filename = 'MndRapp.pdf',
    #content = function(file) file.copy(system.file('NakkeMndRapp.pdf', package = 'Nakke'), file, overwrite = TRUE),
    content = function(file) {
      # permission to the current working directory
      src <- normalizePath(system.file('NakkeMndRapp.Rnw', package='Nakke'))
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'NakkeMndRapp.Rnw', overwrite = TRUE)

       texfil <- knitr::knit(system.file('NakkeMndRapp.Rnw', package='Nakke'), encoding = 'UTF-8')
       texi2pdf(system.file(texfil, package='Nakke'),clean = TRUE) #"NakkeMndRapp.tex"
      #help(render_latex)

      out = system.file('NakkeMndRapp.pdf', package = 'Nakke')
        #knit2pdf(system.file('NakkeMndRapp.Rnw', package='Nakke'), clean = TRUE, encoding = 'UTF-8')

      file.rename(out, file) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )
#  If you already have made the PDF file, you can just copy it to file, i.e.
#  content = function(file) file.copy('your_existing.pdf', file, overwrite = TRUE)

  output$kvalIndFig1 <- renderPlot({

    NakkeFigAndelTid(RegData=RegData, preprosess=0, reshID = reshID,
                   valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                   myelopati = as.numeric(input$myelopatiKvalInd),
                   fremBak = as.numeric(input$fremBakKvalInd),
                   enhetsUtvalg = as.numeric(input$enhetsUtvalgKvalInd), tidsenhet = input$tidsenhetKvalInd)
 } )

  output$kvalIndFig2 <- renderPlot(
    NakkeFigAndelerGrVar(RegData=RegData, preprosess=0,
                         valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                         myelopati = as.numeric(input$myelopatiKvalInd),
                         fremBak = as.numeric(input$fremBakKvalInd))
  )


  output$fordelinger <- renderPlot({

    NakkeFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                    reshID=601161, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                    datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                    erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                    fremBak = as.numeric(input$fremBak))
  })


  output$andelerGrVar <- renderPlot({

    NakkeFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                    reshID=601161,
                    datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                    minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                    erMann=as.numeric(input$erMannAndelGrVar), myelopati = as.numeric(input$myelopatiAndelGrVar),
                    fremBak = as.numeric(input$fremBakAndelGrVar))
  })

}

# Run the application
shinyApp(ui = ui, server = server)

