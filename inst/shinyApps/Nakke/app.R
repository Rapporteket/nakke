#Videre, 26.april:
# NEDLASTING AV MNDRAPP VIRKER. - PUBLISERT, virker ikke på Shinyapps.io
#LEGG TIL MULIGHET FOR NEDLASTING AV TABELLER. lEGG TIL BARE EN EL TO OG PUBLISER
#HVA FORKLUDRER PUBLISERING?


# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#
#I "tabPanel" viser man fram figurer/tabeller
#I "server"-delen gjøres alle beregninger og legges i "output"

library(shiny)
library(knitr)
#library(magrittr)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)

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


startDato <- '2018-01-01' #Sys.Date()-364
idag <- Sys.Date()
sluttDato <- idag

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

regTitle = 'Norsk kvalitetsregister for Ryggkirurgi: Degenerativ Nakke med FIKTIVE data'


#----------Hente data og evt. parametre som er statistke i appen----------
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
  registryName <- "nakke"
  dbType <- "mysql"
  query <- paste0('SELECT  ...')
  RegData <- rapbase::LoadRegData(registryName, qLivs, dbType)
} #hente data på server

#Definere innhold i felles rullegardinmenyer:
kjonn <- c("Begge"=2, "Menn"=1, "Kvinner"=0)
enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))


# Define UI for application
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  theme = "bootstrap.css",

  # lag logo og tittel som en del av navbar. - Funker det med fluidPage?
  title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
  # sett inn tittle ogsÃ¥ i browser-vindu
  windowTitle = regTitle,
  # velg css (forelÃ¸pig den eneste bortsett fra "naken" utgave)
  #theme = "rap/bootstrap.css",

  # Application title #titlePanel("Testing testing, Nakke"),


  #-------------------Sentralmål (gjsn./median)---------------------------
  tabPanel("Gjennomsnitt",
           h2("Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel", align='center'),
           sidebarPanel(width = 3,
             selectInput(inputId = "valgtVarGjsn", label="Velg variabel",
                         choices = c('Alder' = 'Alder',
                                     'EMS før operasjon, myelopatipasienter' = 'EMSscorePreOp',
                                     'EMS-forbedring, myelopati, 12 mnd.' = 'EMSendr12mnd',
                                     'EMS-forbedring, myelopati, 3 mnd.' = 'EMSendr3mnd',
                                     'EQ5D før operasjon' = 'Eq5DScorePreOp',
                                     'EQ5D-forbedring, 12 mnd.' = 'EQ5Dendr12mnd',
                                     'EQ5D-Forbedring, 3 mnd.' = 'EQ5Dendr3mnd',
                                     'Liggetid etter operasjon' = 'LiggeDognPostop',
                                     'Liggetid, totalt' = 'LiggeDognTotalt',
                                     'NDI før operasjon' = 'NDIscorePreOp',
                                     'NDI-forbedring, 3 mnd.' = 'NDIendr3mnd',
                                     'NDI-forbedring, 12 mnd.' = 'NDIendr12mnd',
                                     'NSR, arm før operasjon' = 'NRSsmerteArmPreOp',
                                     'NSR, nakke før operasjon' = 'NRSsmerteNakkePreOp',
                                     'Total knivtid' = 'KnivtidTotalMin'
                         )
             ),
             dateRangeInput(inputId = 'datovalgGjsn', start = "2018-01-01", end = Sys.Date(),
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannGjsn", label="Kjønn",
                         choices = kjonn
             ),
             sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             ),
             selectInput(inputId = "myelopatiGjsn", label="Myelopati",
                         choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
             selectInput(inputId = "fremBakGjsn", label="Tilgang ",
                         choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
             selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                         choices = c("Gjennomsnitt"='Gjsn', "Median"='Med')),
             br(),
             p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
             selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                         choices = enhetsUtvalg
             ),
             selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet",
                         choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                         'Kvartal'='Kvartal', 'Måned'='Mnd')))

           ),
           mainPanel(
           h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                  til venstre. Man kan også gjøre ulike filtreringer."),
           br(),
             tabsetPanel(
               tabPanel("Figurer"
                        #plotOutput("gjsnTid"),
                        #plotOutput("gjsnGrVar")
                        ),
               tabPanel("Tabeller",
                        uiOutput("tittelGjsn"),
                        br(),
                        column(width = 3,
                               h3("Sykehusvise resultater"),
                               tableOutput("gjsnGrVarTab"),
                               downloadButton(outputId = 'lastNed_gjsnGrVarTab', label='Last ned tabell')),
                        column(width = 1),
                        column(width = 5,
                               h3("Utvikling over tid")
                              # tableOutput("gjsnTidTab"),
                               #downloadButton(outputId = 'lastNed_gjsnTidTab', label='Last ned tabell')
                              ))
             )
           )
) #tab, gjsn
) #fluidpage, dvs. alt som vises på skjermen




#----------------- Define server logic required  -----------------------
server <- function(input, output) {

  library(Nakke)
  library(lubridate)
  library(zoo)
  library(tools)
  system.file('NakkeMndRapp.Rnw', package='Nakke')
  #load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data
  #dato <- '2018-03-16'
  #fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
  #RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
  if (context == "DEV" | context == "TEST" | context == "QA" | context == "PRODUCTION") {
    RegData <- NakkeRegDataSQL() #datoFra = '2017-01-01') #datoFra = datoFra, datoTil = datoTil)

    querySD <- paste0('
          SELECT
            Skjemanavn,	SkjemaStatus,	ForlopsID,	HovedDato,	Sykehusnavn,	AvdRESH,	SkjemaRekkeflg
           FROM SkjemaOversikt
           WHERE HovedDato >= "2014-01-01" ')

    SkjemaData <- rapbase::LoadRegData(registryName="Nakke", query=querySD, dbType="mysql")
    knitr::opts_knit$set(root.dir = './')
    knitr::opts_chunk$set(fig.path='')
  } #hente data på server

  if (!exists('RegData')){
    #Funker:
    data('NakkeRegDataSyn', package = 'Nakke')
  }


  RegData <- NakkePreprosess(RegData = RegData)
  datoTil <- as.POSIXlt(Sys.Date())
  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
  # Nye variable:
  aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
  reshIDdummy <- 601161
  reshID <- reshIDdummy

  #SkjemaRekkeflg #1-pasientskjema, 2-legeskjema, 3- Oppf. 3mnd, 4 - Oppf. 12mnd
  data('SkjemaDataSyn', package = 'Nakke')
  #fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
  #SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
  SkjemaData <- SkjemaData[SkjemaData$SkjemaStatus > -1, ]
  SkjemaData$InnDato <- as.POSIXlt(SkjemaData$HovedDato, format="%Y-%m-%d")
  SkjemaData$Aar <- 1900 + strptime(SkjemaData$InnDato, format="%Y")$year
  SkjemaData$Mnd <- as.yearmon(SkjemaData$InnDato)
  SkjemaData$ShNavn <- as.factor(SkjemaData$Sykehusnavn)


#------------ Gjennomsnitt--------------------------
  output$gjsnGrVar <- renderPlot({
    NakkeFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                      reshID=reshIDdummy,
                      datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                      minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                      erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                      fremBak = as.numeric(input$fremBakGjsn),
                      valgtMaal = input$sentralmaal)
  }, height=600, width=500)

  # output$gjsnTid <- renderPlot({
  #   NakkeFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
  #                   reshID=reshIDdummy,
  #                   datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
  #                   minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
  #                   erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
  #                   fremBak = as.numeric(input$fremBakGjsn),
  #                   valgtMaal = input$sentralmaal,
  #                   tidsenhet = input$tidsenhetGjsn,
  #                   enhetsUtvalg = input$enhetsUtvalgGjsn)
  # }, height=300, width=1000)


observe({ #Sykehusvise gjennomsnitt, figur og tabell
  UtDataGjsnGrVar <- NakkeFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                       reshID=reshIDdummy,
                                       datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                       minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                       erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                                       fremBak = as.numeric(input$fremBakGjsn),
                                       valgtMaal = input$sentralmaal)
  output$tittelGjsn <- renderUI({
    tagList(
      h3(UtDataGjsnGrVar$tittel),
      h5(HTML(paste0(UtDataGjsnGrVar$utvalgTxt, '<br />')))
    )}) #, align='center'

  tabGjsnGrVar <- cbind(Antall = UtDataGjsnGrVar$Ngr, #$Hoved,
                        Sentralmål = UtDataGjsnGrVar$AggVerdier$Hoved)
  colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')

  output$gjsnGrVarTab <- function() {
    kableExtra::kable(tabGjsnGrVar, format = 'html'
                      , full_width=F
                      , digits = c(0,1) #,1,1)[1:antKol]
    ) %>%
      column_spec(column = 1, width_min = '7em') %>%
      column_spec(column = 2:3, width = '7em') %>%
      row_spec(0, bold = T)
  }

  output$lastNed_gjsnGrVarTab <- downloadHandler(
    filename = function(){
      paste0(input$valgtVarGjsn, '_tabGjsnSh .csv')
    },
    content = function(file, filename){
      write.csv2(tabGjsnGrVar, file, row.names = T, na = '')
    })

  output$titteltabGjsnGrVar <- renderUI({
    tagList(
      h3(tabGjsnGrVar$tittel),
      h5(HTML(paste0(tabGjsnGrVar$utvalgTxt, '<br />')))
    )}) #, align='center'



  #------gjsnTid

  # UtDataGjsnTid <- NakkeFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
  #                                  reshID=reshIDdummy,
  #                                  datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
  #                                  minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
  #                                  erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
  #                                  fremBak = as.numeric(input$fremBakGjsn),
  #                                  valgtMaal = input$sentralmaal,
  #                                  tidsenhet = input$tidsenhetGjsn,
  #                                  enhetsUtvalg = input$enhetsUtvalgGjsn)
  #
  # tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
  # grtxt <-UtDataGjsnTid$grtxt
  # if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
  #   grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
  # rownames(tabGjsnTid) <- grtxt
  #
  # antKol <- ncol(tabGjsnTid)
  # navnKol <- colnames(tabGjsnTid)
  # if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}
  #
  # kolGruppering <- c(1,3,3)
  # names(kolGruppering) <- c(' ', UtDataGjsnTid$hovedgrTxt, UtDataGjsnTid$smltxt)
  # output$gjsnTidTab <- function() {
  #   kableExtra::kable(tabGjsnTid, format = 'html'
  #                     , full_width=F
  #                     , digits = 1 #c(0,1,1,1)[1:antKol]
  #   ) %>%
  #     add_header_above(kolGruppering[1:(2+UtDataGjsnTid$medSml)]) %>%
  #     column_spec(column = 1, width_min = '7em') %>%
  #     column_spec(column = 2:(antKol+1), width = '7em') %>%
  #     row_spec(0, bold = T)
  # }
  #
  # output$lastNed_gjsnTidTab <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVarGjsn, '_tabGjsnTid .csv')
  #   },
  #   content = function(file, filename){
  #     write.csv2(tabGjsnTid, file, row.names = T, na = '')
  #   })
  #
}) #observe gjsnGrVar

} #server
# Run the application
shinyApp(ui = ui, server = server)

