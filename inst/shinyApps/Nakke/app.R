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


  #------------ Viktigste resultater-----------------
  tabPanel(p("Viktigste resultater", title='Kvalitetsindikatorer og månedsrapport'),
           h2("Kvalitetsindikatorer", align='center' ),
           br(),
           sidebarPanel(width=3,
             h2("Månedsrapport"), #),
             downloadButton(outputId = 'mndRapp.pdf', label='Last ned månedsrapport', class = "butt"),
             tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
             br(),
             br(),
             br(),
             br(),
             h3('Gjør utvalg i figurene'),
             selectInput(inputId = "valgtVarKvalInd", label="Velg variabel",
                         choices = c('Komplikasjon, stemme' = 'KomplStemme3mnd',
                                     'Komplikasjon, svelging' = 'KomplSvelging3mnd')),
             dateInput(inputId = "datoFraKvalInd", label='Velg startdato', value = "2018-01-01"),
             selectInput(inputId = "myelopatiKvalInd", label="Myelopati",
                         choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
             selectInput(inputId = "fremBakKvalInd", label="Tilgang ",
                         choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
             br(),
             helpText('Følgende valg gjelder bare tidsfigur:'),
             selectInput(inputId = "tidsenhetKvalInd", label="Velg tidsenhet",
                         choices = tidsenhetValg),
             selectInput(inputId = 'enhetsUtvalgKvalInd', label='Egen enhet og/eller landet',
                         choices = enhetsUtvalg
             )
           ),

           mainPanel(
                       #h3(em("Utvikling over tid")),
                       plotOutput("kvalIndFig1")),
           br(),
           #h3(em("Sykehusvise resultater")),
           plotOutput("kvalIndFig2")

  ), #tab

  #------------- Tabeller (Velge sykehus og vise antall)--------------------

  tabPanel("Registreringsoversikter",
           sidebarPanel(width=3,
                        h3('Utvalg'),
                        conditionalPanel(condition = "input.ark == 'Antall operasjoner'",
                                         dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                   value = Sys.Date(), max = Sys.Date() )
                        ),
                        conditionalPanel(
                          condition = "input.ark == 'Antall operasjoner'",
                          selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                      choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                        conditionalPanel(
                          condition = "input.ark == 'Antall skjema'",
                          dateRangeInput(inputId = 'datovalgReg', start = startDato, end = Sys.Date(),
                                         label = "Tidsperiode", separator="t.o.m.", language="nb"),
                          selectInput(inputId = 'skjemastatus', label='Velg skjemastatus',
                                      choices = c("Ferdigstilt"=1,
                                                  "Kladd"=0,
                                                  "Åpen"=-1)
                          )

                        )
                        # sidebarPanel( width = 3,
                        #               #side(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
                        #               dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                        #                         label = "Velg sluttdato", language="nb"),
                        #               #helpText("Denne endrer bare tabellen med 12-månedersoversikt"),
                        #               selectInput(inputId = "status", label="Ferdigstilt/kladd (legeskjema), kun tab. med månedsoversikt:",
                        #                           choices = c("Ikke valgt"=2, "Ferdigstilt"=1, "Kladd"=0))),
                        #
           ),

           mainPanel(
             tabsetPanel(id='ark',
                         tabPanel('Antall operasjoner',
                                  uiOutput("undertittelReg"),
                                  p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"),
                                  br(),
                                  fluidRow(
                                    tableOutput("tabAntOpphSh"),
                                    downloadButton(outputId = 'lastNed_tabAntOpphSh', label='Last ned')
                                  )

                                  # h2("Antall registreringer per avdeling"),
                                  # tableOutput("tabAvdMnd12"),
                                  # h2("Antall registreringer per år og avdeling, siste 5 år"),
                                  # tableOutput("tabAvdNAar5"))

                         ),
                         tabPanel('Antall skjema',
                                  h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                  p("Velg tidsperiode i menyen til venstre"),
                                  br(),
                                  fluidRow(
                                    tableOutput("tabAntSkjema"),
                                    downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                  )
                                           # h2("Ferdigstilte skjema ved hver avdeling for valgte 12 måneder"),
                                           # p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                           # tableOutput("tabAvdSkjema12"))
             )))
           ), #tab

#------------- Fordelingsfigurer--------------------

 tabPanel(p("Fordelinger", title='Her finner du resultater for: Alder, antibiotika, arbeidsstatus, BMI, erstatning, fornøydhet, komorbiditet,
            komplikasjoner, liggetid, morsmål, nytteverdi, operasjonskategori, operasjonsindikasjon, radiologi,
            snus, smertestillende, symptomvaribhet, tidl.operert, uføretrygdet, utdanning'),
          h2("Fordeling av valgt variabel", align='center'),
          sidebarPanel(width = 3,
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
                                              'Komplikasjoner ved operasjon' = 'KomplOpr',
                                              'Liggedøgn, postoperativt' = 'LiggeDognPostop',
                                              'Liggedøgn, totalt' = 'LiggeDognTotalt',
                                              'Morsmål' = 'Morsmal',
                                              'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                                              'Nytte av operasjon, 12 mnd. etter' = 'NytteOpr12mnd',
                                              'Operasjonskategori' = 'OperasjonsKategori',
                                              'Operasjonsindikasjon' = 'OprIndik',
                                              'Operasjonsindiaksjon, paresegrad' = 'OprIndikPareseGrad',
                                              'Operasjonsindiaksjon, myelopati' = 'OprIndikMyelopati',
                                              'Operasjonsindiaksjon, smerter' = 'OprIndikSmerter',
                                              'Radiologi' = 'Radiologi', 'Røyker' = 'Roker',
                                              'Snuser' = 'Snuser', 'Sivilstatus' = 'SivilStatus', 'Sårdren' = 'Saardren',
                                              'Smertestill, bruk preoperativt' = 'SmertestillBrukPreOp',
                                              'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                                              'Symptomvarighet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                                              'Tidligere operert' = 'TidlOpr',
                                              'Tidligere operert, antall' = 'TidlOprAntall',
                                              'Uforetrygdet før operasjon' = 'UforetrygdPreOp',
                                              'Utdanning' = 'Utdanning') #c('Alder'='Alder', "Ant. nivå operert" = 'AntallNivaaOpr')
                      ),
                      dateRangeInput(inputId = 'datovalg', start = "2018-01-01", end = Sys.Date(),
                                     label = "Tidsperiode", separator="t.o.m.", language="nb"),
                      selectInput(inputId = "erMann", label="Kjønn",
                                  choices = kjonn
                      ),
                      sliderInput(inputId="alder", label = "Alder", min = 0,
                                  max = 110, value = c(0, 110)
                      ),
                      selectInput(inputId = "myelopati", label="Myelopati",
                                  choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
                      selectInput(inputId = "fremBak", label="Tilgang ",
                                  choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
                      selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                  choices = enhetsUtvalg
                      )
                      #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                      #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
                    ),
           mainPanel(
           h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Der kan man også gjøre ulike filtreringer."),
           tabsetPanel(
             tabPanel('Figur',
           br(),
           plotOutput("fordelinger")),
           tabPanel('Tabell',
                    br(),
                    tableOutput('fordelingTab'),
                    downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell')
                    )
          ))), #main

  #------------ Andeler-----------------
  tabPanel("Andeler",
           h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
           sidebarPanel(width = 3,
             selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                         choices = c('Alder' = 'Alder',
                                     'Andre sykdommer' = 'AndreRelSykdommer',
                                     'Antibiotika' = 'Antibiotika',
                                     'Arbeidstaus før operasjon' = 'ArbeidstausPreOp',
                                     'Arbeidstaus 3 mnd. etter' = 'Arbeidstaus3mnd',
                                     'Arbeidstaus 12 mnd. etter' = 'Arbeidstaus12mnd',
                                     'ASA-grad' = 'ASAgrad', 'BMI' = 'BMI',
                                     'Komplikasjoner, pasientrapportert 3 mnd. etter' = 'EnhverKompl3mnd',
                                     'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                                     'Fornøydhet med behandlinga, 3 mnd. etter' = 'FornoydBeh3mnd',
                                     'Fornøydhet med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd',
                                     'Misfornøyd, 3 mnd. etter' = 'Misfor3mnd',
                                     'Misforøyd, 12 mnd. etter' = 'Misfor12mnd',
                                     'Komplikasjon, dyp infeksjon, 3 mnd. etter' = 'KomplinfekDyp3mnd',
                                     'Komplikasjon, overfladisk infeksjon, 3 mnd. etter' = 'KomplinfekOverfl3mnd',
                                     'Komplikasjon med stemme, 3 mnd. etter' = 'KomplStemme3mnd',
                                     'Komplikasjon med svelging, 3 mnd. etter' = 'KomplSvelging3mnd',
                                     'NDIendring over 30%, 12 mnd. etter' = 'NDIendr12mnd30pst',
                                     'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                                     'Nytte av operasjon, 12 mnd. etter' = 'NytteOpr12mnd',
                                     'NRSendring, smerter i arm, 12.mnd.' = 'NRSsmerteArmEndr12mnd',
                                     'Forverring, 3 mnd. etter' = 'Verre3mnd',
                                     'Forverring, 12 mnd. etter' = 'Verre12mnd',
                                     'Operasjonsindikasjon, myelopati' = 'OprIndikMyelopati',
                                     'Røyker' = 'Roker', 'Sårdren' = 'Saardren',
                                     'Smertestillende, preoperativt' = 'SmertestillPreOp',
                                     'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                                     'Symptomvariaghet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                                     'Søkt uføretrygd før operasjon' = 'UforetrygdPreOp',
                                     'Utdanning' = 'Utdanning')
             ),
             dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2018-01-01", end = Sys.Date(),
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

  ), #sidebarPanel/kolonna til venstre

  mainPanel(
    h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer."),
    br(),
    br(),
    plotOutput("andelTid")),
  plotOutput("andelerGrVar")
  ),


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
           #h3('Utvikling over tid'),
           plotOutput("gjsnTid"),
           #h3('Sykehusvise resultater'),
           plotOutput("gjsnGrVar"))
) #tab
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
  if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
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


  #-------Samlerapporter--------------------

  # output$mndRapp.pdf = downloadHandler(
  #   filename = function(){'MndRapp.pdf'},
  #   #content = function(file) file.copy(system.file('NakkeMndRapp.pdf', package = 'Nakke'), file, overwrite = TRUE),
  #   content = function(file) {
  #     # permission to the current working directory
  #     src <- normalizePath(system.file('NakkeMndRapp.Rnw', package='Nakke'))
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'NakkeMndRapp.Rnw', overwrite = TRUE)
  #
  #     texfil <- knitr::knit(system.file('NakkeMndRapp.Rnw', package='Nakke'), encoding = 'UTF-8')
  #     #print(texfil)
  #     tools::texi2pdf(system.file(texfil, package='Nakke'),clean = TRUE) #"NakkeMndRapp.tex"
  #     # #help(render_latex)
  #     #       out = system.file('NakkeMndRapp.pdf', package = 'Nakke')
  #     #knit2pdf(system.file('NakkeMndRapp.Rnw', package='Nakke'), clean = TRUE, encoding = 'UTF-8')
  #     #      file.rename(out, file) # move pdf to file for downloading
  #     #file.copy(system.file('NakkeMndRapp.pdf', package='Nakke'), file)
  #     file.copy('NakkeMndRapp.pdf', file)
  #   }
  #   , contentType = 'application/pdf' )
  # #  If you already have made the PDF file, you can just copy it to file, i.e.
  # #  content = function(file) file.copy('your_existing.pdf', file, overwrite = TRUE)

  # funksjon for å kjøre Rnw-filer (render file funksjon)
  contentFile <- function(file, srcFil, tmpFil, datoFra=startDato, datoTil=Sys.Date()) {
    src <- normalizePath(system.file(srcFil, package="Nakke"))

    # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFil, overwrite = TRUE)

    texfil <- knitr::knit(tmpFil, encoding = 'UTF-8')
    tools::texi2pdf(texfil, clean = TRUE)

    gc() #Opprydning gc-"garbage collection"
    file.copy(paste0(substr(tmpFil, 1, nchar(tmpFil)-3), 'pdf'), file)
  }

  output$mndRapp.pdf <- downloadHandler(
    filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')},
    content = function(file){contentFile(file, srcFil="NakkeMndRapp.Rnw", tmpFil="tmpNakkeMndRapp.Rnw")})

  #   contentFile <- function(file, srcFil, tmpFile) {
  #   src <- normalizePath(system.file(srcFil, package="intensiv"))
  #
  #   # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #   owd <- setwd(tempdir())
  #   on.exit(setwd(owd))
  #   file.copy(src, tmpFile, overwrite = TRUE)
  #
  #   texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
  #   tools::texi2pdf(texfil, clean = TRUE)
  #
  #   gc() #Opprydning gc-"garbage collection"
  #   file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  #   # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  # }
  #
  # output$mndRapp.pdf <- downloadHandler(
  #   filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')}, #'MndRapp.pdf',
  #   content = function(file){contentFile(file, srcFil="NIRmndRapp.Rnw", tmpFile="tmpNIRmndRapp.Rnw")}
  # )


  #----------Tabeller, registreringsoversikter ----------------------

  # output$tabAvdMnd12 <- renderTable({
  #   # datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))
  #   # SkjemaData12mnd <- SkjemaData[SkjemaData$InnDato < as.POSIXlt(input$datoTil)
  #   #                               & SkjemaData$InnDato > as.POSIXlt(datoFra12), ]
  #   # if (as.numeric(input$status) %in% 0:1) {SkjemaData12mnd <-
  #   #   SkjemaData12mnd[which(SkjemaData12mnd$SkjemaStatus == as.numeric(input$status)), ]
  #   # }
  #   # #Flyttes til overvåkning
  #   tabAvdSiste12mnd <- addmargins(table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, c('Sykehusnavn', 'Mnd')]))
  #   colnames(tabAvdSiste12mnd) <- substring(colnames(tabAvdSiste12mnd),1,3)
  #   xtable::xtable(tabAvdSiste12mnd)
  # },
  # rownames = TRUE, digits=0 #, align = c('l', rep('r', ncol(tabAvdSiste12mnd)))
  # )
  observe({
    tabAntOpphSh <- switch(input$tidsenhetReg,
           Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
           Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))

    output$tabAntOpphSh <- renderTable(tabAntOpphSh, rownames = T, digits=0, spacing="xs")
    output$lastNed_tabAntOpphSh <- downloadHandler(
      filename = function(){'tabAntOpphSh.csv'},
      content = function(file, filename){write.csv2(tabAntOpphSh, file, row.names = T, na = '')})

  output$undertittelReg <- renderUI({
    br()
    t1 <- 'Tabellen viser operasjoner '
    h4(HTML(switch(input$tidsenhetReg, #undertittel <-
                   Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                   Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, '<br />'))
    ))})
  #RegData som har tilknyttede skjema av ulik type. Fra NGER!
  AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaData, datoFra = input$datovalgReg[1], datoTil=input$datovalgReg[2],
                                  skjemastatus=as.numeric(input$skjemastatus))
  output$tabAntSkjema <- renderTable(AntSkjemaAvHver
                                     ,rownames = T, digits=0, spacing="xs" )
  output$lastNed_tabAntSkjema <- downloadHandler(
    filename = function(){'tabAntSkjema.csv'},
    content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, na = '')})
    })


  #Velge ferdigstillelse og tidsintervall.
  output$tabAntSkjema <- renderTable({})

    output$tabAntSkjemaGml <- renderTable({
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

#--------------Viktigste resultater-------------------------
  output$kvalIndFig1 <- renderPlot({

    NakkeFigAndelTid(RegData=RegData, preprosess=0, reshID = reshIDdummy,
                     valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                     myelopati = as.numeric(input$myelopatiKvalInd),
                     fremBak = as.numeric(input$fremBakKvalInd),
                     enhetsUtvalg = as.numeric(input$enhetsUtvalgKvalInd), tidsenhet = input$tidsenhetKvalInd)
  }, height=300, width=1000)

  output$kvalIndFig2 <- renderPlot(
    NakkeFigAndelerGrVar(RegData=RegData, preprosess=0,
                         valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                         myelopati = as.numeric(input$myelopatiKvalInd),
                         fremBak = as.numeric(input$fremBakKvalInd))
    , height=600, width=500
  )

#-----------Fordelinger---------------------
  output$fordelinger <- renderPlot({
    NakkeFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                    reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                    datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                    erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                    fremBak = as.numeric(input$fremBak))
   }, height=700, width=600)


  observe({
    UtDataFord <-  NakkeFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                   reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                   datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                   minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                   erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                                   fremBak = as.numeric(input$fremBak))

    #Følgende kan være likt for fordelingsfigurer i alle registre:
  tabFord <- lagTabavFig(UtDataFraFig = UtDataFord) #lagTabavFigAndeler
  output$tittelFord <- renderUI({
    tagList(
      h3(UtDataFord$tittel),
      h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
    )}) #, align='center'
  #output$fordelingTab <- renderTable(tabFord, rownames = T)

  output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
    antKol <- ncol(tabFord)
    kableExtra::kable(tabFord, format = 'html'
                      , full_width=F
                      , digits = c(0,1,0,1)[1:antKol]
    ) %>%
      add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
      column_spec(column = 1, width_min = '7em') %>%
      column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
      row_spec(0, bold = T)
  }

  output$lastNed_tabFord <- downloadHandler(
    filename = function(){paste0(input$valgtVar, '_fordeling.csv')},
    content = function(file, filename){write.csv2(tabFord, file, row.names = T, na = '')
    })

  }) #observe, fordelinger

  #----------------- Andeler -----------------------

  output$andelerGrVar <- renderPlot({

    NakkeFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                         reshID=reshIDdummy,
                         datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                         minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                         erMann=as.numeric(input$erMannAndelGrVar), myelopati = as.numeric(input$myelopatiAndelGrVar),
                         fremBak = as.numeric(input$fremBakAndelGrVar))
  }, height=700, width=600)

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
  }, height=300, width=1000)

  output$gjsnGrVar <- renderPlot({
    NakkeFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                      reshID=reshIDdummy,
                      datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                      minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                      erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                      fremBak = as.numeric(input$fremBakGjsn),
                      valgtMaal = input$sentralmaal)
  }, height=600, width=500)

  output$gjsnTid <- renderPlot({
    NakkeFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                    reshID=reshIDdummy,
                    datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                    minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                    erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                    fremBak = as.numeric(input$fremBakGjsn),
                    valgtMaal = input$sentralmaal,
                    tidsenhet = input$tidsenhetGjsn,
                    enhetsUtvalg = input$enhetsUtvalgGjsn)
  }, height=300, width=1000)
}

# Run the application
shinyApp(ui = ui, server = server)

