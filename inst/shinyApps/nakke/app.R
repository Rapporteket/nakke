
library(nakke)


context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")
options(knitr.table.format = "html")

idag <- Sys.Date()
startDato <- paste0(as.numeric(format(idag-200, "%Y")), '-01-01') #paste0(1900+as.POSIXlt(idag)$year, '-01-01')
#AarNaa <- as.numeric(format(idag, "%Y"))
datoTil <- as.POSIXlt(idag)
sluttDato <- idag
aarFra <- paste0(1900+as.POSIXlt(idag)$year-5, '-01-01')

regTitle <- ifelse(paaServer,
                   'NKR: Degenerativ nakke',
                   'Degenerativ nakke med FIKTIVE data')


# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

# #----------Hente data og evt. parametre som er statistke i appen----------
if (paaServer == TRUE) {
  RegData <- NakkeRegDataSQL()

  querySD <- paste0('
          SELECT
            Skjemanavn,	SkjemaStatus,	ForlopsID,	HovedDato,	Sykehusnavn,	AvdRESH,	SkjemaRekkeflg
           FROM SkjemaOversikt
           WHERE HovedDato >= "2014-01-01" ')
  SkjemaData <- rapbase::loadRegData(registryName="nakke", query=querySD, dbType="mysql")
  knitr::opts_knit$set(root.dir = './')
  knitr::opts_chunk$set(fig.path='')
} #hente data på server

if (!exists('RegData')){
  data('NakkeRegDataSyn', package = 'nakke')
  data('SkjemaDataSyn', package = 'nakke')
  reshID <- 601161
}


RegData <- NakkePreprosess(RegData = RegData)

#SkjemaRekkeflg #1-pasientskjema, 2-legeskjema, 3- Oppf. 3mnd, 4 - Oppf. 12mnd. Endret til 5*, dvs. 5,10,15,20, juli 2022
SkjemaData$InnDato <- as.Date(SkjemaData$HovedDato)
SkjemaData$Aar <- 1900 + strptime(SkjemaData$InnDato, format="%Y")$year
SkjemaData$Mnd <- zoo::as.yearmon(SkjemaData$InnDato)
SkjemaData$ShNavn <- as.factor(SkjemaData$Sykehusnavn)


#Definere innhold i felles rullegardinmenyer:
kjonn <- c("Begge"=2, "Menn"=1, "Kvinner"=0)
enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))
myelopatiValg <- c("Ikke valgt"=2, "Ja"=1, "Nei"=0)
fremBakValg <- c("Alle"=0, "Fremre"=1, "Bakre"=2)
inngrepValg <- c('Alle'=99, 'Ikke klassifiserbar operasjon'=0, 'Fremre diketomi for prolaps'=1, 'Bakre dekompresjon'=2,
                 'Fremre dekompresjon sp st.uten prolaps'=3, 'Bakre fusjon'=4, 'Korporektomi'=5, 'Andre inngrep'=6)

sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValg <- unique(RegData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Alle',sykehusNavn$x)

#----Define UI for application------
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  id = "tab1nivaa",
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",


  #------------ Viktigste resultater-----------------
  tabPanel(p("Viktigste resultater", title='Kvalitetsindikatorer og halvårsrapport'),
           h2('Velkommen til Rapporteket for NKR, degenerativ nakke!', align='center'),
           shinyjs::useShinyjs(),
           sidebarPanel(width=3,
                        h3("Rapport med halvårsresultater"), #),
                        h5('Rapporten kan man også få regelmessig på e-post.
                        Gå til fanen "Abonnement" for å bestille dette.'),
                        br(),
                        downloadButton(outputId = 'mndRapp.pdf', label='Last ned halvårsrapport', class = "butt"),
                        tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                        br(),
                        br(),
                        br(),
                        br(),
                        h3('Gjør utvalg i figurene'),
                        selectInput(inputId = "valgtVarKvalInd", label="Velg variabel",
                                    choices = c('Komplikasjon, stemme' = 'KomplStemme3mnd',
                                                'Komplikasjon, svelging' = 'KomplSvelging3mnd',
                                                'Komplikasjon, sårinfeksjon' = 'Komplinfek',
                                                'NDI-endr >35%, ett år etter operasjon' = 'NDIendr12mnd35pst')),
                        dateInput(inputId = "datoFraKvalInd", label='Velg startdato', value = startDato),
                        h5('Husk å velge startdato minst ett år tilbake i tid hvis du ønkser å se resultater ett år etter operasjon...'),
                        selectInput(inputId = "bildeformatKvalInd",
                                    label = "Velg format for nedlasting av figur",
                                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                        br(),
                        helpText('Følgende valg gjelder bare tidsfigur:'),
                        selectInput(inputId = "tidsenhetKvalInd", label="Velg tidsenhet",
                                    choices = tidsenhetValg),
                        selectInput(inputId = 'enhetsUtvalgKvalInd', label='Egen enhet og/eller landet',
                                    choices = enhetsUtvalg
                        )
           ),

           mainPanel(
             rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                          organization = uiOutput("appOrgName"),
                                          addUserInfo = TRUE),
             tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),

             h4('Her kan man finne resultater fra NKR. Under hver fane kan man velge hva man
                vil se på og filtrere ut resultater, for eksempel for ulike operasjonstyper
                og/eller tidsperioder. Man kan også sammenlikne ulike sykehus. Hold musepekeren
                over fanen for å se hvilke data som er tilgjengelig. Fanene er i hovedsak organisert
                ut fra hvordan resultatene er presentert, det vil si "andeler" (%) eller
                «gjennomsnitt» (eksempelvis andel som fikk en komplikasjon eller gjennomsnittlig knivtid)'),
             br(),
             h2("Kvalitetsindikatorer", align='center' ),
             #h3(em("Utvikling over tid")),
             plotOutput("kvalIndFig1", height = 'auto'),
             downloadButton('LastNedFigKvalIndTid', label='Velg format (til venstre) og last ned figur'),
             plotOutput("kvalIndFig2", height = 'auto'),
             downloadButton('LastNedFigKvalIndGrVar', label='Velg format (til venstre) og last ned figur')
           )
  ), #tab


  #------------- Tabeller (vise antall)--------------------

  tabPanel(p('Registreringsoversikter',title="Tabeller med registreringsoversikter"),
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
                                                  "Kladd (/oppf.skjema ikke besvart)"=0,
                                                  "Åpen"=-1)
                          )),
                        br()
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
                         ),
                         tabPanel('Antall skjema',
                                  h4("Tabellen viser antall registrerte skjema for valgt tidsperiode"),
                                  p("Velg tidsperiode i menyen til venstre"),
                                  br(),
                                  fluidRow(
                                    tableOutput("tabAntSkjema"),
                                    downloadButton(outputId = 'lastNed_tabAntSkjema', label='Last ned')
                                  )
                         )))
  ), #tab
  #------Registeradministrasjon-----------------------
  tabPanel(p('Registeradministrasjon', title="Verktøy for SC-bruker"),
           value = 'Registeradministrasjon',
           h3('Denne siden skal kun vises for SC-bruker', align='center'),
           tabsetPanel(
             tabPanel(
               h4("Utsending av rapporter"),
               sidebarLayout(
                 sidebarPanel(
                   rapbase::autoReportOrgInput("NakkeUts"),
                   rapbase::autoReportInput("NakkeUts")
                 ),
                 mainPanel(
                   rapbase::autoReportUI("NakkeUts")
                 )
               )
             ),
             tabPanel(
               h4('Datadump og datakvalitet'),
               sidebarPanel(
                 width=4,
                 dateRangeInput(inputId = 'datovalgRegKtr', start = startDato, end = idag,
                                label = "Tidsperiode", separator="t.o.m.", language="nb"),
                 selectInput(inputId = 'velgReshReg', label='Velg sykehus',
                             selected = 0,
                             choices = sykehusValg),
                 br(),
                 downloadButton(outputId = 'lastNed_dataDump', label='Last ned datadump'),
                 br(),
                 # h4('Data til Resultatportalen - utdatert. Vil bli fikset?'),
                 # selectInput(inputId = "valgtVarRes", label="Velg variabel",
                 #             choices = c('Stemmevansker u/myelopati, 3 mnd.' = 'KomplStemme3mnd',
                 #                         'Svelgvansker, 3 mnd.' = 'KomplSvelging3mnd',
                 #                         'Infeksjon, 3 mnd.' = 'Komplinfek',
                 #                         'NDI-endring 12mnd > 35%' = 'NDIendr12mnd35pst')
                 # ),
                 # downloadButton(outputId = 'lastNed_dataTilOffNett', label='Last ned data til SKDEs interaktive nettsider'),
                 br()
               ),

               mainPanel(
                 h3('Potensielle dobbeltregistreringer'),
                 br(),
                 h4('Funksjonen finner alle PID med to operasjoner nærmere enn valgt tidsintervall
                           og tabellen viser alle operasjoner for de aktuelle pasientene.'),
                 downloadButton(outputId = 'lastNed_tabDblReg', label='Last ned tabell med mulige dobbeltregistreringer'),
                 br(),
                 numericInput(inputId = 'valgtTidsavvik',
                              label = 'Dager mellom registrerte operasjoner:',
                              value = 30,
                              min = 0,
                              max = NA,
                              step = 1
                              , width = '100px'
                 ),

                 tableOutput("tabDblReg")
               )
             ),
             shiny::tabPanel(
               h4("Eksport av krypterte data"),
               #shiny::sidebarLayout(
               shiny::sidebarPanel(
                 rapbase::exportUCInput("nakkeExport")
               ),
               shiny::mainPanel(
                 rapbase::exportGuideUI("nakkeExportGuide")
               )
             ) #Eksport-tab
           ) #tabsetPanel
  ), #Registeradm-tab

  #------------- Fordelingsfigurer--------------------

  tabPanel(p("Fordelinger", title='Her finner du resultater for: Alder, antibiotika, arbeidsstatus, BMI, erstatning, fornøydhet, komorbiditet,
            komplikasjoner, liggetid, morsmål, nytteverdi, operasjonskategori, operasjonsindikasjon, radiologi,
            snus, smertestillende, symptomvaribhet, tidl.operert, uføretrygdet, utdanning'),
           h2("Fordeling av valgt variabel", align='center'),
           sidebarPanel(width = 3,
                        selectInput(inputId = "valgtVar", label="Velg variabel",
                                    choices = c('Alder' = 'Alder', 'Antall nivå operert' = 'AntallNivaaOpr',
                                                'Antibiotika' = 'Antibiotika',
                                                'Arbeidsstatus før operasjon' = 'ArbeidstausPreOp',
                                                'Arbeidsstatus 3 mnd. etter' = 'Arbeidstaus3mnd',
                                                'Arbeidsstatus 12 mnd. etter' = 'Arbeidstaus12mnd',
                                                'ASA-grad' = 'ASAgrad',
                                                'BMI' = 'BMI',
                                                'Angst (EQ5D) før operasjon' = 'EqAngstPreOp',
                                                'Fornoydhet med behandlinga, 3 mnd. etter' = 'FornoydBeh3mnd',
                                                'Fornoydhet med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd' ,
                                                'Inngrepstyper' = 'Inngrep',
                                                'Komorbiditet' = 'Komorbiditet',
                                                'Komplikasjoner, pas.rapp. 3 mnd. etter' = 'Kompl3mnd',
                                                'Komplikasjoner ved operasjon' = 'KomplOpr',
                                                'Liggedøgn, postoperativt' = 'LiggeDognPostop',
                                                'Liggedøgn, totalt' = 'LiggeDognTotalt',
                                                'Morsmål' = 'Morsmal',
                                                'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                                                'Nytte av operasjon, 12 mnd. etter' = 'NytteOpr12mnd',
                                                'Hastegrad kirurgi' = 'OperasjonsKategori',
                                                'Operasjonsindikasjon' = 'OprIndik',
                                                'Operasjonsindikasjon, paresegrad' = 'OprIndikPareseGrad',
                                                'Operasjonsindikasjon, myelopati' = 'OprIndikMyelopati',
                                                'Operasjonsindikasjon, smerter' = 'OprIndikSmerter',
                                                'Radiologi' = 'Radiologi', 'Røyker' = 'Roker',
                                                'Registreringsforsinkelse' = 'regForsinkelse',
                                                'Snusbruk' = 'Snuser', 'Sivilstatus' = 'SivilStatus', 'Sårdren' = 'Saardren',
                                                'Smertestillende, hyppighet preoperativt' = 'SmertestillBrukPreOp',
                                                'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                                                'Symptomvarighet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                                                'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                                                'Søkt uføretrygd før operasjon' = 'UforetrygdPreOp',
                                                'Tidligere operert' = 'TidlOpr',
                                                'Tidligere operert, antall' = 'TidlOprAntall',
                                                'Tilgang ved operasjon' = 'OpTilgfrembak',
                                                'Utdanning' = 'Utdanning'),
                                    selected = 'regForsinkelse'
                        ),
                        selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                    choices = enhetsUtvalg
                        ),
                        dateRangeInput(inputId = 'datovalg', start = startDato, end = Sys.Date(),
                                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
                        selectInput(inputId = "erMann", label="Kjønn",
                                    choices = kjonn
                        ),
                        sliderInput(inputId="alder", label = "Alder", min = 0,
                                    max = 110, value = c(0, 110)
                        ),
                        selectInput(inputId = "inngrep", label="Inngrepstype",
                                    choices = inngrepValg),
                        selectInput(inputId = "myelopati", label="Myelopati",
                                    choices = myelopatiValg),
                        selectInput(inputId = "fremBak", label="Tilgang ",
                                    choices = fremBakValg),
                        selectInput(inputId = "bildeformatFord",
                                    label = "Velg format for nedlasting av figur",
                                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
                        #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                        #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
           ),
           mainPanel(
             h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Der kan man også gjøre ulike filtreringer."),
             tabsetPanel(
               tabPanel('Figur',
                        plotOutput("fordelinger", height = 'auto'),
                        downloadButton('LastNedFigFord', label='Velg format (til venstre) og last ned figur')
               ),
               tabPanel('Tabell',
                        uiOutput("tittelFord"),
                        br(),
                        tableOutput('fordelingTab'),
                        br(),
                        downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell')
               )
             ))#main
  ),

  #------------ Andeler-----------------
  tabPanel(p("Andeler", title= 'Alder, arbeidsstatus, ASA-grad, komorbiditet, komplikasjoner, fornøydhet,
                                forverring, NDI, nytte, NSR, registreringsforsinkelse, røyking, smertestillende, symptomvarighet,
                                søkt erstatning/uføretrygd, utdanning'),
           h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
           sidebarPanel(width = 3,
                        selectInput(inputId = "valgtVarAndel", label="Velg variabel",
                                    choices = c('Alder' = 'Alder',
                                                'Andre sykdommer' = 'AndreRelSykdommer',
                                                'Antibiotika' = 'Antibiotika',
                                                'Arbeidstaus før operasjon' = 'ArbeidstausPreOp',
                                                'Arbeidstaus 3 mnd. etter' = 'Arbeidstaus3mnd',
                                                'Arbeidstaus 12 mnd. etter' = 'Arbeidstaus12mnd',
                                                'ASA-grad' = 'ASAgrad', 'BMI' = 'BMI',
                                                'Fornøyd med behandlinga, 3 mnd. etter' = 'FornoydBeh3mnd',
                                                'Fornøyd med behandlinga, 12 mnd. etter' = 'FornoydBeh12mnd',
                                                'Forverring, 3 mnd. etter' = 'Verre3mnd',
                                                'Forverring, 12 mnd. etter' = 'Verre12mnd',
                                                'Komplikasjon, dyp infeksjon, 3 mnd. etter' = 'KomplinfekDyp3mnd',
                                                'Komplikasjon, overfladisk infeksjon, 3 mnd. etter' = 'KomplinfekOverfl3mnd',
                                                'Komplikasjon med stemme, 3 mnd. etter' = 'KomplStemme3mnd',
                                                'Komplikasjon med svelging, 3 mnd. etter' = 'KomplSvelging3mnd',
                                                'Komplikasjoner, pasientrapportert 3 mnd. etter' = 'EnhverKompl3mnd',
                                                'Misfornøyd med behandlinga, 3 mnd.' = 'Misfor3mnd',
                                                'Misfornøyd med behandlinga, 12 mnd.' = 'Misfor12mnd',
                                                'NDIendring over 35%, 12 mnd. etter' = 'NDIendr12mnd35pst',
                                                'Nytte av operasjon, 3 mnd. etter' = 'NytteOpr3mnd',
                                                'Nytte av operasjon, 12 mnd. etter' = 'NytteOpr12mnd',
                                                'NRSendring, smerter i arm, 12.mnd.' = 'NRSsmerteArmEndr12mnd',
                                                'Operasjonsindikasjon, myelopati' = 'OprIndikMyelopati',
                                                'Registreringsforsinkelse' = 'regForsinkelse',
                                                'Røyker' = 'Roker', 'Sårdren' = 'Saardren',
                                                'Smertestillende, preoperativt' = 'SmertestillPreOp',
                                                'Symptomvarighet, armsmerter' = 'SymptVarighetArmer',
                                                'Symptomvariaghet, nakke/hodesmerter' = 'SymptVarighetNakkeHode',
                                                'Søkt erstatning før operasjon' = 'ErstatningPreOp',
                                                'Søkt uføretrygd før operasjon' = 'UforetrygdPreOp',
                                                'Svart på oppfølging, 3 mnd.' = 'Oppf3mnd',
                                                'Svart på oppfølging, 12 mnd.' = 'Oppf12mnd',
                                                'Svart på oppfølging, 3 og 12 mnd.' = 'Oppf3og12mnd',
                                                'Utdanning' = 'Utdanning'),
                                    selected = 'regForsinkelse'
                        ),
                        selectInput(inputId = 'enhetsUtvalgAndelTid', label='Egen enhet og/eller landet (kun for utvikling over tid)',
                                    choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
                        ),
                        selectInput(inputId = "tidsenhetAndelTid", label="Velg tidsenhet (kun for utvikling over tid)",
                                    choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                                    'Kvartal'='Kvartal', 'Måned'='Mnd'))),
                        dateRangeInput(inputId = 'datovalgAndel', start = startDato, end = Sys.Date(),
                                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
                        selectInput(inputId = "erMannAndel", label="Kjønn",
                                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                        ),
                        sliderInput(inputId="alderAndel", label = "Alder", min = 0,
                                    max = 110, value = c(0, 110)
                        ),
                        selectInput(inputId = "inngrepAndel", label="Inngrepstype",
                                    choices = inngrepValg),
                        selectInput(inputId = "myelopatiAndel", label="Myelopati",
                                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
                        selectInput(inputId = "fremBakAndel", label="Tilgang ",
                                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
                        selectInput(inputId = "bildeformatAndel",
                                    label = "Velg format for nedlasting av figur",
                                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                        br()
                        #p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),

           ), #sidebarPanel/kolonna til venstre

           mainPanel(
             h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Der kan man også gjøre ulike filtreringer."),
             tabsetPanel(
               tabPanel("Figurer",
                        helpText('Høyreklikk på figuren for å laste den ned'),
                        br(),
                        h3(em("Utvikling over tid")),
                        plotOutput("andelTid", height = 'auto'),
                        downloadButton('LastNedFigAndelTid', label='Velg format (til venstre) og last ned figur'),
                        br(),
                        h3(em("Sykehusvise resultater")),
                        plotOutput("andelerGrVar", height='auto'),
                        downloadButton('LastNedFigAndelGrVar', label='Velg format (til venstre) og last ned figur')
               ),
               tabPanel("Tabeller",
                        uiOutput("tittelAndel"),
                        br(),
                        #fluidRow(
                        column(width = 3,
                               h3("Sykehusvise resultater"),
                               tableOutput("andelerGrVarTab"),
                               br(),
                               downloadButton(outputId = 'lastNed_tabAndelGrVar', label='Last ned tabell')),
                        column(width = 1),
                        column(width = 5,
                               h3("Utvikling over tid"),
                               tableOutput("andelTidTab"),
                               br(),
                               downloadButton(outputId = 'lastNed_tabAndelTid', label='Last ned tabell'))
                        #DT::DTOutput("andelerGrVarTab")
               ))
           ) #mainPanel
  ), #tab, Andeler

#-------------------Sentralmål (gjsn./median)---------------------------
  tabPanel(p("Gjennomsnitt", title='Alder, EMS, EQ5D, knivtid, liggetid, NDI, NSR'),
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
                                                'NSR, arm, endring 3 mnd.' = 'NRSsmerteArmEndr3mnd',
                                                'NSR, arm, endring 12 mnd.' = 'NRSsmerteArmEndr12mnd',
                                                'NSR, nakke før operasjon' = 'NRSsmerteNakkePreOp',
                                                'NSR, nakke, endring 3 mnd.' = 'NRSsmerteNakkeEndr3mnd',
                                                'NSR, nakke, endring 12 mnd.' = 'NRSsmerteNakkeEndr12mnd',
                                                'Total knivtid' = 'KnivtidTotalMin'
                                    )
                        ),
                        selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet (kun for utvikling over tid)',
                                    choices = enhetsUtvalg
                        ),
                        selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet (kun for utvikling over tid)",
                                    choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                                    'Kvartal'='Kvartal', 'Måned'='Mnd'))),
                        dateRangeInput(inputId = 'datovalgGjsn', start = startDato, end = Sys.Date(),
                                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
                        selectInput(inputId = "erMannGjsn", label="Kjønn",
                                    choices = kjonn
                        ),
                        sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                                    max = 110, value = c(0, 110)
                        ),
                        selectInput(inputId = "inngrepGjsn", label="Inngrepstype",
                                    choices = inngrepValg),
                        selectInput(inputId = "myelopatiGjsn", label="Myelopati",
                                    choices = c("Ikke valgt"=2, "Ja"=1, "Nei"=0)),
                        selectInput(inputId = "fremBakGjsn", label="Tilgang ",
                                    choices = c("Alle"=0, "Fremre"=1, "Bakre"=2)),
                        selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                    choices = c("Gjennomsnitt"='Gjsn', "Median"='Med')),
                        selectInput(inputId = "bildeformatGjsn",
                                    label = "Velg format for nedlasting av figur",
                                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                        br()
                        #p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),

           ),
           mainPanel(
             h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                  til venstre. Man kan også gjøre ulike filtreringer."),
             br(),
             tabsetPanel(
               tabPanel("Figurer",
                        plotOutput("gjsnTid", height = 'auto'),
                        downloadButton('LastNedFigGjsnTid', label='Velg format (til venstre) og last ned figur'),
                        plotOutput("gjsnGrVar", height = 'auto'),
                        downloadButton('LastNedFigGjsnGrVar', label='Velg format (til venstre) og last ned figur')
               ),
               tabPanel("Tabeller",
                        uiOutput("tittelGjsn"),
                        br(),
                        column(width = 4,
                               h3("Sykehusvise resultater"),
                               tableOutput("gjsnGrVarTab"),
                               h5(tags$b('Konf.int.'), 'angir 95% konfidensintervall'),
                               downloadButton(outputId = 'lastNed_gjsnGrVarTab', label='Last ned tabell')),
                        column(width = 1),
                        column(width = 5,
                               h3("Utvikling over tid"),
                               tableOutput("gjsnTidTab"),
                               h5('KImin og KImaks angir øvre og nedre grense i et 95%-konfidensintervall'),
                               downloadButton(outputId = 'lastNed_gjsnTidTab', label='Last ned tabell')) )
             )
           )
  ), #tab, gjsn

  #------------------Abonnement-------------------------

  tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
           value = 'Abonnement',

           sidebarLayout(
             sidebarPanel(
               rapbase::autoReportInput("NakkeAbb")
             ),
             shiny::mainPanel(
               rapbase::autoReportUI("NakkeAbb")
             )
           )
  ) #tab abonnement
) #fluidpage, dvs. alt som vises på skjermen



#----------------- Define server logic required  -----------------------
server <- function(input, output,session) {



  rapbase::appLogger(session, msg='Starter Rapporteket-Nakke')
  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 601161)
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'BrukerNavn')
  fulltNavn <- ifelse(paaServer, rapbase::getUserFullName(shinySession=session), 'FulltNavn')

    # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle, '<br> reshID: ', reshID) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })


  observe({if (rolle != 'SC') {
      shiny::hideTab(inputId = "tab1nivaa",
                     target = 'Registeradministrasjon') }
  })
    #-------Samlerapporter--------------------

  output$mndRapp.pdf <- downloadHandler(
    filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporter(file, rnwFil="NakkeMndRapp.Rnw",
                          reshID = reshID, datoFra = startDato)
    }
  )



  #----------Tabeller, registreringsoversikter ----------------------

  observe({
    tabAntOpphSh <- switch(input$tidsenhetReg,
           Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
           Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))

    output$tabAntOpphSh <- renderTable(tabAntOpphSh, rownames = T, digits=0, spacing="xs")
    output$lastNed_tabAntOpphSh <- downloadHandler(
      filename = function(){'tabAntOpphSh.csv'},
      content = function(file, filename){write.csv2(tabAntOpphSh, file, row.names = T, fileEncoding = 'latin1', na = '')})

  output$undertittelReg <- renderUI({
    br()
    t1 <- 'Tabellen viser operasjoner '
    valgtAar <- as.numeric(format.Date(input$sluttDatoReg, "%Y"))
    h4(HTML(switch(input$tidsenhetReg, #undertittel <-
                   Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                   Aar = paste0(t1, 'siste 5 år til og med ', valgtAar, '<br />'))
    ))})

  #RegData som har tilknyttede skjema av ulik type.
  AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaData, datoFra = input$datovalgReg[1], datoTil=input$datovalgReg[2],
                                  skjemastatus=as.numeric(input$skjemastatus))
  output$tabAntSkjema <- renderTable(AntSkjemaAvHver
                                     ,rownames = T, digits=0, spacing="xs" )
  output$lastNed_tabAntSkjema <- downloadHandler(
    filename = function(){'tabAntSkjema.csv'},
    content = function(file, filename){write.csv2(AntSkjemaAvHver, file, row.names = T, fileEncoding = 'latin1', na = '')})
    })

#--------------Viktigste resultater-------------------------
  output$kvalIndFig1 <- renderPlot({

    NakkeFigAndelTid(RegData=RegData, reshID = reshID,
                     valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                     #myelopati = as.numeric(input$myelopatiKvalInd),
                     #fremBak = as.numeric(input$fremBakKvalInd),
                     enhetsUtvalg = as.numeric(input$enhetsUtvalgKvalInd),
                     tidsenhet = input$tidsenhetKvalInd,
                     session = session)
  }, height=300, width=1000)

    output$LastNedFigKvalIndTid <- downloadHandler(
      filename = function(){
        paste0('FigKvalIndTid_', valgtVar=input$valgtVarKvalInd, '_', Sys.time(), '.', input$bildeformatKvalInd)
      },
      content = function(file){
        NakkeFigAndelTid(RegData=RegData, reshID = reshID,
                         valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd,
                         enhetsUtvalg = as.numeric(input$enhetsUtvalgKvalInd),
                         tidsenhet = input$tidsenhetKvalInd,
                         session = session,
                         outfile = file)
      })

  output$kvalIndFig2 <- renderPlot(
    NakkeFigAndelerGrVar(RegData=RegData,
                         valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd
                         ,session=session)
    , height=700, width=600 #height=600, width=500
  )

  output$LastNedFigKvalIndGrVar <- downloadHandler(
    filename = function(){
      paste0('FigKvalIndSj_', valgtVar=input$valgtVarKvalInd, '_', Sys.time(), '.', input$bildeformatKvalInd)
    },
    content = function(file){
      NakkeFigAndelerGrVar(RegData=RegData,
                           valgtVar=input$valgtVarKvalInd, datoFra = input$datoFraKvalInd
                           ,session=session,
                       outfile = file)
     })


  #-----------Registeradministrasjon-----------
  observe({
    tabDblReg <- PasMdblReg(RegData=RegData, tidsavvik=input$valgtTidsavvik)
    output$tabDblReg <- renderTable(tabDblReg, digits=0)

    output$lastNed_tabDblReg <- downloadHandler(
    filename = function(){paste0('MuligeDobbeltReg.csv')},
    content = function(file, filename){write.csv2(tabDblReg, file, row.names = F, fileEncoding = 'latin1', na = '')})
  })

  queryForl <- 'SELECT ForlopsID, Kommune, Kommunenr, Fylkenr, Avdod, AvdodDato, BasisRegStatus
               FROM ForlopsOversikt'
  RegDataForl <- rapbase::loadRegData(registryName = "nakke", query = queryForl, dbType = "mysql")

  variablePRM <- 'Variabler som skal tas bort for LU-bruker'

  observe({
    DataDumpRaa <- NakkeRegDataSQL(medProm = 0)
    DataDump <- NakkePreprosess(RegData = DataDumpRaa)

    if (rolle =='SC') {
      valgtResh <- as.numeric(input$velgReshReg)
      ind <- if (valgtResh == 0) {1:dim(DataDump)[1]
        } else {which(as.numeric(DataDump$ReshId) %in% as.numeric(valgtResh))}
      tabDataDump <- DataDump[ind,]
    } else { #Kun SC får laste ned data
      tabDataDump <-
        DataDump[which(DataDump$ReshId == reshID), -which(names(DataDump) %in% variablePRM)]
    } # Sjekk at PROM/PREM ikke er med for LU-bruker

    output$lastNed_dataDump <- downloadHandler(
      filename = function(){'dataDumpNakke.csv'},
      content = function(file, filename){write.csv2(tabDataDump, file, row.names = F, fileEncoding = 'latin1', na = '')})
  })

#---Utsendinger---------------
     orgs <- as.list(sykehusValg)

     ## liste med metadata for rapport
     reports <- list(
       HalvaarRapp = list(
         synopsis = "Halvårsrapport",
         fun = "abonnementNakke",
         paramNames = c('rnwFil', "reshID"),
         paramValues = c('NakkeMndRapp.Rnw', 0)
       )
     )

     org <- rapbase::autoReportOrgServer("NakkeUts", orgs)

     # oppdatere reaktive parametre, for å få inn valgte verdier (overskrive de i report-lista)
     paramNames <- shiny::reactive("reshID")
     paramValues <- shiny::reactive(org$value())

     rapbase::autoReportServer(
       id = "NakkeUts", registryName = "nakke", type = "dispatchment",
       org = org$value, paramNames = paramNames, paramValues = paramValues,
       reports = reports, orgs = orgs, eligible = TRUE
     )



     #----------- Eksport ----------------
     registryName <- "nakke"
     ## brukerkontroller
     rapbase::exportUCServer("nakkeExport", registryName)
     ## veileding
     rapbase::exportGuideServer("nakkeExportGuide", registryName)



#-----------Fordelinger---------------------
  output$fordelinger <- renderPlot({
    NakkeFigAndeler(RegData=RegData,  valgtVar=input$valgtVar,
                    reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                    datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                    erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                    fremBak = as.numeric(input$fremBak), inngrep=as.numeric(input$inngrep), session=session)
   }, height=700, width=600)

     output$LastNedFigFord <- downloadHandler(
       filename = function(){
         paste0('FigFord_', valgtVar=input$valgtVar, '_', Sys.time(), '.', input$bildeformatFord)
       },
       content = function(file){
         NakkeFigAndeler(RegData=RegData,  valgtVar=input$valgtVar,
                         reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                         datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                         minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                         erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                         fremBak = as.numeric(input$fremBak), inngrep=as.numeric(input$inngrep),
                         session=session,
                         outfile = file)
       })

  observe({
    #UtDataFord <-  NakkeFigAndeler(RegData=RegData,  valgtVar=valgtVar, enhetsUtvalg = 1, reshID = 601161)
    UtDataFord <-  NakkeFigAndeler(RegData=RegData,  valgtVar=input$valgtVar,
                                   reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                   datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                   minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                   erMann=as.numeric(input$erMann), myelopati = as.numeric(input$myelopati),
                                   fremBak = as.numeric(input$fremBak), inngrep=as.numeric(input$inngrep),
                                   session=session)
    #Følgende kan være likt for fordelingsfigurer i alle registre:
  tabFord <- lagTabavFig(UtDataFraFig = UtDataFord) #lagTabavFigAndeler
  output$tittelFord <- renderUI({
    tagList(
      h3(UtDataFord$tittel),
      h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
    )}) #, align='center'
  #output$fordelingTab <- renderTable(tabFord, rownames = T)

  #tittelKolGr <- c(UtDataFord$hovedgrTxt, UtDataFord$smltxt)
  kolGruppering <- c(1,3,3)
  names(kolGruppering) <- c(' ', UtDataFord$hovedgrTxt, UtDataFord$smltxt)
  output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
    antKol <- ncol(tabFord)
    kableExtra::kable(tabFord, format = 'html'
                      , full_width=F
                      , digits = c(0,0,1,0,0,1)[1:antKol]
    ) %>%
      kableExtra::add_header_above(kolGruppering[1:(2+UtDataFord$medSml)]) %>%
      #kableExtra::add_header_above(c(" "=1, tittelKolGr[1] = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
      kableExtra::column_spec(column = 1, width='5em') %>% #width_min = '3em', width_max = '10em') %>%
      kableExtra::column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
      kableExtra::row_spec(0, bold = T)
  }

  output$lastNed_tabFord <- downloadHandler(
    filename = function(){paste0(input$valgtVar, '_fordeling.csv')},
    content = function(file, filename){write.csv2(tabFord, file, row.names = T, fileEncoding = 'latin1', na = '')
    })

  }) #observe, fordelinger

  #----------------- Andeler -----------------------

  output$andelerGrVar <- renderPlot({
    NakkeFigAndelerGrVar(RegData=RegData,
                         valgtVar=input$valgtVarAndel,
                         reshID=reshID,
                         datoFra=input$datovalgAndel[1],
                         datoTil=input$datovalgAndel[2],
                         minald=as.numeric(input$alderAndel[1]),
                         maxald=as.numeric(input$alderAndel[2]),
                         erMann=as.numeric(input$erMannAndel),
                         myelopati = as.numeric(input$myelopatiAndel),
                         fremBak = as.numeric(input$fremBakAndel),
                         inngrep=as.numeric(input$inngrepAndel), session=session)
  }, height=700, width=600)

  output$LastNedFigAndelGrVar <- downloadHandler(
    filename = function(){
      paste0('FigAndelSh_', valgtVar=input$valgtVarAndel, '_', Sys.time(), '.', input$bildeformatAndel)
    },
    content = function(file){
      NakkeFigAndelerGrVar(RegData=RegData,
                           valgtVar=input$valgtVarAndel,
                           reshID=reshID,
                           datoFra=input$datovalgAndel[1],
                           datoTil=input$datovalgAndel[2],
                           minald=as.numeric(input$alderAndel[1]),
                           maxald=as.numeric(input$alderAndel[2]),
                           erMann=as.numeric(input$erMannAndel),
                           myelopati = as.numeric(input$myelopatiAndel),
                           fremBak = as.numeric(input$fremBakAndel),
                           inngrep=as.numeric(input$inngrepAndel), session=session,
                      outfile = file)
    })

  output$andelTid <- renderPlot({
    NakkeFigAndelTid(RegData=RegData, valgtVar=input$valgtVarAndel,
                     reshID=reshID,
                     datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                     minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                     erMann=as.numeric(input$erMannAndel),
                     myelopati = as.numeric(input$myelopatiAndel),
                     fremBak = as.numeric(input$fremBakAndel),
                     tidsenhet = input$tidsenhetAndelTid,
                     enhetsUtvalg = input$enhetsUtvalgAndelTid,
                     inngrep=as.numeric(input$inngrepAndel),
                     session=session)
  }, height=300, width=1000)

  output$LastNedFigAndelTid <- downloadHandler(
    filename = function(){
      paste0('FigAndelTid_', valgtVar=input$valgtVarAndel, '_', Sys.time(), '.', input$bildeformatAndel)
    },
    content = function(file){
      NakkeFigAndelTid(RegData=RegData, valgtVar=input$valgtVarAndel,
                       reshID=reshID,
                       datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                       minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                       erMann=as.numeric(input$erMannAndel),
                       myelopati = as.numeric(input$myelopatiAndel),
                       fremBak = as.numeric(input$fremBakAndel),
                       tidsenhet = input$tidsenhetAndelTid,
                       enhetsUtvalg = input$enhetsUtvalgAndelTid,
                       inngrep=as.numeric(input$inngrepAndel),
                       session=session,
                       outfile = file)
    })

  observe({
    #AndelTid
    AndelerTid <- NakkeFigAndelTid(RegData=RegData,  valgtVar=input$valgtVarAndel,
                                   reshID=reshID,
                                   datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                   minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                   erMann=as.numeric(input$erMannAndel),
                                   myelopati = as.numeric(input$myelopatiAndel),
                                   fremBak = as.numeric(input$fremBakAndel),
                                   tidsenhet = input$tidsenhetAndelTid,
                                   enhetsUtvalg = input$enhetsUtvalgAndelTid,
                                   inngrep=as.numeric(input$inngrepAndel),
                                   session=session) #,lagFig=0)
    tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid, figurtype = 'andelTid')

    kolGruppering <- c(1,3,3)
    names(kolGruppering) <- c(' ', AndelerTid$hovedgrTxt, AndelerTid$smltxt)
    output$andelTidTab <- function() {
      antKol <- ncol(tabAndelTid)
      kableExtra::kable(tabAndelTid, format = 'html'
                         , full_width=F
                         , digits = c(0,0,1,0,0,1)[1:antKol]
       ) %>%
       #  kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        kableExtra::add_header_above(kolGruppering[1:(2+AndelerTid$medSml)]) %>%
        kableExtra::column_spec(column = 1, width_min = '7em') %>%
        kableExtra::column_spec(column = 2:(antKol+1), width = '7em') %>%
        kableExtra::row_spec(0, bold = T)
    }
    output$lastNed_tabAndelTid <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_andelTid.csv')
      },
      content = function(file, filename){
        write.csv2(tabAndelTid, file, row.names = T, fileEncoding = 'latin1', na = '')
      })


    #AndelGrVar
    AndelerShus <- NakkeFigAndelerGrVar(RegData=RegData,
                                        valgtVar=input$valgtVarAndel,
                                        reshID=reshID,
                                        datoFra=input$datovalgAndel[1],
                                        datoTil=input$datovalgAndel[2],
                                        minald=as.numeric(input$alderAndel[1]),
                                        maxald=as.numeric(input$alderAndel[2]),
                                        erMann=as.numeric(input$erMannAndel),
                                        myelopati = as.numeric(input$myelopatiAndel),
                                        inngrep=as.numeric(input$inngrepAndel),
                                        session=session) #, lagFig = 0))
    tabAndelerShus <- cbind('Antall (n)' = AndelerShus$Nvar,
                            'Antall (N)' = AndelerShus$Ngr,
                            'Andel (%)' = AndelerShus$AggVerdier)
    output$andelerGrVarTab <- function() {
      antKol <- ncol(tabAndelerShus)
      kableExtra::kable(tabAndelerShus, format = 'html'
                        #, full_width=T
                        , digits = c(0,0,1) #,0,1)[1:antKol]
      ) %>%
        kableExtra::column_spec(column = 1, width_min = '5em') %>%
        kableExtra::column_spec(column = 2:(antKol+1), width = '4em') %>%
        kableExtra::row_spec(0, bold = T)
    }
    output$lastNed_tabAndelGrVar <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_andelGrVar.csv')
      },
      content = function(file, filename){
        write.csv2(tabAndelerShus, file, row.names = T, fileEncoding = 'latin1', na = '')
      })

    output$tittelAndel <- renderUI({
      tagList(
        h3(AndelerShus$tittel),
        h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
      )}) #, align='center'
  }) #observe

#------------ Gjennomsnitt--------------------------
  output$gjsnGrVar <- renderPlot({
    NakkeFigGjsnGrVar(RegData=RegData,  valgtVar=input$valgtVarGjsn,
                      reshID=reshID,
                      datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                      minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                      erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                      fremBak = as.numeric(input$fremBakGjsn),
                      inngrep=as.numeric(input$inngrepGjsn),
                      valgtMaal = input$sentralmaal,
                      session=session)
  }, height=600, width=500)

  output$LastNedFigGjsnGrVar <- downloadHandler(
    filename = function(){
      paste0('FigGjsnSh_', valgtVar=input$valgtVarGjsn, '_', Sys.time(), '.', input$bildeformatGjsn)
    },
    content = function(file){
      NakkeFigGjsnGrVar(RegData=RegData,  valgtVar=input$valgtVarGjsn,
                        reshID=reshID,
                        datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                        minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                        erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                        fremBak = as.numeric(input$fremBakGjsn),
                        inngrep=as.numeric(input$inngrepGjsn),
                        valgtMaal = input$sentralmaal,
                        session=session,
                       outfile = file)
    })

  output$gjsnTid <- renderPlot({
    NakkeFigGjsnTid(RegData=RegData,  valgtVar=input$valgtVarGjsn,
                    reshID=reshID,
                    datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                    minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                    erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                    fremBak = as.numeric(input$fremBakGjsn),
                    inngrep=as.numeric(input$inngrepGjsn),
                    valgtMaal = input$sentralmaal,
                    tidsenhet = input$tidsenhetGjsn,
                    enhetsUtvalg = input$enhetsUtvalgGjsn,
                    session = session)
  }, height=300, width=1000)

  output$LastNedFigGjsnTid <- downloadHandler(
    filename = function(){
      paste0('FigGjsnTid_', valgtVar=input$valgtVarGjsn, '_', Sys.time(), '.', input$bildeformatGjsn)
    },
    content = function(file){
      NakkeFigGjsnTid(RegData=RegData,  valgtVar=input$valgtVarGjsn,
                      reshID=reshID,
                      datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                      minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                      erMann=as.numeric(input$erMannGjsn), myelopati = as.numeric(input$myelopatiGjsn),
                      fremBak = as.numeric(input$fremBakGjsn),
                      inngrep=as.numeric(input$inngrepGjsn),
                      valgtMaal = input$sentralmaal,
                      tidsenhet = input$tidsenhetGjsn,
                      enhetsUtvalg = input$enhetsUtvalgGjsn,
                      session = session,
                      outfile = file)
    })

observe({ #Sykehusvise gjennomsnitt, figur og tabell
  UtDataGjsnGrVar <- NakkeFigGjsnGrVar(RegData=RegData,
                                       valgtVar=input$valgtVarGjsn,
                                       reshID=reshID,
                                       datoFra=input$datovalgGjsn[1],
                                       datoTil=input$datovalgGjsn[2],
                                       inngrep=as.numeric(input$inngrepGjsn),
                                       minald=as.numeric(input$alderGjsn[1]),
                                       maxald=as.numeric(input$alderGjsn[2]),
                                       erMann=as.numeric(input$erMannGjsn),
                                       myelopati = as.numeric(input$myelopatiGjsn),
                                       fremBak = as.numeric(input$fremBakGjsn),
                                       valgtMaal = input$sentralmaal,
                                       session = session)
  output$tittelGjsn <- renderUI({
    tagList(
      h3(UtDataGjsnGrVar$tittel),
      h5(HTML(paste0(UtDataGjsnGrVar$utvalgTxt, '<br />')))
    )}) #, align='center'
antDes <- ifelse(input$valgtVarGjsn %in%
                   c('Eq5DScorePreOp', 'EQ5Dendr3mnd', 'EQ5Dendr12mnd'), 2, 1)
antDesFormat <- paste0("%.", antDes, "f")
  tabGjsnGrVar <- cbind(Antall = UtDataGjsnGrVar$Ngr, #$Hoved,
                        Sentralmål = sprintf(antDesFormat,UtDataGjsnGrVar$AggVerdier$Hoved),
                        Konf.int. = paste0(sprintf(antDesFormat,UtDataGjsnGrVar$AggVerdier$KIned), ' - ',
                                          sprintf(antDesFormat,UtDataGjsnGrVar$AggVerdier$KIopp)))
  colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')

  output$gjsnGrVarTab <- function() {
    kableExtra::kable(tabGjsnGrVar, format = 'html'
                      , full_width=F
                      , digits = c(0,1) #,1,1)[1:antKol]
                      , align = 'r'
    ) %>%
      kableExtra::column_spec(column = 1, width_min = '7em') %>%
      kableExtra::column_spec(column = 2:4, width = '8em') %>%
      kableExtra::row_spec(0, bold = T)
  }

  output$lastNed_gjsnGrVarTab <- downloadHandler(
    filename = function(){
      paste0(input$valgtVarGjsn, '_tabGjsnSh .csv')
    },
    content = function(file, filename){
      write.csv2(tabGjsnGrVar, file, row.names = T, fileEncoding = 'latin1', na = '')
    })

  #------gjsnTid

  UtDataGjsnTid <- NakkeFigGjsnTid(RegData=RegData,
                                   valgtVar=input$valgtVarGjsn,
                                   reshID=reshID,
                                   datoFra=input$datovalgGjsn[1],
                                   datoTil=input$datovalgGjsn[2],
                                   inngrep=as.numeric(input$inngrepGjsn),
                                   minald=as.numeric(input$alderGjsn[1]),
                                   maxald=as.numeric(input$alderGjsn[2]),
                                   erMann=as.numeric(input$erMannGjsn),
                                   myelopati = as.numeric(input$myelopatiGjsn),
                                   fremBak = as.numeric(input$fremBakGjsn),
                                   valgtMaal = input$sentralmaal,
                                   tidsenhet = input$tidsenhetGjsn,
                                   enhetsUtvalg = input$enhetsUtvalgGjsn,
                                   session = session)

  tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
  grtxt <-UtDataGjsnTid$grtxt
  if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
    grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
  rownames(tabGjsnTid) <- grtxt

  antKol <- ncol(tabGjsnTid)
  navnKol <- colnames(tabGjsnTid)
  if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}

  kolGruppering <- c(1,3,3)
  names(kolGruppering) <- c(' ', UtDataGjsnTid$hovedgrTxt, UtDataGjsnTid$smltxt)
  output$gjsnTidTab <- function() { #kableExtra::kable
    kableExtra::kable(tabGjsnTid, format = 'html'
                      , full_width=F
                      , digits = antDes #c(0,1,1,1)[1:antKol]
    ) %>%
      kableExtra::add_header_above(kolGruppering[1:(2+UtDataGjsnTid$medSml)]) %>%
      kableExtra::column_spec(column = 1, width_min = '7em') %>%
      kableExtra::column_spec(column = 2:(antKol+1), width = '7em') %>%
      kableExtra::row_spec(0, bold = T)
  }

  output$lastNed_gjsnTidTab <- downloadHandler(
    filename = function(){
      paste0(input$valgtVarGjsn, '_tabGjsnTid .csv')
    },
    content = function(file, filename){
      write.csv2(tabGjsnTid, file, row.names = T, fileEncoding = 'latin1', na = '')
    })

}) #observe gjsnGrVar




#------------------ Abonnement ----------------------------------------------


#Start modul, abonnement
orgs <- as.list(sykehusValg[-1])

## make a list for report metadata
reports <- list(
  Kvartalsrapp = list(
    synopsis = "NKR_Nakke/Rapporteket: Resultatrapport, abonnement",
    fun = "abonnementNakke",
    paramNames = c('rnwFil', 'reshID', 'brukernavn'),
    paramValues = c('NakkeMndRapp.Rnw', reshID, brukernavn) #'Alle')
  )
)
#test <- abonnementNakke(rnwFil = 'NakkeMndRapp.Rnw', brukernavn='hei', reshID=601161, datoTil=Sys.Date())
rapbase::autoReportServer(
  id = "NakkeAbb", registryName = "nakke", type = "subscription",
  paramNames = paramNames, paramValues = paramValues,
  reports = reports, orgs = orgs, eligible = TRUE
)


} #server
# Run the application
shinyApp(ui = ui, server = server)

