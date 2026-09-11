

library(nakke)

#remotes::install_github('Rapporteket/nakke', ref = 'main')
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/nakke1505f0ff0.sql.gz__20260831_090711.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")
# source c://Users/lro2402unn/RegistreGIT/data/nakke1505f0ff0.sql;
setwd('c://Users/lro2402unn/RegistreGIT/nakke')

library(nakke)
source("dev/sysSetenv.R")
nakke::kjorNakkeApp(browser = TRUE)

#dataGML <- NakkeRegDataSQL_FAS_UT(datoFra = '2010-01-01', alleVar = 1)

RegData <- NakkeHentRegData(datoFra = '2025-01-01') #, medOppf = 1)
RegData <- NakkePreprosess(RegData)

# 3,6s - sammenstille
# 6,8s - hente tabeller++
# 10,4s - hente alt

unique(RegData[,c("ReshId", 'SykehusNavn')])
reshID <- 114288

var <- c('MotoriskOexMJOA', 'MotoriskUexMJOA', 'SensoriskOexMJOA', 'BlareSfinkterMJOA', # 'MJOAsumPre',
                       'MotoriskOexMJOA3mnd', 'MotoriskUexMJOA3mnd', 'SensoriskOexMJOA3mnd', 'BlareSfinkterMJOA3mnd',
                       'MotoriskOexMJOA12mnd', 'MotoriskUexMJOA12mnd', 'SensoriskOexMJOA12mnd', 'BlareSfinkterMJOA12mnd')
for (k in 1:length(var)) {
  print(var[k])
  print(table(RegData[,var[k]]))
}

#Data fra 11.mai 2026:
#1:MotoriskOexMJOA og 4:BlareSfinkterMJOA inneholder 0
table(RegData$Aar, RegData$MotoriskOexMJOA) # 1 i jan26, 1 i mar26
table(RegData$Aar, RegData$BlareSfinkterMJOA)  # 2 i jan26, 1 i mar26

"MotoriskOexMJOA"
0   1   2   3   4   5   9
2   9  15  31 142 214  83
"MotoriskUexMJOA"
1   2   3   4   5   6   7   8  99
1   3  32  32  25  76 244   1  82
"SensoriskOexMJOA"
1   2   3   9
72 187 115 122
"BlareSfinkterMJOA"
0   1   2   3   4   9
3  16  74 330   1  72
"MotoriskOexMJOA3mnd"
1  2  3  4  5  6
1  2  8 23 91 55
"MotoriskUexMJOA3mnd"
3  4  5  6  7  8
3  8 12 15 81 58
"SensoriskOexMJOA3mnd"
1  2  3  4
10 45 82 43
"BlareSfinkterMJOA3mnd"
1  2  3  4
1 22 92 65
"MotoriskOexMJOA12mnd"
3 4 5 6
1 1 3 1
"MotoriskUexMJOA12mnd"
6 7 8
1 4 1
"SensoriskOexMJOA12mnd"
2 3
2 4
"BlareSfinkterMJOA12mnd"
2 3 4
3 2 1


"MotoriskOexMJOA"
0   1   2   3   4   5   9
2  12  27  58 290 404 110
"MotoriskUexMJOA"
1   2   3   4   5   6   7   8  99
2   4  42  55  53 163 470   1 113
"SensoriskOexMJOA"
1   2   3   9
145 372 235 151
"BlareSfinkterMJOA"
0   1   2   3   4   9
5  26 131 643   1  97
"MotoriskOexMJOA3mnd"
1   2   3   4   5   6
1   3  13  28 166 284
"MotoriskUexMJOA3mnd"
3   4   5   6   7   8
3  14  30  34 131 275
"SensoriskOexMJOA3mnd"
1   2   3   4
11  60 194 229
"BlareSfinkterMJOA3mnd"
1   2   3   4
4  24 136 328
"MotoriskOexMJOA12mnd"
3 4 5 6
1 1 5 4
"MotoriskUexMJOA12mnd"
6 7 8
1 5 5
"SensoriskOexMJOA12mnd"
2 3 4
2 8 1
"BlareSfinkterMJOA12mnd"
2 3 4
4 2 4


#Henter tilgangstre og mapper om resh og SykehusNavn
Sys.setenv(MRS_ACCESS_HIERARCHY_URL= 'https://qreg.nhn.no/nakke/api/centre-information')
TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
Tilgangstre <- jsonlite::fromJSON(TilgJsn)$AccessUnits


rapbase::runAutoReport(dato = "2025-07-25", group = "nakke", dryRun = TRUE)
#Velg en dato som matcher startDate for rapporten du vil kjøre (som du finner i autoreport-tabellen i db_autoreport-databasen)

#I UI-funksjonen så det slik ut:
shiny::tabPanel(
  "Utsending",
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      rapbase::autoReportOrgInput("norgastDispatch"),
      rapbase::autoReportInput("norgastDispatch"),
      shiny::actionButton(inputId = "run_autoreport",
                          label = "Kjør autorapporter"),
      shiny::dateInput(inputId = "rapportdato",
                       label = "Kjør rapporter med dato:",
                       value = Sys.Date(),
                       min = Sys.Date(),
                       max = Sys.Date() + 366
      ),
      shiny::checkboxInput(inputId = "dryRun", label = "Send e-post")
    ),
    shiny::mainPanel(
      rapbase::autoReportUI("norgastDispatch"),
      p(em("System message:")),
      verbatimTextOutput("sysMessage"),
      p(em("Function message:")),
      verbatimTextOutput("funMessage")
    )
  )
)

#Server:
kjor_autorapport <- shiny::observeEvent(input$run_autoreport, {
  dato <- input$rapportdato
  dryRun <- !(input$dryRun)
  withCallingHandlers({
    shinyjs::html("sysMessage", "")
    shinyjs::html("funMessage", "")
    shinyjs::html("funMessage",
                  rapbase::runAutoReport(group = "nakke",
                                         dato = dato, dryRun = dryRun))
  },
  message = function(m) {
    shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
  })
})

#    For hele konteksten kan du f.eks. se tag v3.0.15 hos norgast.
