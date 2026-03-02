

library(nakke)

#26
#remotes::install_github('Rapporteket/nakke', ref = 'main')
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/nakke119ead892.sql.gz__20260302_080143.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")
# source c://Users/lro2402unn/RegistreGIT/data/nakke119ead892.sql;
setwd('c://Users/lro2402unn/RegistreGIT/nakke')

library(nakke)
source("dev/sysSetenv.R")
nakke::kjorNakkeApp(browser = TRUE)

dataGML <- NakkeRegDataSQL_FAS_UT(datoFra = '2010-01-01', alleVar = 1)

dataNy <- NakkeHentRegData(datoFra = '2023-01-01', datoTil = '2025-12-31')
RegData <- NakkePreprosess(dataNy)

dataNy <- NakkeHentRegData()

# 3,6s - sammenstille
# 6,8s - hente tabeller++
# 10,4s - hente alt

unique(RegData[,c("ReshId", 'SykehusNavn')])
reshID <- 114288

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
