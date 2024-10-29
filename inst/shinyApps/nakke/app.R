#Datauthenting for nakke
library(nakke)
# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QAC", "PRODUCTIONC")) #rapbase::isRapContext()
regTitle = 'Degenerativ Rygg'

ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  id = 'hovedark',

  # lag logo og tittel som en del av navbar
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  # sett inn tittel også i browser-vindu
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",


  #-------Registeradministrasjon----------
  tabPanel("Datanedlasting",

           tabPanel(
               h4("Eksport av krypterte data"),
               sidebarPanel(
                 rapbase::exportUCInput("nakkeExport")
               ),
               mainPanel(
                 rapbase::exportGuideUI("nakkeExportGuide")
               )
             ) #Eksport-tab
  ) #tab SC

) #ui-del




#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {

    #-- Div serveroppstart----

  #reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  #brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(session), 'inkognito')})

  # widget
  # if (paaServer) {
  #   output$appUserName <- renderText(rapbase::getUserFullName(session))
  #   output$appOrgName <- renderText(paste0('rolle: ', rolle, '<br> ReshID: ', reshID) )}

  # User info in widget
  # userInfo <- rapbase::howWeDealWithPersonalData(session)
  # observeEvent(input$userInfo, {
  #   shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
  #                          type = "", imageUrl = "rap/logo.svg",
  #                          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
  #                          html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  # })

  if (rolle=='SC') {

    #----------- Eksport ----------------
    registryName <- "nakke"
    ## brukerkontroller
    rapbase::exportUCServer("nakkeExport", registryName)
    ## veileding
    rapbase::exportGuideServer("nakkeExportGuide", registryName)
  }

} #server
# Run the application
shinyApp(ui = ui, server = server)

