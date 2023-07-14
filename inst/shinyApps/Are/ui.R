
# This is the user-interface definition for the 'report' Shiny web application
# for 'Nakke'

data("RegData")

# not implemented just yet, wait until sample data without preprocessing
if (1==0) {
  RegData <- NakkePreprosess(RegData=RegData)
}

shinyUI(fluidPage(tabsetPanel(

  tabPanel("Andeler",
           pageWithSidebar(
             headerPanel(title=h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figAndeler"),
               selectInput("andelerValgtVar", "Variabel:",
                           valgtVarFigAndeler(),
                           selected = "Alder"),
               selectInput("andelerEnhetsUtvalg", "Enhetsutvalg:",
                           c("Hele landet" = 0,
                             "Egen enhet mot resten av landet" = 1,
                             "Egen enhet" = 2),
                           selected = 1)
             ),
             mainPanel(
               plotOutput("andelerPlot")
             )
           )),
  tabPanel("AndelerGrVar",
           pageWithSidebar(
             headerPanel(title = h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figAndelerGrVar"),
               selectInput("andelerGrVarValgtVar", "Variabel:",
                           valgtVarFigAndelerGrVar(),
                           selected = "Alder"),
               selectInput("andelerGrVarEnhetsUtvalg", "Enhetsutvalg:",
                           c("Hele landet" = 0,
                             "Egen enhet mot resten av landet" = 1,
                             "Egen enhet" = 2),
                           selected = 1)
             ),
             mainPanel(
               plotOutput("andelerGrVarPlot")
             )
           )),
  tabPanel("AndelTid",
           pageWithSidebar(
             headerPanel(title=h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figAndelTid"),
               selectInput("andelTidValgtVar", "Variabel:",
                           valgtVarFigAndelTid(),
                           selected = "Alder"),
               selectInput("andelTidEnhetsUtvalg", "Enhetsutvalg:",
                           c("Hele landet" = 0,
                             "Egen enhet mot resten av landet" = 1,
                             "Egen enhet" = 2),
                           selected = 1)
             ),
             mainPanel(
               plotOutput("andelTidPlot")
             )
           )),
  tabPanel("GjsnGrVar",
           pageWithSidebar(
             headerPanel(title = h3("Brukervalg")),
             sidebarPanel(
               nakkeStandardInput("figGjsnGrVar"),
               selectInput("gjsnGrVarValgtVar", "Variabel:",
                           valgtVarFigGjsnGrVar(),
                           selected = "Alder"),
               selectInput("gjsnGrVarValgtMaal", "Sentralmål:",
                           c("Gjennomsnitt" = "Gjsn",
                             "Median" = "Med"),
                           selected = "Gjsn")
             ),
             mainPanel(
               plotOutput("gjsnGrVarPlot")
             )
           ))
)
)
)
