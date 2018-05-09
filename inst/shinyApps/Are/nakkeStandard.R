#' Server logic function for skiny Nakke
#'
#' nakkeStandard is the module server function for shiny Nakke
#'
#' @param input inngangsdata
#' @param output info som sendes ut
#' @param session Hmmm
#' @inheritParams FigAndeler
#' @export

nakkeStandard <- function(input, output, session, valgtVar, enhetsUtvalg,
                          valgtMaal) {
  sessionName <- session$ns("name")
  # namespace id comes with an extra '-name'. Remove it
  sessionName <- gsub("-name", "", sessionName)
  if (sessionName == "figAndeler") {
    plotObj <- reactive({
      FigAndeler(RegData=RegData,
                 valgtVar = valgtVar(),
                 datoFra = input$periode[1],
                 datoTil = input$periode[2],
                 erMann = as.numeric(input$erMann),
                 minald = as.numeric(input$alder[1]),
                 maxald = as.numeric(input$alder[2]),
                 enhetsUtvalg = enhetsUtvalg(),
                 reshID = as.numeric(input$avdeling)
      )
    })
  }

  if (sessionName == "figAndelerGrVar") {
    plotObj <- reactive({
      FigAndelerGrVar(RegData=RegData,
                      valgtVar = valgtVar(),
                      datoFra = input$periode[1],
                      datoTil = input$periode[2],
                      erMann=as.numeric(input$erMann),
                      minald = as.numeric(input$alder[1]),
                      maxald = as.numeric(input$alder[2]),
                      enhetsUtvalg = enhetsUtvalg(),
                      reshID = as.numeric(input$avdeling)
      )
    })
  }

  if (sessionName == "figAndelTid") {
    plotObj <- reactive({
      FigAndelTid(RegData=RegData,
                  valgtVar = valgtVar(),
                  datoFra = input$periode[1],
                  datoTil = input$periode[2],
                  erMann = as.numeric(input$erMann),
                  minald = as.numeric(input$alder[1]),
                  maxald = as.numeric(input$alder[2]),
                  enhetsUtvalg = enhetsUtvalg(),
                  reshID = as.numeric(input$avdeling)
      )
    })
  }

  if (sessionName == "figGjsnGrVar") {
    plotObj <- reactive({
      FigGjsnGrVar(RegData=RegData,
                   valgtVar = valgtVar(),
                   valgtMaal = valgtMaal(),
                   datoFra = input$periode[1],
                   datoTil = input$periode[2],
                   erMann=as.numeric(input$erMann),
                   minald = as.numeric(input$alder[1]),
                   maxald = as.numeric(input$alder[2]),
                   reshID = as.numeric(input$avdeling)
      )
    })
  }

  return(plotObj)
}
