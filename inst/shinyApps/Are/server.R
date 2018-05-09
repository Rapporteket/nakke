# This is the server logic for the 'report' Shiny web application for 'Nakke'

# load libs, scripts and data here, once

shinyServer(function(input, output) {

  # reuse server module, but with different namespaces and per report user
  # controls outside namespace (if any)
  nakkeStandardFigAndeler <-
    callModule(nakkeStandard, "figAndeler", session = getDefaultReactiveDomain(),
               valgtVar=reactive(input$andelerValgtVar),
               enhetsUtvalg=reactive(input$andelerEnhetsUtvalg))

  nakkeStandardFigAndelerGrVar <-
    callModule(nakkeStandard, "figAndelerGrVar",
               valgtVar=reactive(input$andelerGrVarValgtVar),
               enhetsUtvalg=reactive(input$andelerGrVarEnhetsUtvalg))

  nakkeStandardFigAndelTid <-
    callModule(nakkeStandard, "figAndelTid",
               valgtVar=reactive(input$andelTidValgtVar),
               enhetsUtvalg=reactive(input$andelTidEnhetsUtvalg))

  nakkeStandardFigGjsnGrVar <-
    callModule(nakkeStandard, "figGjsnGrVar",
               valgtVar=reactive(input$gjsnGrVarValgtVar),
               valgtMaal=reactive(input$gjsnGrVarValgtMaal))


  output$andelerPlot <- renderPlot({
    nakkeStandardFigAndeler()
  })

  output$andelerGrVarPlot <- renderPlot({
    nakkeStandardFigAndelerGrVar()
  })

  output$andelTidPlot <- renderPlot({
    nakkeStandardFigAndelTid()
  })

  output$gjsnGrVarPlot <- renderPlot({
    nakkeStandardFigGjsnGrVar()
  })
})
