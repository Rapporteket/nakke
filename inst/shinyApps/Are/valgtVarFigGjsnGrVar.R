#' valgtVarFigGjsnGrVar provides a list of registry vars for the report
#'
#' The list of possible variables to be used in the report type
#' \emph{figGjsnGrVar}. Placed as its own function to trim the shiny app for
#' Nakke
#'
#' @return valgtVarListFigGjsnGrVar list of names and values
#' @export

valgtVarFigGjsnGrVar <- function() {

  valgtVarListFigGjsnGrVar <-
    list("Alder (år)"="Alder",
         "EMS hos Myelopatipasienter før"="EMSscorePreOp",
         "Total knivtid"="KnivtidTotalMin",
         "NDI før operasjon"="NDIscorePreOp",
         "Liggetid etter operasjon"="LiggeDognPostop",
         "Antall liggedøgn, totalt"="LiggeDognTotalt",
         "NSR, arm før operasjon"="NRSsmerteArmPreOp",
         "NSR, nakke før operasjon"="NRSsmerteNakkePreOp")

  return(valgtVarListFigGjsnGrVar)
}
