#' nakkeValgtVarFigAndeler provides a list of registry vars for the report
#'
#' The list of possible variables to be used in the report type
#' \emph{figAndeler}. Placed as its own function to trim the shiny app for
#' Nakke
#'
#' @return valgtVarListFigAndeler list of names and values
#' @export

valgtVarFigAndeler <- function() {

  valgtVarListFigAndeler <-
    list("Aldersfordeling"="Alder",
         "Antall nivå operert"="AntallNivaaOpr",
         "Er det gitt antibiotikaprofylakse"="Antibiotika",
         "Arbeidsstatus 12mnd etter operasjon"="Arbeidstaus12mnd",
         "Arbeidsstatus 3mnd etteroperasjon"="Arbeidstaus3mnd",
         "Arbeidsstatus før operasjon"="ArbeidstausPreOp",
         "ASAgrad"="ASAgrad",
         "Pasientenes BMI BodyMassIndex"="BMI",
         "Helsetilstand Angst"="EqAngstPreOp",
         "Søkt erstatning"="ErstatningPreOp",
         "Fornøydhet med behandlinga på sykehuset 12mnd"="FornoydBeh12mnd",
         "Fornøydhet med behandlinga påsykehuset 3mnd"="FornoydBeh3mnd",
         "Hastegrad"="OperasjonsKategori",
         "Antall liggedøgn postoperativt"="LiggeDognPostop",
         "Totalt antall liggedøgn"="LiggeDognTotalt",
         "Morsmål"="Morsmal",
         "Nytte av operasjon 12mnd"="NytteOpr12mnd",
         "Nytte av operasjon 3mnd"="NytteOpr3mnd",
         "Paresegrad før operasjon"="OprIndikPareseGrad",
         "Røyker pasienten"="Roker",
         "Har pasienten fått sårdren"="Saardren",
         "Sivilstatus"="SivilStatus",
         "Hyppighet av smertestillende før operasjonen"="SmertestillBrukPreOp",
         "Snuser pasienten"="Snuser",
         "Varighet av utstrålende armsmerter"="SymptVarighetArmer",
         "Varighet av nakke hodesmerter"="SymptVarighetNakkeHode",
         "Er pasienten tidligere operert"="TidlOpr",
         "Antall tidligere operasjoner"="TidlOprAntall",
         "Søkt uføretrygd"="UforetrygdPreOp",
         "Utdanningsnivå"="Utdanning")

  return(valgtVarListFigAndeler)
}
