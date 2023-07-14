#' valgtVarFigAndelTid provides a list of registry vars for the report
#'
#' The list of possible variables to be used in the report type
#' \emph{figAndelTid}. Placed as its own function to trim the shiny app for
#' Nakke
#'
#' @return valgtVarListFigAndelTid list of names and values
#' @export

valgtVarFigAndelTid <- function() {

  valgtVarListFigAndelTid <-
    list("Aldersfordeling"="Alder",
         "Andre sykdommer"="AndreRelSykdommer",
         "Fått antibiotika"="Antibiotika",
         "Mottar sykepenger, 12 mnd etter operasjon?"="Arbeidstaus12mnd",
         "Mottar sykepenger, 3 mnd etter operasjon?"="Arbeidstaus3mnd",
         "Mottar sykepenger, preoperativt?"="ArbeidstausPreOp",
         "ASA-grad > II"="ASAgrad",
         "Pasienter med fedme"="BMI",
         "Alle komplikasjoner"="EnhverKompl3mnd",
         "Søkt/planlegger å søke erstatning"="ErstatningPreOp",
         "Fornøyde pasienter, 12 mnd."="FornoydBeh12mnd",
         "Fornøyde pasienter, 3 mnd."="FornoydBeh3mnd",
         "Pasientrapportert dyp infeksjon, 3 mnd."="KomplinfekDyp3mnd",
         "Overfladisk infeksjon, 3 mnd."="KomplinfekOverfl3mnd",
         "Stemmevansker, 3 mnd."="KomplStemme3mnd",
         "Svelgvansker, 3 mnd."="KomplSvelging3mnd",
         "Misfornøyde pasienter, 12 mnd."="Misfor12mnd",
         "Misfornøyde pasienter, 3 mnd."="Misfor3mnd",
         "Klart bedre, 12 mnd."="NytteOpr12mnd",
         "Klart bedre, 3 mnd."="NytteOpr3mnd",
         "Operasjonsårsak, Myelopati"="OprIndikMyelopati",
         "Operasjonsårsak, Smerter"="OprIndikSmerter",
         "Komplikasjoner ved operasjon"="PerOpEnhverKompl",
         "Røykere"="Roker",
         "Andel som får sårdren"="Saardren",
         "Bruker smertestillende, preop."="SmertestillPreOp",
         "Varighet av hode-/nakkesmerter over 1 år"="SymptVarighetNakkeHode",
         "Varighet av armsmerter, minst 1 år   SymptVarighetSmerterUker"="SymptVarighetArmer",
         "Søkt eller planlegger å søke uføretrygd?"="UforetrygdPreOp",
         "Andel høyskole-/universitetsutdannede"="Utdanning",
         "Klart verre, 12 mnd."="Verre12mnd",
         "Klart verre, 3 mnd."="Verre3mnd")

  return(valgtVarListFigAndelTid)
}
