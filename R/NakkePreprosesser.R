#' Preprosesser data fra Degenerativ Nakke
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @inheritParams FigAndeler
#'
#' @return RegData En dataramme med det preprosesserte datasettet
#'
#' @export

NakkePreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer:
	RegData <- RegData[which(RegData$LegeskjemaStatus == 1), ]  #Vi ønsker kun ferdigstilte legeskjema
	#Kjønnsvariabel:ErMann - vil senere benytte denne
	RegData$ErMann <- RegData$Kjonn
	RegData$ErMann[which(RegData$Kjonn == 2)] <- 0
	#names(which(names(RegData) == 'ErMann')) <- 'erMann'
	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")
	RegData$Aar <- 1900 + strptime(RegData$InnDato, format="%Y")$year
	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'
	class(RegData$ReshId) <- 'numeric'


  return(invisible(RegData))
}
