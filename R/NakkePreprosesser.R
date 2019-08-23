#' Preprosesser data fra Degenerativ Nakke
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @inheritParams NakkeFigAndeler
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
	RegData$Mnd <- RegData$InnDato$mon +1
	RegData$MndNum <- RegData$InnDato$mon +1
	RegData$MndAar <- format(RegData$InnDato, '%b%y')
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	RegData$Aar <- 1900 + RegData$InnDato$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year

	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'
	class(RegData$ReshId) <- 'numeric'
	#names(RegData)[which(names(RegData) == 'SykehusNavn')] <- 'ShNavn'

	RegData$ShNavn <- as.character(RegData$SykehusNavn) #Får bort tomme navn

  return(invisible(RegData))
}
