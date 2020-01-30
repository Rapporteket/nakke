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

	#Lage hovekategorier
	# OprMetodeDiskektomi OprMetodeKirDekompresjon OprMetodeAnnenBakreDekompr OprMetodeKorpektomi
	# OprMetodeAndre OprMetodeBakreFusjon

	RegData$OpKat <- 0
	RegData$OpKat[RegData$OprMetodeAndre > 0] <- 6

	ind1 <- with(RegData, which(OpKat == 0
	                            & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1 |
	                                 OprMetodeTilgangFremreH == 1 | OprMetodeTilgangFremreV == 1 |
	                                 OprMetodeDiskektomiBenblokk == 1 | OprMetodeDiskektomiPlate == 1 |
	                                 OprMetodeDiskektomiCage == 1 & OprMetodeDiskektomiSkiveprotese ==1 )
	                            & RtgFunnProlaps == 1
	                            & (OprMetodeTilgangBakre == 0 | OprMetodeAndre == 0 |
	                                 OprMetodeForamenotomiBakreUniLat == 0 | OprMetodeForamenotomiBakreBiLat == 0 |
	                                 OprMetodeAnnenBakreDekompr < 1 )
	                            & OprMetodeBakreFusjon == 0
	                            & OprMetodeKorpektomi == 0))
	RegData$OpKat[ind1] <- 1

	ind2 <- with(RegData, which(OpKat == 0
	                            & (OprMetodeDiskektomi == 0 | OprMetodeTilgangFremre == 0)
	                            & (OprMetodeTilgangBakre == 1 | OprMetodeAndre == 0 |
	                                 OprMetodeKirDekompresjon == 1 | OprMetodeForamenotomiBakreUniLat == 1 |
	                                 OprMetodeForamenotomiBakreBiLat == 1 | OprMetodeAnnenBakreDekompr > 0 )
	                            & OprMetodeBakreFusjon == 0
	                            & OprMetodeKorpektomi == 0))
RegData$OpKat[ind2] <- 2

	ind3 <- with(RegData, which(OpKat == 0
	                            & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1)
	                            & RtgFunnProlaps == 0
	                            & (RtgFunnRotkanalstenose == 1 | RtgFunnCervicalSpStenose == 1 | RtgFunnDegnerasjonNakke == 1)
	                            & (OprMetodeTilgangBakre == 0 | OprMetodeAndre == 0 |
	                                 OprMetodeForamenotomiBakreUniLat == 0| OprMetodeForamenotomiBakreBiLat == 0 |
	                                 OprMetodeAnnenBakreDekompr < 1 )
	                            & OprMetodeBakreFusjon == 0 & OprMetodeKorpektomi == 0))
RegData$OpKat[ind3] <- 3

	ind4 <- with(RegData, which(OpKat == 0
	                            & (OprMetodeDiskektomi == 0 | OprMetodeTilgangFremre == 0)
	                            & OprMetodeBakreFusjon == 1
	                            & (OprMetodeTilgangBakre == 1 | OprMetodeAndre == 0 | BakreFusjonWire == 1
	                               | BakreFusjonSkruer == 1 | BakreFusjonStag == 1)
	                            & OprMetodeKorpektomi == 0))
	RegData$OpKat[ind4] <- 4 	#DO IF

	RegData$OpKat[RegData$OprMetodeKorpektomi == 1] <- 5

	ind3_2 <- with(RegData, which(OpKat == 0 & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1
	                                      | OprMetodeDiskektomiCage == 1 | OprMetodeDiskektomiSkiveprotese == 1 )
	               & RtgFunnProlaps == 0))
	RegData$OpKat[ind3_2] <- 3
	ind1_2 <- with(RegData, which(OpKat == 0
	                              & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1
	                                 | OprMetodeDiskektomiCage == 1 | OprMetodeDiskektomiSkiveprotese == 1 )
	                              & RtgFunnProlaps == 0))
	RegData$OpKat[ind1_2] <- 1
	RegData$OpKat[RegData$OpKat == 0  & RegData$OprMetodeBakreFusjon > 0] <- 4

	grtxt <- c('Ikke klassifiserbar operasjon', 'Fremre diketomi for prolaps', 'Bakre dekompresjon',
	           'Fremre dekompresjon sp st.uten prolaps', 'Bakre fusjon', 'Korporektomi', 'Andre inngrep') #for verdiene 0:6
	# 0 'Ikke klassifiserbar operasjon'
	# 1 'Fremre diketomi for prolaps'
	# 2 'Bakre dekompresjon'
	# 3 'Fremre dekompresjon sp st.uten prolaps'
	# 4 'Bakre fusjon'
	# 5 'Korporektomi'
	# 6 'Andre inngrep'.







  return(invisible(RegData))
}
