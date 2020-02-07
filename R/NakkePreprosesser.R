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
	# RegData$InnDato <- as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")
	# RegData$Mnd <- RegData$InnDato$mon +1
	# RegData$MndNum <- RegData$InnDato$mon +1
	# RegData$MndAar <- format(RegData$InnDato, '%b%y')
	# RegData$Kvartal <- ceiling(RegData$MndNum/3)
	# RegData$Halvaar <- ceiling(RegData$MndNum/6)
	# RegData$Aar <- 1900 + RegData$InnDato$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year

	RegData$InnDato <- as.Date(RegData$OprDato, format="%Y-%m-%d")
	RegData$MndNum <- as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")$mon +1
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	RegData$Aar <- 1900 + as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
	RegData$MndAar <- format(RegData$InnDato, '%b%y')

	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AvdRESH')] <- 'ReshId'
	class(RegData$ReshId) <- 'numeric'
	#names(RegData)[which(names(RegData) == 'SykehusNavn')] <- 'ShNavn'

	RegData$ShNavn <- as.character(RegData$SykehusNavn) #Får bort tomme navn

	#Lage hovekategorier
	# OprMetodeDiskektomi OprMetodeKirDekompresjon OprMetodeAnnenBakreDekompr OprMetodeKorpektomi
	# OprMetodeAndre OprMetodeBakreFusjon
variable <- c('OprMetodeDiskektomi', 'OprMetodeKirDekompresjon', 'OprMetodeAnnenBakreDekompr',
              'OprMetodeKorpektomi', 'OprMetodeAndre', 'OprMetodeBakreFusjon')
ind <- which(is.na(RegData[ ,variable]), arr.ind = T)
RegData[,variable][ind] <- 0

#table(RegData$OprMetodeDiskektomi, useNA = 'a')
	RegData$Inngrep <- 0
	RegData$Inngrep[RegData$OprMetodeAndre > 0] <- 6

	ind1 <- with(RegData, which(Inngrep == 0
	                            & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1 |
	                                 OprMetodeTilgangFremreH == 1 | OprMetodeTilgangFremreV == 1 |
	                                 OprMetodeDiskektomiBenblokk == 1 | OprMetodeDiskektomiPlate == 1 |
	                                 OprMetodeDiskektomiCage == 1 & OprMetodeDiskektomiSkiveprotese ==1 )
	                            & RtgFunnProlaps == 1
	                            & (OprMetodeTilgangBakre == 0 | OprMetodeAndre == 0 |
	                                 OprMetodeForamenotomiBakreUniLat == 0 | OprMetodeForamenotomiBakreBiLat == 0 |
	                                 OprMetodeAnnenBakreDekompr == 1 | is.na(OprMetodeAnnenBakreDekompr) )
	                            & (OprMetodeBakreFusjon == 0)
	                            & OprMetodeKorpektomi == 0))
	RegData$Inngrep[ind1] <- 1

	ind2 <- with(RegData, which(Inngrep == 0
	                            & (OprMetodeDiskektomi == 0 | OprMetodeTilgangFremre == 0)
	                            & (OprMetodeTilgangBakre == 1 | OprMetodeAndre == 0 |
	                                 OprMetodeKirDekompresjon == 1 | OprMetodeForamenotomiBakreUniLat == 1 |
	                                 OprMetodeForamenotomiBakreBiLat == 1 | OprMetodeAnnenBakreDekompr > 0 )
	                            & OprMetodeBakreFusjon == 0
	                            & OprMetodeKorpektomi == 0))
RegData$Inngrep[ind2] <- 2

	ind3 <- with(RegData, which(Inngrep == 0
	                            & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1)
	                            & RtgFunnProlaps == 0
	                            & (RtgFunnRotkanalstenose == 1 | RtgFunnCervicalSpStenose == 1 | RtgFunnDegnerasjonNakke == 1)
	                            & (OprMetodeTilgangBakre == 0 | OprMetodeAndre == 0 |
	                                 OprMetodeForamenotomiBakreUniLat == 0| OprMetodeForamenotomiBakreBiLat == 0 |
	                                 OprMetodeAnnenBakreDekompr < 1 )
	                            & OprMetodeBakreFusjon == 0 & OprMetodeKorpektomi == 0))
RegData$Inngrep[ind3] <- 3

	ind4 <- with(RegData, which(Inngrep == 0
	                            & (OprMetodeDiskektomi == 0 | OprMetodeTilgangFremre == 0)
	                            & OprMetodeBakreFusjon == 1
	                            & (OprMetodeTilgangBakre == 1 | OprMetodeAndre == 0 | BakreFusjonWire == 1
	                               | BakreFusjonSkruer == 1 | BakreFusjonStag == 1)
	                            & OprMetodeKorpektomi == 0))
	RegData$Inngrep[ind4] <- 4 	#DO IF

	RegData$Inngrep[RegData$OprMetodeKorpektomi == 1] <- 5

	ind3_2 <- with(RegData, which(Inngrep == 0 & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1
	                                      | OprMetodeDiskektomiCage == 1 | OprMetodeDiskektomiSkiveprotese == 1 )
	               & RtgFunnProlaps == 0))
	RegData$Inngrep[ind3_2] <- 3
	ind1_2 <- with(RegData, which(Inngrep == 0
	                              & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1
	                                 | OprMetodeDiskektomiCage == 1 | OprMetodeDiskektomiSkiveprotese == 1 )
	                              & RtgFunnProlaps == 0))
	RegData$Inngrep[ind1_2] <- 1
	RegData$Inngrep[RegData$Inngrep == 0  & RegData$OprMetodeBakreFusjon > 0] <- 4

	# 0 'Ikke klassifiserbar operasjon'
	# 1 'Fremre diketomi for prolaps'
	# 2 'Bakre dekompresjon'
	# 3 'Fremre dekompresjon sp st.uten prolaps'
	# 4 'Bakre fusjon'
	# 5 'Korporektomi'
	# 6 'Andre inngrep'.


	RegData$OpFremBak <- 0
	RegData$OpFremBak[RegData$OprMetodeTilgangFremre==1] <- 1
	RegData$OpFremBak[RegData$OprMetodeTilgangBakre==1] <- 2

	#variable <- c('OprMetodeDiskektomi', 'OprMetodeKirDekompresjon', 'OprMetodeAnnenBakreDekompr',
	#              'OprMetodeKorpektomi', 'OprMetodeAndre', 'OprMetodeBakreFusjon')

	RegData$OpTilgfrembak<-0
	RegData$OpTilgfrembak[RegData$OprMetodeAndre > 0] <- 3
	with(RegData, OpTilgfrembak[OpTilgfrembak == 0 & (OprMetodeTilgangFremre == 1 & OprMetodeTilgangBakre == 1)] <- 4)
	ind1 <- with(RegData, which(OpTilgfrembak == 0
	                            & OprMetodeAndre == 0
	                            & (OprMetodeDiskektomi == 1 | OprMetodeTilgangFremre == 1 | OprMetodeTilgangFremreH == 1 |
	                                 OprMetodeTilgangFremreV == 1 | OprMetodeDiskektomiBenblokk == 1 |
	                                 OprMetodeDiskektomiPlate == 1 | OprMetodeDiskektomiCage == 1 & OprMetodeDiskektomiSkiveprotese==1
	                               |  OprMetodeKorpektomi == 1)  ))


	RegData$OpTilgfrembak[ind1] <- 1
	ind2 <- with(RegData, which(OpTilgfrembak == 0 & OprMetodeAndre == 0
	                            & (OprMetodeTilgangBakre == 1 | OprMetodeForamenotomiBakreUniLat == 1
	                               | OprMetodeForamenotomiBakreBiLat == 1)
	                            | (OprMetodeAnnenBakreDekompr > 0 & OprMetodeAnnenBakreDekompr < 9 )
	                            | OprMetodeBakreFusjon == 1))
	RegData$OpTilgfrembak[ind2] <- 2

	grtxt <- c('Ikke klassifiserbar', 'Fremre tilgang', 'Bakre tilgang', 'Andre inngrep', 'Kombinert bakre fremre')
	#               VARIABLE LABELS OpTilgfrembak 'Type tilgang'.




  return(invisible(RegData))
}
