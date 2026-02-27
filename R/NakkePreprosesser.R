#' Preprosesser data fra Degenerativ Nakke
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @param RegData dataramme med nakkedata
#'
#' @return RegData En dataramme med det preprosesserte datasettet
#' @export

NakkePreprosess <- function(RegData=RegData) {
  #Kun ferdigstilte registreringer:
	RegData <- RegData[which(RegData$StatusLegeSkjema == 1), ]  # Vi ønsker kun ferdigstilte legeskjema
	RegData$ErMann <- RegData$Kjonn
	RegData$ErMann[which(RegData$Kjonn == 2)] <- 0

	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.Date(RegData$OprDato, format="%Y-%m-%d")
	RegData$MndNum <- as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")$mon +1
	RegData$Kvartal <- ceiling(RegData$MndNum/3)
	RegData$Halvaar <- ceiling(RegData$MndNum/6)
	RegData$Aar <- 1900 + as.POSIXlt(RegData$OprDato, format="%Y-%m-%d")$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
	RegData$MndAar <- format(RegData$InnDato, '%b%y')

	RegData$DiffUtFerdig <- as.numeric(difftime(as.Date(RegData$ForstLukketLege), RegData$UtDato,units = 'days'))

	#Variabel som identifiserer avdelingas resh
	class(RegData$ReshId) <- 'numeric'
	RegData$ReshId[which(RegData$ReshId %in% c(999975, 4212372))] <- 107511

	RegData$SykehusNavn <- as.character(RegData$SykehusNavn) #Får bort tomme navn
	RegData$SykehusNavn <- trimws(as.character(RegData$SykehusNavn))  #Fjerner mellomrom etter navn
	RegData$SykehusNavn[which(RegData$ReshId == 107511)] <- 'Aleris Oslo'
	RegData$SykehusNavn <- RegData$SykehusNavn

	#Tomme sykehusnavn får resh som navn:
	indTom <- which(is.na(RegData$SykehusNavn) | RegData$SykehusNavn == '')
	RegData$SykehusNavn[indTom] <- RegData$ReshId[indTom]

	#Sjekker om alle resh har egne enhetsnavn
	dta <- unique(RegData[ ,c('ReshId', 'SykehusNavn')])
	duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId)>1)])
	duplSh <- names(table(dta$SykehusNavn)[which(table(dta$SykehusNavn)>1)])

	if (length(c(duplSh, duplResh)) > 0) {
	  ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$SykehusNavn %in% duplSh))
	  RegData$SykehusNavn[ind] <- paste0(RegData$SykehusNavn[ind],' (', RegData$ReshId[ind], ')')
	}

#--------------Beregnede variabler---------------------
#	TidlOpr, AntallNivaaOpr, KomplPerOp, Kompl3mnd, Kompl12mnd

	RegData <- RegData %>%
	  dplyr::mutate(TidlOpr = dplyr::case_when(
	      TidlOprAnnetNiv == 1 & TidlOprSammeNiv == 1 ~ 3,
	      TidlOprSammeNiv == 1 ~ 1,
	      TidlOprAnnetNiv == 1 ~ 2,
	      TidlOprNei == 1 ~ 4,
	      TRUE ~ 9)
	  )

	varNivaa <- c("SideNivaaC0C1", "SideNivaaC1C2", "SideNivaaC2C3", "SideNivaaC3C4",
	              "SideNivaaC4C5", "SideNivaaC5C6", "SideNivaaC6C7",  "SideNivaaC7TH1")
	RegData$AntallNivaaOpr <- rowSums(RegData[,varNivaa], na.rm = T)

#	PerOpEnhverKompl -> KomplPerOp
varKomplPerOp <-
  c("PerOpKomplDurarift", "PerOpKomplNerverotSkade", "PerOpKomplOpFeilNivaa",
    "PerOpKomplFeilplasseringImplant", "PerOpKomplBlodning", "PerOpKomplRespiratorisk",
    "PerOpKomplAnafylaksiI", "PerOpKomplMedullaskade", "PerOpKomplOsofagusSkade",
    "PerOpKomplSkadeStoreBlodkar", "PerOpKomplKardioVaskulare", "PerOpKomplAnnenNerveskade",
    "PerOpKomplAnnet")

RegData$KomplPerOp <- ifelse(rowSums(RegData[ ,varKomplPerOp]) > 0, 1, 0)


# EnhverKompl3mnd -> Kompl3mnd
varKompl3mnd <- c("KomplUVI3mnd", "KomplPneumoni3mnd",
                  "KomplDVT3mnd", "KomplLungeEmboli3mnd", "KomplinfekOverfl3mnd",
                  "KomplinfekDyp3mnd", "KomplKraftsvikt3mnd", "KomplSvelging3mnd",
                  "KomplStemme3mnd")
RegData$Kompl3mnd <- ifelse(rowSums(RegData[ ,varKompl3mnd]) > 0, 1, 0)

# EnhverKompl12mnd -> Kompl12mnd
varKompl12mnd <- c("KomplUVI12mnd", "KomplPneumoni12mnd",
                   "KomplDVT12mnd", "KomplLungeEmboli12mnd", "KomplinfekOverfl12mnd",
                   "KomplinfekDyp12mnd", "KomplKraftsvikt12mnd", "KomplSvelging12mnd",
                   "KomplStemme12mnd")
RegData$Kompl12mnd <- ifelse(rowSums(RegData[ ,varKompl12mnd]) > 0, 1, 0)


	# ----------------Lage hovekategorier
variabler <- c('OprMetodeDiskektomi', 'OprMetodeKirDekompresjon', 'OprMetodeAnnenBakreDekompr',
              'OprMetodeKorpektomi', 'OprMetodeAndre', 'OprMetodeBakreFusjon')
ind <- which(is.na(RegData[ ,variabler]), arr.ind = T)
RegData[,variabler][ind] <- 0

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


  return(invisible(RegData))
}
