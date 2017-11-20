#' Søylediagram, horisontalt eller vertikalt, som viser andeler av valgt variabel.
#'
#' Søylediagrammet viser fordelinga til den valgte variabelen. Søylene er horisontale eller vertikale
#' avhengig av hvor stor plass kategorinavnet til søyla tar.
#'
#' @param RegData Dataramme med alle nødvendige variable fra registeret
#' @param outfile Navn på fil figuren skrives ned til
#' @param reshID Avdelingsid (reshID) for egen avdeling,
#' @param hentData Angir om funksjonen skal kjøre spørring for å hente data eller ikke.
#'					0: ikke kjør (standard)
#'					1: kjør
#' @param preprosess Skal data preprosesseres, dvs. gjøre standard omregning av variable og beregne nye.
#'						TRUE (standard) / FALSE
#' @param tittel Om tittel skal vises i figuren eller ikke. Tittel tas bort i samlerapporter.
#'					0: ikke vis tittel, 1: vis tittel (standard)
#' @param datoFra Operasjonsdato, fra og med. Standard: '2012-01-01'
#' @param datoTil Operasjonsdato, til og med. Standard: '3000-01-01' (siste registreringsdato)
#' @param minald Alder, fra og med
#' @param maxald Alder, til og med
#' @param erMann Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param enhetsUtvalg Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#' @param valgtVar Variabelen det skal vises resultat for.
#'             Alder: Aldersfordeling
#'             AntallNivaaOpr: Antall nivå operert
#'             Antibiotika: Er det gitt antibiotikaprofylakse?
#'             Arbeidstaus12mnd: Arbeidsstatus 12 mnd. etter operasjon
#'             Arbeidstaus3mnd: Arbeidsstatus 3 mnd. etter operasjon
#'             ArbeidstausPreOp: Arbeidsstatus før operasjon
#'             ASAgrad: ASA-grad
#'             BMI: Pasientenes BMI (Body Mass Index)
#'             EqAngstPreOp: Helsetilstand, Angst
#'             ErstatningPreOp: Søkt erstatning?
#'             FornoydBeh12mnd: Fornøydhet med behandlinga på sykehuset, 12 mnd
#'             FornoydBeh3mnd: Fornøydhet med behandlinga på sykehuset, 3 mnd
#'             OperasjonsKategori: Hastegrad
#'             LiggeDognPostop: Antall liggedøgn postoperativt
#'             LiggeDognTotalt: Totalt antall liggedøgn
#'             Morsmal: Morsmål
#'             NytteOpr12mnd: Nytte av operasjon, 12 mnd
#'             NytteOpr3mnd: Nytte av operasjon, 3 mnd
#'             OprIndikPareseGrad: Paresegrad før operasjon
#'             Roker: Røyker pasienten?
#'             Saardren: Har pasienten fått sårdren?
#'             SivilStatus: Sivilstatus
#'             SmertestillBrukPreOp: Hyppighet av smertestillende før operasjonen
#'             Snuser: Snuser pasienten?
#'             SymptVarighetArmer: Varighet av utstrålende armsmerter
#'             SymptVarighetNakkeHode: Varighet av nakke-/hodesmerter
#'             TidlOpr: Er pasienten tidligere operert?
#'             TidlOprAntall: Antall tidligere operasjoner
#'             UforetrygdPreOp: Søkt uføretrygd?
#'             Utdanning: Utdanningsnivå
#'
#' Detajer...:
#'
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export

FigAndeler  <- function(RegData, valgtVar, datoFra='2012-01-01', datoTil='3000-12-31',
		minald=0, maxald=130, erMann='', myelopati=99, fremBak=0, tittel=1, outfile='',
		hentData=0, preprosess=TRUE, reshID, enhetsUtvalg=1)
{

	if (hentData == 1) {
		RegData <- NakkeRegDataSQL(datoFra=datoFra, datoTil=datoTil)
	  }

# Preprosessere data
     if (preprosess){
       RegData <- NakkePreprosess(RegData=RegData)
     }


#----------- Figurparametre ------------------------------

retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1
NB <- ''

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
	}

if (valgtVar=='Alder') {
	gr <- c(0,seq(20,90,10),150)
	RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')	#c(levels(RegData$VariabelGr)[-length(gr)], '90+')	#c(names(AndelLand)[-length(gr)], '90+')
	subtxt <- 'Aldersgruppe'
	TittelUt <- 'Aldersfordeling'
}

if (valgtVar=='AntallNivaaOpr') {
	gr <- c(0:5,1000)
	RegData$VariabelGr <- cut(RegData$AntallNivaaOpr, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(0:4,'5+')	#sort(unique(RegData$AntNivOpr))
	subtxt <- 'Antall'
	TittelUt <- 'Antall nivå operert'
}

if (valgtVar == 'Antibiotika') {
	grtxt <- c('Nei', 'Ja', 'Ukjent')	#Ukjent= Ikke utfylt og evt. manglende
	RegData$VariabelGr <- 9
	indDum <- RegData$Antibiotika %in% 0:1
	RegData$VariabelGr[indDum] <- RegData$Antibiotika[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
	TittelUt <- 'Er det gitt antibiotikaprofylakse?'
	}

if (valgtVar %in% c('ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd')) {
  retn <- 'H'
  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
  grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
		'Delvis sykemeldt', 'Attføring/rehab.', 'Uførepensjon', 'Ufør og sykem.', 'Ikke utfylt')
	RegData$VariabelGr <- 99
	indDum <- which(RegData[ ,valgtVar] %in% 1:10)
	RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:10,99))
	TittelUt <- switch(valgtVar,
	    ArbeidstausPreOp = 'Arbeidsstatus før operasjon',
	    Arbeidstaus3mnd = 'Arbeidsstatus 3 mnd. etter operasjon' ,
	    Arbeidstaus12mnd = 'Arbeidsstatus 12 mnd. etter operasjon')
}

if (valgtVar == 'ASAgrad') {
	grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'V:Døende', 'Ukjent')
	subtxt <- 'Sykdomsgrad'
	RegData$VariabelGr <- 99
	indDum <- which(RegData[, valgtVar] %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,99))
	TittelUt <-  'ASA-grad'
	}

if (valgtVar=='BMI') {
	gr <- c(-1, 0, 18.5, 25, 30, 1000)
	RegData$VariabelGr <- -1
	ind <- which(RegData$BMI>0)
#	RegData$VariabelGr[ind] <- cut(RegData[ind ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
	RegData$VariabelGr <- cut(RegData[,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c('', '<18,5', levels(RegData$VariabelGr)[3:(length(gr)-2)],'30+')
	grtxt2 <- c('Ukjent', 'Undervekt', 'Normalvekt', 'Overvekt', 'Fedme')
	subtxt <- "Body Mass Index"
	TittelUt <-  'Pasientenes BMI (Body Mass Index)'
}

if (valgtVar == 'EqAngstPreOp') {
	RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
	grtxt <- c('Ingen', 'Litt', 'Svært', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(as.numeric(RegData$EqAngstPreOp) %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$EqAngstPreOp[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
	subtxt <- 'Grad av engstelighet/deprimerthet'	#Tilstand i forhold til angst'
	TittelUt <-  'Helsetilstand: Angst'
}
if (valgtVar == 'ErstatningPreOp') {
  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
  grtxt <- c('Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$ErstatningPreOp %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$ErstatningPreOp[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
	TittelUt <- 'Søkt erstatning?'
}

if (valgtVar %in% c('FornoydBeh3mnd','FornoydBeh12mnd')) {
	#datoTil <- min(datoTil, as.character(Sys.Date()-90))
	RegData <- switch(valgtVar,
	    FornoydBeh3mnd = RegData[which(RegData$OppFolgStatus3mnd==1), ],
	    FornoydBeh12mnd = RegData[which(RegData$OppFolgStatus12mnd==1), ])
	retn <- 'H'
	grtxt <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData[ ,valgtVar] %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
	TittelUt <- switch(valgtVar,
	         FornoydBeh3mnd = 'Fornøydhet med behandlinga på sykehuset, 3 mnd' ,
	         FornoydBeh12mnd = 'Fornøydhet med behandlinga på sykehuset, 12 mnd')
	}

if (valgtVar=='LiggeDognPostop') {
	#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
	#dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
	#RegData$Liggedogn[dagind]<-0
	gr <- c(0:5,100)
	RegData$VariabelGr <- cut(RegData$LiggeDognPostop, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(0:4, '5+')
	subtxt <- 'Antall liggedøgn'
	TittelUt <- 'Antall liggedøgn postoperativt'
}
if (valgtVar=='LiggeDognTotalt') {
  #For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
  #dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
  #RegData$Liggedogn[dagind]<-0
  gr <- c(0:7,100)
  RegData$VariabelGr <- cut(RegData$LiggeDognTotalt, breaks=gr, include.lowest=TRUE, right=FALSE)
  grtxt <- c(0:6, '7+')
  subtxt <- 'Antall liggedøgn'
  TittelUt <- 'Totalt antall liggedøgn'
}

if (valgtVar == 'Morsmal') {
  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
  grtxt <- c('Norsk', 'Samisk', 'Annet', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Morsmal %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$Morsmal[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
	TittelUt <- 'Morsmål'
}
if (valgtVar %in% c('NytteOpr3mnd', 'NytteOpr12mnd')) {
	RegData <- switch(valgtVar,
		NytteOpr3mnd = RegData[which(RegData$OppFolgStatus3mnd==1), ],
		NytteOpr12mnd = RegData[which(RegData$OppFolgStatus12mnd==1), ])
	retn <- 'H'
	RegData$VariabelGr <- 9
	#grtxt <- c('Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
	#				'Verre enn noen gang', 'Ukjent')
	grtxt <- c('Klart bedre', 'Små endringer', 'Klart verre', 'Ukjent')
	indDum <- which(RegData[ , valgtVar] %in% 1:7)
	RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
	#RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:7,9))
	oldvalues <- c(1:7,9)
	newvalues <- c(1,1,2,2,2,3,3,4)
	                    #levels=c('Klart bedre','Klart bedre', 'Små endringer', 'Små endringer', 'Små endringer',
	                     # 'Klart verre', 'Klart verre', 'Ukjent'))  # Make this a factor
	RegData$VariabelGr <- factor(newvalues[ match(RegData$VariabelGr, oldvalues) ], levels=1:4)
	#RegData$VariabelGr <- factor(RegData$VariabelGr, levels=1:4)
	TittelUt <- switch(valgtVar,
		NytteOpr3mnd = 'Nytte av operasjon, 3 mnd',
		NytteOpr12mnd = 'Nytte av operasjon, 12 mnd')
}

if (valgtVar == 'OprIndikPareseGrad') {
  grtxt <- c(0:5, 'Ukjent')
  RegData <- RegData[which(RegData$OprIndikParese == 1), ]
  indDum <- which(RegData$OprIndikPareseGrad %in% 0:5)
	RegData$VariabelGr <- 9
	RegData$VariabelGr[indDum] <- RegData$OprIndikPareseGrad[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:5,9))
	TittelUt <- 'Paresegrad'
	}

if (valgtVar == 'OperasjonsKategori') {
	retn <- 'H'
	grtxt <- c('Elektiv', 'Øhjelp', 'Subakutt', 'Ukjent')
	indDum <- which(RegData$OperasjonsKategori %in% 1:3)
	RegData$VariabelGr <- 9
	RegData$VariabelGr[indDum] <- RegData$OperasjonsKategori[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
	TittelUt <- 'Hastegrad'
	}
if (valgtVar == 'Roker') {
	grtxt <- c('Nei', 'Ja', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- RegData$Roker %in% 0:1
	RegData$VariabelGr[indDum] <- RegData$Roker[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
	TittelUt <- 'Røyker pasienten?'
}
if (valgtVar == 'Saardren') {
	grtxt <- c('Nei', 'Ja', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Saardren %in% 0:1)
	RegData$VariabelGr[indDum] <- RegData$Saardren[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:1,9))
	TittelUt <- 'Har pasienten fått sårdren?'
}
if (valgtVar == 'SivilStatus') {
	grtxt <- c('Gift', 'Samboer', 'Enslig', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$SivilStatus %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$SivilStatus[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
	TittelUt <- 'Sivilstatus'
}
if (valgtVar == 'SmertestillBrukPreOp') {
 # 1 - Sjeldnere enn hver uke, 2 - Hver uke, 3 - Daglig, 4 - Flere ganger daglig, 9 - Ikke utfylt
	grtxt <- c('Aldri', '< Ukentlig', 'Ukentlig', 'Daglig', '> Daglig', 'Ukjent')
	RegData$VariabelGr <- 9
	RegData$VariabelGr[which(RegData$SmertestillPreOp == 0)] <- 0
	indDum <- which(RegData$SmertestillBrukPreOp %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$SmertestillBrukPreOp[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:4,9))
	TittelUt <- 'Hyppighet av smertestillende før operasjonen'
	}
if (valgtVar == 'Snuser') {
  grtxt <- c('Nei', 'Ja', 'Ukjent')
  RegData$VariabelGr <- 9
  indDum <- RegData$Snuser %in% 0:1
  RegData$VariabelGr[indDum] <- RegData$Snuser[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
	TittelUt <- 'Snuser pasienten?'
}
if (valgtVar %in% c('SymptVarighetArmer','SymptVarighetNakkeHode')) {
	grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData[,valgtVar] %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData[indDum,valgtVar]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
	TittelUt <- switch(valgtVar,
		SymptVarighetArmer = 'Varighet av utstrålende armsmerter',
        SymptVarighetNakkeHode = 'Varighet av nakke-/hodesmerter')
}

if (valgtVar == 'TidlOpr') {
	retn <- 'H'
	grtxt <- c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$TidlOpr %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$TidlOpr[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
	TittelUt <- 'Er pasienten tidligere operert?'
	}
if (valgtVar=='TidlOprAntall') {
	gr <- c(0:3, 100,1000)
	RegData$Variabel <- 999
	indDum <- which(RegData$TidlOprAntall>=0)
	RegData$Variabel[indDum] <- RegData$TidlOprAntall[indDum]
	RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(0:2,paste0('3-', max(RegData$TidlOprAntall, na.rm=T)), 'Ukjent')
	TittelUt <- 'Antall tidligere operasjoner'
}
if (valgtVar == 'UforetrygdPreOp') {
	retn <- 'H'
	grtxt <- c('Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$UforetrygdPreOp %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$UforetrygdPreOp[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
	TittelUt <- 'Søkt uføretrygd før operasjon?'
	}
if (valgtVar=='Utdanning') {
	retn <- 'H'
	grtxt <- c('Grunnskole++, 7-10år','Real-, yrkes- el vg skole',
				 'Allmennfaglig vg skole','Høyskole/universitet, <4 år','Høyskole/universitet, 4år+', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Utdanning %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$Utdanning[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
	TittelUt <- 'Utdanningsnivå'
}


#------------Gjøre utvalg-------------------------
NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann)
RegData <- NakkeUtvalg$RegData
utvalgTxt <- NakkeUtvalg$utvalgTxt



#Generere hovedgruppe og sammenlikningsgruppe
#Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$SykehusNavn[indEgen1]) } else {
		shtxt <- 'Hele landet'
			}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }


#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0

if (flerevar == 0 ) {
AntHoved <- table(RegData$VariabelGr[indHoved])
NHoved <- sum(AntHoved)
Andeler$Hoved <- 100*AntHoved/NHoved
	if (medSml==1) {
		AntRest <- table(RegData$VariabelGr[indRest])
		NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
		Andeler$Rest <- 100*AntRest/NRest
	}
}


#FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
if (valgtVar %in% c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
                    'OprIndikMyelopati', 'Radiologi')){
  flerevar <-  1
  utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
  RegDataLand <- RegData
  NHoved <-length(indHoved)
  NRest <- length(indRest)

  for (teller in 1:(medSml+1)) {
  #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
    RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]


     if (valgtVar=='OprIndik') {
         retn <- 'H'
         #OprIndiasjonasjonUfylt <>1 - tom variabel,
         #Svært få (ca 20 av 3000) har tom registrering. Setter derfor felles N lik alle reg.

         #indAnnet <- which(RegData$OprIndikAnnet == 1)
         #indPareser <- which(RegData$OprIndikParese == 1)
         indSmerterk <- which(RegData$OprIndikSmerter == 1)
         indMyelopati <- which(RegData$OprIndikMyelopati == 1)
         Nmyelopati <- sum(RegData$OprIndikMyelopati, na.rm=T)
         AntVar <- cbind(
              #length(indAnnet),
              Pareser = sum(RegData$OprIndikParese, na.rm=T), #length(indPareser),
              Myelopati = length(indMyelopati),
              Smerter = length(indSmerterk),
              SmerterMyelop = length(intersect(indMyelopati, indSmerterk)),
              Annet = sum(RegData$OprIndikAnnet, na.rm=T)
         )
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Pareser', 'Myelopati', 'Smerter', 'Sm. og Myelop.', 'Annet')
         TittelUt <- 'Operasjonsårsak'
    }

    if (valgtVar=='Radiologi') {
         retn <- 'H'
         #RadilogiUnderokelseUfylt  - tom variabel,
         #RadiologiRtgCcolFunkOpptak  - tom variabel,
         #Svært få har tom registrering. Setter derfor felles N lik alle reg.

         AntVar <- cbind(
              #length(indAnnet),
              CT = sum(RegData$RadiologiCt, na.rm=T), #length(indPareser),
              MR = sum(RegData$RadiologiMr, na.rm=T),
              Myelografi = sum(RegData$RadiologiMyelografi, na.rm=T),
              RontgenCcol = sum(RegData$RadiologiRtgCcol, na.rm=T)
         )
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('CT', 'MR', 'Myelografi', 'Røntgen-Ccol')
         TittelUt <- 'Radiologisk undersøkelse'
    }

    if (valgtVar=='Komorbiditet') {
         retn <- 'H'
          RegData <- RegData[which(RegData$AndreRelSykdommer>-1), ]
         RegData$SykdReumatisk <- 0
          indSykdReumatisk <- (RegData$SykdAnnenreumatisk ==1 | (RegData$SykdBechtrew==1 | RegData$SykdReumatoidartritt==1))
          RegData$SykdReumatisk[indSykdReumatisk] <- 1
         Variable <- c('SykdAnnenendokrin', 'SykdAnnet','SykdCarpalTunnelSyndr', 'SykdCerebrovaskular',
               'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHodepine', 'SykdHypertensjon', 'SykDiabetesMellitus',
              'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk', 'SykdKrSmerterMuskelSkjelSyst',
             'SykdOsteoporose', 'SykdSkulderImpigment', 'SykdWhiplashNakke')
         AntVar <- colSums (RegData[ ,c("SykdReumatisk", Variable, "AndreRelSykdommer")], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Annen Reumatisk', 'Annen endokrin', 'Andre', 'Carpal TS', 'Cerebrovaskulær', 'Depresjon/Angst',
         'Hjerte-/Karsykd.', 'Hodepine', 'Hypertensjon', 'Diabetes', 'Kreft', 'Kr. lungesykdom',
         'Kr. nevrologisk', 'Kr. muskel/skjelettsm.', 'Osteoporose', 'Skuldersyndrom', 'Whiplash/skade', 'Tot. komorb')

         TittelUt <- 'Komorbiditet'
    }

    if (valgtVar=='KomplOpr') {
         retn <- 'H'
         Variable <- c('PerOpKomplAnafylaksiI','PerOpKomplAnnet','PerOpKomplBlodning','PerOpKomplDurarift',
                       'PerOpKomplFeilplasseringImplant','PerOpKomplKardioVaskulare','PerOpKomplMedullaskade',
                      'PerOpKomplNerverotSkade','PerOpKomplAnnenNerveskade','PerOpKomplOpFeilNivaa',
                       'PerOpKomplRespiratorisk','PerOpKomplOsofagusSkade','PerOpEnhverKompl')

         AntVar <- colSums (RegData[ ,Variable], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Anafylaksi','Annet','Blødning','Durarift','Feilplassering, impl.','Kardiovaskulære','Medullaskade',
                    'Nerverotskade','Nerveskade','Op. feil nivå','Respiratorisk','Øsofagusskade','Komplikasjoner, alle')
         TittelUt <- 'Komplikasjoner ved operasjon'
    }

    if (valgtVar=='Kompl3mnd') {
         retn <- 'H'
         RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1), ]
         Variable <- c('KomplDVT3mnd', 'KomplinfekDyp3mnd', 'KomplLungeEmboli3mnd', 'KomplinfekOverfl3mnd',
                       'KomplPneumoni3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'KomplUVI3mnd', 'EnhverKompl3mnd')
         AntVar <- colSums (RegData[ ,Variable], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('DVT', 'Dyp infeksjon', 'Lungeemboli', 'Overfladisk infeksjon', 'Pneumoni',
                    'Stemmevansker', 'Svelgvansker', 'UVI', 'Totalt, 3 mnd.')
         TittelUt <- 'Komplikasjoner etter operasjon'
    }


  if (valgtVar=='OprIndikSmerter') {
  	retn <- 'H'
  	indSmerteArm <- which(RegData$OprIndikSmerteLokArm == 1)
  	indSmerteNakke <- which(RegData$OprIndikSmerteLokNakke == 1)
  	Nsmerte <- sum(RegData$OprIndikSmerter, na.rm=T)
  	AntVar <- cbind(
  		Smerte = Nsmerte,
  		SmerteArm = length(indSmerteArm),
  		SmerteNakke = length(indSmerteNakke),
  		SmerteArmNakke = length(intersect(indSmerteArm, indSmerteNakke))
  	)
  	NVar<- cbind(
  		Smerte = length(which(RegData$OprIndikSmerter > -1)),
  		SmerteArm = Nsmerte,
  		SmerteNakke = Nsmerte,
  		SmerteArmNakke = Nsmerte
  	)
  	grtxt <- c('Smerter', '...Arm', '...Nakke', '...Arm og Nakke')
  	TittelUt <- 'Operasjonsårsak: Smerter'
  		}

    if (valgtVar=='OprIndikMyelopati') {
         retn <- 'H'
         indMotorisk <- which(RegData$OprIndikMyelopatiMotorisk == 1)
         indSensorisk <- which(RegData$OprIndikMyelopatiSensorisk == 1)
         Nmyelopati <- sum(RegData$OprIndikMyelopati, na.rm=T)
         AntVar <- cbind(
              Myelopati = Nmyelopati,
              Motorisk = length(indMotorisk),
              Sensorisk = length(indSensorisk),
              MotorSensor = length(intersect(indMotorisk, indSensorisk))
         )
         NVar<- cbind(
              Myelopati = length(which(RegData$OprIndikMyelopatiMotorisk > -1)),
              Motorisk = Nmyelopati,
              Sensorisk = Nmyelopati,
              MotorSensor = Nmyelopati
         )
         grtxt <- c('Myelopati', '...Sensorisk', '...Motorisk', '...Begge deler')
         TittelUt <- 'Operasjonsårsak: Myelopati'
    }



#Generelt for alle figurer med sammensatte variable:
  	if (teller == 1) {
  		AntHoved <- AntVar
  		NHoved <- max(NVar, na.rm=T)
  		Andeler$Hoved <- 100*AntVar/NVar
  	}
  	if (teller == 2) {
  		AntRest <- AntVar
  		NRest <- max(NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
  		Andeler$Rest <- 100*AntVar/NVar
  	}
  } #end medSml (med sammenligning)
}	#end sjekk om figuren inneholder flere variable



#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
#NB: DENNE MÅTEN Å BEREGNE PÅ KAN EVT. BARE BENYTTES NÅR VARIABLENE SKAL FORHOLDE SEG TIL SAMME N (nevner)
#Andeler <- list(Hoved = 0, Rest =0)
#NRest <- 0
#AntRest <- 0
#AntHoved <- switch(as.character(flerevar),
#				'0' = table(RegData$VariabelGr[indHoved]),
#				'1' = colSums(sapply(RegData[indHoved ,variable], as.numeric), na.rm=T))
#NHoved <- switch(as.character(flerevar),
#				'0' = sum(AntHoved),	#length(indHoved)- Kan inneholde NA
#				'1' = length(indHoved))
#Andeler$Hoved <- 100*AntHoved/NHoved

#if (medSml==1) {
#	AntRest <- switch(as.character(flerevar),
#					'0' = table(RegData$VariabelGr[indRest]),
#					'1' = colSums(sapply(RegData[indRest ,variable], as.numeric), na.rm=T))
#	NRest <- switch(as.character(flerevar),
#					'0' = sum(AntRest),	#length(indRest)- Kan inneholde NA
#					'1' = length(indRest))
#	Andeler$Rest <- 100*AntRest/NRest
#}


if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}

#SKILLE UT FIGURDELEN SOM EGEN FUNKSJON???????
#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ( NHoved %in% 1:5 | 	(medSml ==1 & NRest<10)) {	#(valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) |
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(Tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
	if ( outfile != '') {dev.off()}

} else {

#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
cexgr <- 1	#Kan endres for enkeltvariable


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste('%.', antDes, 'f', sep='')
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
	if (NHoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
						paste(smltxt, ' (N=', NRest,')', sep='')),
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {
		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (length(grtxt2) == 1) {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
	ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
		xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')),
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {
	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
}

if (tittel==1) {title(Tittel, line=1, font.main=1)}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3-(1-tittel)+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
}

#Beregninger som returneres fra funksjonen.
AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
rownames(AndelerUt) <- c('Hoved', 'Rest')
AntallUt <- rbind(AntHoved, AntRest)
rownames(AntallUt) <- c('Hoved', 'Rest')

UtData <- list(paste(toString(Tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
