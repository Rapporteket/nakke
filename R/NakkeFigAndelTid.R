#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Funksjon som genererer en figur med andeler av en variabel for hvert år.
#'
#' Detaljer
#'
#' @inheritParams FigAndeler
#' @param valgtVar Variabelen det skal vises resultat for.
#'             Alder: Aldersfordeling
#'             AndreRelSykdommer: Andre sykdommer
#'             Antibiotika: Fått antibiotika
#'             Arbeidstaus12mnd: Mottar sykepenger, 12 mnd etter operasjon?
#'             Arbeidstaus3mnd: Mottar sykepenger, 3 mnd etter operasjon?
#'             ArbeidstausPreOp: Mottar sykepenger, preoperativt?
#'             ASAgrad: ASA-grad > II
#'             BMI: Pasienter med fedme
#'             EnhverKompl3mnd: Alle komplikasjoner
#'             ErstatningPreOp: Søkt/planlegger å søke erstatning
#'             FornoydBeh12mnd: Fornøyde pasienter, 12 mnd.
#'             FornoydBeh3mnd: Fornøyde pasienter, 3 mnd.
#'             KomplinfekDyp3mnd: Pasientrapportert dyp infeksjon, 3 mnd.
#'             KomplinfekOverfl3mnd: Overfladisk infeksjon, 3 mnd.
#'             KomplStemme3mnd: Stemmevansker, 3 mnd.
#'             KomplSvelging3mnd: Svelgvansker, 3 mnd.
#'             Misfor12mnd: Misfornøyde pasienter, 12 mnd.
#'             Misfor3mnd: Misfornøyde pasienter, 3 mnd.
#'             NytteOpr12mnd: Klart bedre, 12 mnd.
#'             NytteOpr3mnd: Klart bedre, 3 mnd.
#'             OprIndikMyelopati: Operasjonsårsak, Myelopati
#'             OprIndikSmerter: Operasjonsårsak, Smerter
#'             PerOpEnhverKompl: Komplikasjoner ved operasjon
#'             Roker: Røykere
#'             Saardren: Andel som får sårdren
#'             SmertestillPreOp: Bruker smertestillende, preop.
#'             SymptVarighetNakkeHode: Varighet av hode-/nakkesmerter over 1 år
#'             SymptVarighetArmer: Varighet av armsmerter, minst 1 år  #' SymptVarighetSmerterUker
#'             UforetrygdPreOp: Søkt eller planlegger å søke uføretrygd?
#'             Utdanning: Andel høyskole-/universitetsutdannede
#'             Verre12mnd: Klart verre, 12 mnd.
#'             Verre3mnd. Klart verre, 3 mnd.
#'
#' @return Figur med ...
#'
#' @export


FigAndelTid <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
                        minald=0, maxald=130, erMann='', myelopati=99, fremBak=0, tittel=1,
                        reshID=0, outfile='', enhetsUtvalg=0, preprosess=TRUE, hentData=0) {


     if (hentData == 1) {
          RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
     }

     # Preprosessere data
     if (preprosess){
          RegData <- NakkePreprosess(RegData=RegData)
     }


#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
	}

'%i%' <- intersect
RegData$Variabel <- 0


if (valgtVar=='Alder') {
	#Pasienter over 70 år
  	RegData$Variabel[which(RegData$Alder >= 70)] <- 1
  	VarTxt <- 'pasienter >=70år'
	TittelUt <- 'Andel pasienter over 70 år'
}
if (valgtVar=='AndreRelSykdommer') {
	#Tar med blanke som 0. (Hver sykdom får også verdien 0 når denne er tom.)
  	RegData$Variabel[which(RegData[ ,valgtVar] == 1)] <- 1
  	VarTxt <- 'med andre sykdommer'
	TittelUt <- 'Andre sykdommer'
}

if (valgtVar=='Antibiotika') {
	#Andel av de som har fått antibiotika
	RegData <- RegData[which(RegData$Antibiotika %in% 0:1),]
  	RegData$Variabel <-RegData$Antibiotika
  	VarTxt <- 'som har fått antibiotika'
	TittelUt <- 'Antibiotika'
}


if (valgtVar %in% c('ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd')) {
	# Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
#  #grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
#		'Delvis sykemeldt', 'Attføring/rehab.', 'Uførepensjon', 'Ufør og sykem.', 'Ikke utfylt')
	indSkjema <- switch(valgtVar,
	    ArbeidstausPreOp = which(RegData$PasientSkjemaStatus == 1),
	    Arbeidstaus3mnd = which(RegData$OppFolgStatus3mnd == 1),
	    Arbeidstaus12mnd = which(RegData$OppFolgStatus12mnd == 1))
	indDum <- which(RegData[ ,valgtVar] %in% 1:9)
	RegData <- RegData[intersect(indDum, indSkjema), ]
	TittelUt <- switch(valgtVar,
	    ArbeidstausPreOp = 'Mottar sykepenger, preoperativt?',
	    Arbeidstaus3mnd = 'Mottar sykepenger, 3 mnd etter operasjon?' ,
	    Arbeidstaus12mnd = 'Mottar sykepenger, 12 mnd etter operasjon?')
  	RegData$Variabel[which(RegData[ ,valgtVar] %in% 6:9)] <- 1
  	VarTxt <- 'som mottar sykepenger'
}


if (valgtVar=='ASAgrad') {
	#Legeskjema. Andel av de som har ASA-grad 3-5
	RegData <- RegData[which(RegData$ASAgrad %in% 1:5),]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 3:5)] <- 1
  	VarTxt <- 'med ASA>II'
	TittelUt <- 'Andel pasienter med ASA-grad III-V'
}

if (valgtVar=='BMI') {
	#Pasientskjema. Andel med BMI>30
	RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1), which(RegData$BMI > 0)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] >30)] <- 1
  	VarTxt <- 'med BMI>30'
	TittelUt <- 'Andel pasienter med fedme'
}

if (valgtVar=='EnhverKompl3mnd') {
     #Pasientskjema. Alle komplikasjoner, 3mnd.
     RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1), ]
     RegData$Variabel[which(RegData[ ,valgtVar] ==1 )] <- 1
     VarTxt <- 'komplikasjoner'
     TittelUt <- 'Komplikasjoner (totalt) 3 mnd. etter operasjon'
}

if (valgtVar=='ErstatningPreOp') {
	#Pasientskjema. Andel med ErstatningPreOp 1 el 3
	#Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
	RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1), which(RegData$ErstatningPreOp %in% 1:4)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
  	VarTxt <- 'søkt erstatning'
	TittelUt <- 'Har søkt/planlegger å søke erstatning før operasjon'
}
if (valgtVar %in% c('FornoydBeh3mnd','FornoydBeh12mnd')) {
	#3/12mndSkjema. Andel med Fornøyd/litt fornøyd (1,2)
	#Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
	indSkjema <- switch(valgtVar,
	    FornoydBeh3mnd = intersect(which(RegData$FornoydBeh3mnd %in% 1:5),which(RegData$OppFolgStatus3mnd==1)),
	    FornoydBeh12mnd = intersect(which(RegData$FornoydBeh12mnd %in% 1:5),which(RegData$OppFolgStatus12mnd==1)))
	RegData <- RegData[indSkjema, ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
	VarTxt <- 'fornøyde'
	TittelUt <- switch(valgtVar,
	         FornoydBeh3mnd = 'Fornøyde pasienter, 3 mnd' ,
	         FornoydBeh12mnd = 'Fornøyde pasienter, 12 mnd')
}

if (valgtVar %in% c('Misfor3mnd','Misfor12mnd')) {
	#3/12mndSkjema. Andel med Fornøyd/litt fornøyd (1,2)
	#Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
	indSkjema <- switch(valgtVar,
	    Misfor3mnd = intersect(which(RegData$FornoydBeh3mnd %in% 1:5),which(RegData$OppFolgStatus3mnd==1)),
	    Misfor12mnd = intersect(which(RegData$FornoydBeh12mnd %in% 1:5),which(RegData$OppFolgStatus12mnd==1)))
	RegData <- RegData[indSkjema, ]
	indVar <- switch(valgtVar,
	    Misfor3mnd = which(RegData$FornoydBeh3mnd %in% 4:5),
	    Misfor12mnd = which(RegData$FornoydBeh12mnd %in% 4:5))
	RegData$Variabel[indVar] <- 1
	VarTxt <- 'fornøyde'
	TittelUt <- switch(valgtVar,
	         Misfor3mnd = 'Misfornøyde pasienter, 3 mnd' ,
	         Misfor12mnd = 'Misfornøyde pasienter, 12 mnd')
}

if (valgtVar=='KomplinfekDyp3mnd') {
	#3MndSkjema. Andel med KomplinfekDyp3mnd=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekDyp3mnd %in% 0:1)), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'dype infeksjoner'
	TittelUt <- 'Pasientrapportert dyp infeksjon, 3 mnd.'
}
if (valgtVar=='KomplinfekOverfl3mnd') {
	#3MndSkjema. Andel med KomplinfekOverfl3mnd=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'overfladiske infeksjoner'
	TittelUt <- 'Overfladisk infeksjon, 3 mnd.'
}
if (valgtVar=='Komplinfek') {
  #3MndSkjema. Andel med KomplinfekDyp3mnd=1
  #Kode 0,1: Nei, Ja +tomme
  ind <- which(RegData$OppFolgStatus3mnd == 1) %i%
    union(which(RegData$KomplinfekDyp3mnd %in% 0:1), which(RegData$KomplinfekOverfl3mnd %in% 0:1))
  RegData <- RegData[ind, ]
  RegData$Variabel[union(which(RegData$KomplinfekDyp3mnd==1), which(RegData$KomplinfekOverfl3mnd==1))] <- 1
  VarTxt <- 'infeksjoner'
  TittelUt <- 'Pasientrapportert dyp eller overfladisk infeksjon, 3 mnd.'
}

if (valgtVar=='KomplStemme3mnd') {
	#3MndSkjema. Andel med KomplStemme3mnd=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplStemme3mnd %in% 0:1)), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'med stemmevansker'
	TittelUt <- 'Stemmevansker, 3 mnd.'
}

if (valgtVar=='KomplSvelging3mnd') {
	#3MndSkjema. Andel med KomplSvelging3mnd=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplSvelging3mnd %in% 0:1)), ]
	RegData$Variabel <- RegData[ ,valgtVar]
  	VarTxt <- 'med svelgvansker'
	TittelUt <- 'Svelgvansker, 3 mnd.'
}

	#Tall ved punktene angir antall "VarTxt"

if (valgtVar %in% c('NytteOpr3mnd', 'NytteOpr12mnd')) {
	#3/12mndSkjema. Andel med helt bra/mye bedre (1:2)
	#Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
	#				'Verre enn noen gang', 'Ukjent')
	indSkjema <- switch(valgtVar,
	    NytteOpr3mnd = intersect(which(RegData$NytteOpr3mnd %in% 1:7),which(RegData$OppFolgStatus3mnd==1)),
	    NytteOpr12mnd = intersect(which(RegData$NytteOpr12mnd %in% 1:7),which(RegData$OppFolgStatus12mnd==1)))
	RegData <- RegData[indSkjema, ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
	VarTxt <- '"mye bedre" og "helt bra"'
	TittelUt <- switch(valgtVar,
	         NytteOpr3mnd = 'Klart bedre, 3 mnd' ,
	         NytteOpr12mnd = 'Klart bedre, 12 mnd')
}

if (valgtVar %in% c('Verre3mnd','Verre12mnd')) {
	#3/12mndSkjema. Andel med helt mye verre og noen sinne (6:7)
	#Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
	#				'Verre enn noen gang', 'Ukjent')
	indSkjema <- switch(valgtVar,
	    Verre3mnd = intersect(which(RegData$NytteOpr3mnd %in% 1:7),which(RegData$OppFolgStatus3mnd==1)),
	    Verre12mnd = intersect(which(RegData$NytteOpr12mnd %in% 1:7),which(RegData$OppFolgStatus12mnd==1)))
	RegData <- RegData[indSkjema, ]
	indVar <- switch(valgtVar,
	    Verre3mnd = which(RegData$NytteOpr3mnd %in% 6:7),
	    Verre12mnd = which(RegData$NytteOpr12mnd %in% 6:7))
	RegData$Variabel[indVar] <- 1
	VarTxt <- 'med klar forverring'
	TittelUt <- switch(valgtVar,
                   Verre3mnd = 'Mye verre/verre enn noen gang, 3 mnd.' ,
                   Verre12mnd = 'Mye verre/verre enn noen gang, 12 mnd.')
}

if (valgtVar=='OprIndikMyelopati') {
	#LegeSkjema. Andel med OprIndikMyelopati=1
	#Kode 0,1: Nei, Ja +tomme
	#RegData <- RegData[which(RegData$OprIndikMyelopati %in% 0:1), ]
	#RegData$Variabel <- RegData$OprIndikMyelopati
    #Antar tomme = nei. (Tore 3.okt. 2016)
    RegData$Variabel[which(RegData$OprIndikMyelopati == 1)] <- 1
 	VarTxt <- 'med myelopati'
	TittelUt <- 'Operasjonsårsak: Myelopati'
}
if (valgtVar=='OprIndikSmerter') {
	#LegeSkjema. Andel med OprIndikSmerter=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[which(RegData$OprIndikSmerter %in% 0:1), ]
	RegData$Variabel <- RegData$OprIndikSmerter
  	VarTxt <- 'med smerter'
	TittelUt <- 'Operasjonsårsak: Smerter'
}

if (valgtVar=='PerOpEnhverKompl') {
	#LegeSkjema. Andel med PerOpEnhverKompl=1
	#Kode 0,1: Nei, Ja +tomme
	RegData <- RegData[which(RegData$PerOpEnhverKompl %in% 0:1), ]
	RegData$Variabel <- RegData$PerOpEnhverKompl
  	VarTxt <- 'med komplikasjoner'
	TittelUt <- 'Komplikasjoner (alle) ved operasjon'
}

if (valgtVar=='Roker') {
	#PasientSkjema. Andel med Roker=1
	#Kode 0,1,9: Nei, Ja Ukjent
	RegData <- RegData[intersect(which(RegData$Roker %in% 0:1), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel <- RegData$Roker
  	VarTxt <- 'røykere'
	TittelUt <- 'Røykere'
}

if (valgtVar == 'Saardren') {
	#LegeSkjema. Andel med Saardren=1
	#Kode 0,1,9: Nei, Ja Ukjent
	RegData <- RegData[which(RegData$Saardren %in% 0:1), ]
	RegData$Variabel <- RegData$Saardren
  	VarTxt <- 'med sårdren'
	TittelUt <- 'Andel som får sårdren'
}

if (valgtVar == 'SmertestillPreOp') {
	#PasientSkjema. Andel med SmertestillPreOp=1
	#Kode 0,1,9: Nei, Ja Ukjent
	RegData <- RegData[intersect(which(RegData$SmertestillPreOp %in% 0:1), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel <- RegData$SmertestillPreOp
  	VarTxt <- 'på smertestillende'
	TittelUt <- 'Bruker smertestillende før operasjon'
}

if (valgtVar == 'SymptVarighetNakkeHode') {
	#PasientSkjema. Andel med SymptVarighetNakkeHode 4 el 5
	#Kode 1:5,9: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
	RegData <- RegData[intersect(which(RegData$SymptVarighetNakkeHode %in% 1:5), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
  	VarTxt <- 'med varighet >1 år'
	TittelUt <- 'Varighet av hode-/nakkesmerter minst ett år'
}

if (valgtVar == 'SymptVarighetArmer') {
	#PasientSkjema. Andel med SymptVarighetArmer 4 el 5
	#Kode: Antall uker
	RegData <- RegData[intersect(which(RegData$SymptVarighetArmer >-1), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
  	VarTxt <- 'med varighet > 1 år'
	TittelUt <- 'Varighet av armsmerter minst ett år'
}

if (valgtVar == 'UforetrygdPreOp') {
	#PasientSkjema. Andel med UforetrygdPreOp 1 og 3
	#Kode 1:4,9: 'Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
	RegData <- RegData[intersect(which(RegData$UforetrygdPreOp %in% 1:4), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
  	VarTxt <- 'søkt/planlagt å søke'
	TittelUt <- 'Søkt eller planlegger å søke uføretrygd?'
}
if (valgtVar == 'Utdanning') {
	#PasientSkjema. Andel med Utdanning 4 el 5
	#Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
			#Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
	RegData <- RegData[intersect(which(RegData$Utdanning %in% 1:5), which(RegData$PasientSkjemaStatus ==1)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
  	VarTxt <- 'med høyere utdanning'
	TittelUt <- 'Andel høyskole-/universitetsutdannede'
}


#Gjør utvalg
NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                              erMann=erMann, myelopati=myelopati, fremBak=fremBak)
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


NHovedRes <- length(indHoved)
NSmlRes <- length(indRest)


#-------------------------Beregning av andel-----------------------------------------
Aartxt <- min(RegData$Aar):max(RegData$Aar)
RegData$Aar <- factor(RegData$Aar, levels=Aartxt)

NAarRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest], length)
NAarHendRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest],sum, na.rm=T)
AndelRest <- NAarHendRest/NAarRest*100
NAarHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'], length)
NAarHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'],sum, na.rm=T)
AndelHoved <- NAarHendHoved/NAarHoved*100
Andeler <- rbind(AndelRest, AndelHoved)


if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}

#----------FIGUR------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & medSml == 1)) {
if (length(indHoved) < 10 | (medSml ==1 & length(indRest)<10)) {
	#-----------Figur---------------------------------------
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {




#-----------Figur---------------------------------------

#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
farger <- FigTypUt$farger
fargeHoved <- farger[3]
fargeRest <- farger[1]
NutvTxt <- length(utvalgTxt)
hmarg <- 0.04+0.01*NutvTxt
par('fig' = c(0,1,0,1-hmarg))
cexleg <- 1	#Størrelse på legendtekst


ymax <- min(119, 1.25*max(Andeler,na.rm=T))
plot(Aartxt, AndelHoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
		xlim= c(Aartxt[1], max(Aartxt)), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(Aartxt), max(Aartxt),length(Aartxt)-1)
		cex=2, xlab='Innleggelsesår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	#Operasjonsår,

#plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
#		#cex=0.8, cex.lab=0.9, cex.axis=0.9,
#		ylab=c(ytxt,'med 95% konfidensintervall'),
#		xlab='Operasjonsår', xaxt='n',
#		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = Aartxt)

title(Tittel, line=1, font.main=1)

#Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
if ((ymax > 10) & (ymax < 40)) {lines(range(Aartxt),rep(10,2), col=farger[4])}
if (ymax > 20) {lines(range(Aartxt),rep(20,2), col=farger[4])}
if ((ymax > 30) & (ymax < 40)) {lines(range(Aartxt),rep(30,2), col=farger[4])}
if (ymax > 40) {lines(range(Aartxt),rep(40,2), col=farger[4])}
if (ymax > 60) {lines(range(Aartxt),rep(60,2), col=farger[4])}
if (ymax > 80) {lines(range(Aartxt),rep(80,2), col=farger[4])}
if (ymax > 100) {lines(range(Aartxt),rep(100,2), col=farger[4])}
#		axis(2, at=c(0,20,40,60,80,100), pos=0),


lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
text(Aartxt, AndelHoved, pos=3, NAarHendHoved, cex=0.9, col=fargeHoved)

lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}

Ttxt <- paste('(Tall ved punktene angir antall ', VarTxt, ')', sep='')
if (medSml == 1) {
	text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
	legend('topleft', border=NA, c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''),
		paste(smltxt, ' (N=', NSmlRes, ')', sep=''), Ttxt), bty='n', ncol=1, cex=cexleg,
		col=c(fargeHoved, fargeRest, NA), lwd=3)
	} else {
		legend('top', c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''), Ttxt),
		col=c(fargeHoved, NA), lwd=3, bty='n')
	}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
#------------------------------------------------------------------------------

}	#end else statement
}	#end function



