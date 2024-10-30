#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok
#'
#' @inheritParams NakkeFigAndeler
#' @param valgtVar parameter som angir hvilke(n) variabel man ønsker å tilrettelegge for videre
#' beregning.
#' @param ktr Angir om 3 eller 12 mnd kontroll. 0-ikke valgt (standard), 1-3mnd., 2-12mnd.,
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NakkeVarTilrettelegg  <- function(RegData, valgtVar, ktr=0, figurtype='andeler'){ #AndelGrVar #grVar='',


  "%i%" <- intersect

  #----------- Figurparametre ------------------------------
  cexgr <- 1	#Kan endres for enkeltvariable
  retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
  flerevar <- 0
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
  varTxt <- ''
  xAkseTxt <- ''	#Benevning
  if (figurtype == 'andelGrVar') {xAkseTxt <- 'Andel operasjoner (%)'}
  ytxt1 <- ''
  sortAvtagende <- FALSE  #Sortering av resultater. FALSE-laveste best
  tittel <- 'Mangler tittel'
  deltittel <- ''
  variable <- 'Ingen'
  KIekstrem <- NULL
  RegData$Variabel <- 0
  #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller
  #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
  #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???


  #--------------- Definere variable ------------------------------
  #Variabeltyper: Numeriske, kategoriske, indikator
  # For hver valgtVar:
  # Definer og gjør utvalg for variabelen
  # tittel, xAkseTxt, sortAvtagende (standard: TRUE)

  tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen
  ktrtxt <- c(', 3 mnd etter', ', 12 mnd. etter')[ktr]
  #	}


  #-------------------------------------

  if (valgtVar=='Alder') {	#Fordeling, #AndelGrVar,AndelTid, GjsnGrVar, GjsnTid
    RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
    xAkseTxt <- 'alder (år)'
    tittel <- 'Alder ved innleggelse'
    if (figurtype %in% c('andelTid', 'andelGrVar')) {
      #Pasienter over 70 år
      RegData$Variabel[which(RegData$Alder >= 70)] <- 1
      varTxt <- 'pasienter >=70år'
      tittel <- 'Pasienter over 70 år'
    }
    if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
      RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
	  xaksetxt <- 'alder (år)'
	  deltittel <- 'alder ved innleggelse'}
    if (figurtype == 'andeler') {	#Fordelingsfigur
      gr <- c(seq(0, 100, 10),150)
      gr <- c(0,seq(20,90,10),150)
      RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')	#c(levels(RegData$VariabelGr)[-length(gr)], '90+')	#c(names(AndelLand)[-length(gr)], '90+')
      xAkseTxt <- 'Aldersgrupper (år)'}
    sortAvtagende <- FALSE
    KIekstrem <- c(0,110)

  }

  if (valgtVar=='AndreRelSykdommer') { #AndelGrVar #AndelTid
    #IKKE LENGER (feb 18): Tar med blanke som 0. (Hver sykdom får også verdien 0 når denne er tom.)
    #RegData$Variabel[which(RegData[ ,valgtVar] == 1)] <- 1
    RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'med andre sykdommer'
    tittel <- 'Pasienter med komorbiditet ved operasjon'
  }
  if (valgtVar=='AntallNivaaOpr') { #Andeler
    gr <- c(0:5,1000)
    RegData$VariabelGr <- cut(RegData$AntallNivaaOpr, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(0:4,'5+')	#sort(unique(RegData$AntNivOpr))
    xAkseTxt <- 'Antall'
    tittel <- 'Antall nivå operert'
  }
  if (valgtVar=='Antibiotika') { #Andeler #AndelGrVar #AndelTid
    #Fått antibiotikaprofylakse
    grtxt <- c('Nei', 'Ja', 'Ukjent')	#Ukjent= Ikke utfylt og evt. manglende
    varTxt <- 'som har fått antibiotika'
    tittel <- 'Fått antibiotikaprofylakse'
    indDum <- RegData$Antibiotika %in% 0:1
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum,]}
    RegData$Variabel <- RegData$Antibiotika
    if (figurtype == 'andeler') {
      tittel <- 'Er det gitt antibiotikaprofylakse?'
      RegData$VariabelGr <- 9
      RegData$VariabelGr[indDum] <- RegData$Antibiotika[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
    }
  }

  if (valgtVar %in% c('ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd')) { #Andeler, #AndelGrVar  #AndelTid
    # Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
    grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
               'Delvis sykemeldt', 'Attføring/rehab.', 'Uførepensjon', 'Ufør og sykem.', 'Ikke utfylt')
    indSkjema <- switch(valgtVar,
                        ArbeidstausPreOp = which(RegData$PasientSkjemaStatus == 1),
                        Arbeidstaus3mnd = which(RegData$OppFolgStatus3mnd == 1),
                        Arbeidstaus12mnd = which(RegData$OppFolgStatus12mnd == 1))
    indDum <- which(RegData[ ,valgtVar] %in% 1:9)
    RegData <- RegData[indSkjema, ]
    RegData$VariabelGr <- 99
    indDum <- which(RegData[ ,valgtVar] %in% 1:10)
    RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:10,99))
    tittel <- switch(valgtVar,
                     ArbeidstausPreOp = 'Arbeidsstatus før operasjon',
                     Arbeidstaus3mnd = 'Arbeidsstatus 3 mnd. etter operasjon' ,
                     Arbeidstaus12mnd = 'Arbeidsstatus 12 mnd. etter operasjon')
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
    tittel <- switch(valgtVar,
                     ArbeidstausPreOp = 'Mottar sykepenger, preoperativt?',
                     Arbeidstaus3mnd = 'Mottar sykepenger, 3 mnd etter operasjon?' ,
                     Arbeidstaus12mnd = 'Mottar sykepenger, 12 mnd etter operasjon?')
    RegData$Variabel[which(RegData[ ,valgtVar] %in% 6:9)] <- 1
}
    varTxt <- 'som mottar sykepenger'
    retn <- 'H'
  }
  if (valgtVar=='ASAgrad') {#Andeler,  #AndelGrVar  #AndelTid
    #Legeskjema. Andel av de som har ASA-grad 3-5
    grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'V:Døende', 'Ukjent')
    indDum <- which(RegData[, valgtVar] %in% 1:5)
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum,]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 3:5)] <- 1
      varTxt <- 'med ASA>II'
      tittel <- 'Pasienter med ASA-grad III-V'
    }
    if (figurtype == 'andeler') {
      RegData$VariabelGr <- 99
      RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,99))
      tittel <-  'ASA-grad (komorbiditet)'
    }
    xAkseTxt <- 'Sykdomsgrad'
  }

  if (valgtVar=='BMI') { #Andeler #AndelGrVar  #AndelTid
    #Pasientskjema.
    RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1), which(RegData$BMI > 0)), ]
    gr <- c(-1, 0, 18.5, 25, 30, 1000)
    RegData$VariabelGr <- -1
    RegData$VariabelGr <- cut(RegData[,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('', '<18,5', levels(RegData$VariabelGr)[3:(length(gr)-2)],'30+')
    grtxt2 <- c('Ukjent', 'Undervekt', 'Normalvekt', 'Overvekt', 'Fedme')
    xAkseTxt <- "Body Mass Index"
    tittel <-  'Pasientenes BMI (Body Mass Index)'
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData$Variabel[which(RegData[ ,valgtVar] >30)] <- 1
      varTxt <- 'med BMI>30'
      tittel <- 'Pasienter med fedme  (BMI>30)'
    }
  }
	if (valgtVar=='EMSendr12mnd') { #GjsnTid #GjsnGrVar
		#Pasientkjema og 12mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-18,18)
		RegData$Variabel <- RegData$EMSscore12mnd - RegData$EMSscorePreOp
		indMyelopati <- which(RegData$OprIndikMyelopati == 1)
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indMyelopati, intersect(indVar, indSkjema)), ]
		tittel <- 'Forbedring av EMS hos myelopati-pasienter, 12 mnd.'
		ytxt1 <- '(endring av EMS-skår)'
	deltittel <- 'forbedring av EMS, myelopati-pas., 12 mnd.'
		}
			if (valgtVar=='EMSendr3mnd') { #GjsnTid #GjsnGrVar
		#Pasientkjema og 3mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-18,18)
		RegData$Variabel <- RegData$EMSscore3mnd - RegData$EMSscorePreOp
		indMyelopati <- which(RegData$OprIndikMyelopati == 1)
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indMyelopati, intersect(indVar, indSkjema)), ]
		tittel <- 'Forbedring av EMS hos myelopati-pasienter, 3 mnd.'
		  deltittel <- 'forbedring av EMS, myelopati-pas., 3 mnd.'
		ytxt1 <- '(endring av EMS-skår)'
		}

if (valgtVar == 'EMSscorePreOp') { #GjsnGrVar, GjsnTid
	#Pasientskjema. Bare myelopatipasienter (OprIndikMyelopati == 1)
	indPas <- which(RegData$PasientSkjemaStatus==1)
	indMye <- which(RegData$OprIndikMyelopati == 1)
	indVar <- which(RegData[ ,valgtVar] >-1)
	RegData <- RegData[intersect(intersect(indPas, indMye),indVar), ]
	RegData$Variabel <- RegData[ ,valgtVar]
	deltittel <- 'EMS hos myelopatipas. før operasjon'
	xAkseTxt <- ''
	KIekstrem <- c(0,18)

	}

  if (valgtVar=='EnhverKompl3mnd') { #AndelGrVar  #AndelTid
    #Pasientskjema. Alle komplikasjoner, 3mnd.
    indSkjema <- which(RegData$OppFolgStatus3mnd == 1)
    RegData <- RegData[intersect(which(RegData[,valgtVar] %in% 0:1), indSkjema), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'komplikasjoner'
    tittel <- 'Komplikasjoner (totalt) 3 mnd. etter operasjon'
  }
  	if (valgtVar=='EQ5Dendr3mnd') { #GjsnTid #GjsnGrVar
		#Pasientkjema og 3mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-1.6, 1.6)
		RegData$Variabel <- RegData$Eq5DScore3mnd - RegData$Eq5DScorePreOp
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		tittel <- 'Forbedring av EQ5D, 3 mnd.'
 deltittel <- 'forbedring av EQ5D, 3 mnd.'
 		ytxt1 <- '(endring av EQ5D-skår)'
		}
	if (valgtVar=='EQ5Dendr12mnd') { #GjsnTid #GjsnGrVar
		#Pasientkjema og 12mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-1.6, 1.6)
		RegData$Variabel <- RegData$Eq5DScore12mnd - RegData$Eq5DScorePreOp
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		tittel <- 'Forbedring av EQ5D, 12 mnd.'
  deltittel <- 'forbedring av EQ5D, 12 mnd.'
 		ytxt1 <- '(endring av EQ5D-skår)'
		}

if (valgtVar=='Eq5DScorePreOp') { #gjsnTid, gjsnGrVar
     #Pasientkjema.
     KIekstrem <- c(-0.6, 1)
     indVar <- which(RegData[ , valgtVar] >= KIekstrem[1])
     indSkjema <- which(RegData$PasientSkjemaStatus==1)
     RegData <- RegData[intersect(indVar, indSkjema), ]
     RegData$Variabel <- RegData[, valgtVar]
     tittel <- 'EQ5D før operasjon'
     deltittel <- 'EQ5D før operasjon'
     ytxt1 <- '(EQ5D-skåring)'
}

  if (valgtVar == 'EqAngstPreOp') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
    grtxt <- c('Ingen', 'Litt', 'Mye', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(as.numeric(RegData$EqAngstPreOp) %in% 1:3)
    RegData$VariabelGr[indDum] <- RegData$EqAngstPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    xAkseTxt <- 'Grad av engstelighet/deprimerthet'	#Tilstand i forhold til angst'
    tittel <-  'Problemer med angst/depresjon'
  }
  if (valgtVar=='ErstatningPreOp') { #Andeler #AndelGrVar #AndelTid
    #Pasientskjema. Andel med ErstatningPreOp 1 el 3
    #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
    RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
    grtxt <- c('Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent')
    tittel <- 'Har søkt/planlegger å søke erstatning før operasjon'
    indDum <- which(RegData$ErstatningPreOp %in% 1:4)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$ErstatningPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
      varTxt <- 'søkt erstatning'
    }
  }

  if (valgtVar %in% c('FornoydBeh3mnd','FornoydBeh12mnd')) { #Andeler #AndelGrVar #AndelTid
    #3/12mndSkjema. Andel med Fornøyd/litt fornøyd (1,2)
    #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
    grtxt <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
    RegData <- switch(valgtVar,
                      FornoydBeh3mnd = RegData[which(RegData$OppFolgStatus3mnd==1), ],
                      FornoydBeh12mnd = RegData[which(RegData$OppFolgStatus12mnd==1), ])
    indDum <- which(RegData[ ,valgtVar] %in% 1:5)
    retn <- 'H'
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
      varTxt <- 'fornøyde'
      tittel <- switch(valgtVar,
                       FornoydBeh3mnd = 'Fornøyd med behandlinga på sykehuset, 3 mnd' ,
                       FornoydBeh12mnd = 'Fornøyd med behandlinga på sykehuset, 12 mnd')
    }
    tittel <- switch(valgtVar,
                     FornoydBeh3mnd = 'Fornøyd med behandlinga på sykehuset, 3 mnd' ,
                     FornoydBeh12mnd = 'Fornøyd med behandlinga på sykehuset, 12 mnd')
    sortAvtagende <- TRUE
  }

  if (valgtVar == 'Inngrep'){
    grtxt <- c('Ikke klassifiserbar', 'Fremre diskektomi, prolaps', 'Bakre dekompresjon',
               'Fremre dekomp. SS u/prolaps', 'Bakre fusjon', 'Korporektomi', 'Andre inngrep') #for verdiene 0:6
    RegData <- RegData[which(RegData$Inngrep %in% 0:6),]
    RegData$VariabelGr <- factor(RegData$Inngrep, levels = 0:6)
    tittel <- 'Inngrepstyper'
    retn <- 'H'
  }
if (valgtVar=='KnivtidTotalMin') { #GjsnTid #GjsnGrVar#Legeskjema.
		RegData <- RegData[which(RegData[ ,valgtVar]>0), ]
		RegData$Variabel <- RegData[ ,valgtVar]
		KIekstrem <- c(0, 500)
		tittel <- 'Total knivtid'
		ytxt1 <- '(minutter)'
	deltittel <- 'total knivtid'
	xAkseTxt <- 'minutter'
	}


  if (valgtVar=='KomplinfekDyp3mnd') { #AndelGrVar, AndelTid
    #3MndSkjema. Andel med KomplinfekDyp3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekDyp3mnd %in% 0:1)), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'dype infeksjoner'
    tittel <- 'Pasientrapportert dyp infeksjon, 3 mnd.'
  }
  if (valgtVar=='KomplinfekOverfl3mnd') { #AndelGrVar, AndelTid
    #3MndSkjema. Andel med KomplinfekOverfl3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'overfladiske infeksjoner'
    tittel <- 'Overfladisk infeksjon, 3 mnd.'
  }
  if (valgtVar=='Komplinfek') { #AndelTid, #AndelGrVar
    #3MndSkjema. Andel med KomplinfekDyp3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    ind <- intersect(which(RegData$OppFolgStatus3mnd == 1),
      union(which(RegData$KomplinfekDyp3mnd %in% 0:1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)))
    RegData <- RegData[ind, ]
    RegData$Variabel[union(which(RegData$KomplinfekDyp3mnd==1), which(RegData$KomplinfekOverfl3mnd==1))] <- 1
    varTxt <- 'infeksjoner'
    tittel <- 'Pasientrapportert dyp eller overfladisk infeksjon, 3 mnd.'
    }

  if (valgtVar=='KomplStemme3mnd') { #AndelTid, #AndelGrVar
    #3MndSkjema. Andel med KomplStemme3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1) %i%
                         which(RegData$KomplStemme3mnd %in% 0:1) %i%
                         which(RegData$OprMetodeTilgangFremre==1) # %i%which(RegData$OprIndikMyelopati==0)
                       , ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'med stemmevansker'
    tittel <- 'Stemmevansker, fremre tilgang, 3 mnd.'
  }
  if (valgtVar=='KomplStemme12mnd') { #AndelGrVar,
    #3MndSkjema. Andel med KomplStemme12mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[which(RegData$OppFolgStatus12mnd == 1) %i%
                         which(RegData$KomplStemme12mnd %in% 0:1) %i%
                         which(RegData$OprMetodeTilgangFremre==1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Stemmevansker, fremre tilgang, 12 mnd.'
  }


  if (valgtVar=='KomplSvelging12mnd') { #AndelGrVar,
    #3MndSkjema. Andel med KomplSvelging12mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[(which(RegData$OppFolgStatus12mnd == 1) %i%
                          which(RegData$KomplSvelging12mnd %in% 0:1) %i%
                          which(RegData$OprMetodeTilgangFremre==1)), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Svelgevansker, fremre tilgang, 12 mnd.'
  }

  if (valgtVar=='KomplSvelging3mnd') { #AndelTid
    #3MndSkjema. Andel med KomplSvelging3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[(which(RegData$OppFolgStatus3mnd == 1) %i%
                          which(RegData$KomplSvelging3mnd %in% 0:1) %i%
                          which(RegData$OprMetodeTilgangFremre==1)), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'med svelgevansker'
    tittel <- 'Svelgevansker, fremre tilgang, 3 mnd.'
  }
  if (valgtVar=='LiggeDognPostop') { #Andeler #GjsnTid #GjsnGrVar
    #Legeskjema.
	#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
    #dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
    #RegData$Liggedogn[dagind]<-0
  	RegData <- RegData[which(RegData[ ,valgtVar]>-1), ]
			RegData$Variabel <- RegData[ ,valgtVar]
	gr <- c(0:5,100)
    RegData$VariabelGr <- cut(RegData$LiggeDognPostop, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(0:4, '5+')
    tittel <- 'Antall liggedøgn postoperativt'
	KIekstrem <- c(0, 30)
	ytxt1 <- '(døgn)'
	deltittel <- 'liggetid etter operasjon'
	xAkseTxt <- 'dager'
	}

  if (valgtVar=='LiggeDognTotalt') { #Andeler #GjsnTid #GjsnGrVar
    #Legeskjema.
	#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
    #dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
    #RegData$Liggedogn[dagind]<-0
    tittel <- 'Totalt antall liggedøgn'
	RegData <- RegData[which(RegData[ ,valgtVar]>-1), ]
		RegData$Variabel <- RegData[ ,valgtVar]
    gr <- c(0:7,100)
    RegData$VariabelGr <- cut(RegData$LiggeDognTotalt, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(0:6, '7+')
		KIekstrem <- c(0, 30)
		ytxt1 <- '(døgn)'
	deltittel <- 'antall liggedøgn, totalt'
	xAkseTxt <- 'dager'
	}

  if (valgtVar %in% c('Misfor3mnd','Misfor12mnd')) { #AndelGrVar
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
    varTxt <- 'misfornøyde'
    tittel <- switch(valgtVar,
                     Misfor3mnd = 'Misfornøyd med behandlinga på sykehuset, 3 mnd' ,
                     Misfor12mnd = 'Misfornøyd med behandlinga på sykehuset, 12 mnd')
  }
  if (valgtVar == 'Morsmal') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
    grtxt <- c('Norsk', 'Samisk', 'Annet', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(RegData$Morsmal %in% 1:3)
    RegData$VariabelGr[indDum] <- RegData$Morsmal[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    tittel <- 'Morsmål'
  }

	if (valgtVar=='NDIscorePreOp') { #GjsnTid #GjsnGrVar
		#Pasientkjema.
	  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
	  KIekstrem <- c(0,100)
		indVar <- which(RegData[ ,valgtVar] >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		RegData$Variabel <- RegData[ ,valgtVar]
		tittel <- 'NDI før operasjon'
		ytxt1 <- '(NDI-skåring)'
	deltittel <- 'NDI før operasjon'
	xAkseTxt <- 'skåring'
	}


if (valgtVar %in% c('NDIendr12mnd35pst', 'NDIendr12mnd35pstKI')) { #AndelGrVar, AndelTid
    #Pasientkjema og 12mndskjema. Lav skår, lite plager -> forbedring = nedgang.
    RegData$NDIEndr <- 100*(RegData$NDIscorePreOp - RegData$NDIscore12mnd)/RegData$NDIscorePreOp
    indVar <- which(is.finite(RegData$NDIEndr))
    indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
    RegData <- RegData[intersect(indVar, indSkjema), ]
    RegData$Variabel[RegData$NDIEndr>=35] <- 1
    tittel <- 'Minst 35% forbedring av NDI, 12 mnd.'
	varTxt <- 'med NDI>35%'
	sortAvtagende <- TRUE
	# if (valgtVar == 'NDIendr12mnd35pstKI'){
	#   KImaalGrenser <- rev(c(0,40,70,100)) #c(0,70,100)
	# }
}
  # if (valgtVar=='NDIendr12mnd35pstKI') { #AndelGrVar, AndelTid
  #   #Pasientkjema og 12mndskjema. Lav skår, lite plager -> forbedring = nedgang.
  #   RegData$NDIEndr <- 100*(RegData$NDIscorePreOp - RegData$NDIscore12mnd)/RegData$NDIscorePreOp
  #   indVar <- which(is.finite(RegData$NDIEndr) & RegData$OprIndikMyelopati==0 & RegData$OprMetodeTilgangFremre==1)
  #   indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
  #   RegData <- RegData[intersect(indVar, indSkjema), ]
  #   RegData$Variabel[RegData$NDIEndr>=35] <- 1
  #   tittel <- 'Minst 35% forb. av NDI, 12 mnd., fremre, ikke-myelopati'
  #   varTxt <- 'med NDI>35%'
  #   KImaalGrenser <- rev(c(0,40,70,100)) #c(0,70,100)
  # }
	if (valgtVar=='NDIendr12mnd') { #GjsnTid, GjsnGrVar
		#Pasientkjema og 12mndskjema. Lav skår, lite plager -> forbedring = nedgang.
		KIekstrem <- c(-100,100)
		RegData$Variabel <- RegData$NDIscorePreOp - RegData$NDIscore12mnd
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		tittel <- 'Forbedring av NDI, 12 mnd. etter operasjon'
		deltittel <- 'forbedring av NDI, 12 mnd. etter operasjon'
 		ytxt1 <- '(endring av NDI-skår)'
 		sortAvtagende <- TRUE
		}

	if (valgtVar=='NDIendr3mnd') { #GjsnTid, GjsnGrVar
		#Pasientkjema og 3mndskjema. Lav skår, lite plager -> forbedring = nedgang.
		KIekstrem <- c(-100,100)
		RegData$Variabel <- RegData$NDIscorePreOp - RegData$NDIscore3mnd
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		tittel <- 'Forbedring av NDI, 3 mnd. etter operasjon'
	 deltittel <- 'forbedring av NDI, 3 mnd. etter operasjon'
 	ytxt1 <- '(endring av NDI-skår)'
		}

  if (valgtVar == 'NRSsmerteArmEndr12mnd') { #AndelGrVar #GjsnGrVar
    #Pasientskjema.
    KIekstrem <- c(-10,10)
    RegData$NRSEndr <- 100*(RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm12mnd)/RegData$NRSsmerteArmPreOp
    indPas <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
    indVar <- which(is.finite(RegData$NRSEndr))
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel[which(RegData$NRSEndr >=30)] <- 1
    tittel <- 'Minst 30% forbedring av NRS-arm, 12 mnd.'
	varTxt <- 'med NRSendr.>30%'
	if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
		RegData$Variabel <- RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm12mnd} #RegData$NRSEndr
	deltittel <- 'NRS, armsmerte, endring 12 mnd.'
	xAkseTxt <- 'skåring'
	sortAvtagende <- TRUE
	}
  if (valgtVar == 'NRSsmerteArmEndr3mnd') { #AndelGrVar #GjsnGrVar, GjsnTid
    #Pasientskjema.
    KIekstrem <- c(-10,10)
    RegData$NRSEndr <- 100*(RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm3mnd)/RegData$NRSsmerteArmPreOp
    indPas <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
    indVar <- which(is.finite(RegData$NRSEndr)) #Kun de som har reg både før og etter
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel[which(RegData$NRSEndr >=30)] <- 1
    tittel <- 'Minst 30% forbedring av NRS-arm, 3 mnd.'
    if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
      RegData$Variabel <- RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm3mnd}
    deltittel <- 'NRS, armsmerte, endring 3 mnd.'
    tittel <- 'NRS, armsmerte, endring 3 mnd. etter'
    xAkseTxt <- 'skåring'
  }

	if (valgtVar == 'NRSsmerteArmPreOp') { #GjsnGrVar, GjsnTid
	#Pasientskjema.
	  KIekstrem <- c(0,10)
	  indPas <- which(RegData$PasientSkjemaStatus==1)
	indVar <- which(RegData[ ,valgtVar] >-1)
	RegData <- RegData[intersect(indPas ,indVar), ]
	RegData$Variabel <- RegData[ ,valgtVar]
	deltittel <- 'NRS, arm før operasjon'
	xAkseTxt <- 'skåring'
	}

  if (valgtVar == 'NRSsmerteNakkeEndr3mnd') { #GjsnGrVar, GjsnTid
    #Pasientskjema.
    KIekstrem <- c(-10,10)
    RegData$NRSEndr <- (RegData$NRSsmerteNakkePreOp - RegData$NRSsmerteNakke3mnd) #100*/RegData$NRSsmerteArmPreOp
    indPas <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
    indVar <- which(is.finite(RegData$NRSEndr))
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel <- RegData$NRSEndr
    deltittel <- 'NRS, nakkesmerte, endring 3 mnd. etter'
    tittel <- deltittel
    xAkseTxt <- 'skåring'
  }
  if (valgtVar == 'NRSsmerteNakkeEndr12mnd') { #GjsnGrVar, GjsnTid
    #Pasientskjema.
    KIekstrem <- c(-10,10)
    RegData$NRSEndr <- (RegData$NRSsmerteNakkePreOp - RegData$NRSsmerteNakke12mnd) #100*/RegData$NRSsmerteArmPreOp
    indPas <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
    indVar <- which(is.finite(RegData$NRSEndr))
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel <- RegData$NRSEndr
    deltittel <- 'NRS, nakkesmerte, endring 12 mnd. etter'
    tittel <- deltittel
    xAkseTxt <- 'skåring'
  }
  if (valgtVar == 'NRSsmerteNakkePreOp') { #GjsnGrVar, GjsnTid
    #Pasientskjema.
    KIekstrem <- c(0,10)
    indPas <- which(RegData$PasientSkjemaStatus==1)
    indVar <- which(RegData[ ,valgtVar] >-1)
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    deltittel <- 'NRS, nakke før operasjon'
    tittel <- deltittel
    xAkseTxt <- 'skåring'
  }
  if (valgtVar %in% c('NytteOpr3mnd', 'NytteOpr12mnd')) { #Andeler #AndelTid  #AndelGrVar
    #3/12mndSkjema. Andel med helt bra/mye bedre (1:2)
    #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
    #				'Verre enn noen gang', 'Ukjent')
    grtxt <- c('Klart bedre', 'Små endringer', 'Klart verre', 'Ukjent')
    tittel <- switch(valgtVar,
                     NytteOpr3mnd = 'Nytte av operasjon, 3 mnd',
                     NytteOpr12mnd = 'Nytte av operasjon, 12 mnd')
    RegData <- switch(valgtVar,
                      NytteOpr3mnd = RegData[which(RegData$OppFolgStatus3mnd==1), ],
                      NytteOpr12mnd = RegData[which(RegData$OppFolgStatus12mnd==1), ])
    retn <- 'H'
    RegData$VariabelGr <- 9
    indDum <- which(RegData[ , valgtVar] %in% 1:7)
    RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
    oldvalues <- c(1:7,9)
    newvalues <- c(1,1,2,2,2,3,3,4)
    #levels=c('Klart bedre','Klart bedre', 'Små endringer', 'Små endringer', 'Små endringer',
    # 'Klart verre', 'Klart verre', 'Ukjent'))  # Make this a factor
    RegData$VariabelGr <- factor(newvalues[ match(RegData$VariabelGr, oldvalues) ], levels=1:4)
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
      varTxt <- '"mye bedre" og "helt bra"'
      tittel <- switch(valgtVar,
                       NytteOpr3mnd = 'Helt bra eller mye bedre, 3 mnd.' ,
                       NytteOpr12mnd = 'Helt bra eller mye bedre, 12 mnd.')

    }
  }
  if (valgtVar %in% c('NytteOpr3mndAlleKat', 'NytteOpr12mndAlleKat')) { #Andeler
    #3/12mndSkjema. Andel med helt bra/mye bedre (1:2)
    #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
    #				'Verre enn noen gang', 'Ukjent')
    grtxt <- c('Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
               				'Verre enn noen gang', 'Ukjent')
    RegData$Nytte <- switch(valgtVar,
                            'NytteOpr3mndAllekat' = RegData$NytteOpr3mnd,
                            'NytteOpr12mndAlleKat' =RegData$NytteOpr12mnd)
    tittel <- switch(valgtVar,
                     'NytteOpr3mndAlleKat' = 'Nytte av operasjon, 3 mnd. etter',
                     'NytteOpr12mndAlleKat' = 'Nytte av operasjon, 12 mnd. etter')
    RegData$VariabelGr <- 9
    ind <- switch(valgtVar,
                  'NytteOpr3mndAlleKat' = which(RegData$OppFolgStatus3mnd==1),
                  'NytteOpr12mndAlleKat' = which(RegData$OppFolgStatus12mnd==1))
    RegData <- RegData[ind, ]
    retn <- 'H'
    indDum <- which(RegData$Nytte %in% 1:7)
    RegData$VariabelGr[indDum] <- RegData$Nytte[indDum]
  }

  if (valgtVar %in% c('Verre3mnd','Verre12mnd')) { #AndelTid  #AndelGrVar
    #3/12mndSkjema. Andel med helt mye verre og noen sinne (6:7)
    #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
    #				'Verre enn noen gang', 'Ukjent')
    indSkjema <- switch(valgtVar,
                        Verre3mnd = which(RegData$NytteOpr3mnd %in% 1:7) %i% which(RegData$OppFolgStatus3mnd==1),
                        Verre12mnd = which(RegData$NytteOpr12mnd %in% 1:7) %i% which(RegData$OppFolgStatus12mnd==1))
    RegData <- RegData[indSkjema, ]
    indVar <- switch(valgtVar,
                     Verre3mnd = which(RegData$NytteOpr3mnd %in% 6:7),
                     Verre12mnd = which(RegData$NytteOpr12mnd %in% 6:7))
    RegData$Variabel[indVar] <- 1
    varTxt <- 'med klar forverring'
    tittel <- switch(valgtVar,
                     Verre3mnd = 'Mye verre/verre enn noen gang, 3 mnd.' ,
                     Verre12mnd = 'Mye verre/verre enn noen gang, 12 mnd.')
  }
  if (valgtVar == 'OperasjonsKategori') { #Andeler
    retn <- 'H'
    grtxt <- c('Elektiv', 'Øhjelp', 'Subakutt', 'Ukjent')
    indDum <- which(RegData$OperasjonsKategori %in% 1:3)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$OperasjonsKategori[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    tittel <- 'Hastegrad'
  }
  if (valgtVar %in% c('Oppf3mnd', 'Oppf12mnd', 'Oppf3og12mnd')) { #AndelGrVar, -Tid
    #Oppfølgingsskjema: OppFolgStatus12mnd, OppFolgStatus3mnd
    trekkfraDager <- ifelse(valgtVar == 'Oppf3mnd', 100, 400)
    RegData <- RegData[RegData$InnDato < min(max(RegData$InnDato), Sys.Date()-trekkfraDager), ]
    ind <- switch(valgtVar,
                  Oppf3mnd = which(RegData$OppFolgStatus3mnd==1),
                  Oppf12mnd = which(RegData$OppFolgStatus12mnd==1),
                  Oppf3og12mnd = which(RegData$OppFolgStatus3mnd==1 & RegData$OppFolgStatus12mnd==1 ))

    RegData$Variabel[ind] <- 1

    tittel <- paste0('Svart på oppfølging, ',
                     switch(valgtVar,
                            Oppf3mnd = '3 mnd. etter',
                            Oppf12mnd = '12 mnd. etter',
                            Oppf3og12mnd = 'både 3 og 12 mnd. etter'))
    sortAvtagende <- T
  }
  if (valgtVar=='OprIndikMyelopati') { #AndelTid #AndelGrVar
    #LegeSkjema. Andel med OprIndikMyelopati=1
    #Kode 0,1: Nei, Ja +tomme
    #RegData <- RegData[which(RegData$OprIndikMyelopati %in% 0:1), ]
    #RegData$Variabel <- RegData$OprIndikMyelopati
    #Antar tomme = nei. (Tore 3.okt. 2016)
    RegData$Variabel[which(RegData$OprIndikMyelopati == 1)] <- 1
    varTxt <- 'med myelopati'
    tittel <- 'Operasjonsårsak: Myelopati'
  }
  if (valgtVar == 'OprIndikPareseGrad') { #Andeler
    grtxt <- c(0:5, 'Ukjent')
    RegData <- RegData[which(RegData$OprIndikParese == 1), ]
    indDum <- which(RegData$OprIndikPareseGrad %in% 0:5)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$OprIndikPareseGrad[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:5,9))
    tittel <- 'Paresegrad'
  }


  if (valgtVar=='OprIndikSmerter') { #AndelTid
    #LegeSkjema. Andel med OprIndikSmerter=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[which(RegData$OprIndikSmerter %in% 0:1), ]
    RegData$Variabel <- RegData$OprIndikSmerter
    varTxt <- 'med smerter'
    tittel <- 'Operasjonsårsak: Smerter'
  }

  if (valgtVar=='PerOpEnhverKompl') { #AndelTid
    #LegeSkjema. Andel med PerOpEnhverKompl=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[which(RegData$PerOpEnhverKompl %in% 0:1), ]
    RegData$Variabel <- RegData$PerOpEnhverKompl
    varTxt <- 'med komplikasjoner'
    tittel <- 'Komplikasjoner (alle) ved operasjon'
  }

  if (valgtVar=='OpTilgfrembak') { #Andeler
    #LegeSkjema. Andel med PerOpEnhverKompl=1
    #Kode 0,1: Nei, Ja +tomme
    grtxt <- c('Ikke klassifiserbar', 'Fremre tilgang', 'Bakre tilgang', 'Andre inngrep', 'Bakre og fremre')
    RegData$VariabelGr <- factor(RegData$OpTilgfrembak, levels=0:4)
    #varTxt <- 'med komplikasjoner'
    tittel <- 'Operasjonstilgang'
  }

  if (valgtVar == 'regForsinkelse') {  #Fordeling, Andeler
    #Verdier: 0-3402
    RegData <- RegData[which(RegData$DiffUtFerdig > -1), ]
    tittel <- switch(figurtype,
                     andeler='Tid fra utskriving til ferdigstilt registrering',
                     andelGrVar = 'Mer enn 30 dager fra utskriving til ferdig registrering') #
    subtxt <- 'døgn'
    # gr <- c(0,1,7,14,30,90,365,5000) #gr <- c(seq(0, 90, 10), 1000)
    # RegData$VariabelGr <- cut(RegData$DiffUtFerdig, breaks = gr, include.lowest = TRUE, right = TRUE)
    # grtxt <- c('1', '(1-7]', '(7-14]', '(14-30]', '(30-90]', '(90-365]', '>365')
    cexgr <- 0.9
    xAkseTxt <- 'dager'
    sortAvtagende <- FALSE
    RegData <- RegData[RegData$InnDato < min(max(RegData$InnDato), Sys.Date()-30), ]

    if (figurtype == 'andeler') {	#Fordelingsfigur
      gr <- c(seq(0,98,7), 2000)
      RegData$VariabelGr <- cut(RegData$DiffUtFerdig, breaks=gr, include.lowest=TRUE, right=FALSE)
      #plot(RegData$VariabelGr)
      grtxt <- c(1:14, '>3 mnd.')
      subtxt <- 'innen gitte uker etter utskriving'
    }

    if (figurtype %in% c('andelTid', 'andelGrVar')) {
      RegData$Variabel[which(RegData$DiffUtFerdig >90)] <- 1
      tittel <- 'Registrert for sent for 3 mnd. oppfølging'
      varTxt <- 'for sent registrert'
      sortAvtagende <- F}
    KImaalGrenser <- c(0,3,10,100)
  }

  if (valgtVar=='Roker') { #Andeler #AndelTid #AndelGrVar
    #PasientSkjema. Andel med Roker=1
    #Kode 0,1,9: Nei, Ja Ukjent
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    tittel <- 'Røyker pasienten?'
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    indDum <- RegData$Roker %in% 0:1
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$Roker[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum,]
      RegData$Variabel <- RegData$Roker
      varTxt <- 'røykere'
      tittel <- 'Røykere'
    }
  }
  if (valgtVar == 'Saardren') { #Andeler #AndelTid #AndelGrVar
    #LegeSkjema.
    #Kode 0,1,9: Nei, Ja Ukjent
    tittel <- 'Har pasienten fått sårdren?'
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    indDum <- which(RegData$Saardren %in% 0:1)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$Saardren[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:1,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum, ]
      RegData$Variabel <- RegData$Saardren
      varTxt <- 'med sårdren'
      tittel <- 'Pasienter som får sårdren'
    }
  }
  if (valgtVar == 'SivilStatus') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    grtxt <- c('Gift', 'Samboer', 'Enslig', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(RegData$SivilStatus %in% 1:3)
    RegData$VariabelGr[indDum] <- RegData$SivilStatus[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    tittel <- 'Sivilstatus'
  }
  if (valgtVar == 'SmertestillBrukPreOp') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    # 1 - Sjeldnere enn hver uke, 2 - Hver uke, 3 - Daglig, 4 - Flere ganger daglig, 9 - Ikke utfylt
    grtxt <- c('Aldri', 'Sjeldnere enn ukentlig', 'Ukentlig', 'Daglig', 'Flere ganger daglig', 'Ukjent')
    RegData$VariabelGr <- 9
    RegData$VariabelGr[which(RegData$SmertestillPreOp == 0)] <- 0
    indDum <- which(RegData$SmertestillBrukPreOp %in% 1:4)
    RegData$VariabelGr[indDum] <- RegData$SmertestillBrukPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:4,9))
    tittel <- 'Hyppighet av smertestillende før operasjonen'
    retn <- 'H'
  }
  if (valgtVar == 'SmertestillPreOp') { #AndelTid  #AndelGrVar
    #PasientSkjema. Andel med SmertestillPreOp=1
    #Kode 0,1,9: Nei, Ja Ukjent
    RegData <- RegData[intersect(which(RegData$SmertestillPreOp %in% 0:1), which(RegData$PasientSkjemaStatus ==1)), ]
    RegData$Variabel <- RegData$SmertestillPreOp
    varTxt <- 'på smertestillende'
    tittel <- 'Bruker smertestillende før operasjon'
  }
  if (valgtVar == 'Snuser') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- RegData$Snuser %in% 0:1
    RegData$VariabelGr[indDum] <- RegData$Snuser[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
    tittel <- 'Bruker pasienten snus?'
  }
  if (valgtVar == 'SymptVarighetArmer') { #Andeler #AndelTid  #AndelGrVar
    #PasientSkjema. Andel med SymptVarighetArmer 4 el 5
    #Kode: Antall uker
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    tittel <-'Varighet av utstrålende armsmerter'
    grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent')
    RegData <- RegData[intersect(which(RegData$SymptVarighetArmer >-1), which(RegData$PasientSkjemaStatus ==1)), ]
    indDum <- which(RegData[,valgtVar] %in% 1:5)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData[indDum,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum,]
      RegData$Variabel[which(RegData[,valgtVar] %in% 4:5)] <- 1
      varTxt <- 'med varighet > 1 år'
      tittel <- 'Varighet av armsmerter minst ett år'
    }
  }
  if (valgtVar == 'SymptVarighetNakkeHode') { #Andeler #AndelTid #AndelGrVar
    #PasientSkjema. Andel med SymptVarighetNakkeHode 4 el 5
    #Kode 1:5,9: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
    tittel <- 'Varighet av nakke-/hodesmerter'
    grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent')
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    indDum <- which(RegData[,valgtVar] %in% 1:5)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData[indDum,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum,]
      RegData$Variabel[which(RegData[,valgtVar] %in% 4:5)] <- 1
      varTxt <- 'med varighet >1 år'
      tittel <- 'Varighet av hode-/nakkesmerter minst ett år'
    }
  }

  if (valgtVar == 'TidlOpr') { #Andeler
    retn <- 'H'
    grtxt <- c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(RegData$TidlOpr %in% 1:4)
    RegData$VariabelGr[indDum] <- RegData$TidlOpr[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
    tittel <- 'Er pasienten tidligere operert?'
  }
  if (valgtVar=='TidlOprAntall') { #Andeler
    gr <- c(0:3, 100,1000)
    RegData$Variabel <- 999
    indDum <- which(RegData$TidlOprAntall>=0)
    RegData$Variabel[indDum] <- RegData$TidlOprAntall[indDum]
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(0:2,paste0('3-', max(RegData$TidlOprAntall, na.rm=T)), 'Ukjent')
    tittel <- 'Antall tidligere operasjoner'
  }

  if (valgtVar == 'UforetrygdPreOp') { #Andeler #AndelTid #AndelGrVar
    #PasientSkjema. Andel med UforetrygdPreOp 1 og 3
    #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
    grtxt <- c('Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
    tittel <- 'Søkt/planlegger å søke uføretrygd før operasjon'
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    indDum <- which(RegData$UforetrygdPreOp %in% 1:4)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$UforetrygdPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum,]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
      varTxt <- 'søkt/planlagt å søke'
      tittel <- 'Søkt / planlegger å søke uføretrygd før operasjon'
      retn <- 'H'
    }
  }


  if (valgtVar == 'Utdanning') { #Andeler  #AndelGrVar
    #PasientSkjema. Andel med Utdanning 4 el 5
    #Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
    #Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
    grtxt <- c('Grunnskole, 7-10år','Real-, yrkes- el vg skole',
               'Allmennfaglig vg skole','Høyskole/universitet, <4 år','Høyskole/universitet, 4år+', 'Ukjent')
    tittel <- 'Utdanningsnivå'
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    indDum <- which(RegData$Utdanning %in% 1:5)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$Utdanning[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
      varTxt <- 'med høyere utdanning'
      tittel <- 'Pasienter med høyskole-/universitetsutdannelse'
    }
    retn <- 'H'
  }




   #-------------- SAMMENSATTE variable
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA

 # if (valgtVar %in% c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
#                      'OprIndikMyelopati', 'Radiologi')){


      if (valgtVar=='OprIndik') {
        tittel <- 'Operasjonsårsak'
        retn <- 'H'
		flerevar <-  1
        #OprIndiasjonasjonUfylt <>1 - tom variabel,
        #Svært få (ca 20 av 3000) har tom registrering. Setter derfor felles N lik alle reg.
		  #01.03.2018: Mange har tom registrering... Må derfor skille på N for de ulike variable
		variable <- c('OprIndikParese', 'OprIndikMyelopati', 'OprIndikSmerter', 'SmerteMyelo', 'OprIndikAnnet')
        grtxt <- c('Pareser', 'Myelopati', 'Smerter', 'Sm. og Myelop.', 'Annet')
        #Smerter og Myelopati
		indSmerter <- which(RegData$OprIndikSmerter == 1)
    indMyelopati <- which(RegData$OprIndikMyelopati == 1)
		RegData$SmerteMyelo <- NA
		RegData$SmerteMyelo[(RegData$OprIndikSmerter %in% 0:1) & (RegData$OprIndikMyelopati %in% 0:1)] <- 0
    RegData$SmerteMyelo[ (RegData$OprIndikSmerter == 1) & (RegData$OprIndikMyelopati == 1)] <- 1
            # ind01 <- which(RegData[ ,variable] %in% 0:1, arr.ind = T) #Alle ja/nei
            # ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
            # RegData[ ,variable] <- NA
            # RegData[ ,variable][ind01] <- 0
            # RegData[ ,variable][ind1] <- 1
            # xAkseTxt <- 'Andel opphold (%)'
            #
      }

      if (valgtVar=='Radiologi') {
        retn <- 'H'
        flerevar <- 1
        #RadilogiUnderokelseUfylt  - tom variabel,
        #RadiologiRtgCcolFunkOpptak  - tom variabel,
        #Svært få har tom registrering. Setter derfor felles N lik alle reg.
        tittel <- 'Radiologisk undersøkelse'
        grtxt <- c('CT', 'MR', 'Myelografi', 'Røntgen-Ccol')
		variable <- c('RadiologiCt', 'RadiologiMr', 'RadiologiMyelografi', 'RadiologiRtgCcol')
           ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            # ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
            # RegData[ ,variable] <- NA
            # RegData[ ,variable][ind01] <- 0
            # RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
      }

      if (valgtVar=='Komorbiditet') {
        tittel <- 'Komorbiditet'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$AndreRelSykdommer %in% 0:1), ] #Alle videre variable utfylt
        variable <- c('SykdReumatisk','SykdAnnenendokrin', 'SykdAnnet','SykdCarpalTunnelSyndr', 'SykdCerebrovaskular',
                      'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHodepine', 'SykdHypertensjon',
					  'SykDiabetesMellitus', 'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk',
					  'SykdKrSmerterMuskelSkjelSyst', 'SykdOsteoporose', 'SykdSkulderImpigment',
					  'SykdWhiplashNakke', 'AndreRelSykdommer')
        grtxt <- c('Annen Reumatisk', 'Annen endokrin', 'Andre', 'Carpal TS', 'Cerebrovaskulær',
					'Depresjon/Angst', 'Hjerte-/Karsykd.', 'Hodepine', 'Hypertensjon',
					'Diabetes', 'Kreft', 'Kr. lungesykdom', 'Kr. nevrologisk',
					'Kr. muskel/skjelettsm.', 'Osteoporose', 'Skuldersyndrom',
					'Whiplash/skade', 'Tot. komorb')
        RegData$SykdReumatisk <- 0
		    indSykdReumatisk <- (RegData$SykdAnnenreumatisk ==1 | (RegData$SykdBechtrew==1 | RegData$SykdReumatoidartritt==1))
        RegData$SykdReumatisk[indSykdReumatisk] <- 1
        # ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
        # ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
        # RegData[ ,variable] <- NA
        # RegData[ ,variable][ind01] <- 0
        # RegData[ ,variable][ind1] <- 1
      }

      if (valgtVar=='KomplOpr') {
        tittel <- 'Komplikasjoner ved operasjon'
        retn <- 'H'
        flerevar <- 1
        variable <- c('PerOpKomplAnafylaksiI','PerOpKomplAnnet','PerOpKomplBlodning','PerOpKomplDurarift',
                      'PerOpKomplFeilplasseringImplant','PerOpKomplKardioVaskulare','PerOpKomplMedullaskade',
                      'PerOpKomplNerverotSkade','PerOpKomplAnnenNerveskade','PerOpKomplOpFeilNivaa',
                      'PerOpKomplRespiratorisk','PerOpKomplOsofagusSkade','PerOpEnhverKompl')
       grtxt <- c('Anafylaksi','Annet','Blødning','Durarift','Feilplassering, impl.','Kardiovaskulære','Medullaskade',
                   'Nerverotskade','Nerveskade','Op. feil nivå','Respiratorisk','Øsofagusskade','Komplikasjoner, alle')
      }

      if (valgtVar=='Kompl3mnd') {
        tittel <- 'Komplikasjoner 3 mnd. etter operasjon'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1), ]
        variable <- c('KomplDVT3mnd', 'KomplinfekDyp3mnd', 'KomplLungeEmboli3mnd', 'KomplinfekOverfl3mnd',
                      'KomplPneumoni3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'KomplUVI3mnd', 'KomplKraftsvikt3mnd',
                      'EnhverKompl3mnd')
        grtxt <- c('DVT', 'Dyp infeksjon', 'Lungeemboli', 'Overfladisk infeksjon',
                   'Pneumoni', 'Stemmevansker', 'Svelgevansker', 'UVI', 'Kraftsvikt', 'Totalt, 3 mnd.')
      }

      if (valgtVar=='OprIndikSmerter') {
        tittel <- 'Operasjonsårsak: Smerter'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$OprIndikSmerter %in% 0:1), ]
        variable <- c('OprIndikSmerter', 'OprIndikSmerteLokArm', 'OprIndikSmerteLokNakke', 'SmerteArmNakke' )
        grtxt <- c('Smerter', '...Arm', '...Nakke', '...Arm og Nakke')
        RegData$SmerteArmNakke <- 0
        RegData$SmerteArmNakke[RegData$OprIndikSmerteLokArm == 1 & RegData$OprIndikSmerteLokNakke == 1] <- 1
      }

      if (valgtVar=='OprIndikMyelopati') {
        tittel <- 'Operasjonsårsak: Myelopati'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$OprIndikMyelopati %in% 0:1), ]
        variable <- c('OprIndikMyelopati', 'OprIndikMyelopatiMotorisk', 'OprIndikMyelopatiSensorisk',
                      'MotorSensor')
        grtxt <- c('Myelopati', '...Motorisk', '...Sensorisk', '...Begge deler')
        RegData$MotorSensor <- 0
        RegData$MotorSensor[RegData$OprIndikMyelopatiMotorisk & RegData$OprIndikMyelopatiSensorisk] <- 1
      }



    UtData <- list(RegData=RegData, grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt,
                   tittel=tittel, varTxt=varTxt, flerevar=flerevar, #KImaalGrenser=KImaalGrenser,
                   variable=variable, sortAvtagende=sortAvtagende,
                 retn=retn, ytxt1=ytxt1, deltittel=deltittel, KIekstrem=KIekstrem)
  #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
  return(invisible(UtData))

}
