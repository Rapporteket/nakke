#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok
#'
#' @inheritParams FigAndeler
#' @inheritParams NakkeLibUtvalg
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NakkeVarTilrettelegg  <- function(RegData, valgtVar, ktr=0, figurtype='andeler'){ #grVar='',


      "%i%" <- intersect

      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      grNavn <- ''
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      if (figurtype == 'andelGrVar') {xAkseTxt <- 'Andel operasjoner (%)'}
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- TRUE  #Sortering av resultater
      KImaal <- NA
      tittel <- 'Mangler tittel'
      variable <- 'Ingen'
      #deltittel <- ''
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
      #if (valgtVar %in% c('OswEndr20', 'OswEndr30pst' )) {
      #ktr kan ha verdiene 0, 1 eller 2
      ktrtxt <- c(', 3 mnd etter', ', 12 mnd. etter')[ktr]
      #	}


      #-------------------------------------

      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (figurtype == 'andeler') {	#Fordelingsfigur
                  gr <- c(seq(0, 100, 10),150)
                  RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
                  grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
                  xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }

if (valgtVar=='Alder') { #AndelTid
	#Pasienter over 70 år
  	RegData$Variabel[which(RegData$Alder >= 70)] <- 1
  	VarTxt <- 'pasienter >=70år'
	TittelUt <- 'Andel pasienter over 70 år'
}
if (valgtVar=='AndreRelSykdommer') { #AndelTid
	#IKKE LENGER (feb 18): Tar med blanke som 0. (Hver sykdom får også verdien 0 når denne er tom.)
  	#RegData$Variabel[which(RegData[ ,valgtVar] == 1)] <- 1
    RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
  	VarTxt <- 'med andre sykdommer'
	TittelUt <- 'Andre sykdommer'
}
if (valgtVar=='Antibiotika') { #AndelTid
	#Andel av de som har fått antibiotika
	RegData <- RegData[which(RegData$Antibiotika %in% 0:1),]
  	RegData$Variabel <-RegData$Antibiotika
  	VarTxt <- 'som har fått antibiotika'
	TittelUt <- 'Fått antibiotika'
}

if (valgtVar %in% c('ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd')) { #AndelTid
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
if (valgtVar=='ASAgrad') { #AndelTid
	#Legeskjema. Andel av de som har ASA-grad 3-5
	RegData <- RegData[which(RegData$ASAgrad %in% 1:5),]
	RegData$Variabel[which(RegData[ ,valgtVar] %in% 3:5)] <- 1
  	VarTxt <- 'med ASA>II'
	TittelUt <- 'Andel pasienter med ASA-grad III-V'
}
 if (valgtVar=='BMI') { #AndelTid
	#Pasientskjema. Andel med BMI>30
	RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1), which(RegData$BMI > 0)), ]
	RegData$Variabel[which(RegData[ ,valgtVar] >30)] <- 1
  	VarTxt <- 'med BMI>30'
	TittelUt <- 'Andel pasienter med fedme  (BMI>30)'
}
if (valgtVar=='EnhverKompl3mnd') { #AndelTid
     #Pasientskjema. Alle komplikasjoner, 3mnd.
    indSkjema <- which(RegData$OppFolgStatus3mnd == 1)
    RegData <- RegData[intersect(which(RegData[,valgtVar] %in% 0:1), indSkjema), ]
    RegData$Variabel <- RegData[ ,valgtVar]
     VarTxt <- 'komplikasjoner'
     TittelUt <- 'Komplikasjoner (totalt) 3 mnd. etter operasjon'
}

if (valgtVar=='ErstatningPreOp') { #AndelTid
          #Pasientskjema. Andel med ErstatningPreOp 1 el 3
          #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
          RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1),
                                       which(RegData$ErstatningPreOp %in% 1:4)), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
  	VarTxt <- 'søkt erstatning'
	TittelUt <- 'Har søkt/planlegger å søke erstatning før operasjon'
}

if (valgtVar %in% c('FornoydBeh3mnd','FornoydBeh12mnd')) { #AndelTid
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
          TittelUt <- 'Overfladisk infeksjon, 3 mnd.'
     }


     if (valgtVar=='KomplStemme12mnd') {
          #3MndSkjema. Andel med KomplStemme12mnd=1
          #Kode 0,1: Nei, Ja +tomme
          RegData <- RegData[which(RegData$OppFolgStatus12mnd == 1) %i%
                                which(RegData$KomplStemme12mnd %in% 0:1) %i%
                                which(RegData$OprMetodeTilgangFremre==1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Stemmevansker, 12 mnd.'
     }


     if (valgtVar=='KomplSvelging12mnd') {
          #3MndSkjema. Andel med KomplSvelging12mnd=1
          #Kode 0,1: Nei, Ja +tomme
          RegData <- RegData[(which(RegData$OppFolgStatus12mnd == 1) %i%
                                       which(RegData$KomplSvelging12mnd %in% 0:1) %i%
                                         which(RegData$OprMetodeTilgangFremre==1)), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Svelgvansker, 12 mnd.'
     }

 if (valgtVar %in% c('Misfor3mnd','Misfor12mnd')) { #AndelTid
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
     if (valgtVar=='NDIendr12mnd') {
          #Pasientkjema og 12mndskjema. Lav skår, lite plager -> forbedring = nedgang.
          RegData$NDIEndr <- 100*(RegData$NDIscorePreOp - RegData$NDIscore12mnd)/RegData$NDIscorePreOp
          indVar <- which(is.finite(RegData$NDIEndr))
          indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
          RegData <- RegData[intersect(indVar, indSkjema), ]
          RegData$Variabel[RegData$NDIEndr>=30] <- 1
          TittelUt <- 'Minst 30% forbedring av NDI, 12 mnd.'
     }
     if (valgtVar == 'NRSsmerteArmEndr12mnd') {
          #Pasientskjema.
          RegData$NRSEndr <- 100*(RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm12mnd)/RegData$NRSsmerteArmPreOp
          indPas <- which(RegData$PasientSkjemaStatus==1)
          indVar <- which(is.finite(RegData$NRSEndr))
          RegData <- RegData[intersect(indPas ,indVar), ]
          RegData$Variabel[which(RegData$NRSEndr >=30)] <- 1
          TittelUt <- 'Minst 30% forbedring av NSR-arm, 12 mnd.'
     }

     if (valgtVar %in% c('NytteOpr3mnd', 'NytteOpr12mnd')) {
          #3/12mndSkjema. Andel med helt bra/mye bedre (1:2)
          #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
          #				'Verre enn noen gang', 'Ukjent')
          indSkjema <- switch(valgtVar,
                              NytteOpr3mnd = intersect(which(RegData$NytteOpr3mnd %in% 1:7),which(RegData$OppFolgStatus3mnd==1)),
                              NytteOpr12mnd = intersect(which(RegData$NytteOpr12mnd %in% 1:7),which(RegData$OppFolgStatus12mnd==1)))
          RegData <- RegData[indSkjema, ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
          TittelUt <- switch(valgtVar,
                             NytteOpr3mnd = 'Helt bra eller mye bedre, 3 mnd.' ,
                             NytteOpr12mnd = 'Helt bra eller mye bedre, 12 mnd.')
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
          TittelUt <- 'Operasjonsårsak: Myelopati'
     }

     if (valgtVar=='Roker') {
          #PasientSkjema. Andel med Roker=1
          #Kode 0,1,9: Nei, Ja Ukjent
          RegData <- RegData[intersect(which(RegData$Roker %in% 0:1), which(RegData$PasientSkjemaStatus ==1)), ]
          RegData$Variabel <- RegData$Roker
          TittelUt <- 'Røykere'
     }

     if (valgtVar == 'Saardren') {
          #LegeSkjema. Andel med Saardren=1
          #Kode 0,1,9: Nei, Ja Ukjent
          RegData <- RegData[which(RegData$Saardren %in% 0:1), ]
          RegData$Variabel <- RegData$Saardren
          TittelUt <- 'Andel som får sårdren'
     }

     if (valgtVar == 'SmertestillPreOp') {
          #PasientSkjema. Andel med SmertestillPreOp=1
          #Kode 0,1,9: Nei, Ja Ukjent
          RegData <- RegData[intersect(which(RegData$SmertestillPreOp %in% 0:1), which(RegData$PasientSkjemaStatus ==1)), ]
          RegData$Variabel <- RegData$SmertestillPreOp
          TittelUt <- 'Bruker smertestillende, før operasjon'
     }

     if (valgtVar == 'SymptVarighetNakkeHode') {
          #PasientSkjema. Andel med SymptVarighetNakkeHode 4 el 5
          #Kode 1:5,9: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
          RegData <- RegData[intersect(which(RegData$SymptVarighetNakkeHode %in% 1:5), which(RegData$PasientSkjemaStatus ==1)), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
          VarTxt <- 'med varighet minst 1 år'
          TittelUt <- 'Varighet av hode-/nakkesmerter minst 1 år'
     }

     if (valgtVar == 'SymptVarighetArmer') {
          #PasientSkjema. Andel med SymptVarighetArmer 4 el 5
          #Kode: Antall uker
          RegData <- RegData[intersect(which(RegData$SymptVarighetArmer >-1), which(RegData$PasientSkjemaStatus ==1)), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
          VarTxt <- 'med varighet minst 1år'
          TittelUt <- 'Varighet av armsmerter minst 1 år'
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


      UtData <- list(RegData=RegData, grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, retn=retn,
                     tittel=TittelUt, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData))

}
