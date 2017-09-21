#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
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
#'             KomplStemme12mnd: Stemmevansker, 12 mnd.
#'             KomplSvelging3mnd: Svelgvansker, 3 mnd.
#'             KomplSvelging12mnd: Svelgvansker, 12 mnd.
#'             Misfor12mnd: Misfornøyde pasienter, 12 mnd.
#'             Misfor3mnd: Misfornøyde pasienter, 3 mnd.
#'             NDIendr12mnd: Minst 30% forbedring av NDI, 12 mnd.
#'             NRSsmerteArmEndr12mnd: Minst 30% forbedring av NSR-arm, 12 mnd.
#'             NytteOpr12mnd: Klart bedre, 12 mnd.
#'             NytteOpr3mnd: Klart bedre, 3 mnd.
#'             Verre12mnd: Klart verre, 12 mnd.
#'             Verre3mnd. Klart verre, 3 mnd.
#'             OprIndikMyelopati: Operasjonsårsak, Myelopati
#'             Roker: Røykere
#'             Saardren: Andel som får sårdren
#'             SmertestillPreOp: Bruker smertestillende, preop.
#'             SymptVarighetNakkeHode: Varighet av hode-/nakkesmerter over 1 år
#'             SymptVarighetSmerterUker: Varighet av smerter minst 1 år
#'             UforetrygdPreOp: Søkt eller planlegger å søke uføretrygd?
#'             Utdanning: Andel høyskole-/universitetsutdannede
#'
#' @return Figur med...
#'
#' @export

FigAndelerGrVar <- function(RegData, valgtVar, datoFra='2012-01-01', datoTil='3000-12-31', enhetsUtvalg=0,
                            minald=0, maxald=130, erMann='', hentData=0, preprosess=TRUE,
                            tittel=1, reshID=0, outfile='') {

     if (hentData == 1) {
          RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
     }

     # Preprosessere data
     if (preprosess){
          RegData <- NakkePreprosess(RegData=RegData)
     }


     #----------- Figurparametre ------------------------------
     cexShNavn <- 1 #0.85

     #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
     #trengs ikke data for hele landet:
     reshID <- as.numeric(reshID)
     indEgen1 <- match(reshID, RegData$ReshId)
     smltxt <- 'Hele landet'
     if (enhetsUtvalg == 7) {
          smltxt <- as.character(RegData$Region[indEgen1])
          RegData <- RegData[which(RegData$Region == smltxt), ]	#kun egen region
          cexShNavn <- 1
     }

     grVar <- 'SykehusNavn'
     RegData[ ,grVar] <- factor(RegData[ ,grVar])
     Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist


     RegData$Variabel <- 0

     #Tar ut de med manglende registrering av valgt variabel og gjør utvalg
     NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                   erMann=erMann)
     RegData <- NakkeUtvalg$RegData
     utvalgTxt <- NakkeUtvalg$utvalgTxt


     if (valgtVar == 'Alder') {
          #Andel over 70 år
          RegData$Variabel[which(RegData[ ,valgtVar] >= 70)] <- 1
          TittelUt <- 'Pasienter over 70 år'
     }

     if (valgtVar == 'AndreRelSykdommer') {
          #Komorbiditet
          RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Andre sykdommer'
     }
     if (valgtVar == 'Antibiotika') {
          #Komorbiditet
          RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Fått antibiotika'
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
     }
     if (valgtVar == 'ASAgrad') {
          RegData <- RegData[which(RegData[,valgtVar] %in% 1:5), ]
          RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
          TittelUt <- 'ASA-grad > II'
     }
     if (valgtVar == 'BMI') {
          #BMI > 30
          RegData <- RegData[which(RegData[,valgtVar] >10), ]
          RegData$Variabel[which(RegData[ ,valgtVar] > 30)] <- 1
          TittelUt <- 'Pasienter med fedme (BMI>30)'
     }
     if (valgtVar == 'EnhverKompl3mnd') {
          #Komplikasjoner
          indSkjema <- which(RegData$OppFolgStatus3mnd == 1)
          RegData <- RegData[intersect(which(RegData[,valgtVar] %in% 0:1), indSkjema), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Alle komplikasjoner'
     }
     if (valgtVar == 'ErstatningPreOp') {
          #Pasientskjema. Andel med ErstatningPreOp 1 el 3
          #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
          RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1),
                                       which(RegData$ErstatningPreOp %in% 1:4)), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
          TittelUt <- 'Pasienten har søkt/planlegger å søke erstatning'
     }
     if (valgtVar %in% c('FornoydBeh3mnd','FornoydBeh12mnd')) {
          #3/12mndSkjema. Andel med Fornøyd/litt fornøyd (1,2)
          #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
          indSkjema <- switch(valgtVar,
                              FornoydBeh3mnd = intersect(which(RegData$FornoydBeh3mnd %in% 1:5),which(RegData$OppFolgStatus3mnd==1)),
                              FornoydBeh12mnd = intersect(which(RegData$FornoydBeh12mnd %in% 1:5),which(RegData$OppFolgStatus12mnd==1)))
          RegData <- RegData[indSkjema, ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
          TittelUt <- switch(valgtVar,
                             FornoydBeh3mnd = 'Fornøyde pasienter, 3 mnd.' ,
                             FornoydBeh12mnd = 'Fornøyde pasienter, 12 mnd.')
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

     if (valgtVar=='KomplStemme3mnd') {
          #3MndSkjema. Andel med KomplStemme3mnd=1
          #Kode 0,1: Nei, Ja +tomme
          RegData <- RegData[(which(RegData$OppFolgStatus3mnd == 1) &
                                which(RegData$KomplStemme3mnd %in% 0:1) &
                               which(RegData$OprMetodeTilgangFremre==1)), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Stemmevansker, 3 mnd.'
     }

     if (valgtVar=='KomplStemme12mnd') {
          #3MndSkjema. Andel med KomplStemme12mnd=1
          #Kode 0,1: Nei, Ja +tomme
          RegData <- RegData[(which(RegData$OppFolgStatus12mnd == 1) &
                                which(RegData$KomplStemme12mnd %in% 0:1) &
                                which(RegData$OprMetodeTilgangFremre==1)), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Stemmevansker, 12 mnd.'
     }

     if (valgtVar=='KomplSvelging3mnd') {
          #3MndSkjema. Andel med KomplSvelging3mnd=1
          #Kode 0,1: Nei, Ja +tomme
       ind <-
          RegData <- RegData[(which(RegData$OppFolgStatus3mnd == 1) &
                                       which(RegData$KomplSvelging3mnd %in% 0:1) &
                                       which(RegData$OprMetodeTilgangFremre==1)), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Svelgvansker, 3 mnd.'
     }

     if (valgtVar=='KomplSvelging12mnd') {
          #3MndSkjema. Andel med KomplSvelging12mnd=1
          #Kode 0,1: Nei, Ja +tomme
          RegData <- RegData[(which(RegData$OppFolgStatus12mnd == 1) &
                                       which(RegData$KomplSvelging12mnd %in% 0:1) &
                                         which(RegData$OprMetodeTilgangFremre==1)), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Svelgvansker, 12 mnd.'
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
          TittelUt <- switch(valgtVar,
                             Misfor3mnd = 'Misfornøyde pasienter, 3 mnd.' ,
                             Misfor12mnd = 'Misfornøyde pasienter, 12 mnd.')
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


     #Gjør utvalg
     NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                   erMann=erMann)
     RegData <- NakkeUtvalg$RegData
     utvalgTxt <- NakkeUtvalg$utvalgTxt


     dummy0 <- -0.001
     N <- dim(RegData)[1]
     Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
     if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
     AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
     AndelerGr <- round(100*Nvar/Ngr,2)

     indGrUt <- as.numeric(which(Ngr < Ngrense))
     if (length(indGrUt)==0) { indGrUt <- 0}
     AndelerGr[indGrUt] <- dummy0
     sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE)
     Ngrtxt <- paste('N=', as.character(Ngr), sep='')	#
     Ngrtxt[indGrUt] <- paste('N<', Ngrense,sep='')	#paste(' (<', Ngrense,')',sep='')	#

     AndelerGrSort <- AndelerGr[sortInd]
     AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
     #	GrNavnSort <- paste(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd], sep='')
     GrNavnSort <- names(Ngr)[sortInd]

     andeltxt <- paste(sprintf('%.1f',AndelerGrSort), '%',sep='') 	#round(as.numeric(AndelerGrSort),1)
     if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}

     if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}

     #-----------Figur---------------------------------------
     if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
          FigTypUt <- figtype(outfile)
          farger <- FigTypUt$farger
          plot.new()
          if (dim(RegData)[1]>0) {
               tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
          } else {tekst <- 'Ingen registrerte data for dette utvalget'}
          title(main=Tittel)
          text(0.5, 0.6, tekst, cex=1.2)
          legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
          if ( outfile != '') {dev.off()}

     } else {

          #--------------------------FIGUR---------------------------------------------------
          #Innparametre: ...


          FigTypUt <- figtype(outfile, height=3*800, fargepalett=NakkeUtvalg$fargepalett)
          farger <- FigTypUt$farger
          #Tilpasse marger for å kunne skrive utvalgsteksten
          NutvTxt <- length(utvalgTxt)
          vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
          #NB: strwidth oppfører seg ulikt avh. av device...
          par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

          xmax <- min(max(AndelerGrSort),100)*1.15
          pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=Tittel,
                         xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=cexShNavn*0.9)
          ybunn <- 0.1
          ytopp <- pos[AntGr]+1	#-length(indGrUt)]
          lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
          legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
                 legend=paste(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N,sep='' ),
                 bty='o', bg='white', box.col='white')
          mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
          text(x=0.005*xmax, y=pos, Ngrtxt[sortInd], las=1, cex=cexShNavn, adj=0, col=farger[4], lwd=3)	#c(Nshtxt[sortInd],''),
          title(Tittel, line=1, font.main=1, cex.main=1.3)

          text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
               las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

          #Tekst som angir hvilket utvalg som er gjort
          mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


          par('fig'=c(0, 1, 0, 1))
          if ( outfile != '') {dev.off()}
          #----------------------------------------------------------------------------------
     }
}
