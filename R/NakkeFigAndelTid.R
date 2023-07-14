#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Denne funksjonen lager et linjediagram som viser utvikling over tid  for andeler av valgt variabel,
#' filtrert på de utvalg som er gjort.
#'
#' Detaljer
#'
#' @inheritParams NakkeUtvalgEnh
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
#' @inheritParams NakkeFigAndeler
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
#' @return Figur med ...
#'
#' @export


NakkeFigAndelTid <- function(RegData, valgtVar='Alder', datoFra='2013-01-01', datoTil='3000-12-31', tidsenhet='Aar',
                             minald=0, maxald=130, erMann='', myelopati=99, fremBak=0, inngrep=99,
                             reshID=0, outfile='', enhetsUtvalg=0, preprosess=0,
                             hentData=0,...) {

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('NakkeFigAndelTid: ',valgtVar))
  }


  if (hentData == 1) {
    RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
  }

  # Preprosessere data
  if (preprosess==1){
    RegData <- NakkePreprosess(RegData=RegData)
  }

  '%i%' <- intersect

  NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelTid')
  RegData <- NakkeVarSpes$RegData
  varTxt <- NakkeVarSpes$varTxt
  # KImaal <- NakkeVarSpes$KImaal #Tatt bort mai 2021
  # KImaaltxt <- NakkeVarSpes$KImaaltxt
  tittel <- NakkeVarSpes$tittel

  #Gjør utvalg
  NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                                minald=minald, maxald=maxald, inngrep = inngrep,
                                erMann=erMann, myelopati=myelopati, fremBak=fremBak, enhetsUtvalg=enhetsUtvalg,
                                reshID=reshID)
  RegData <- NakkeUtvalg$RegData
  utvalgTxt <- NakkeUtvalg$utvalgTxt
  ind <- NakkeUtvalg$ind

  NHovedRes <- length(ind$Hoved)
  NSmlRes <- length(ind$Rest)
  AggVerdier <- list(Hoved = 0, Rest =0)
  #N <- list(Hoved = dim(RegData)[1], Rest=0)
  N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))

  yAkseTxt <- 'Andel (%)'
  vektor <- c('Aar','Halvaar','Kvartal','Mnd')
  xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår-halvår', 'Innleggelsesår-kvartal',
                       'Innleggelsesmåned')[which(tidsenhet==vektor)])

  #Hvis for få observasjoner..
  if (length(ind$Hoved) < 10 | (NakkeUtvalg$medSml ==1 & length(ind$Rest)<10)) {
    #-----------Figur---------------------------------------
    Ngr <- list(Hoved = 0, Rest = 0)
    Nvar <- list(Hoved = 0, Rest = 0)

    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    tittel <- paste('variabel: ', valgtVar, sep='')
    title(main=tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}

    } else {


  #Legge på tidsenhet
        RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
        RegData <- RegDataFunk$RegData
        tidNum <- min(RegData$TidsEnhetSort, na.rm=T):max(RegData$TidsEnhetSort, na.rm = T) #as.numeric(levels(RegData$TidsEnhetSort))

  #-------------------------Beregning av andel-----------------------------------------

      NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per år
      NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per år
      AggVerdier$Hoved <- NAarHendHoved/NAarHoved*100
      NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
      NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
      AggVerdier$Rest <- NAarHendRest/NAarRest*100
      Ngr <- list(Hoved = NAarHoved, Rest = NAarRest)
      Nvar <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)

      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')

      hovedgrTxt <- NakkeUtvalg$hovedgrTxt
      smltxt <- NakkeUtvalg$smltxt

#}
  #----------FIGUR------------------------------

    #-----------Figur---------------------------------------

                 #Plottspesifikke parametre:
                  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
                  farger <- FigTypUt$farger
                  fargeHoved <- farger[3]
                  fargeRest <- farger[1]
                  NutvTxt <- length(utvalgTxt)
                  hmarg <- 0.04+0.01*NutvTxt
                  par('fig' = c(0,1,0,1-hmarg))
                  cexleg <- 1	#St?rrelse p? legendtekst
                  ylabtext <- "Andel (%)"
                  #xskala <- 1:length(tidtxt)
                  xmax <- max(tidNum)


                  ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
                  plot(tidNum, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
                       xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
                       cex=2, xlab=xAkseTxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

                  #Legge på linjer i plottet.
                  grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

                  #axis(side=1, at = xskala, labels = tidtxt, cex.axis=0.85)
                  axis(side=1, at = tidNum, labels = levels(RegData$TidsEnhet))

                  title(tittel, line=1, font.main=1, cex.main=1.3)


                  lines(tidNum, AggVerdier$Hoved, col=fargeHoved, lwd=3)
                  points(tidNum, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
                  text(tidNum, AggVerdier$Hoved, pos=3, Nvar$Hoved, cex=0.9, col=fargeHoved)

                  lines(tidNum, AggVerdier$Rest, col=fargeRest, lwd=3)
                  points(tidNum, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)

                  # #KImål Tatt bort mai 2021
                  # lines(tidNum,rep(KImaal[2],length(tidNum)), col= '#FF7260', lwd=3)
                  # #mtext(text=paste0('Mål:',KImaaltxt), at=50, side=4, las=1, adj=1,  cex=0.9, col='#FF7260')
                  # #text(max(tidNum), KImaal, pos=4, paste0('Mål:',KImaaltxt), cex=0.9, col='#FF7260')

                  Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
                  if (NakkeUtvalg$medSml == 1) {
                        text(tidNum, AggVerdier$Rest, pos=3, Nvar$Rest, cex=0.9, col=fargeRest)
                        legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                                       paste0(smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1,
                               col=c(fargeHoved, fargeRest, NA), lwd=3, cex=cexleg)
                  } else {
                        legend('top', c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt),
                               col=c(fargeHoved, NA), lwd=3, bty='n')
                  }

                  #Tekst som angir hvilket utvalg som er gjort
                  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))
                  #                  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

                  par('fig'=c(0, 1, 0, 1))
                  if ( outfile != '') {dev.off()}
                  #------------------------------------------------------------------------------

  }	#end else statement
  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=N,
                       Ngr=Ngr,
                       Nvar=Nvar,
                       grtxt2=grtxt2,
                       varTxt=varTxt,
                       tittel=tittel,
                       xAkseTxt=xAkseTxt,
                       yAkseTxt=yAkseTxt,
                       utvalgTxt=NakkeUtvalg$utvalgTxt,
                       fargepalett=NakkeUtvalg$fargepalett,
                       medSml=NakkeUtvalg$medSml,
                       hovedgrTxt=NakkeUtvalg$hovedgrTxt,
                       smltxt=NakkeUtvalg$smltxt
  )
  return(invisible(FigDataParam))
}	#end function



