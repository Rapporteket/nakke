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


NakkeFigAndelTid <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31', tidsenhet='Aar',
                             minald=0, maxald=130, erMann='', myelopati=99, fremBak=0,
                             reshID=0, outfile='', enhetsUtvalg=0, preprosess=TRUE, hentData=0) {


  if (hentData == 1) {
    RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
  }

  # Preprosessere data
  if (preprosess){
    RegData <- NakkePreprosess(RegData=RegData)
  }

  '%i%' <- intersect

  NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelTid')
  RegData <- NakkeVarSpes$RegData
  sortAvtagende <- NakkeVarSpes$sortAvtagende
  varTxt <- NakkeVarSpes$varTxt
  KImaal <- NakkeVarSpes$KImaal
  KImaaltxt <- NakkeVarSpes$KImaaltxt
  tittel <- NakkeVarSpes$tittel

  #Gjør utvalg
  NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                erMann=erMann, myelopati=myelopati, fremBak=fremBak, enhetsUtvalg=enhetsUtvalg,
                                reshID=reshID)
  RegData <- NakkeUtvalg$RegData
  utvalgTxt <- NakkeUtvalg$utvalgTxt
  ind <- NakkeUtvalg$ind

  NHovedRes <- length(ind$Hoved)
  NSmlRes <- length(ind$Rest)

      #------------------------Klargjøre tidsenhet--------------
  RegData$Mnd <- RegData$InnDato$mon +1
  RegData$Kvartal <- ceiling(RegData$Mnd/3)
  RegData$Halvaar <- ceiling(RegData$Mnd/6)
  RegData$Aar <- 1900 + RegData$InnDato$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year

      #Brukes til sortering
      RegData$TidsEnhet <- switch(tidsenhet,
                                  Aar = RegData$Aar-min(RegData$Aar)+1,
                                  Mnd = RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
                                  +(RegData$Aar-min(RegData$Aar))*12,
                                  Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                        (RegData$Aar-min(RegData$Aar))*4,
                                  Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                        (RegData$Aar-min(RegData$Aar))*2
      )

      tidtxt <- switch(tidsenhet,
                       Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                   sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='.'),
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]))

      RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=1:max(RegData$TidsEnhet)) #evt. levels=tidtxt

  #-------------------------Beregning av andel-----------------------------------------
#  Aartxt <- min(RegData$Aar):max(RegData$Aar)
#  RegData$Aar <- factor(RegData$Aar, levels=Aartxt)

#  NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest], length)
#  NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest],sum, na.rm=T)
#  AndelRest <- NAarHendRest/NAarRest*100
#  NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'], length)
#  NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'],sum, na.rm=T)
#  AndelHoved <- NAarHendHoved/NAarHoved*100
#  Andeler <- rbind(AndelRest, AndelHoved)

      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))


      NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per år
      NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per år
      AggVerdier$Hoved <- NAarHendHoved/NAarHoved*100
      NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
      NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
      AggVerdier$Rest <- NAarHendRest/NAarRest*100
      Ngr <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)

      #grtxt <- paste0(rev(NIRVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Hoved)), '%)')
      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      yAkseTxt <- 'Andel (%)'
      vektor <- c('Aar','Halvaar','Kvartal','Mnd')
      xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal', 'Innleggelsesmåned')
                         [which(tidsenhet==vektor)])

  #----------FIGUR------------------------------
  #Hvis for få observasjoner..
  #if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & medSml == 1)) {
  if (length(ind$Hoved) < 10 | (NakkeUtvalg$medSml ==1 & length(ind$Rest)<10)) {
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
    # FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
    # farger <- FigTypUt$farger
    # fargeHoved <- farger[3]
    # fargeRest <- farger[1]
    # NutvTxt <- length(utvalgTxt)
    # hmarg <- 0.04+0.01*NutvTxt
    # par('fig' = c(0,1,0,1-hmarg))
    # cexleg <- 1	#Størrelse på legendtekst
    #
    #
    # ymax <- min(119, 1.25*max(Andeler,na.rm=T))
    # plot(Aartxt, AndelHoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
    #      xlim= c(Aartxt[1], max(Aartxt)), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(Aartxt), max(Aartxt),length(Aartxt)-1)
    #      cex=2, xlab='Innleggelsesår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	#Operasjonsår,
    # axis(side=1, at = Aartxt)
    # title(tittel, line=1, font.main=1)
    #
    # #Legge på linjer i plottet.
    # grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")
    #
    # lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
    # points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
    # text(Aartxt, AndelHoved, pos=3, NAarHendHoved, cex=0.9, col=fargeHoved)
    #
    # lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
    # points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}
    #
    # Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
    # if (NakkeUtvalg$medSml == 1) {
    #   text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
    #   legend('topleft', border=NA, c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', NHovedRes, ')'),
    #                                  paste0(NakkeUtvalg$smltxt, ' (N=', NSmlRes, ')'), Ttxt), bty='n', ncol=1, cex=cexleg,
    #          col=c(fargeHoved, fargeRest, NA), lwd=3)
    # } else {
    #   legend('top', c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', NHovedRes, ')'), Ttxt),
    #          col=c(fargeHoved, NA), lwd=3, bty='n')
    # }
    #
    # #Tekst som angir hvilket utvalg som er gjort
    # mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))
    #
    # par('fig'=c(0, 1, 0, 1))
    # if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------
                 #Plottspesifikke parametre:
                  FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
                  farger <- FigTypUt$farger
                  fargeHoved <- farger[3]
                  fargeRest <- farger[1]
                  NutvTxt <- length(utvalgTxt)
                  hmarg <- 0.04+0.01*NutvTxt
                  par('fig' = c(0,1,0,1-hmarg))
                  cexleg <- 1	#St?rrelse p? legendtekst
                  ylabtext <- "Andel (%)"
                  xskala <- 1:length(tidtxt)
                  xmax <- max(xskala)


                  ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
                  plot(xskala, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
                       xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
                       cex=2, xlab=xAkseTxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

                  #Legge på linjer i plottet.
                  grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

                  axis(side=1, at = xskala, labels = tidtxt, cex.axis=0.85)

                  title(tittel, line=1, font.main=1)


                  lines(xskala, AggVerdier$Hoved, col=fargeHoved, lwd=3)
                  points(xskala, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
                  text(xskala, AggVerdier$Hoved, pos=3, Ngr$Hoved, cex=0.9, col=fargeHoved)

                  lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
                  points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)

                  #KImål
                  lines(xskala,rep(KImaal,length(xskala)), col= '#FF7260', lwd=3)
                  #mtext(text=paste0('Mål:',KImaaltxt), at=50, side=4, las=1, adj=1,  cex=0.9, col='#FF7260')
                  #text(max(xskala), KImaal, pos=4, paste0('Mål:',KImaaltxt), cex=0.9, col='#FF7260')

                  Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
                  if (NakkeUtvalg$medSml == 1) {
                        text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
                        legend('topleft', border=NA, c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', N$Hoved, ')'),
                                                       paste0(NakkeUtvalg$smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1,
                               col=c(fargeHoved, fargeRest, NA), lwd=3, cex=cexleg)
                  } else {
                        legend('top', c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt),
                               col=c(fargeHoved, NA), lwd=3, bty='n')
                  }

                  #Tekst som angir hvilket utvalg som er gjort
                  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

                  par('fig'=c(0, 1, 0, 1))
                  if ( outfile != '') {dev.off()}
                  #------------------------------------------------------------------------------

  }	#end else statement
}	#end function



