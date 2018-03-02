#' Tidstrend (år) av andel for en gitt variabel.
#'
#' Funksjon som genererer en figur med andeler av en variabel for hvert år.
#'
#' Detaljer
#'
#' @inheritParams NakkeFigAndeler
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


NakkeFigAndelTid <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31',
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
                                erMann=erMann, myelopati=myelopati, fremBak=fremBak, enhetsUtvalg=enhetsUtvalg)
  RegData <- NakkeUtvalg$RegData
  utvalgTxt <- NakkeUtvalg$utvalgTxt
  ind <- NakkeUtvalg$ind

  NHovedRes <- length(ind$Hoved)
  NSmlRes <- length(ind$Rest)


  #-------------------------Beregning av andel-----------------------------------------
  Aartxt <- min(RegData$Aar):max(RegData$Aar)
  RegData$Aar <- factor(RegData$Aar, levels=Aartxt)

  NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest], length)
  NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest],sum, na.rm=T)
  AndelRest <- NAarHendRest/NAarRest*100
  NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'], length)
  NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'],sum, na.rm=T)
  AndelHoved <- NAarHendHoved/NAarHoved*100
  Andeler <- rbind(AndelRest, AndelHoved)


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

    title(tittel, line=1, font.main=1)

    #Legge på linjer i plottet.
    grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

    lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
    points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
    text(Aartxt, AndelHoved, pos=3, NAarHendHoved, cex=0.9, col=fargeHoved)

    lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
    points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}

    Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
    if (NakkeUtvalg$medSml == 1) {
      text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', NHovedRes, ')'),
                                     paste0(NakkeUtvalg$smltxt, ' (N=', NSmlRes, ')'), Ttxt), bty='n', ncol=1, cex=cexleg,
             col=c(fargeHoved, fargeRest, NA), lwd=3)
    } else {
      legend('top', c(paste0(NakkeUtvalg$hovedgrTxt, ' (N=', NHovedRes, ')'), Ttxt),
             col=c(fargeHoved, NA), lwd=3, bty='n')
    }

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------

  }	#end else statement
}	#end function



