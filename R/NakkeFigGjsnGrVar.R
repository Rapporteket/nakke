#'Søylediagram som viser sentralmål (gj.sn./median) for hvert sykehus
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' @inheritParams NakkeFigAndeler
#' @inheritParams NakkeUtvalgEnh
#' @param valgtMaal Sentralmål 'Med' gir median, alt annet gir gjennomsnitt
#' @param medKI vise konfidensintervall? 0-nei, 1-ja
#' @param Ngrense nedre grense for antall observasjoner for at et resultat (ei søyle) skal vises
#' @param valgtVar Variabelen det skal vises resultat for.
#'             Alder: alder (år)
#'             EMSscorePreOp: EMS hos Myelopatipasienter før
#'             EMSendr12mnd: Forbedring av EMS hos myelopati-pasienter, 12 mnd.
#'             EMSendr3mnd: Forbedring av EMS hos myelopati-pasienter, 3 mnd.
#'             EQ5Dendr12mnd: Forbedring av EQ5D, 12 mnd.
#'             EQ5Dendr3mnd: Forbedring av EQ5D, 3 mnd.
#'             KnivtidTotalMin: total knivtid
#'             NDIscorePreOp: NDI før operasjon
#'             NDIendr3mnd:
#'             LiggeDognPostop: liggetid etter operasjon
#'             LiggeDognTotalt: antall liggedøgn, totalt
#'             NRSsmerteArmPreOp: NSR, arm før operasjon
#'             NRSsmerteNakkePreOp: NSR, nakke før operasjon
#'
#' @return Figur med...
#'
#' @export


NakkeFigGjsnGrVar <- function(RegData, valgtVar='Alder', valgtMaal='Gjsn',
                              datoFra='2012-01-01', datoTil='2050-12-31',
                              myelopati=99, fremBak=0, Ngrense=10, medKI=1,
                              minald=0, maxald=130, erMann='', inngrep=99,
                              reshID=0, outfile='', hentData=0, preprosess=0,...) {

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = paste0('NakkeFigGjsnTid: ',valgtVar))
  }

  if (hentData == 1) {
    RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
  }

  # Preprosessere data
  if (preprosess==1){
    RegData <- NakkePreprosess(RegData=RegData)
  }


  #----------- Figurparametre ------------------------------

  #Når bare skal sammenlikne med region trengs ikke data for hele landet:
  #reshID <- as.numeric(reshID)
  #indEgen1 <- match(reshID, RegData$ReshId)
  #smltxt <- 'alle enheter'

  grVar <- 'ShNavn'
  RegData[ ,grVar] <- factor(RegData[ ,grVar])


  NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
  RegData <- NakkeVarSpes$RegData
  sortAvtagende <- NakkeVarSpes$sortAvtagende
  #varTxt <- NakkeVarSpes$varTxt
  # KIekstrem <- NakkeVarSpes$NakkeVarSpes
  KImaal <- NakkeVarSpes$KImaal
  KImaaltxt <- NakkeVarSpes$KImaaltxt
  deltittel <- NakkeVarSpes$deltittel
  xAkseTxt <- NakkeVarSpes$xAkseTxt
  #ytxt1 <- NakkeVarSpes$ytxt1

  #Gjør utvalg
  NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                                minald=minald, maxald=maxald, inngrep=inngrep,
                                erMann=erMann, myelopati=myelopati, fremBak=fremBak)	#, tidlOp=tidlOp
  RegData <- NakkeUtvalg$RegData
  utvalgTxt <- NakkeUtvalg$utvalgTxt

  N <- dim(RegData)[1]
  if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

  #Ngrtxt <- paste(', N=', as.character(Ngr), sep='') #paste('N=', as.character(Ngr), sep='')
  Ngrtxt <- paste0('N=', as.character(Ngr)) #paste('N=', as.character(Ngr), sep='')
  indGrUt <- as.numeric(which(Ngr < Ngrense))
  if (length(indGrUt)==0) { indGrUt <- 0}
  Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')	#paste('N<', Ngrense,sep='')


  if (valgtMaal=='Med') {
    t1 <- 'Median'
    tleg <- t1} else {
      t1 <- 'Gj.sn.'
      tleg <- 'Gjennomsnitt'}

  tittel <- paste(t1, deltittel, sep=' ')

  #--------------------------------------------------------
  dummy0 <- NA #-0.001
  #Kommer ut ferdig sortert!
  if (valgtMaal=='Med') {
    MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
    MedIQR$stats[ ,indGrUt] <- dummy0
    MedIQR$conf[ ,indGrUt] <- dummy0
    sortInd <- order( MedIQR$stats[3,], decreasing=TRUE)
    Midt <- as.numeric(MedIQR$stats[3, sortInd])
    KIned <- MedIQR$conf[1, sortInd]
    KIopp <- MedIQR$conf[2, sortInd]
    MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
    MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
    KIHele <- MedIQRHele$conf
    #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
    #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
    #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
    #roughly a 95% confidence interval for the difference in two medians.

  } else {	#Gjennomsnitt blir standard.
    Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
    SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
    Gjsn[indGrUt] <- dummy0
    SE[indGrUt] <- 0
    sortInd <- order(Gjsn, decreasing=TRUE)
    Midt <- as.numeric(Gjsn[sortInd])
    KIned <- Gjsn[sortInd] - 2*SE[sortInd]
    KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
    MidtHele <- round(mean(RegData$Variabel),1)
    KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
  }

  AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
  soyletxt <- c(sprintf('%.1f',Midt[1:AntGr]), rep('',length(Ngr)-AntGr))	#	#round(Midt[1:AntGr],1)
  xmax <-  min(1.1*max(c(Midt, KIned, KIopp), na.rm = T), 1.5*max(Midt, na.rm = T), na.rm = T)
  xmin <- min(c(0,c(Midt, KIned, KIopp)), na.rm=T)
  Ngr <- Ngr[sortInd]
  GrNavnSort <- names(Ngr)

  cexGrNavn <- 1
  cexSoyletxt <- 1

  AggVerdier <- list(Hoved=Midt, Rest=NULL, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
  SentralmaalTxt <- switch(valgtMaal,
                           gjsn='Gjennomsnitt',
                           med='Median')

  if (valgtVar == 'NDIendr12mnd' & myelopati==0 & fremBak==1) {
    KImaalGrenser <- c(0, MidtHele, 100) } else {
      KImaalGrenser <- NA }

  FigDataParam <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                        N=list(Hoved=N),
                        Ngr=Ngr,
                        grtxt2='',
                        medKI=medKI,
                        KImaal = NakkeVarSpes$KImaal,
                        soyletxt=soyletxt,
                        grtxt=GrNavnSort,
                        valgtMaal=valgtMaal,
                        SentralmaalTxt=SentralmaalTxt,
                        tittel=tittel,    #NakkeVarSpes$tittel,
                        #yAkseTxt=yAkseTxt,
                        retn='H',
                        xAkseTxt=NakkeVarSpes$xAkseTxt,
                        grTypeTxt=NakkeUtvalg$grTypeTxt,
                        utvalgTxt=NakkeUtvalg$utvalgTxt,
                        fargepalett=NakkeUtvalg$fargepalett,
                        medSml=NakkeUtvalg$medSml,
                        smltxt=NakkeUtvalg$smltxt)


  #-----------Figur---------------------------------------
  #print(Ngr)
  #print(Ngrense)
  if 	(max(Ngr, na.rm = T) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    if (dim(RegData)[1]>0) {
      tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main=tittel, cex=0.95)	#line=-8,
    text(0.5, 0.6, tekst, cex=1.2)
    #text(0.5, 0.3, , cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}
  } else {


    #--------------------------FIGUR---------------------------------------------------
    FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=NakkeUtvalg$fargepalett) #, res=96
    farger <- FigTypUt$farger
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexGrNavn)*0.7)
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    pos <- barplot(Midt, horiz=T, border=NA, col=farger[3],
                   xlim=c(xmin,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='', las=1, cex.names=cexGrNavn)
    indGrUtPlot <- AntGr+(1:length(indGrUt))
    posKI <- pos[1:AntGr]
    ybunn <- 0
    ytopp <- max(posKI)*1.05	 #min(posKI)

    # if (!is.na(KImaalGrenser)) {
    #   lines(x=rep(KImaalGrenser, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55),
    #   text(x=KImaalGrenser, y=maxpos+0.6, paste0('Mål:', KImaaltxt), cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0))
    # }
    if (!is.na(KImaalGrenser[1])) {
      antMaalNivaa <- length(KImaalGrenser)-1
      rekkef <- 1:antMaalNivaa
      if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
      fargerMaalNiva <-  c('#4fc63f', '#fbf850', '#c6312a')[rekkef] #c('green','yellow')# #c('#ddffcc', '#ffffcc') #, '#fff0e6') #Grønn, gul, rød
      maalOppTxt <- c('Høy', 'Moderat til lav', 'Lav')[rekkef]
      rect(xleft=KImaalGrenser[1:antMaalNivaa], ybottom=0, xright=KImaalGrenser[2:(antMaalNivaa+1)],
           ytop=max(posKI)+0.5, col = fargerMaalNiva[1:antMaalNivaa], border = NA) #add = TRUE, #pos[AntGrNgr+1],
      legend(x=0, y=-2.5, pch=c(NA,rep(15, antMaalNivaa)), col=c(NA, fargerMaalNiva[1:antMaalNivaa]),
             ncol=antMaalNivaa+1,
             xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
             legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,
    }

    polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),
             col=farger[4], border=farger[4])
    lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)

    legend('top', fill=c('white', farger[4]),  border='white', lwd=2,
           col=c(farger[2], farger[4]), seg.len=0.6, merge=TRUE, bty='n',
           c(paste0(tleg, ', alle: ', sprintf('%.1f', MidtHele), ', N=', N),
             paste0('95% konf.int., alle (',
                    sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')')))


    barplot(Midt, horiz=T, border=NA, col=farger[3], xlim=c(xmin, xmax), add=TRUE,
            font.main=1, xlab = xAkseTxt, las=1) 	#xlim=c(0,ymax), #, cex.names=0.5
    title(tittel, line=1, font.main=1, cex.main=1.3)
    #title('med 95% konfidensintervall', line=0.5, font.main=1, cex.main=0.95)
    mtext(at=pos+0.18, GrNavnSort, side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn
    mtext(at=pos-0.18, Ngrtxt[sortInd], side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn

    text(x=1.1*max(strwidth(soyletxt, units='user', cex=cexSoyletxt)), y=pos,	#y=pos+0.1,
         soyletxt, las=1, cex=cexSoyletxt, adj=1, col=farger[4])	#Tekst på søylene (verdi)
    #OK	text(x=xmax/20, y=pos+0.1, soyletxt, las=1, cex=0.75, adj=1, col=farger[1])	#Tekst på søylene (verdi)


    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))
    #    mtext(utvalgTxt, side=3, las=1, cex=cexGrNavn*0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
    arrows(x0=Midt[-indGrUtPlot]*0.999, y0=posKI, x1=KIopp[-indGrUtPlot], y1=posKI,
           length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
    arrows(x0=Midt[-indGrUtPlot]*1.001, y0=posKI, x1=KIned[-indGrUtPlot], y1=posKI,
           length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #----------------------------------------------------------------------------------
  }
  return(invisible(FigDataParam))

} #end function
