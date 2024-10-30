#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#'
#' @inheritParams NakkeFigAndeler
#' @inheritParams NakkeUtvalgEnh
#' @param Ngrense Minste antall registreringer for at ei gruppe skal bli vist
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
#'             Komplinfek: Pasientrapportert infeksjon (dyp og/eller overfladisk), 3 mnd.
#'             KomplinfekDyp3mnd: Pasientrapportert dyp infeksjon, 3 mnd.
#'             KomplinfekOverfl3mnd: Overfladisk infeksjon, 3 mnd.
#'             KomplStemme3mnd: Stemmevansker, 3 mnd.
#'             KomplStemme12mnd: Stemmevansker, 12 mnd.
#'             KomplSvelging3mnd: Svelgvansker, 3 mnd.
#'             KomplSvelging12mnd: Svelgvansker, 12 mnd.
#'             Misfor12mnd: Misfornøyde pasienter, 12 mnd.
#'             Misfor3mnd: Misfornøyde pasienter, 3 mnd.
#'             NDIendr12mnd30pst: Minst 30% forbedring av NDI, 12 mnd. (NB: Endret fra NDIendr12mnd)
#'             NRSsmerteArmEndr12mnd: Minst 30% forbedring av NSR-arm, 12 mnd.
#'             NytteOpr12mnd: Klart bedre, 12 mnd.
#'             NytteOpr3mnd: Klart bedre, 3 mnd.
#'             OprIndikMyelopati: Operasjonsårsak, Myelopati
#'             Roker: Røykere
#'             Saardren: Andel som får sårdren
#'             SmertestillPreOp: Bruker smertestillende, preop.
#'             SymptVarighetNakkeHode: Varighet av hode-/nakkesmerter over 1 år
#'             SymptVarighetSmerterUker: Varighet av smerter minst 1 år
#'             UforetrygdPreOp: Søkt eller planlegger å søke uføretrygd?
#'             Utdanning: Andel høyskole-/universitetsutdannede
#'             Verre12mnd: Klart verre, 12 mnd.
#'             Verre3mnd. Klart verre, 3 mnd.
#'
#' @return Figur med...
#'
#' @export

NakkeFigAndelerGrVar <- function(RegData=0, valgtVar='Alder', minald=0, maxald=130,
                                 datoFra='2012-01-01', datoTil='3000-12-31', inngrep=99,
                                 erMann='', myelopati=99, fremBak=0, Ngrense=10,
                                 hentData=0, preprosess=0, reshID=0, outfile='',...) { #tittel=1,

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('NakkeFigAndelGrVar: ',valgtVar))
  }

  if (hentData == 1) {
    RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
  }

  # Preprosessere data
  if (preprosess==1){
    RegData <- NakkePreprosess(RegData=RegData)
  }

  '%i%' <- intersect
  #----------- Figurparametre ------------------------------

  NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
  RegData <- NakkeVarSpes$RegData
  sortAvtagende <- NakkeVarSpes$sortAvtagende
  Tittel <- NakkeVarSpes$tittel
  KImaalGrenser <- NA

  #Gjør utvalg
  NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                                minald=minald, maxald=maxald, erMann=erMann,
                                myelopati=myelopati, fremBak=fremBak, inngrep = inngrep)
  RegData <- NakkeUtvalg$RegData
  utvalgTxt <- NakkeUtvalg$utvalgTxt

  grVar <- 'ShNavn'
  RegData[ ,grVar] <- factor(RegData[ ,grVar])

  if(dim(RegData)[1] > 0) {
    RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
    RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
    Ngr <- table(RegData[ ,grVar])
    Ngrtxt <- as.character(Ngr)
  }	else {Ngr <- 0}

  N <- dim(RegData)[1]
  AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
  AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
  Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
  AndelerGr <- round(100*Nvar/Ngr,2)

  GrNavn <- names(Ngr)
  xAkseTxt <- "Andel opphold (%)"

  indGrUt <- which(Ngr < Ngrense)
  if (sum(indGrUt)>0) {
    AndelGrUt <- NA #sum(AndelerGr[indGrUt]*Ngr[indGrUt], na.rm = T)/sum(Ngr[indGrUt])
    AndelerGr <- c(AndelerGr[-indGrUt],AndelGrUt)
    GrUtNavn <- paste0(length(indGrUt), ' avd. med N<',Ngrense)
    Ngrtxt <- c(Ngr[-indGrUt],sum(Ngr[indGrUt]))
    GrNavn <- c(GrNavn[-indGrUt], GrUtNavn)
  }

  if (valgtVar == 'Komplinfek') {KImaalGrenser <- c(0,2,100)}
  if (valgtVar == 'NDIendr12mnd35pst' & fremBak == 1 & myelopati == 0) {KImaalGrenser <- c(0,40,70,100)}
  if (valgtVar == 'KomplSvelging3mnd' & myelopati == 0) {KImaalGrenser <- c(0,17,100)}
  if (valgtVar == 'KomplStemme3mnd' & myelopati == 0) {KImaalGrenser <- c(0,10,100)}
  #KomplStemme3mnd og KomplSvelging3mnd - blir filtrert på fremre i tilrettelegging
  fargepalett <- NakkeUtvalg$fargepalett

sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE)
AndelerGrSort <- AndelerGr[sortInd]
GrNavnSort <- GrNavn[sortInd]
Ngrtxt <- Ngrtxt[sortInd]

andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %')
andeltxt <- andeltxtUsort[sortInd]


FigDataParam <- list(AggVerdier=AndelerGrSort,
                     AggTot=AndelHele,
                     N=N,
                     Ngr=as.numeric(Ngrtxt),
                     Nvar=Nvar[sortInd],
                     soyletxt=andeltxt,
                     grtxt=GrNavnSort,
                     Tittel=Tittel,
                     utvalgTxt=utvalgTxt,
                     fargepalett =fargepalett
)


#-----------Figur---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
  FigTypUt <- rapFigurer::figtype(outfile)
  farger <- FigTypUt$farger
  plot.new()
  if (!is.null(dim(RegData))) { #>0
    tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
  } else {tekst <- 'Ingen registrerte data for dette utvalget'}
  title(main=Tittel)
  text(0.5, 0.6, tekst, cex=1.2)
  legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
  if ( outfile != '') {dev.off()}

} else {

  #--------------------------FIGUR---------------------------------------------------
  #Innparametre: ...
  #----------- Figurparametre ------------------------------
  cexShNavn <- 0.9 #0.85

  FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=fargepalett)
  farger <- FigTypUt$farger
  #Tilpasse marger for å kunne skrive utvalgsteksten
  NutvTxt <- length(utvalgTxt)
  vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.8)
  #NB: strwidth oppfører seg ulikt avh. av device...
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

  xmax <- min(max(AndelerGrSort, na.rm=T),100)*1.15
  #paste0(GrNavnSort,' (',Ngrtxt , ')')
  pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4],
                     xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                     las=1, cex.names=cexShNavn*0.9))
  posOver <- max(pos)+0.7

  #Legge på målnivå, Rygg

  #Legge på målnivå
  if (!is.na(KImaalGrenser[1])) {
    antMaalNivaa <- length(KImaalGrenser)-1
    rekkef <- 1:antMaalNivaa
    if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
    fargerMaalNiva <-  c('#4fc63f', '#fbf850', '#c6312a')[rekkef] #c('green','yellow')# #c('#ddffcc', '#ffffcc') #, '#fff0e6') #Grønn, gul, rød
    tetth <- c(100, 70,20)[rekkef]
    maalOppTxt <- c('Høy', 'Moderat til lav', 'Lav')[rekkef]
    if (antMaalNivaa==3) {maalOppTxt[2] <- 'Moderat' }
    rect(xleft=KImaalGrenser[1:antMaalNivaa], ybottom=0, xright=KImaalGrenser[2:(antMaalNivaa+1)],
         ytop=max(pos)+0.4, col = fargerMaalNiva[1:antMaalNivaa],
         density = tetth, angle = 60, border = NA)

    # legPos <- ifelse(AntGr < 31, ifelse(AntGr < 15, -1, -2.5), -3.5)
    legend(x=1, y=posOver, yjust = 0.5, #x=xmax, y=posOver, xjust=1, yjust=0,
           ncol=antMaalNivaa+1,
           density = c(NA, tetth),
           angle = c(NA,rep(60, antMaalNivaa)),
           fill=c('white', fargerMaalNiva[1:antMaalNivaa]),
           xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
           legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,

    # rect(xleft=KImaalGrenser[1:antMaalNivaa], ybottom=0, xright=KImaalGrenser[2:(antMaalNivaa+1)],
    #      ytop=max(pos)+0.5, col = fargerMaalNiva[1:antMaalNivaa], border = NA)
    # legend(x=1, y=posOver, yjust = 0.5,
    #        pch=c(NA,rep(15, antMaalNivaa)), col=c(NA, fargerMaalNiva[1:antMaalNivaa]),
    #        ncol=antMaalNivaa+1,
    #        xpd=TRUE, border=NA, box.col='white',cex=0.8, pt.cex=1.5,
    #        legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,
  }
  pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
                     xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                     las=1, cex.names=cexShNavn*0.9, add=T))
  mtext('Andel (%)', side=1, line=2)
  ybunn <- 0.1
  ytopp <- max(pos)+0.5
  #Linje for hele landet/utvalget:
  lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
  legend(x=max(AndelerGrSort, na.rm = T), y=posOver+0.5, xjust=1, yjust = 0.5,
         cex=0.95, lwd=2, col=farger[2],
         legend=paste0('Hele landet', ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
         bty='o', bg='white', box.col='white')
  mtext(at=pos+max(pos)*0.0045, paste0(GrNavnSort,' (',Ngrtxt , ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg


  title(Tittel, line=1, font.main=1, cex.main=1.3)

  text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
       las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

  mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=1, adj=1, line=0.25)

  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}
  #----------------------------------------------------------------------------------
}
return(invisible(FigDataParam))
}

