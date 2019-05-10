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

NakkeFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2012-01-01', datoTil='3000-12-31',
                            minald=0, maxald=130, erMann='', myelopati=99, fremBak=0, Ngrense=10,
                            hentData=0, preprosess=TRUE, reshID=0, outfile='') { #tittel=1,

     if (hentData == 1) {
          RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
     }

     # Preprosessere data
     if (preprosess){
          RegData <- NakkePreprosess(RegData=RegData)
     }

    '%i%' <- intersect
     #----------- Figurparametre ------------------------------
     cexShNavn <- 1 #0.85

     grVar <- 'SykehusNavn'
     RegData[ ,grVar] <- factor(RegData[ ,grVar])
     #Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist


     NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
     RegData <- NakkeVarSpes$RegData
     sortAvtagende <- NakkeVarSpes$sortAvtagende
     tittel <- NakkeVarSpes$tittel

     #Gjør utvalg
     NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                   erMann=erMann, myelopati=myelopati, fremBak=fremBak)
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
     Ngrtxt <- paste0('\n(N=', as.character(Ngr),')')	#
     Ngrtxt[indGrUt] <- paste0('\n(<', Ngrense,')')	#paste(' (<', Ngrense,')',sep='')	#
	Ngr <- Ngr[sortInd]
	Nvar <- Nvar[sortInd]

   AggVerdier <- list(Hoved = NULL, Tot =NULL)
    AndelerGrSort <- AndelerGr[sortInd]
  AggVerdier$Hoved <- AndelerGrSort
  #AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
  AggVerdier$Tot <- round(100*sum(RegData$Variabel)/N, 2)
     #	GrNavnSort <- paste(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd], sep='')
     GrNavnSort <- paste0(names(Ngr)[sortInd], Ngrtxt[sortInd]) #names(Ngr)[sortInd]

     andeltxt <- paste0(sprintf('%.1f',AndelerGrSort), '%') 	#round(as.numeric(AndelerGrSort),1)
     if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}

     #if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}
     FigDataParam <- list(AggVerdier=AggVerdier,
                          N=N,
                          Ngr=Ngr,
                          Nvar=Nvar,
                          #KImaal <- KImaal,
                          #soyletxt=soyletxt,
                          #grtxt2=grtxt2,
                          tittel=tittel,
                          andeltxt=andeltxt,
                          #xAkseTxt=xAkseTxt,
                          #yAkseTxt=yAkseTxt,
                          utvalgTxt=NakkeUtvalg$utvalgTxt,
                          fargepalett=NakkeUtvalg$fargepalett
                          #medSml=NakkeUtvalg$medSml
                          #hovedgrTxt=hovedgrTxt,
                          #smltxt=RyggUtvalg$smltxt
     )

     #-----------Figur---------------------------------------
     if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
          FigTypUt <- figtype(outfile)
          farger <- FigTypUt$farger
          plot.new()
          if (dim(RegData)[1]>0) {
               tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
          } else {tekst <- 'Ingen registrerte data for dette utvalget'}
          title(main=tittel)
          text(0.5, 0.6, tekst, cex=1.2)
          legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
          if ( outfile != '') {dev.off()}

     } else {

          #--------------------------FIGUR---------------------------------------------------
          #Innparametre: ...


          FigTypUt <- figtype(outfile, height=3*800, fargepalett=NakkeUtvalg$fargepalett)
                  #Påvirker ikke filtype ''
          farger <- FigTypUt$farger
          #Tilpasse marger for å kunne skrive utvalgsteksten
          NutvTxt <- length(utvalgTxt)
          vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
          #NB: strwidth oppfører seg ulikt avh. av device...
          par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

          xmax <- min(max(AndelerGrSort),100)*1.15
          pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=tittel,
                         xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=cexShNavn*0.9)
          ybunn <- 0.1
          ytopp <- pos[AntGr]+1	#-length(indGrUt)]
          lines(x=rep(AggVerdier$Tot, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
          legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
                 legend=paste0('Hele landet', ' (', sprintf('%.1f',AggVerdier$Tot), '%), ', 'N=', N),
                 bty='o', bg='white', box.col='white')
          mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
          #text(x=0.005*xmax, y=pos, Ngrtxt[sortInd], las=1, cex=cexShNavn, adj=0, col=farger[4], lwd=3)	#c(Nshtxt[sortInd],''),
          title(tittel, line=1, font.main=1, cex.main=1.3)

          text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
               las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

          #mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)

          #Tekst som angir hvilket utvalg som er gjort
          mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))
          #mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


          par('fig'=c(0, 1, 0, 1))
          if ( outfile != '') {dev.off()}
          #----------------------------------------------------------------------------------
     }
     return(invisible(FigDataParam))
}
