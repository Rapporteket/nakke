#' Tidsutvikling av gjennomsnitt/median for en gitt variabel
#'
#' Gjennomsnitt/median per år med konfidensintervall for valgt variabel.
#' Hvis man har valgt å sammenlikne, vises  konfidensintervall for resten av landet (evt. annen
#' sammenlikningsgruppe) i bakgrunnen.
#'
#' Smerte: Skalaen går fra 0 til 10, dvs. at differansen ligger mellom -10 og 10..
#' EQ5D: Skala fra -0.594 tl 1, jo høyere jo bedre.
#' Oswestry: Skala fra 0 til 100, hvor lavest er friskest
#'
#' @param valgtMaal Sentralmål: 'Med' gir median, alt annet gir gjennomsnitt
#' @param konfInt konfidensintervall 1: ja (standard), 0: nei
#' @inheritParams NakkeFigAndeler
#' @inheritParams NakkeUtvalgEnh
#' @param valgtVar - Variabelen det skal vises resultat for.
#'             Alder: alder (år)
#'             EMSendr12mnd: Forbedring av EMS hos myelopati-pasienter, 12 mnd.
#'             EMSendr3mnd: Forbedring av EMS hos myelopati-pasienter, 3 mnd.
#'             EQ5Dendr12mnd: Forbedring av EQ5D, 12 mnd.
#'             EQ5Dendr3mnd: Forbedring av EQ5D, 3 mnd.
#'             Eq5DScorePreOp: EQ5D, før operasjon
#'             KnivtidTotalMin: total knivtid
#'             LiggeDognPostop: liggetid etter operasjon
#'             LiggeDognTotalt: antall liggedøgn, totalt
#'             NDIscorePreOp: NDI før operasjon
#'             NDIendr3mnd: Forbedring av NDI, 3 mnd. etter operasjon
#'             NDIendr12mnd: Forbedring av NDI, 12 mnd. etter operasjon
#'
#'@return Figur med...
#'
#' @export


NakkeFigGjsnTid <- function(RegData, outfile='', valgtVar='Alder', erMann='',
		minald=0, maxald=130, datoFra='2007-01-01', datoTil='3000-01-01',
		myelopati=99, fremBak=0, tidsenhet='Aar', inngrep=99, konfInt=1,
		valgtMaal='', enhetsUtvalg=0, hentData=0, preprosess=0, reshID=0,...){ #tittel=1,

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('NakkeFigGjsnTid: ',valgtVar))
  }
	if (hentData == 1) {
		RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
	  }

# Preprosessere data
     if (preprosess==1){
       RegData <- NakkePreprosess(RegData=RegData)
     }


#----------- Figurparametre ------------------------------
# subtxt <- ''	#Benevning
# antDes <- 1

NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnTid')
RegData <- NakkeVarSpes$RegData
#varTxt <- NakkeVarSpes$varTxt
KIekstrem <- NakkeVarSpes$KIekstrem
# KImaal <- NakkeVarSpes$KImaal
# KImaaltxt <- NakkeVarSpes$KImaaltxt
ytxt1 <- NakkeVarSpes$ytxt1

#Gjør utvalg
NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, inngrep = inngrep,
                              minald=minald, maxald=maxald, erMann=erMann, myelopati=myelopati,
                              fremBak=fremBak, enhetsUtvalg=enhetsUtvalg, reshID = reshID)	#, tidlOp=tidlOp
RegData <- NakkeUtvalg$RegData
utvalgTxt <- NakkeUtvalg$utvalgTxt
ind <- NakkeUtvalg$ind
hovedgrTxt <- NakkeUtvalg$hovedgrTxt
medSml <- NakkeUtvalg$medSml

t1 <-ifelse(valgtMaal=='Med', 'Median', 'Gjennomsnittlig')
#tleg <- ifelse(valgtMaal=='Med', 'Median', 'Gjennomsnitt')
tittel <-  c(paste(t1, NakkeVarSpes$deltittel, sep=' '), hovedgrTxt)	#c(TittelVar, hovedkattxt, paste(kjtxt, ', ', optxt, sep=''), hovedgrTxt)
#if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}

#------------------------Klargjøre tidsenhet--------------
N <- list(Hoved = dim(RegData)[1], Rest=0)
Ngr <- list(Hoved=N, Rest=NULL)
grtxt <- NULL
ResData <- NA

if (N$Hoved>4) {
  RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
  RegData <- RegDataFunk$RegData
  #tidtxt <- RegDataFunk$tidtxt
  tidNum <- min(RegData$TidsEnhetSort, na.rm=T):max(RegData$TidsEnhetSort, na.rm = T) #as.numeric(levels(RegData$TidsEnhetSort))

  #Resultat for hovedgruppe
  N <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], length)
  if (valgtMaal=='Med') {
    MedIQR <- plot(RegData$TidsEnhet[ind$Hoved],RegData$Variabel[ind$Hoved],  notch=TRUE, plot=FALSE)
    Midt <- as.numeric(MedIQR$stats[3, ])	#as.numeric(MedIQR$stats[3, sortInd])
    Konf <- MedIQR$conf
    #Hvis vil bruke vanlige konf.int:
    #j <- ceiling(N/2 - 1.96*sqrt(N/4))
    #k <- ceiling(N/2 + 1.96*sqrt(N/4))
    #KIHele <- sort(RegData$Variabel)[c(j,k)]
    #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
    #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
    #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
    #roughly a 95% confidence interval for the difference in two medians.
  } else {	#Gjennomsnitt blir standard.
    Midt <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], mean)
    SD <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], sd)
    Konf <- rbind(Midt - 2*SD/sqrt(N), Midt + 2*SD/sqrt(N))
  }
  Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
  Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])
  Ngr$Hoved <- N

  #Resten (gruppa det sammenliknes mot)
  MidtRest <- NULL
  KonfRest <- NULL
  if (medSml ==  1) {
    NRest <- tapply(RegData[ind$Rest ,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
    if (valgtMaal=='Med') {
      MedIQRrest <- plot(RegData$TidsEnhet[ind$Rest],RegData$Variabel[ind$Rest],  notch=TRUE, plot=FALSE)
      MidtRest <- as.numeric(MedIQRrest$stats[3, ])
      KonfRest <- MedIQRrest$conf
    } else {
      MidtRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], mean)	#ind$Rest
      SDRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], sd)
      NRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
      KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(NRest), MidtRest + 2*SDRest/sqrt(NRest))
    }
    KonfRest <- replace(KonfRest, which(KonfRest < KIekstrem[1]), KIekstrem[1])
    KonfRest <- replace(KonfRest, which(KonfRest > KIekstrem[2]), KIekstrem[2])
    indDum <- which(is.na(KonfRest[1,]))
    if (length(indDum)>0) {KonfRest[,indDum] <- KIekstrem}
    Ngr$Rest <- NRest
    }

  if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnitt '}
  grtxt <- levels(RegData$TidsEnhet)
ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 2)
rownames(ResData) <- c(maaltxt, 'KImin', 'KImaks',
                       paste0(maaltxt, 'Resten'), 'KImin, Resten', 'KImaks, Resten')[1:(3*(medSml+1))]
#rownames(ResData) <- c('Midt', 'KIned', 'KIopp', 'MidtRest', 'KIRestned', 'KIRestopp')[1:(3*(medSml+1))]
}
UtData <- list(AggVerdier=ResData,
                     N=N,
                     Ngr=Ngr,
                     grtxt=grtxt,
                     tittel=NakkeVarSpes$tittel,
                     utvalgTxt=utvalgTxt,
                     fargepalett=NakkeUtvalg$fargepalett,
                     medSml=medSml,
                     hovedgrTxt=NakkeUtvalg$hovedgrTxt,
                     smltxt=NakkeUtvalg$smltxt)
#-----------Figur---------------------------------------
#Plottspesifikke parametre:
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]
NutvTxt <- length(utvalgTxt)
#Tilpasse marger for å kunne skrive utvalgsteksten
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))

if (length(ind$Hoved)<5 | ((medSml == 1) & (length(ind$Rest) < 5))) {
    #-----------Figur---------------------------------------
rapFigurer::figtype(outfile)
	tekst <- 'Færre enn 5 registreringer i egen eller sammenligningsgruppa'
	plot.new()
	title(main=tittel)
	text(0.5, 0.5, tekst,cex=1.2)	#, family="sans")
	#Tekst som angir hvilket utvalg som er gjort
	mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))

	if ( outfile != '') {dev.off()}

	} else {

#xskala <- 1:length(tidtxt)
xmax <- max(tidNum) # max(xskala)
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
ytxt <- paste0(maaltxt, ytxt1)


plot(tidNum, Midt,  xlim=c(0.9,xmax+0.1), ylim=c(ymin,ymax), type='n', frame.plot=FALSE, col='white',
     ylab=c(ytxt,'med 95% konfidensintervall'),
		xlab='Operasjonsår', xaxt='n',
		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = tidNum, labels = levels(RegData$TidsEnhet))

#Sammenlikning:

if (medSml==1) {
  AntTidsenh <- max(which(!is.na(KonfRest[1,])))
  xverdi <- c(tidNum, tidNum[AntTidsenh:1]) #c(xskala[1:9], xskala[(AntTidsenh-1):1]) #c(xskala, xskala[AntTidsenh:1])
  yverdi <- c(KonfRest[1,], KonfRest[2,AntTidsenh:1]) #c(KonfRest[1,1:9], KonfRest[2,(AntTidsenh-1):1]) #c(KonfRest[1,], KonfRest[2,AntTidsenh:1])
	polygon(x=xverdi, y=yverdi,
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste0('95% konfidensintervall for ', NakkeUtvalg$smltxt, ', N=', sum(NRest, na.rm=T)))
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(N, na.rm=T), cex=cexgr)/2	#length(tidtxt)/30
rect(tidNum-b, Midt-h, tidNum+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(tidNum, Midt, N, col=fargeHovedRes, cex=cexgr)

#Konfidensintervall:
ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=tidNum, y0=Midt-h, x1=tidNum, length=0.08, code=2, angle=90,
		y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=tidNum, y0=Midt+h, x1=tidNum, y1=replace(Konf[2, ], ind, Midt[ind]+h),
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

#if (tittel==1) {
title(tittel, line=1, font.main=1, cex.main=1.3)
#Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))

if ( outfile != '') {dev.off()}

}	#end if statement for 0 observations

return(invisible(UtData))
}	#end function
