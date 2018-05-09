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
#' @inheritParams NakkeFigAndeler
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


NakkeFigGjsnTid <- function(RegData, outfile='', valgtVar, erMann='',
		minald=0, maxald=130, datoFra='2007-01-01', datoTil='3000-01-01',
		myelopati=99, fremBak=0, tidsenhet='Aar',
		valgtMaal='', enhetsUtvalg=0, hentData=0, preprosess=TRUE, reshID=0){ #tittel=1,


	if (hentData == 1) {
		RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
	  }

# Preprosessere data
     if (preprosess){
       RegData <- NakkePreprosess(RegData=RegData)
     }


#----------- Figurparametre ------------------------------
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1
#!!!Alt av utvalg basert på enhetsUtvalg kan evt. inngå i NakkeLibUtvalg. Må da returnere to datasett...

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
# reshID <- as.numeric(reshID)
# indEgen1 <- match(reshID, RegData$ReshId)
# if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
# 	}

NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnTid')
RegData <- NakkeVarSpes$RegData
sortAvtagende <- NakkeVarSpes$sortAvtagende
varTxt <- NakkeVarSpes$varTxt
KIekstrem <- NakkeVarSpes$KIekstrem
KImaal <- NakkeVarSpes$KImaal
KImaaltxt <- NakkeVarSpes$KImaaltxt
ytxt1 <- NakkeVarSpes$ytxt1

#Gjør utvalg
NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann, myelopati=myelopati, fremBak=fremBak, enhetsUtvalg=enhetsUtvalg, reshID = reshID)	#, tidlOp=tidlOp
RegData <- NakkeUtvalg$RegData
utvalgTxt <- NakkeUtvalg$utvalgTxt
ind <- NakkeUtvalg$ind
hovedgrTxt <- NakkeUtvalg$hovedgrTxt
medSml <- NakkeUtvalg$medSml

t1 <-ifelse(valgtMaal=='Med', 'Median', 'Gjennomsnittlig')
tleg <- ifelse(valgtMaal=='Med', 'Median', 'Gjennomsnitt')
tittel <-  c(paste(t1, NakkeVarSpes$deltittel, sep=' '), hovedgrTxt)	#c(TittelVar, hovedkattxt, paste(kjtxt, ', ', optxt, sep=''), hovedgrTxt)
#if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}



if (length(ind$Hoved)<5 | ((medSml == 1) & (length(ind$Rest) < 5))) {
    #-----------Figur---------------------------------------
figtype(outfile)
	tekst <- 'Færre enn 5 registreringer i egen eller sammenligningsgruppa'
	plot.new()
	title(main=tittel)
	text(0.5, 0.5, tekst,cex=1.2)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {

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
AntTidsenh <- length(tidtxt)
#--------------------------------
#Aartxt <- min(RegData$Aar):max(RegData$Aar)
#RegData$Aar <- factor(RegData$Aar, levels=Aartxt)
#AntAar <- length(Aartxt)


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
	KonfRest[ ,which(is.na(KonfRest[1,]))] <- KIekstrem
}
#-----------Figur---------------------------------------
xskala <- 1:length(tidtxt)
xmax <- max(xskala)
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnitt '}
ytxt <- paste0(maaltxt, ytxt1)

#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))

farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]

plot(xskala, Midt, xlim=c(0.9,xmax+0.1), ylim=c(ymin,ymax), type='n', frame.plot=FALSE, col='white',
		ylab=c(ytxt,'med 95% konfidensintervall'),
		xlab='Operasjonsår', xaxt='n',
		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = xskala, labels = tidtxt)
#Sammenlikning:
if (medSml==1) {
  xverdi <- c(xskala, xskala[AntTidsenh:1]) #c(xskala[1:9], xskala[(AntTidsenh-1):1]) #c(xskala, xskala[AntTidsenh:1])
  yverdi <- c(KonfRest[1,], KonfRest[2,AntTidsenh:1]) #c(KonfRest[1,1:9], KonfRest[2,(AntTidsenh-1):1]) #c(KonfRest[1,], KonfRest[2,AntTidsenh:1])
	polygon(x=xverdi, y=yverdi,
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste0('95% konfidensintervall for ', NakkeUtvalg$smltxt, ', N=', sum(NRest, na.rm=T)))
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(N, na.rm=T), cex=cexgr)/2	#length(tidtxt)/30
rect(xskala-b, Midt-h, xskala+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(xskala, Midt, N, col=fargeHovedRes, cex=cexgr)

#Konfidensintervall:
ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=xskala, y0=Midt-h, x1=xskala, length=0.08, code=2, angle=90,
		y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=xskala, y0=Midt+h, x1=xskala, y1=replace(Konf[2, ], ind, Midt[ind]+h),
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

#if (tittel==1) {
  title(main=tittel, font.main=1, line=1)
#Tekst som angir hvilket utvalg som er gjort
#if (length(utvalgTxt)>0) {
  #mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3-(1-tittel)+0.8*((NutvTxt-1):0)))}
  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

if ( outfile != '') {dev.off()}

ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
rownames(ResData) <- c('Midt', 'KIned', 'KIopp', 'MidtRest', 'KIRestned', 'KIRestopp')[1:(3*(medSml+1))]
UtData <- list(paste0(toString(tittel),'.'), ResData )
names(UtData) <- c('Tittel', 'Data')
return(invisible(UtData))

}	#end if statement for 0 observations
}	#end function
