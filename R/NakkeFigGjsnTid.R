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


FigGjsnTid <- function(RegData, outfile, valgtVar, erMann='',
		minald=0, maxald=130, tittel=1, datoFra='2007-01-01', datoTil='3000-01-01',
		myelopati=99, fremBak=0,
		valgtMaal='', enhetsUtvalg=0, hentData=0, preprosess=TRUE, reshID=0){


	if (hentData == 1) {
		RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
	  }

# Preprosessere data
     if (preprosess){
       RegData <- NakkePreprosess(RegData=RegData)
     }


#----------- Figurparametre ------------------------------

#retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1
#!!!Alt av utvalg basert på enhetsUtvalg kan evt. inngå i NakkeLibUtvalg. Må da returnere to datasett...

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
	}

if (length(grep('endr', valgtVar)) != 1 ) {	#Søke på strengen "endr"
	RegData$Variabel <- RegData[ ,valgtVar]
}

if (valgtVar=='Eq5DScorePreOp') {
     #Pasientkjema.
     KIekstrem <- c(-0.6, 1)
     indVar <- which(RegData[ , valgtVar] >= KIekstrem[1])
     indSkjema <- which(RegData$PasientSkjemaStatus==1)
     RegData <- RegData[intersect(indVar, indSkjema), ]
     TittelVar <- 'EQ5D, før operasjon'
     ytxt1 <- '(EQ5D-skåring)'
}
if (valgtVar=='KnivtidTotalMin') {
		#Legeskjema.
		RegData <- RegData[which(RegData[ ,valgtVar]>0), ]
		KIekstrem <- c(0, 500)
		TittelVar <- 'Total knivtid'
		ytxt1 <- '(minutter)'
		}

	if (valgtVar=='LiggeDognPostop') {
		#Legeskjema.
		RegData <- RegData[which(RegData[ ,valgtVar]>-1), ]
		KIekstrem <- c(0, 30)
		TittelVar <- 'Antall liggedøgn postoperativt'
		ytxt1 <- '(døgn)'
		}

	if (valgtVar=='LiggeDognTotalt') {
		#Legeskjema.
		RegData <- RegData[which(RegData[ ,valgtVar]>-1), ]
		KIekstrem <- c(0, 30)
		TittelVar <- 'Antall liggedøgn totalt'
		ytxt1 <- '(døgn)'
		}
	if (valgtVar=='NDIscorePreOp') {
		#Pasientkjema.
		KIekstrem <- c(0,100)
		indVar <- which(RegData[ ,valgtVar] >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		TittelVar <- 'NDI før operasjon'
		ytxt1 <- '(NDI-skåring)'
		}
	if (valgtVar=='NDIendr3mnd') {
		#Pasientkjema og 3mndskjema. Lav skår, lite plager -> forbedring = nedgang.
		KIekstrem <- c(-100,100)
		RegData$Variabel <- RegData$NDIscorePreOp - RegData$NDIscore3mnd
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		TittelVar <- 'Forbedring av NDI, 3 mnd. etter operasjon'
		ytxt1 <- '(endring av NDI-skår)'
		}

	if (valgtVar=='NDIendr12mnd') {
		#Pasientkjema og 12mndskjema. Lav skår, lite plager -> forbedring = nedgang.
		KIekstrem <- c(-100,100)
		RegData$Variabel <- RegData$NDIscorePreOp - RegData$NDIscore12mnd
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		TittelVar <- 'Forbedring av NDI, 12 mnd. etter operasjon'
		ytxt1 <- '(endring av NDI-skår)'
		}


	if (valgtVar=='EMSendr12mnd') {
		#Pasientkjema og 12mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-18,18)
		RegData$Variabel <- RegData$EMSscore12mnd - RegData$EMSscorePreOp
		indMyelopati <- which(RegData$OprIndikMyelopati == 1)
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indMyelopati, intersect(indVar, indSkjema)), ]
		TittelVar <- 'Forbedring av EMS hos myelopati-pasienter, 12 mnd.'
		ytxt1 <- '(endring av EMS-skår)'
		}
	if (valgtVar=='EMSendr3mnd') {
		#Pasientkjema og 3mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-18,18)
		RegData$Variabel <- RegData$EMSscore3mnd - RegData$EMSscorePreOp
		indMyelopati <- which(RegData$OprIndikMyelopati == 1)
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indMyelopati, intersect(indVar, indSkjema)), ]
		TittelVar <- 'Forbedring av EMS hos myelopati-pasienter, 3 mnd.'
		ytxt1 <- '(endring av EMS-skår)'
		}
	if (valgtVar=='EQ5Dendr3mnd') {
		#Pasientkjema og 3mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-1.6, 1.6)
		RegData$Variabel <- RegData$Eq5DScore3mnd - RegData$Eq5DScorePreOp
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		TittelVar <- 'Forbedring av EQ5D, 3 mnd.'
		ytxt1 <- '(endring av EQ5D-skår)'
		}
	if (valgtVar=='EQ5Dendr12mnd') {
		#Pasientkjema og 12mndskjema. Lav skår, mye plager -> Forbedring = økning.
		#Kun myelopati-pasienter
		KIekstrem <- c(-1.6, 1.6)
		RegData$Variabel <- RegData$Eq5DScore12mnd - RegData$Eq5DScorePreOp
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		TittelVar <- 'Forbedring av EQ5D, 12 mnd.'
		ytxt1 <- '(endring av EQ5D-skår)'
		}


#Gjør utvalg
NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann, myelopati=myelopati, fremBak=fremBak)	#, tidlOp=tidlOp
RegData <- NakkeUtvalg$RegData
utvalgTxt <- NakkeUtvalg$utvalgTxt


indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$SykehusNavn[indEgen1]) } else {
		shtxt <- 'Hele landet'
			}

if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
			medSml <- 0
			indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
			indRest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			indHoved <-which(as.numeric(RegData$ReshId)==reshID)
			smltxt <- 'landet forøvrig'
			indRest <- which(as.numeric(RegData$ReshId) != reshID)
			}

TittelUt <-  c(TittelVar, shtxt)	#c(TittelVar, hovedkattxt, paste(kjtxt, ', ', optxt, sep=''), shtxt)
if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}



if (length(indHoved)<5 | ((medSml == 1) & (length(indRest) < 5))) {
    #-----------Figur---------------------------------------
figtype(outfile)
	tekst <- 'Mindre enn 5 registreringer i egen eller sammenligningsgruppa'
	plot.new()
	title(main=Tittel)
	text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {


Aartxt <- min(RegData$Aar):max(RegData$Aar)
RegData$Aar <- factor(RegData$Aar, levels=Aartxt)
AntAar <- length(Aartxt)


#Resultat for hovedgruppe
N <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Aar'], length)
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData$Aar[indHoved],RegData$Variabel[indHoved],  notch=TRUE, plot=FALSE)
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
	Midt <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Aar'], mean)
	SD <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Aar'], sd)
	Konf <- rbind(Midt - 2*SD/sqrt(N), Midt + 2*SD/sqrt(N))
}
	Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
	Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])

#Resten (gruppa det sammenliknes mot)
MidtRest <- NULL
KonfRest <- NULL
if (medSml ==  1) {
NRest <- tapply(RegData[indRest ,'Variabel'], RegData[indRest, 'Aar'], length)
	if (valgtMaal=='Med') {
		MedIQRrest <- plot(RegData$Aar[indRest],RegData$Variabel[indRest],  notch=TRUE, plot=FALSE)
		MidtRest <- as.numeric(MedIQRrest$stats[3, ])
		KonfRest <- MedIQRrest$conf
	} else {
	MidtRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Aar'], mean)	#indRest
	SDRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Aar'], sd)
	NRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Aar'], length)
	KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(NRest), MidtRest + 2*SDRest/sqrt(NRest))
	}
	KonfRest <- replace(KonfRest, which(KonfRest < KIekstrem[1]), KIekstrem[1])
	KonfRest <- replace(KonfRest, which(KonfRest > KIekstrem[2]), KIekstrem[2])
}
#-----------Figur---------------------------------------
xmin <- Aartxt[1]-0.5
xmax <- max(Aartxt)+0.5
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnitt '}
ytxt <- paste(maaltxt, ytxt1, sep='')

#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))

farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]

plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
		#cex=0.8, cex.lab=0.9, cex.axis=0.9,
		ylab=c(ytxt,'med 95% konfidensintervall'),
		xlab='Operasjonsår', xaxt='n',
		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = Aartxt)
#Sammenlikning:
if (medSml==1) {
	polygon( c(Aartxt, Aartxt[AntAar:1]), c(KonfRest[1,], KonfRest[2,AntAar:1]),
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste('95% konfidensintervall for ', smltxt, ', N=', sum(NRest, na.rm=T), sep=''))
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(N, na.rm=T), cex=cexgr)/2	#length(Aartxt)/30
rect(Aartxt-b, Midt-h, Aartxt+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(Aartxt, Midt, N, col=fargeHovedRes, cex=cexgr)

#Konfidensintervall:
ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=Aartxt, y0=Midt-h, x1=Aartxt, length=0.08, code=2, angle=90,
		y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=Aartxt, y0=Midt+h, x1=Aartxt, y1=replace(Konf[2, ], ind, Midt[ind]+h),
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

if (tittel==1) {title(main=Tittel, font.main=1, line=1)}
#Tekst som angir hvilket utvalg som er gjort
if (length(utvalgTxt)>0) {
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3-(1-tittel)+0.8*((NutvTxt-1):0)))}

if ( outfile != '') {dev.off()}

ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
rownames(ResData) <- c('Midt', 'KIned', 'KIopp', 'MidtRest', 'KIRestned', 'KIRestopp')[1:(3*(medSml+1))]
UtData <- list(paste(toString(TittelUt),'.', sep=''), ResData )
names(UtData) <- c('Tittel', 'Data')
return(invisible(UtData))

}	#end if statement for 0 observations
}	#end function
