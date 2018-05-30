#' Søylediagram, horisontalt eller vertikalt, som viser andeler av valgt variabel.
#'
#' Søylediagrammet viser fordelinga til den valgte variabelen. Søylene er horisontale eller vertikale
#' avhengig av hvor stor plass kategorinavnet til søyla tar.
#'
#' @param RegData Dataramme med alle nødvendige variable fra registeret
#' @param outfile Navn på fil figuren skrives ned til
#' @param reshID Avdelingsid (reshID) for egen avdeling,
#' @param hentData Angir om funksjonen skal kjøre spørring for å hente data eller ikke.
#'					0: ikke kjør (standard)
#'					1: kjør
#' @param preprosess Skal data preprosesseres, dvs. gjøre standard omregning av variable og beregne nye.
#'						TRUE (standard) / FALSE
#' @inheritParams NakkeUtvalgEnh
#' @param valgtVar Variabelen det skal vises resultat for.
#'             Alder: Aldersfordeling
#'             AntallNivaaOpr: Antall nivå operert
#'             Antibiotika: Er det gitt antibiotikaprofylakse?
#'             Arbeidstaus12mnd: Arbeidsstatus 12 mnd. etter operasjon
#'             Arbeidstaus3mnd: Arbeidsstatus 3 mnd. etter operasjon
#'             ArbeidstausPreOp: Arbeidsstatus før operasjon
#'             ASAgrad: ASA-grad
#'             BMI: Pasientenes BMI (Body Mass Index)
#'             EqAngstPreOp: Helsetilstand, Angst
#'             ErstatningPreOp: Søkt erstatning?
#'             FornoydBeh12mnd: Fornøydhet med behandlinga på sykehuset, 12 mnd
#'             FornoydBeh3mnd: Fornøydhet med behandlinga på sykehuset, 3 mnd
#'             OperasjonsKategori: Hastegrad
#'             LiggeDognPostop: Antall liggedøgn postoperativt
#'             LiggeDognTotalt: Totalt antall liggedøgn
#'             Morsmal: Morsmål
#'             NytteOpr12mnd: Nytte av operasjon, 12 mnd
#'             NytteOpr3mnd: Nytte av operasjon, 3 mnd
#'             OprIndikPareseGrad: Paresegrad før operasjon
#'             Roker: Røyker pasienten?
#'             Saardren: Har pasienten fått sårdren?
#'             SivilStatus: Sivilstatus
#'             SmertestillBrukPreOp: Hyppighet av smertestillende før operasjonen
#'             Snuser: Snuser pasienten?
#'             SymptVarighetArmer: Varighet av utstrålende armsmerter
#'             SymptVarighetNakkeHode: Varighet av nakke-/hodesmerter
#'             TidlOpr: Er pasienten tidligere operert?
#'             TidlOprAntall: Antall tidligere operasjoner
#'             UforetrygdPreOp: Søkt uføretrygd?
#'             Utdanning: Utdanningsnivå
#'
#' Detajer...:
#' @inheritParams NakkeUtvalgEnh
#'
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export

NakkeFigAndeler  <- function(RegData, valgtVar, datoFra='2012-01-01', datoTil='3000-12-31',
		minald=0, maxald=110, erMann='', myelopati=99, fremBak=0, outfile='',
		hentData=0, preprosess=TRUE, reshID=0, enhetsUtvalg=0)
{

	if (hentData == 1) {
		RegData <- NakkeRegDataSQL(datoFra=datoFra, datoTil=datoTil)
	  }

# Preprosessere data
     if (preprosess){
       RegData <- NakkePreprosess(RegData=RegData)
     }


#----------- Figurparametre ------------------------------
retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
xaksetxt <- ''	#Benevning
antDes <- 1
NB <- ''

#--------------- Definere variable ------------------------------
NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andeler')
RegData <- NakkeVarSpes$RegData
sortAvtagende <- NakkeVarSpes$sortAvtagende
tittel <- NakkeVarSpes$tittel
flerevar <- NakkeVarSpes$flerevar



#------------Gjøre utvalg-------------------------
NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann, myelopati=myelopati, fremBak=fremBak, enhetsUtvalg=enhetsUtvalg, reshID = reshID)
RegData <- NakkeUtvalg$RegData
utvalgTxt <- NakkeUtvalg$utvalgTxt
hovedgrTxt <- NakkeUtvalg$hovedgrTxt


#--------------- Gjøre beregninger ------------------------------
#FRA INTENSIV

      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = 0, Rest =0)
      Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
      Ngr <- list(Hoved = 0, Rest =0)
      ind <- NakkeUtvalg$ind
	  variable <- NakkeVarSpes$variable

      Ngr$Hoved <- switch(as.character(flerevar),
                          '0' = table(RegData$VariabelGr[ind$Hoved]),
                          '1' = apply(RegData[ind$Hoved,variable], MARGIN=2,
                                      FUN=function(x) sum(x == 1, na.rm=T)))
      #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
     N$Hoved <- switch(as.character(flerevar),
                        '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                        '1' = apply(RegData[ind$Hoved,variable], MARGIN=2,
                                 FUN=function(x) sum(x %in% 0:1, na.rm=T)))
          AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved

      if (NakkeUtvalg$medSml==1) {
           Ngr$Rest <- switch(as.character(flerevar),
                               '0' = table(RegData$VariabelGr[ind$Rest]),
                               '1' = apply(RegData[ind$Rest,variable], MARGIN=2,
                                           FUN=function(x) sum(x == 1, na.rm=T)))
            N$Rest <- switch(as.character(flerevar),
                             '0' = sum(Ngr$Rest),
                             '1' = apply(RegData[ind$Rest,variable], MARGIN=2,
                                   FUN=function(x) sum(x %in% 0:1, na.rm=T)))
            AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
      }

      if(flerevar==1) {
            Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                                 min(N$Hoved[1]),
                                 paste0(min(N$Hoved),'-',max(N$Hoved)))
            Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                                min(N$Rest[1]),
                                paste0(min(N$Rest),'-',max(N$Rest)))
      } else {
            Nfig <- N}



      xAkseTxt <- NakkeVarSpes$xAkseTxt
      yAkseTxt <- 'Andel opphold (%)'
      retn <- NakkeVarSpes$retn
      tittel <- NakkeVarSpes$tittel
      hovedgrTxt <- NakkeUtvalg$hovedgrTxt
      medSml <- NakkeUtvalg$medSml
      grtxt <- NakkeVarSpes$grtxt
      cexgr <- NakkeVarSpes$cexgr
      grTypeTxt <- NakkeUtvalg$grTypeTxt
      smltxt <- NakkeUtvalg$smltxt
      KImaal <- NakkeVarSpes$KImaal
      fargepalett <- NakkeUtvalg$fargepalett


      antDes <- if (valgtVar == 'KomplOpr') {2} else {1}
      #grtxt2 <- paste0(sprintf(paste('%.', antDes, 'f'),AggVerdier$Hoved), '%')
      NutvTxt <- length(utvalgTxt)
      antDesTxt <- paste0('%.', antDes, 'f')
      #txtpst <- paste0(' (', rev(sprintf(antDesTxt, AggVerdier$Hoved)), '%)')
      txtpst <- paste0(' (', sprintf(antDesTxt, AggVerdier$Hoved), '%)')
      grtxtpst <- paste0(rev(grtxt),  rev(txtpst))   #sprintf("%.3f", pi)

#SKILLE UT FIGURDELEN SOM EGEN FUNKSJON???????
#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ( Nfig$Hoved %in% 1:5 | 	(NakkeUtvalg$medSml ==1 & Nfig$Rest<10)) {	#(valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) |
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
	if ( outfile != '') {dev.off()}

} else {

#-----------Figur---------------------------------------
#Innparametre: xaksetxt, grtxt, grtxt2, tittel, AggVerdier, utvalgTxt, retn, cexgr
cexgr <- 1	#Kan endres for enkeltvariable


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.025*(NutvTxt-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (NakkeVarSpes$retn == 'H') {
	xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
	xmax <- min(xmax, 100)
	ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved)

	pos <- barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, las=1, xlab="Andel pasienter (%)", #beside=TRUE, main=tittel,
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(ymin,ymax))	#
    #Intensiv: pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	#				   xlab=xAkseTxt, horiz=T, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
	if (Nfig$Hoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

	if (NakkeUtvalg$medSml == 1) {
		points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
		legend('top', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
						paste0(smltxt, ' (N=', Nfig$Rest,')')),
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {
		legend('top', paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (NakkeVarSpes$retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
		xlab=xaksetxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=xaksetxt,
	#if (length(grtxt2) == 1) {grtxt2 <- txtpst}
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, txtpst, side=1, las=1, cex=0.9*cexgr, adj=0.5, line=1.5)
if (NakkeUtvalg$medSml == 1) {
	points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
	legend('top', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), paste0(smltxt, ' (N=', Nfig$Rest,')')),
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {
	legend('top', paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
}

title(tittel, line=1, font.main=1, cex.main=1.3)

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
}

#Beregninger som returneres fra funksjonen.
AggVerdierUt <- rbind(AggVerdier$Hoved, AggVerdier$Rest)
rownames(AggVerdierUt) <- c('Hoved', 'Rest')
AntallUt <- rbind(N$Hoved, N$Rest)
rownames(AntallUt) <- c('Hoved', 'Rest')

UtData <- list(paste0(toString(tittel),'.'), AggVerdierUt, AntallUt, grtxt )
names(UtData) <- c('tittel', 'AggVerdier', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
