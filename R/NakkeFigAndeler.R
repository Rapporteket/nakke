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
#' @param datoFra Operasjonsdato, fra og med. Standard: '2012-01-01'
#' @param datoTil Operasjonsdato, til og med. Standard: '3000-01-01' (siste registreringsdato)
#' @param minald Alder, fra og med
#' @param maxald Alder, til og med
#' @param erMann Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param enhetsUtvalg Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
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
#'
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export

NakkeFigAndeler  <- function(RegData, valgtVar, datoFra='2012-01-01', datoTil='3000-12-31',
		minald=0, maxald=130, erMann='', myelopati=99, fremBak=0, outfile='',
		hentData=0, preprosess=TRUE, reshID, enhetsUtvalg=1)
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
flerevar <- 0
antDes <- 1
NB <- ''

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
	}

#--------------- Definere variable ------------------------------
NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andeler')
RegData <- NakkeVarSpes$RegData
sortAvtagende <- NakkeVarSpes$sortAvtagende
tittel <- NakkeVarSpes$tittel


#------------Gjøre utvalg-------------------------
NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann, myelopati=myelopati, fremBak=fremBak)
RegData <- NakkeUtvalg$RegData
utvalgTxt <- NakkeUtvalg$utvalgTxt



#Generere hovedgruppe og sammenlikningsgruppe
#Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
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
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$ReshId) != reshID)
    }
  }


#--------------- Gjøre beregninger ------------------------------
ikke <- 1
if (ikke == 0) {
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0

if (flerevar == 0 ) {
AntHoved <- table(RegData$VariabelGr[indHoved])
NHoved <- sum(AntHoved)
Andeler$Hoved <- 100*AntHoved/NHoved
	if (medSml==1) {
		AntRest <- table(RegData$VariabelGr[indRest])
		NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
		Andeler$Rest <- 100*AntRest/NRest
	}
}

#FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
if (valgtVar %in% c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
                    'OprIndikMyelopati', 'Radiologi')){
  flerevar <-  1
  utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
  RegDataLand <- RegData
  NHoved <-length(indHoved)
  NRest <- length(indRest)

  for (teller in 1:(medSml+1)) {
  #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
    RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]


     if (valgtVar=='OprIndik') {
         retn <- 'H'
         #OprIndiasjonasjonUfylt <>1 - tom variabel,
         #Svært få (ca 20 av 3000) har tom registrering. Setter derfor felles N lik alle reg.

         #indAnnet <- which(RegData$OprIndikAnnet == 1)
         #indPareser <- which(RegData$OprIndikParese == 1)
         indSmerterk <- which(RegData$OprIndikSmerter == 1)
         indMyelopati <- which(RegData$OprIndikMyelopati == 1)
         Nmyelopati <- sum(RegData$OprIndikMyelopati, na.rm=T)
         AntVar <- cbind(
              #length(indAnnet),
              Pareser = sum(RegData$OprIndikParese, na.rm=T), #length(indPareser),
              Myelopati = length(indMyelopati),
              Smerter = length(indSmerterk),
              SmerterMyelop = length(intersect(indMyelopati, indSmerterk)),
              Annet = sum(RegData$OprIndikAnnet, na.rm=T)
         )
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Pareser', 'Myelopati', 'Smerter', 'Sm. og Myelop.', 'Annet')
         tittel <- 'Operasjonsårsak'
    }

    if (valgtVar=='Radiologi') {
         retn <- 'H'
         #RadilogiUnderokelseUfylt  - tom variabel,
         #RadiologiRtgCcolFunkOpptak  - tom variabel,
         #Svært få har tom registrering. Setter derfor felles N lik alle reg.

         AntVar <- cbind(
              #length(indAnnet),
              CT = sum(RegData$RadiologiCt, na.rm=T), #length(indPareser),
              MR = sum(RegData$RadiologiMr, na.rm=T),
              Myelografi = sum(RegData$RadiologiMyelografi, na.rm=T),
              RontgenCcol = sum(RegData$RadiologiRtgCcol, na.rm=T)
         )
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('CT', 'MR', 'Myelografi', 'Røntgen-Ccol')
         tittel <- 'Radiologisk undersøkelse'
    }

    if (valgtVar=='Komorbiditet') {
         retn <- 'H'
          RegData <- RegData[which(RegData$AndreRelSykdommer>-1), ]
         RegData$SykdReumatisk <- 0
          indSykdReumatisk <- (RegData$SykdAnnenreumatisk ==1 | (RegData$SykdBechtrew==1 | RegData$SykdReumatoidartritt==1))
          RegData$SykdReumatisk[indSykdReumatisk] <- 1
         Variable <- c('SykdAnnenendokrin', 'SykdAnnet','SykdCarpalTunnelSyndr', 'SykdCerebrovaskular',
               'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHodepine', 'SykdHypertensjon', 'SykDiabetesMellitus',
              'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk', 'SykdKrSmerterMuskelSkjelSyst',
             'SykdOsteoporose', 'SykdSkulderImpigment', 'SykdWhiplashNakke')
         AntVar <- colSums (RegData[ ,c("SykdReumatisk", Variable, "AndreRelSykdommer")], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Annen Reumatisk', 'Annen endokrin', 'Andre', 'Carpal TS', 'Cerebrovaskulær', 'Depresjon/Angst',
         'Hjerte-/Karsykd.', 'Hodepine', 'Hypertensjon', 'Diabetes', 'Kreft', 'Kr. lungesykdom',
         'Kr. nevrologisk', 'Kr. muskel/skjelettsm.', 'Osteoporose', 'Skuldersyndrom', 'Whiplash/skade', 'Tot. komorb')

         tittel <- 'Komorbiditet'
    }

    if (valgtVar=='KomplOpr') {
         retn <- 'H'
         Variable <- c('PerOpKomplAnafylaksiI','PerOpKomplAnnet','PerOpKomplBlodning','PerOpKomplDurarift',
                       'PerOpKomplFeilplasseringImplant','PerOpKomplKardioVaskulare','PerOpKomplMedullaskade',
                      'PerOpKomplNerverotSkade','PerOpKomplAnnenNerveskade','PerOpKomplOpFeilNivaa',
                       'PerOpKomplRespiratorisk','PerOpKomplOsofagusSkade','PerOpEnhverKompl')

         AntVar <- colSums (RegData[ ,Variable], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Anafylaksi','Annet','Blødning','Durarift','Feilplassering, impl.','Kardiovaskulære','Medullaskade',
                    'Nerverotskade','Nerveskade','Op. feil nivå','Respiratorisk','Øsofagusskade','Komplikasjoner, alle')
         tittel <- 'Komplikasjoner ved operasjon'
    }

    if (valgtVar=='Kompl3mnd') {
         retn <- 'H'
         RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1), ]
         Variable <- c('KomplDVT3mnd', 'KomplinfekDyp3mnd', 'KomplLungeEmboli3mnd', 'KomplinfekOverfl3mnd',
                       'KomplPneumoni3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'KomplUVI3mnd', 'EnhverKompl3mnd')
         AntVar <- colSums (RegData[ ,Variable], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('DVT', 'Dyp infeksjon', 'Lungeemboli', 'Overfladisk infeksjon', 'Pneumoni',
                    'Stemmevansker', 'Svelgvansker', 'UVI', 'Totalt, 3 mnd.')
         tittel <- 'Komplikasjoner etter operasjon'
    }


  if (valgtVar=='OprIndikSmerter') {
  	retn <- 'H'
  	indSmerteArm <- which(RegData$OprIndikSmerteLokArm == 1)
  	indSmerteNakke <- which(RegData$OprIndikSmerteLokNakke == 1)
  	Nsmerte <- sum(RegData$OprIndikSmerter, na.rm=T)
  	AntVar <- cbind(
  		Smerte = Nsmerte,
  		SmerteArm = length(indSmerteArm),
  		SmerteNakke = length(indSmerteNakke),
  		SmerteArmNakke = length(intersect(indSmerteArm, indSmerteNakke))
  	)
  	NVar<- cbind(
  		Smerte = length(which(RegData$OprIndikSmerter > -1)),
  		SmerteArm = Nsmerte,
  		SmerteNakke = Nsmerte,
  		SmerteArmNakke = Nsmerte
  	)
  	grtxt <- c('Smerter', '...Arm', '...Nakke', '...Arm og Nakke')
  	tittel <- 'Operasjonsårsak: Smerter'
  		}

    if (valgtVar=='OprIndikMyelopati') {
         retn <- 'H'
         indMotorisk <- which(RegData$OprIndikMyelopatiMotorisk == 1)
         indSensorisk <- which(RegData$OprIndikMyelopatiSensorisk == 1)
         Nmyelopati <- sum(RegData$OprIndikMyelopati, na.rm=T)
         AntVar <- cbind(
              Myelopati = Nmyelopati,
              Motorisk = length(indMotorisk),
              Sensorisk = length(indSensorisk),
              MotorSensor = length(intersect(indMotorisk, indSensorisk))
         )
         NVar<- cbind(
              Myelopati = length(which(RegData$OprIndikMyelopatiMotorisk > -1)),
              Motorisk = Nmyelopati,
              Sensorisk = Nmyelopati,
              MotorSensor = Nmyelopati
         )
         grtxt <- c('Myelopati', '...Sensorisk', '...Motorisk', '...Begge deler')
         tittel <- 'Operasjonsårsak: Myelopati'
    }

}

#Generelt for alle figurer med sammensatte variable:
  	if (teller == 1) {
  		AntHoved <- AntVar
  		NHoved <- max(NVar, na.rm=T)
  		Andeler$Hoved <- 100*AntVar/NVar
  	}
  	if (teller == 2) {
  		AntRest <- AntVar
  		NRest <- max(NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
  		Andeler$Rest <- 100*AntVar/NVar
  	}
  } #end medSml (med sammenligning)
}	#end sjekk om figuren inneholder flere variable



#--------------- Gjøre beregninger ------------------------------
#FRA INTENSIV

      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = 0, Rest =0)
      Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
      Ngr <- list(Hoved = 0, Rest =0)
      ind <- NIRUtvalg$ind
	  variable <- NIRVarSpes$variable
      
      Ngr$Hoved <- switch(as.character(flerevar), 
                          '0' = table(RegData$VariabelGr[ind$Hoved]),
                          # '1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm=T))
                          '1' = apply(RegData[ind$Hoved,variable], MARGIN=2, 
                                      FUN=function(x) sum(x == 1, na.rm=T)))
      #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
     N$Hoved <- switch(as.character(flerevar), 
                        '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                  #      '1' = length(ind$Hoved)
                        '1' = apply(RegData[ind$Hoved,variable], MARGIN=2, 
                                 FUN=function(x) sum(x %in% 0:1, na.rm=T)))
          AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved
      
      if (NIRUtvalg$medSml==1) {
           Ngr$Rest <- switch(as.character(flerevar), 
                               '0' = table(RegData$VariabelGr[ind$Rest]),
                              # '1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm=T))
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

      grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Hoved), '%') #paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      


#SKILLE UT FIGURDELEN SOM EGEN FUNKSJON???????
#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ( NHoved %in% 1:5 | 	(medSml ==1 & NRest<10)) {	#(valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) |
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
	if ( outfile != '') {dev.off()}

} else {

#-----------Figur---------------------------------------
#Innparametre: xaksetxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
cexgr <- 1	#Kan endres for enkeltvariable


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NakkeUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste('%.', antDes, 'f', sep='')
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
	if (NHoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
						paste(smltxt, ' (N=', NRest,')', sep='')),
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {
		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (length(grtxt2) == 1) {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
	ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
		xlab=xaksetxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=xaksetxt,
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')),
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {
	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
}

title(tittel, line=1, font.main=1)

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
}

#Beregninger som returneres fra funksjonen.
AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
rownames(AndelerUt) <- c('Hoved', 'Rest')
AntallUt <- rbind(AntHoved, AntRest)
rownames(AntallUt) <- c('Hoved', 'Rest')

UtData <- list(paste(toString(tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
names(UtData) <- c('tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
