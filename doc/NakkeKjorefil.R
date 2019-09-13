#Three things to keep in mind when dealing with character strings in R:
#  The encoding should be specified explicitly per (file) connection basis, if you want your
#R code to be portable;
#After you read Unicode characters into R, convert them to the native encoding of your system,
#e.g. using enc2native(); x <- readLines('foo.txt', encoding = 'UTF-8')
#         x <- enc2native(x)
#Do not set options(encoding) $encoding: [1] "native.enc". Tester

#--------------------------------------SAMLERAPPORT-----------------------------------
setwd("C:/ResultattjenesteGIT/Nakke/inst")
knitr::knit('NakkeAarsRapp.Rnw')
tools::texi2pdf('NakkeAarsRapp.tex')
#---------------- Tulledata ----------------------------------------
#Permuter alle variable
#Erstatt sykehusnavn med fiktive.
#Beholde noen egenskaper?

#set.seed(seed, kind = NULL, normal.kind = NULL)
#x <- as.data.frame(matrix(1:10,nrow=5))
#ind <- sample(x, replace = FALSE, prob = NULL)
#ind <- synthpop::syn(x, method = "sample", seed = 500) #

	library(synthpop)
library(dplyr)
ForlopsID <- RegData$ForlopsID
varBort <- c('FodselsDato', 'SykehusNavn3mnd', 'SykehusNavn12mnd', 'ForlopsID')
RegData <- RegData[,-which(names(RegData) %in% varBort)]
sykehus <- paste('Sykehus', LETTERS[1:10])
mengdePasienter <- c(0.3, 4, 10, 3, 7, 5, 1, 8, 9.5, 6) #For å få ulikt antall pasienter på de fiktive sykehusene
RegData$SykehusNavn <- sample(sykehus, prob=mengdePasienter/sum(mengdePasienter), size=dim(RegData)[1], replace=T)
	RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500) #Trekker med tilbakelegging
	RegData <- data.frame(RegDataSyn$syn, ForlopsID)
	write.table(RegData, file='C:/ResultattjenesteGIT/Nakke/data/NakkeRegDataTest.csv', sep = ';', row.names = F, col.names = T)
	save(RegData, file=paste0('C:/ResultattjenesteGIT/Nakke/data/NakkeRegDataSyn.RData'))
	load('C:/ResultattjenesteGIT/Nakke/data/NakkeRegDataSyn.RData')

	fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
	SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
	SkjemaDataRed <- SkjemaData[ ,c('SkjemaStatus','ForlopsID','HovedDato','SkjemaRekkeflg')]
	#SkjemaDataSyn <- synthpop::syn(SkjemaData, method = "sample", seed = 500) #Trekker med tilbakelegging
	SkjemaData <- dplyr::left_join(SkjemaDataRed, RegData[,c('ForlopsID','SykehusNavn')], by = "ForlopsID", copy=FALSE)
	names(SkjemaData)[which(names(SkjemaData) == 'SykehusNavn')] <- 'Sykehusnavn'
	save(SkjemaData, file=paste0('C:/ResultattjenesteGIT/Nakke/data/SkjemaDataSyn.RData'))
	load('C:/ResultattjenesteGIT/Nakke/data/SkjemaDataSyn.RData')

#--------------Koble med fødselsnr-------------------------------
  dato <- '2018-12-06'
	PersNr <- read.table(paste0('A:/Nakke/PersNrNakke',dato,'.csv'), sep=';', header=T, encoding = 'UTF-8')
	AlleVarNum <- read.table(paste0('A:/Nakke/AlleVarNum',dato,'.csv'), sep=';', header=T, encoding = 'UTF-8')
	AlleVarNumRed <- AlleVarNum[which(AlleVarNum$SykehusNavn == 'Haukeland USH') ,c('OprDato','PasientID', 'SykehusNavn', 'ForlopsID')]
KobletFil <- merge(x=AlleVarNumRed, y=PersNr, by='ForlopsID', all.x = T, all.y = F)
write.table(KobletFil, file='A:/Nakke/HaukelandPers.csv', sep = ';', row.names = F, col.names = T)

#----------------------------Laste data og parametre----------------------------------------
	library(nkr)

	rm(list=ls())
	library(Nakke)
	dato <- '2019-04-25'
	fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
	NakkeData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
	RegData <- NakkeData
	#?RegData$SykehusNavn <- enc2native(as.character(RegData$SykehusNavn))
	# RegData <- NakkePreprosess(RegData=RegData)
	# NakkeData <- RegData[which(RegData$Aar<2018),]
	# save(NakkeData, file=paste0('A:/Nakke/','NakkeAarsrapp2017','.Rdata'))
	#load(paste0(fil,".Rdata")) #RegData
	#load('A:/Nakke/AlleVarNum2017-09-21.csv.Rdata')

	load('A:/Nakke/NakkeAarsrapp2018.Rdata')
RegData <- NakkeData

	datoFra='2018-01-01'
	datoTil='2018-12-31'
	reshID <- 601161 #De tre med flest reg:
	enhetsUtvalg=0
	minald=0
	maxald=110
	erMann=9
	myelopati=9
	fremBak=0
	Ngrense=10
	grVar='ShNavn'
	ktr=0
	aar=2015:2016
	tidlAar=2015:2016
	tidsenhet <- 'Mnd'
	valgtMaal <- 'Gjsn'
	hentData=0
	outfile=''

#-------------------------------Månedsrapport---------------------------
	library(knitr)
	library(devtools)
	dato <- Sys.Date()#'2018-12-06'
	SkjemaData <- read.table(paste0('A:/Nakke/SkjemaOversikt',dato,'.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #, encoding = 'UTF-8')
	RegData <- read.table(paste0('A:/Nakke/AlleVarNum',dato,'.csv'), sep=';', header=T, encoding = 'UTF-8')

	#SkjemaData$Sykehusnavn <- enc2native(as.character(SkjemaData$Sykehusnavn))
	#SkjemaData$Sykehusnavn <- iconv(SkjemaData$Sykehusnavn, from = 'UTF-8', to = '')
	#table(Encoding(as.character(SkjemaData$Sykehusnavn)))
	#ind <- which(Encoding(as.character(SkjemaData$Sykehusnavn)) == 'UTF-8')
	#table(SkjemaData$Sykehusnavn) #[ind])
	reshID <- 601161

	setwd('C:/ResultattjenesteGIT/nakke/inst/')
	#options(encoded_text_to_latex= 'UTF-8')
	knit('NakkeMndRapp.Rnw', encoding = 'UTF-8')
	texi2pdf(file='NakkeMndRapp.tex')
#	knit2pdf('NakkeMndRapp.Rnw', encoding = 'UTF-8')


#------------------------------ Andeler flere var --------------------------
#------------------------------ (Fordelinger) --------------------------

#---------------Offentliggjøring fra 2016----------------
setwd('C:/ResultattjenesteGIT/Nakke/aarsrapp/2016')

#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
FigAndelerGrVarAar(RegData=RegData, valgtVar='KomplStemme3mnd',
                   myelopati=0, fremBak=1, Ngrense=30,
                   ktr=0,aar=2015:2016,tidlAar=2013:2014, outfile='OffKomplStemme3mnd.png')

#Svelgvansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
FigAndelerGrVarAar(RegData=RegData, valgtVar='KomplSvelging3mnd',
                   myelopati=0, fremBak=1, Ngrense=30,
                   ktr=0,aar=2015:2016,tidlAar=2013:2014, outfile='OffKomplSvelging3mnd.png')

#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav
FigAndelerGrVarAar(RegData=RegData, valgtVar='Komplinfek',
                   fremBak=2, Ngrense=30,
                   ktr=0,aar=2015:2016,tidlAar=2013:2014, outfile='OffKomplinfek.png')
setwd("C:/ResultattjenesteGIT/Nakke/")

valgtVar <- 'OprIndikSmerter' #OprIndikSmerter'
outfile <- '' #paste0(valgtVar, '.png')	#''	#Navn angis av Jasper

utdata <- NakkeFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,myelopati = myelopati,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, fremBak = 1,
           reshID=reshID, enhetsUtvalg=0, hentData=0, outfile=outfile)

variable <- c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
              'OprIndikMyelopati', 'Radiologi')

setwd('C:/Registerinfo og historie/Nakke/Figurer')
variable <- c('Alder', 'AntallNivaaOpr', 'Antibiotika', 'ArbeidstausPreOp',
              'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'EqAngstPreOp', 'ErstatningPreOp',
              'FornoydBeh3mnd','FornoydBeh12mnd', 'Komorbiditet', 'Kompl3mnd', 'KomplOpr', 'LiggeDognPostop',
               'LiggeDognTotalt', 'Morsmal', 'NytteOpr3mnd', 'NytteOpr12mnd', 'OperasjonsKategori',
               'OprIndik', 'OprIndikPareseGrad', 'OprIndikMyelopati', 'OprIndikSmerter', 'Radiologi',
               'Roker', 'Snuser',
              'SivilStatus', 'Saardren', 'SmertestillBrukPreOp', 'SymptVarighetArmer', 'SymptVarighetNakkeHode',
              'TidlOpr', 'TidlOprAntall', 'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste0(valgtVar, '_ny.png')
     NakkeFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
     }


#------------------------------ Andel, utvikling over tid --------------------------
#-----------------------------------------------------------------------------------
# Inndata til funksjon:
#...NB: SkjemaID
valgtVar <- 'Komplinfek'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, ErstatningPreOp,
		  #Fornoyd12mnd, FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, , NytteOpr3mnd, NytteOpr12mnd
		  #Verre3mnd, Verre12mnd, OprIndikMyelopati, OprIndikSmerter, PerOpEnhverKompl, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning

outfile <- '' #paste0(valgtVar, 'Syn.png')	#''	#Navn angis av Jasper
AndelerTid <- NakkeFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                 datoTil=datoTil, enhetsUtvalg=0, outfile=outfile)

lagTabavFig(UtDataFraFig = AndelerTid, figurtype = 'andelTid')

NakkeFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, tidsenhet=tidsenhet,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=1, outfile=outfile)

variable <- c('Alder', 'AndreRelSykdommer', 'Antibiotika',
          'ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'ErstatningPreOp',
		  'FornoydBeh3mnd', 'FornoydBeh12mnd', 'Misfor3mnd', 'Misfor12mnd', 'KomplinfekDyp3mnd',
		  'KomplinfekOverfl3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'NytteOpr3mnd', 'NytteOpr12mnd',
		  'Verre3mnd', 'Verre12mnd', 'OprIndikMyelopati', 'OprIndikSmerter', 'PerOpEnhverKompl', 'Roker',
		  'Saardren', 'SmertestillPreOp', 'SymptVarighetNakkeHode', #'SymptVarighetSmerterUker',
		  'UforetrygdPreOp', 'Utdanning')
setwd('C:/Registerinfo og historie/Nakke/Figurer')
for (valgtVar in variable) {
     outfile <- paste0(valgtVar, '.png')
     NakkeFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, outfile=outfile)
}


#------------------------------ Andel, per enhet --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('A:/Nakke/AlleVarNum2018-06-21.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/ResultattjenesteGIT/Nakke/")

valgtVar <- 'Alder'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, EnhverKompl3mnd
		  #ErstatningPreOp,
		  #FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NDIendr12mnd, NytteOpr3mnd, NytteOpr12mnd
		  #NRSsmerteArmEndr12mnd,Verre3mnd, Verre12mnd, OprIndikMyelopati, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning
outfile <- paste0(valgtVar, '_ShusSyn.pdf')	#''	#Navn angis av Jasper
outfile <- '' #paste0(valgtVar, '_ShusSyn.png')	#''	#Navn angis av Jasper
NakkeFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, fremBak = fremBak, myelopati = myelopati,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, outfile=outfile)

variable <- c('Alder', 'AndreRelSykdommer', 'Antibiotika',
          'ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'EnhverKompl3mnd', 'ErstatningPreOp',
             'FornoydBeh3mnd', 'FornoydBeh12mnd', 'Misfor3mnd', 'Misfor12mnd', 'KomplinfekDyp3mnd',
             'KomplinfekOverfl3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'NDIendr12mnd30pst', 'NytteOpr3mnd',
          'NytteOpr12mnd', 'NRSsmerteArmEndr12mnd','Verre3mnd', 'Verre12mnd', 'OprIndikMyelopati', 'Roker', 'Saardren',
             'SmertestillPreOp', 'SymptVarighetArmer', 'SymptVarighetNakkeHode', 'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste0(valgtVar, '_sh.png')
     NakkeFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, outfile=outfile)
}
#------------------------------ Gjennomsnitt/Median per år --------------------------
#-----------------------------------------------------------------------------------

valgtVar <- 'NRSsmerteArmEndr3mnd'	#Må velges: EMSendr12mnd, EMSendr3mnd, EQ5Dendr12mnd, EQ5Dendr3mnd, Eq5DScorePreOp,
               #KnivtidTotalMin, LiggeDognPostop, LiggeDognTotalt
               #NDIendr12mnd, NDIendr3mnd, NDIscorePreOp

outfile <- '' #paste(valgtVar, '.png', sep='')	#''	#Navn angis av Jasper
utdata <-
  NakkeFigGjsnTid(RegData=RegData, datoFra='2018-01-01', valgtVar=valgtVar, valgtMaal='',
           datoTil=datoTil, minald=minald, maxald=36, erMann=erMann, tidsenhet='Mnd',
           fremBak = 9, myelopati = 0,
           reshID=reshID, enhetsUtvalg=1, outfile=outfile)

variable <- c('EMSendr12mnd', 'EMSendr3mnd', 'EQ5Dendr12mnd', 'EQ5Dendr3mnd', 'Eq5DScorePreOp',
              'KnivtidTotalMin', 'LiggeDognPostop', 'LiggeDognTotalt',
              'NDIendr12mnd', 'NDIendr3mnd', 'NDIscorePreOp')
for (valgtVar in variable) {
     outfile <- paste0(valgtVar, '_GjsnTid.png')
     NakkeFigGjsnTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal='',
                                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                                reshID=reshID, enhetsUtvalg=1, outfile=outfile)
}



#------------------------------ Gjsn/med per enhet --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
# Inndata til funksjon:
valgtVar <- 'Alder'	#Må velge... Alder, EMSscorePreOp, LiggeDognPostop,KnivtidTotalMin, LiggeDognTotalt,
          #NDIscorePreOp, NRSsmerteArmPreOp, NRSsmerteNakkePreOp
          #EMSendr12mnd, EMSendr3mnd, EQ5Dendr12mnd, EQ5Dendr3mnd

outfile <- '' #paste0(valgtVar, '_', valgtMaal, '.pdf')	#''	#Navn angis av Jasper

#utdata <-
  NakkeFigGjsnGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal=valgtMaal,
            datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
            reshID=reshID, outfile=outfile)


variable <- c('Alder', 'EMSscorePreOp', 'LiggeDognPostop','KnivtidTotalMin', 'LiggeDognTotalt',
          'NDIscorePreOp', 'NRSsmerteArmPreOp', 'NRSsmerteNakkePreOp')
for (valgtVar in variable) {
     outfile <- paste0(valgtVar, '.png')
     NakkeFigGjsnGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, outfile=outfile)
}


#------------------Kvalitetsindikatorer, Resultatportalen -----------------------
rm(list=ls())
library(Nakke)
NakkeData <- read.table('A:/Nakke/AlleVarNum2019-09-12.csv', sep=';', header=T) #, encoding = 'UTF-8')
RegData <- NakkePreprosess(NakkeData)
RegData <- RegData[RegData$Aar>=2014,]
# Stemmevansker, 3 mnd.'
# Mål: lavest
#   #Kode 0,1: Nei, Ja +tomme
#   OppFolgStatus3mnd == 1 %i% KomplStemme3mnd %in% 0:1) %i% OprMetodeTilgangFremre==1
# Andel med KomplStemme3mnd=1
# Utvalg, ikke-myelopati, fremre tilgang: OprIndikMyelopati=0, OprMetodeTilgangFremre=1
#Variable: OppFolgStatus3mnd, KomplStemme3mnd, OprMetodeTilgangFremre, OprIndikMyelopati
variable <- c('ReshId','SykehusNavn','Aar','KomplStemme3mnd') #
ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangFremre==1)
             & (RegData$KomplStemme3mnd %in% 0:1) & RegData$OprIndikMyelopati==0)
write.table(RegData[ind,variable], file='A:/ind2_Stemmevansker_Nakke.csv', sep=';', row.names = F)

# 'Svelgvansker, 3 mnd.'
#Mål: lavt
# Kode 0,1: Nei, Ja +tomme
# OppFolgStatus3mnd == 1 %i%
# KomplSvelging3mnd %in% 0:1 %i%
# OprMetodeTilgangFremre==1
# Andel med KomplSvelging3mnd=1
# Utvalg, ikke-myelopati, fremre tilgang: OprIndikMyelopati=0, OprMetodeTilgangFremre=1
#Variable: OppFolgStatus3mnd, KomplSvelging3mnd, OprMetodeTilgangFremre,
#OprIndikMyelopati
variable <- c('ReshId','SykehusNavn','Aar','KomplSvelging3mnd') #
ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangFremre==1)
             & (RegData$KomplSvelging3mnd %in% 0:1) & RegData$OprIndikMyelopati==0)
write.table(RegData[ind,variable], file='A:/ind1_Svelgvansker_Nakke.csv', sep=';', row.names = F)

#Komplikasjoner (endret fra overfladisk, bakre til dyp og overfladisk, alle)
#Pasientskjema. Alle komplikasjoner (dype og overfladiske), 3mnd.
#Mål: lavt
#Kode 0,1: Nei, Ja +tomme
#    OppFolgStatus3mnd == 1, KomplinfekDyp3mnd eller KomplinfekOverfl3mnd %in% 0:1
#    tittel <- 'Komplikasjoner (totalt) 3 mnd. etter operasjon'
#valgtVar = Komplinfek
ind <- intersect(which(RegData$OppFolgStatus3mnd == 1),
  union(which(RegData$KomplinfekDyp3mnd %in% 0:1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)))
RegData <- RegData[ind, ]
RegData$KomplInfek <- 0
RegData$KomplInfek[union(which(RegData$KomplinfekDyp3mnd==1), which(RegData$KomplinfekOverfl3mnd==1))] <- 1
variable <- c('ReshId', 'SykehusNavn', 'Aar', 'KomplInfek')
write.table(RegData[ ,variable], file='A:/ind3_Sårinfeksjon_Nakke.csv', sep=';', row.names = F)

#--------Nøkkeltall, Resultatportalen
rm(list=ls())
library(Nakke)
NakkeData <- read.table('A:/Nakke/AlleVarNum2019-09-12.csv', sep=';', header=T) #, encoding = 'UTF-8')
RegData <- NakkePreprosess(NakkeData)
NakkeData <- RegData[RegData$Aar>=2014,]

# Antall sykehusavdelinger	?	9
# Antall operasjoner 2018	5302	1091
# Andel >70 år	27%	6%
# Gjennomsnittsalder	57	52
# Andel kvinner operert	47,5%	44%
# Fornøyd med behandlingen de fikk på sykehuset 3 mnd. etter kirurgi	91%	91%
# Suksess: «helt restituert» etter «mye bedre» 3 mnd. etter kirurgi	65%	61%* (Fremre nakkekirurgi)
# Andel som angir at de er verre 3 mnd. etter kirurgi	3%	3%* (Fremre nakkekirurgi)

antSh <- colSums(table(as.character(NakkeData$ShNavn),NakkeData$Aar)>0)
antOp <- table(NakkeData$Aar)
NakkeData$over70 <- 0
NakkeData$over70[NakkeData$Alder>=70] <- 1
andel70aar <- tapply(NakkeData$over70,NakkeData$Aar, FUN='mean', na.rm=T)
alderGjsn <- tapply(NakkeData$Alder,NakkeData$Aar, FUN='mean', na.rm=T)
alderMedian <- tapply(NakkeData$Alder,NakkeData$Aar, FUN='median', na.rm=T)
andelKvinner <- 1-tapply(NakkeData$ErMann,NakkeData$Aar, FUN='mean', na.rm=T)

#datoTil <- min(datoTil, as.character(Sys.Date()-100))
NakkeData$Fornoyd <- 0
NakkeDataForn <- NakkeData[intersect(which(NakkeData$OppFolgStatus3mnd==1), which(NakkeData$FornoydBeh3mnd %in% 1:5)),
                         c('FornoydBeh3mnd', 'Fornoyd', 'Aar')]
NakkeDataForn$Fornoyd[NakkeDataForn$FornoydBeh3mnd %in% 1:2] <- 1
andelFornoyd <- tapply(NakkeDataForn$Fornoyd, NakkeDataForn$Aar, FUN='mean', na.rm=T)

#datoTil <- min(datoTil, as.character(Sys.Date()-100))
NakkeData$Bedre <- 0
NakkeData$Verre <- 0
NakkeDataEndring <-  NakkeData[intersect(intersect(which(NakkeData$OppFolgStatus3mnd==1),
                                         which(NakkeData$NytteOpr3mnd %in% 1:7)),
                               which(NakkeData$OprMetodeTilgangFremre==1)),
                             c('NytteOpr3mnd', 'Bedre', 'Verre','Aar')]
NakkeDataEndring$Bedre[NakkeDataEndring$NytteOpr3mnd %in% 1:2] <- 1
NakkeDataEndring$Verre[NakkeDataEndring$NytteOpr3mnd %in% 6:7] <- 1

andelSuksess <- tapply(NakkeDataEndring$Bedre, NakkeDataEndring$Aar, FUN='mean', na.rm=T)
andelVerre <- tapply(NakkeDataEndring$Verre, NakkeDataEndring$Aar, FUN='mean', na.rm=T)

NokkeltallNakke <- rbind(
  'Antall avdelinger' = antSh,
  'Antall operasjoner' = antOp,
  'Andel over 70 år'	= andel70aar,
  'Gjennomsnittsalder' = alderGjsn,
  #   'Medianalder' = alderMedian,
  'Andel kvinner' = andelKvinner,
  'Fornøyd med behandlingen, 3 mnd. etter' = andelFornoyd,
  'Helt restituert/mye bedre, 3 mnd. etter fremre kir.' = andelSuksess,
  'Verre 3 mnd. etter fremre kir.' = andelVerre
)
tabNokkeltallNakke <- cbind(row.names(NokkeltallNakke),NokkeltallNakke)

write.table(tabNokkeltallNakke, file = 'A:/Resultatportalen/NokkeltallNakke.csv', row.names=F, sep=';', fileEncoding = 'UTF-8' )



#------------------ Data til NPR, Dekningsgradsanalyse
'SELECT ForlopsID, Alder, Kjonn, Organisasjon, AvdRESH, SykehusNavn,
OprDato, OprKode, InngrepType, Dagkirurgi
FROM AlleVarNum
WHERE (OprDato >= '2017-01-01' AND OprDato <= '2017-12-31')'
variable <- c('ForlopsID', 'Alder', 'FodselsDato', 'Kjonn',
              'AvdRESH', 'Avdeling', 'OprDato', 'OprKode', 'InngrepType', 'Dagkirurgi')
Har ikke: RHF_RESH, RHF, ORG_RESH, Organisasjon,ORG_RESH
Endret: AVD_RESH -> AvdRESH, Avdeling->SykehusNavn,

NakkeNPR2017data <- read.table('A:/Nakke/NakkeNPR2017data.csv', sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE)  # na.strings = "NULL",
NakkeNPR2017data$OprDato <- as.Date(NakkeNPR2017data$OprDato, format='%Y-%m-%d')
NakkeNPR2017data$FodselsDato <- as.Date(NakkeNPR2017data$FodselsDato, format='%Y-%m-%d')
write.table(NakkeNPR2017data, file="A:/Nakke/NakkeNPR2017data.csv", sep=';')

NakkeNPR2017data <- read.table('A:/Nakke/NakkeNPR2017persnr.csv', sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE)  # na.strings = "NULL",


#------------------------------ Shiny --------------------------
library(Nakke)
setwd('C:/ResultattjenesteGIT/Nakke/R')
runShinyAppReports()

#Konto
library(shinyapps)
??shinyappsProxies
?shinyappsOptions
#Publisere:
- Sette proxy

library(rsconnect)
rsconnect::setAccountInfo(name='lenatest',
                          token='..',
                          secret='..')
rsconnect::deployApp('C:/ResultattjenesteGIT/Nakke/inst/shinyApps')
#options(rpubs.upload.method = "internal")
