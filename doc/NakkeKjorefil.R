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
varBort <- c('FodselsDato', 'SykehusNavn3mnd', 'SykehusNavn12mnd', 'ForlopsID')
ForlopsID <- RegData$ForlopsID
RegData <- RegData[,-which(names(RegData) %in% varBort)]
sykehus <- paste('Sykehus', LETTERS[1:10])
mengdePasienter <- c(0.3, 4, 10, 3, 7, 5, 1, 8, 9.5, 6)
RegData$SykehusNavn <- sample(sykehus, prob=mengdePasienter/sum(mengdePasienter), size=dim(RegData)[1], replace=T)
	RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500) #Trekker med tilbakelegging
	RegData <- data.frame(RegDataSyn$syn, ForlopsID)
	write.table(RegData, file='C:/ResultattjenesteGIT/Nakke/data/NakkeRegDataTest.csv', sep = ';', row.names = F, col.names = T)
	save(RegData, file=paste0('C:/ResultattjenesteGIT/Nakke/data/NakkeRegDataSyn.RData'))
	load('C:/ResultattjenesteGIT/Nakke/data/NakkeRegDataSyn.Rdata')

	fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
	SkjemaData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
	SkjemaDataRed <- SkjemaData[ ,c('SkjemaStatus','ForlopsID','HovedDato','SkjemaRekkeflg')]
	#SkjemaDataSyn <- synthpop::syn(SkjemaData, method = "sample", seed = 500) #Trekker med tilbakelegging
	SkjemaData <- dplyr::left_join(SkjemaDataRed, RegData[,c('ForlopsID','SykehusNavn')], by = "ForlopsID", copy=FALSE)
	names(SkjemaData)[which(names(SkjemaData) == 'SykehusNavn')] <- 'Sykehusnavn'
	save(SkjemaData, file=paste0('C:/ResultattjenesteGIT/Nakke/data/SkjemaDataSyn.RData'))
	load('C:/ResultattjenesteGIT/Nakke/data/SkjemaDataSyn.Rdata')

#----------------------------Laste data og parametre----------------------------------------
	load('A:/Nakke/NakkeAarsrapp2016.Rdata')
	library(nkr)

	rm(list=ls())
	library(Nakke)
	dato <- '2018-05-08'
	fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
	NakkeData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
	RegData <- NakkeData
	#RegData$SykehusNavn <- enc2native(as.character(RegData$SykehusNavn))
	RegData <- NakkePreprosess(RegData=RegData)
	NakkeData <- RegData[which(RegData$Aar<2018),]
	save(NakkeData, file=paste0('A:/Nakke/','NakkeAarsrapp2017','.Rdata'))
	#load(paste0(fil,".Rdata")) #RegData
	load('A:/Nakke/AlleVarNum2017-09-21.csv.Rdata')

	datoFra='2016-01-01'
	datoTil='3000-12-31'
	reshID <- 601161 #De tre med flest reg:
	enhetsUtvalg=0
	minald=0
	maxald=110
	erMann=9
	myelopati=9
	fremBak=9
	Ngrense=10
	grVar='ShNavn'
	ktr=0
	aar=2015:2016
	tidlAar=2013:2014
	tidsenhet <- 'Mnd'
	valgtMaal <- 'Gjsn'
	hentData=0
	outfile=''

#-------------------------------Månedsrapport---------------------------
	library(knitr)
	library(devtools)
	fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
	SkjemaData <- read.table(fil, sep=';', header=T, fileEncoding = 'UTF-8') #, encoding = 'UTF-8')
	RegData <- read.table('A:/Nakke/AlleVarNum2018-03-16.csv', sep=';', header=T, encoding = 'UTF-8')

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
	knit2pdf('NakkeMndRapp.Rnw', encoding = 'UTF-8')


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

NakkeFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,myelopati = myelopati,
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
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 110	#alder, til og med
datoFra <- '2017-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-12-31'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
myelopati <- 2
fremBak <- 0
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
tidsenhet <- 'Mnd'
valgtVar <- 'Komplinfek'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, ErstatningPreOp,
		  #Fornoyd12mnd, FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, , NytteOpr3mnd, NytteOpr12mnd
		  #Verre3mnd, Verre12mnd, OprIndikMyelopati, OprIndikSmerter, PerOpEnhverKompl, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning

outfile <- '' #paste0(valgtVar, 'Syn.png')	#''	#Navn angis av Jasper
NakkeFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                 datoTil=datoTil, enhetsUtvalg=0, outfile=outfile)
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

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 110	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'NDIendr12mnd'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, EnhverKompl3mnd
		  #ErstatningPreOp,
		  #FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NDIendr12mnd, NytteOpr3mnd, NytteOpr12mnd
		  #NRSsmerteArmEndr12mnd,Verre3mnd, Verre12mnd, OprIndikMyelopati, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning
outfile <- paste0(valgtVar, '_ShusSyn.pdf')	#''	#Navn angis av Jasper
outfile <- '' #paste0(valgtVar, '_ShusSyn.png')	#''	#Navn angis av Jasper
NakkeFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
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
utdata <- NakkeFigGjsnTid(RegData=RegData, datoFra='2017-03-01', valgtVar=valgtVar, valgtMaal='',
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, tidsenhet='Mnd',
           fremBak = 1, myelopati = 0,
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
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 110	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2018-06-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
valgtMaal = 'Gjsn'
valgtVar <- 'Alder'	#Må velge... Alder, EMSscorePreOp, LiggeDognPostop,KnivtidTotalMin, LiggeDognTotalt,
          #NDIscorePreOp, NRSsmerteArmPreOp, NRSsmerteNakkePreOp
          #EMSendr12mnd, EMSendr3mnd, EQ5Dendr12mnd, EQ5Dendr3mnd

outfile <- '' #paste0(valgtVar, '_', valgtMaal, '.pdf')	#''	#Navn angis av Jasper

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


#------------------Kvalitetsindikatorer-----------------------
library(Nakke)
NakkeData <- read.table('A:/Nakke/AlleVarNum2018-09-05.csv', sep=';', header=T, encoding = 'UTF-8') #
RegData <- NakkePreprosess(NakkeData)
RegData <- RegData[RegData$Aar>=2014,]
# Stemmevansker, 3 mnd.'
# Mål: lavest
#   #Kode 0,1: Nei, Ja +tomme
#   OppFolgStatus3mnd == 1 %i% KomplStemme3mnd %in% 0:1) %i% OprMetodeTilgangFremre==1
# Andel med KomplStemme3mnd=1
# Utvalg, ikke-myelopati, fremre tilgang: OprIndikMyelopati=0, OprMetodeTilgangFremre=1
#Variable: OppFolgStatus3mnd, KomplStemme3mnd, OprMetodeTilgangFremre, OprIndikMyelopati
variable <- c('ReshId','SykehusNavn','ErMann','Aar','KomplStemme3mnd') #
ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangFremre==1)
             & (RegData$KomplStemme3mnd %in% 0:1) & RegData$OprIndikMyelopati==0)
write.table(RegData[ind,variable], file='A:/NakkeTilOffStemme.csv', sep=';', row.names = F)

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
variable <- c('ReshId','SykehusNavn','ErMann','Aar','KomplSvelging3mnd') #
ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangFremre==1)
             & (RegData$KomplSvelging3mnd %in% 0:1) & RegData$OprIndikMyelopati==0)
write.table(RegData[ind,variable], file='A:/NakkeTilOffSvelg.csv', sep=';', row.names = F)


#Komplikasjoner (endret fra overfladisk, bakre til dyp og overfladisk, alle)
#Pasientskjema. Alle komplikasjoner (dype og overfladiske), 3mnd.
#Mål: lavt
#Kode 0,1: Nei, Ja +tomme
#    OppFolgStatus3mnd == 1, EnhverKompl3mnd %in% 0:1
#    tittel <- 'Komplikasjoner (totalt) 3 mnd. etter operasjon'
variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'EnhverKompl3mnd')
ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$KomplinfekOverfl3mnd %in% 0:1))
write.table(RegData[ind,variable], file='A:/NakkeTilOffInfOverfl.csv', sep=';', row.names = F)


#Ett datasett for alle
variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'OprIndikMyelopati',
    'OprMetodeTilgangBakre', 'OprMetodeTilgangFremre', 'EnhverKompl3mnd',
    'KomplSvelging3mnd', 'KomplStemme3mnd')
# Kanskje: 'OprDato',

ind <- which(RegData$OppFolgStatus3mnd==1)
write.table(RegData[ind,variable], file='A:/NakkeTilOff.csv', sep=';', row.names = F)

#!!!!!!!!!FASET UT:
#3MndSkjema. Andel med KomplinfekOverfl3mnd=1
#Mål: lavt
#Kode 0,1: Nei, Ja +tomme
# OppFolgStatus3mnd == 1, KomplinfekOverfl3mnd %in% 0:1
#variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'KomplinfekOverfl3mnd')
#ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangBakre==1)
#             & (RegData$KomplinfekOverfl3mnd %in% 0:1))
# TittelUt <- 'Overfladisk infeksjon, 3 mnd.'
#Utvalg, bakre tilgang: OprMetodeTilgangBakre==1
#Variable: OppFolgStatus3mnd, KomplinfekOverfl3mnd, OprMetodeTilgangBakre,

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
httr::set_config(httr::use_proxy(url="http://www-proxy.helsenord.no", port=8080))
httr::set_config(httr::use_proxy(url="http://flex-proxy.nhn.no", port=8080))
Sys.setenv(http_proxy="http://www-proxy.helsenord.no:8080")
#httr::set_config(use_proxy(url="18.91.12.23", port=8080))
options(RCurlOptions = list(proxy = "http://www-proxy.helsenord.no:8080"))
options(shinyapps.http = "rcurl")


library(rsconnect)
rsconnect::setAccountInfo(name='lenatest',
                          token='00581594B2D659880FFB21E82C8ED3A3',
                          secret='lvlAX2DFawjnMtrr0nFxANa7ARaRDOE2AROkYU+C')
rsconnect::deployApp('C:/ResultattjenesteGIT/Nakke/inst/shinyApps')
#options(rpubs.upload.method = "internal")
