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


  RegData <- NakkeRegDataSQL()
	RegData <- NakkePreprosess(RegData=RegData)

	library(synthpop)
	RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500)
	RegData <- RegDataSyn$syn
<<<<<<< HEAD
#------------------------------ Laste data ------------------------------
rm(list=ls())
library(Nakke)
dato <- '2017-09-27'
fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
NakkeData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
RegData <- NakkeData
#RegData <- NakkePreprosess(RegData=RegData)
#RegData <- RegData[which(RegData$Aar<2017),]
#save(RegData, file=paste0('A:/Nakke/','NakkeAarsrapp2016','.Rdata'))
load(paste0(fil,".Rdata")) #RegData
load('A:/NakkeAarsrapp2016.Rdata')
table(RegData$SykehusNavn, RegData$Aar)
#------------------------------ Andeler flere var --------------------------
#------------------------------ (Fordelinger) --------------------------
=======

	Skjemanavn	SkjemaStatus	ForlopsID	OpprettetAv	OpprettetDato	SistLagretAv	SistLagretDato	HovedDato	Sykehusnavn	AvdRESH	SkjemaRekkeflg


#----------------------------Laste data og parametre----------------------------------------
	load('A:/Nakke/NakkeAarsrapp2016.Rdata')
	library(nkr)

	rm(list=ls())
	library(Nakke)
	dato <- '2018-03-02'
	fil <- paste0('A:/Nakke/AlleVarNum',dato,'.csv')
	NakkeData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
	RegData <- NakkeData
	#RegData$SykehusNavn <- enc2native(as.character(RegData$SykehusNavn))
	#RegData <- NakkePreprosess(RegData=RegData)
	#RegData <- RegData[which(RegData$Aar<2017),]
	#save(RegData, file=paste0('A:/Nakke/','NakkeAarsrapp2016','.Rdata'))
	#load(paste0(fil,".Rdata")) #RegData
	load('A:/Nakke/AlleVarNum2017-09-21.csv.Rdata')

	datoFra='2012-01-01'
	datoTil='3000-12-31'
	reshID <- 601161 #De tre med flest reg:
	enhetsUtvalg=0
	minald=0
	maxald=130
	erMann=''
	myelopati=99
	fremBak=0
	Ngrense=10
	grVar='ShNavn'
	ktr=0
	aar=2015:2016
	tidlAar=2013:2014
	tidsenhet <- 'aar'
	hentData=0
	outfile=''
#-------------------------------Månedsrapport---------------------------
	library(knitr)
	library(devtools)
	fil <- paste0('A:/Nakke/SkjemaOversikt',dato,'.csv')
	SkjemaData <- read.table(fil, sep=';', header=T, fileEncoding = 'UTF-8') #, encoding = 'UTF-8')
	RegData <- read.table('A:/Nakke/AlleVarNum2018-03-02.csv', sep=';', header=T, encoding = 'UTF-8')

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

#Offentliggjøring fra 2016
setwd('C:/ResultattjenesteGIT/Nakke/aarsrapp/2016')
>>>>>>> c9a70cc4be64053147929270e1f242aab88e74d6

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

NakkeFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
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
maxald <- 130	#alder, til og med
datoFra <- '2017-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2018-04-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
myelopati <- 2
fremBak <- 0
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
tidsenhet <- 'Mnd'
valgtVar <- 'KomplStemme3mnd'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, ErstatningPreOp,
		  #Fornoyd12mnd, FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NytteOpr3mnd, NytteOpr12mnd
		  #Verre3mnd, Verre12mnd, OprIndikMyelopati, OprIndikSmerter, PerOpEnhverKompl, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning

outfile <- '' #paste0(valgtVar, 'Syn.png')	#''	#Navn angis av Jasper
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
NakkeData <- read.table('C:/Registre/Nakke/AlleVarNum2016-10-03.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/ResultattjenesteGIT/Nakke/")

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2016-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
<<<<<<< HEAD
tittel=1
enhetsUtvalg <-01	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'KomplStemme3mnd'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
=======
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'ErstatningPreOp'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
>>>>>>> c9a70cc4be64053147929270e1f242aab88e74d6
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, EnhverKompl3mnd
		  #ErstatningPreOp,
		  #FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NDIendr12mnd, NytteOpr3mnd, NytteOpr12mnd
		  #NRSsmerteArmEndr12mnd,Verre3mnd, Verre12mnd, OprIndikMyelopati, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning
<<<<<<< HEAD
outfile <- paste0(valgtVar, '_ShusSyn.pdf')	#''	#Navn angis av Jasper
outfile <- ''
FigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
=======
outfile <- '' #paste0(valgtVar, '_ShusSyn.png')	#''	#Navn angis av Jasper
NakkeFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
>>>>>>> c9a70cc4be64053147929270e1f242aab88e74d6
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

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
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/Registre/Nakke/trunk/GjsnTid")

valgtVar <- 'Eq5DScorePreOp'	#Må velges: EMSendr12mnd, EMSendr3mnd, EQ5Dendr12mnd, EQ5Dendr3mnd, Eq5DScorePreOp,
               #KnivtidTotalMin, LiggeDognPostop, LiggeDognTotalt
               #NDIendr12mnd, NDIendr3mnd, NDIscorePreOp

outfile <- '' #paste(valgtVar, '.png', sep='')	#''	#Navn angis av Jasper
utdata <- NakkeFigGjsnTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal='',
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

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
maxald <- 130	#alder, til og med
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

# Stemmevansker, 3 mnd.'
# Mål: lavest
#   #Kode 0,1: Nei, Ja +tomme
#   OppFolgStatus3mnd == 1 %i% KomplStemme3mnd %in% 0:1) %i% OprMetodeTilgangFremre==1
# Andel med KomplStemme3mnd=1
# Utvalg, ikke-myelopati, fremre tilgang: OprIndikMyelopati=0, OprMetodeTilgangFremre=1
#Variable: OppFolgStatus3mnd, KomplStemme3mnd, OprMetodeTilgangFremre, OprIndikMyelopati
variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'KomplStemme3mnd')
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
variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'KomplSvelging3mnd')
ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangFremre==1)
             & (RegData$KomplSvelging3mnd %in% 0:1) & RegData$OprIndikMyelopati==0)
write.table(RegData[ind,variable], file='A:/NakkeTilOffSvelg.csv', sep=';', row.names = F)


#3MndSkjema. Andel med KomplinfekOverfl3mnd=1
#Mål: lavt
#Kode 0,1: Nei, Ja +tomme
# OppFolgStatus3mnd == 1, KomplinfekOverfl3mnd %in% 0:1
# TittelUt <- 'Overfladisk infeksjon, 3 mnd.'
#Utvalg, bakre tilgang: OprMetodeTilgangBakre==1
#Variable: OppFolgStatus3mnd, KomplinfekOverfl3mnd, OprMetodeTilgangBakre,
variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'KomplinfekOverfl3mnd')

ind <- which((RegData$OppFolgStatus3mnd==1) & (RegData$OprMetodeTilgangBakre==1)
             & (RegData$KomplinfekOverfl3mnd %in% 0:1))
write.table(RegData[ind,variable], file='A:/NakkeTilOffInfOverfl.csv', sep=';', row.names = F)

#Andre variable: ErMann, ShNavn, ReshId, Aar

#Ett datasett for alle
variable <- c('ReshId', 'SykehusNavn', 'ErMann', 'Aar', 'OprIndikMyelopati',
    'OprMetodeTilgangBakre', 'OprMetodeTilgangFremre', 'KomplinfekOverfl3mnd',
    'KomplSvelging3mnd', 'KomplStemme3mnd')
# Kanskje: 'OprDato',

ind <- which(RegData$OppFolgStatus3mnd==1)
write.table(RegData[ind,variable], file='A:/NakkeTilOff.csv', sep=';', row.names = F)




#------------------------------ Shiny --------------------------
library(Nakke)
setwd('C:/ResultattjenesteGIT/Nakke/R')
runShinyAppReports()

