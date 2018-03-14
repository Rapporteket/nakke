

#--------------------------------------SAMLERAPPORT-----------------------------------
setwd("C:/ResultattjenesteGIT/Nakke/inst")
knitr::knit('NakkeAarsRapp.Rnw')
tools::texi2pdf('NakkeAarsRapp.tex')
#---------------- Tulledata ----------------------------------------


  RegData <- NakkeRegDataSQL()
	RegData <- NakkePreprosess(RegData=RegData)

	library(synthpop)
	RegDataSyn <- syn(RegData, method = "sample", seed = 500)
	RegData <- RegDataSyn$syn
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

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2015-12-31'
erMann <- 99			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
enhetsUtvalg <- 0	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus

outfile <- paste0(valgtVar, '.png')	#''	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/Nakke/")

FigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, hentData=0, outfile=outfile)



variable <- c('Alder', 'AntallNivaaOpr', 'Antibiotika', 'ArbeidstausPreOp',
              'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'EqAngstPreOp', 'ErstatningPreOp',
              'FornoydBeh3mnd','FornoydBeh12mnd', 'Komorbiditet', 'Kompl3mnd', 'KomplOpr', 'LiggeDognPostop',
               'LiggeDognTotalt', 'Morsmal', 'NytteOpr3mnd', 'NytteOpr12mnd', 'OperasjonsKategori',
               'OprIndik', 'OprIndikPareseGrad', 'OprIndikMyelopati', 'OprIndikSmerter', 'Radiologi',
               'Roker', 'Snuser',
              'SivilStatus', 'Saardren', 'SmertestillBrukPreOp', 'SymptVarighetArmer', 'SymptVarighetNakkeHode',
              'TidlOpr', 'TidlOprAntall', 'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigAndeler(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, enhetsUtvalg=enhetsUtvalg, libkat=libkat, outfile=outfile)
     }


#------------------------------ Andel, utvikling over tid --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/ResultattjenesteGIT//Nakke")

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-04-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'OprIndikMyelopati'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, ErstatningPreOp,
		  #Fornoyd12mnd, FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NytteOpr3mnd, NytteOpr12mnd
		  #Verre3mnd, Verre12mnd, OprIndikMyelopati, OprIndikSmerter, PerOpEnhverKompl, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning

outfile <- paste(valgtVar, 'Syn.png', sep='')	#''	#Navn angis av Jasper
FigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

variable <- c('Alder', 'AndreRelSykdommer', 'Antibiotika',
          'ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'ErstatningPreOp',
		  'FornoydBeh3mnd', 'FornoydBeh12mnd', 'Misfor3mnd', 'Misfor12mnd', 'KomplinfekDyp3mnd',
		  'KomplinfekOverfl3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'NytteOpr3mnd', 'NytteOpr12mnd',
		  'Verre3mnd', 'Verre12mnd', 'OprIndikMyelopati', 'OprIndikSmerter', 'PerOpEnhverKompl', 'Roker',
		  'Saardren', 'SmertestillPreOp', 'SymptVarighetNakkeHode', 'SymptVarighetSmerterUker',
		  'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigAndelTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, libkat=libkat, outfile=outfile)
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
tittel=1
enhetsUtvalg <-01	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'KomplStemme3mnd'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, EnhverKompl3mnd
		  #ErstatningPreOp,
		  #FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NDIendr12mnd, NytteOpr3mnd, NytteOpr12mnd
		  #NRSsmerteArmEndr12mnd,Verre3mnd, Verre12mnd, OprIndikMyelopati, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning
outfile <- paste0(valgtVar, '_ShusSyn.pdf')	#''	#Navn angis av Jasper
outfile <- ''
FigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

variable <- c('Alder', 'AndreRelSykdommer', 'Antibiotika',
          'ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd', 'ASAgrad', 'BMI', 'EnhverKompl3mnd', 'ErstatningPreOp',
             'FornoydBeh3mnd', 'FornoydBeh12mnd', 'Misfor3mnd', 'Misfor12mnd', 'KomplinfekDyp3mnd',
             'KomplinfekOverfl3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'NDIendr12mnd', 'NytteOpr3mnd', 'NytteOpr12mnd',
             'NRSsmerteArmEndr12mnd','Verre3mnd', 'Verre12mnd', 'OprIndikMyelopati', 'Roker', 'Saardren',
             'SmertestillPreOp', 'SymptVarighetNakkeHode', 'SymptVarighetSmerterUker', 'UforetrygdPreOp', 'Utdanning')

for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigAndelerGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                reshID=reshID, libkat=libkat, outfile=outfile)
}
#------------------------------ Gjennomsnitt/Median per år --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NakkeData <- read.table('C:/Registre/Nakke/data/AlleVarNum2016-04-13.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, AlleVarNum2016-01-04Num
RegData <- NakkeData
setwd("C:/Registre/Nakke/trunk/GjsnTid")

# Inndata til funksjon:
#...NB: SkjemaID
reshID <- 601161 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-04-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
tittel=1
enhetsUtvalg <- 0	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus
valgtVar <- 'KnivtidTotalMin'	#Må velges: EMSendr12mnd, EMSendr3mnd, EQ5Dendr12mnd, EQ5Dendr3mnd, Eq5DScorePreOp,
               #KnivtidTotalMin, LiggeDognPostop, LiggeDognTotalt
               #NDIendr12mnd, NDIendr3mnd, NDIscorePreOp

outfile <- paste(valgtVar, '.png', sep='')	#''	#Navn angis av Jasper
utdata <- FigGjsnTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal='',
           datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

variable <- c('EMSendr12mnd', 'EMSendr3mnd', 'EQ5Dendr12mnd', 'EQ5Dendr3mnd', 'Eq5DScorePreOp',
              'KnivtidTotalMin', 'LiggeDognPostop', 'LiggeDognTotalt',
              'NDIendr12mnd', 'NDIendr3mnd', 'NDIscorePreOp')
for (valgtVar in variable) {
     outfile <- paste(valgtVar, '_GjsnTid.png', sep='')
     FigGjsnTid(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal='',
                                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                                reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)
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
datoTil <- '2016-06-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
valgtMaal = 'Med'
valgtVar <- 'NRSsmerteArmPreOp'	#Må velge... Alder, EMSscorePreOp, LiggeDognPostop,KnivtidTotalMin, LiggeDognTotalt,
          #NDIscorePreOp, NRSsmerteArmPreOp, NRSsmerteNakkePreOp

outfile <- paste(valgtVar, '_', valgtMaal, '.pdf', sep='')	#''	#Navn angis av Jasper

FigGjsnGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar, valgtMaal=valgtMaal,
            datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
            reshID=reshID, outfile=outfile)


variable <- c('Alder', 'EMSscorePreOp', 'LiggeDognPostop','KnivtidTotalMin', 'LiggeDognTotalt',
          'NDIscorePreOp', 'NRSsmerteArmPreOp', 'NRSsmerteNakkePreOp')
for (valgtVar in variable) {
     outfile <- paste(valgtVar, '.png', sep='')
     FigGjsnGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
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


