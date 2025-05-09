#Three things to keep in mind when dealing with character strings in R:
#  The encoding should be specified explicitly per (file) connection basis, if you want your
#R code to be portable;
#After you read Unicode characters into R, convert them to the native encoding of your system,
#e.g. using enc2native(); x <- readLines('foo.txt', encoding = 'UTF-8')
#         x <- enc2native(x)
#Do not set options(encoding) $encoding: [1] "native.enc". Tester
#test


# Uttrekk med alle på DEG_NAKKE med PID + Operasjonsdato + string variabelen tilknyttet OprIndikAnnet (Annet spesifiser…..)
# +  stringvariablen tilknyttet RtgFunnANNET (Annet spesifiser…..).


RegDataAVN <- rapbase::loadRegData(registryName = "nakke", dbType = "mysql",
                                   query = 'select * from allevarnum')
#OprIndikAnnet - Ingen med "Spesifiser" e.l. i navnet.

#DataNakke <- RegDataAVN[ ,c('PasientID', 'OprDato', 'OprIndikAnnet', 'RtgFunnANNET')]
table(DataNakke$RtgFunnANNET, useNA = 'a')


#Dobbeltregistrering
library(magrittr)
RegDataRaa <- NakkeRegDataSQL()
RegData <- NakkePreprosess(RegDataRaa)
DblReg <- RegData %<% group_by(PasientID) %<%
  summarise()

library(nakke)
AntSkjemaAvHver <- tabAntSkjema(SkjemaOversikt=SkjemaData) #, skjemastatus=as.numeric(input$skjemastatus))

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
  dato <- '2020-02-27'
	PersNrID <- read.table(paste0('A:/Nakke/koblingstabell',dato,'.csv'), sep=';', header=T, encoding = 'UTF-8')
	allevarnum <- read.table(paste0('A:/Nakke/allevarnum',dato,'.csv'), sep=';', header=T, encoding = 'UTF-8')
	#allevarnumRed <- allevarnum[which(allevarnum$SykehusNavn == 'Haukeland USH') ,c('OprDato','PasientID', 'SykehusNavn', 'ForlopsID')]
KobletFil <- merge(x=allevarnum, y=PersNrID, by.x='PasientID', by.y = 'ID') #"", all.x = T, all.y = F)
write.table(KobletFil[ ,-which(names(KobletFil) %in% c('X.x', 'X.y'))], file=paste0('A:/Nakke/allevarnumPersNr', dato, '.csv'), sep = ';', row.names = F, col.names = T)

#----------------------------Laste data og parametre----------------------------------------
	library(nkr)

	rm(list=ls())
	library(nakke)
	dato <- '2019-04-25'
	fil <- paste0('A:/Nakke/allevarnum',dato,'.csv')
	NakkeData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
	RegData <- NakkeData
	#?RegData$SykehusNavn <- enc2native(as.character(RegData$SykehusNavn))
	# RegData <- NakkePreprosess(RegData=RegData)
	# NakkeData <- RegData[which(RegData$Aar<2018),]
	# save(NakkeData, file=paste0('A:/Nakke/','NakkeAarsrapp2017','.Rdata'))
	#load(paste0(fil,".Rdata")) #RegData
	#load('A:/Nakke/allevarnum2017-09-21.csv.Rdata')

	load('A:/Nakke/NakkeAarsrapp2018.Rdata')
library(nakke)
RegData <-NakkePreprosess(NakkeRegDataSQL(datoFra = '2011'))

variable <- c('KomplDVT3mnd', 'KomplinfekDyp3mnd', 'KomplLungeEmboli3mnd', 'KomplinfekOverfl3mnd',
              'KomplPneumoni3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'KomplUVI3mnd',
              'EnhverKompl3mnd')
test <- RegData[,variable]
test$test <- rowSums(RegData[,variable[1:8]])
table(test$test)
colSums(test, na.rm = T)
RegData[which(test$test==0 & test$EnhverKompl3mnd==1),variable]
table(RegData[,variable[9]], useNA = 'a')

	datoFra='2018-01-01'
	datoTil='2020-12-31'
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
	#aar=2015:2016
	#tidlAar=2015:2016
	inngrep <- 9
	tidsenhet <- 'Mnd'
	valgtMaal <- 'Gjsn'
	hentData=0
	outfile=''
	valgtVar <- 'Kompl3mnd'

#-------------------------------Månedsrapport---------------------------
	library(knitr)
	library(devtools)
	dato <- Sys.Date()#'2018-12-06'
	SkjemaData <- read.table(paste0('A:/Nakke/SkjemaOversikt',dato,'.csv'), sep=';', header=T, fileEncoding = 'UTF-8') #, encoding = 'UTF-8')
	RegData <- read.table(paste0('A:/Nakke/allevarnum',dato,'.csv'), sep=';', header=T, encoding = 'UTF-8')

	#SkjemaData$Sykehusnavn <- enc2native(as.character(SkjemaData$Sykehusnavn))
	#SkjemaData$Sykehusnavn <- iconv(SkjemaData$Sykehusnavn, from = 'UTF-8', to = '')
	#table(Encoding(as.character(SkjemaData$Sykehusnavn)))
	#ind <- which(Encoding(as.character(SkjemaData$Sykehusnavn)) == 'UTF-8')
	#table(SkjemaData$Sykehusnavn) #[ind])
	reshID <- 601161

	setwd('./inst')
	#options(encoded_text_to_latex= 'UTF-8')
	knit('NakkeMndRapp.Rnw', encoding = 'UTF-8')
	texi2pdf(file='NakkeMndRapp.tex')
	knit2pdf('NakkeMndRapp.Rnw')


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

#------------------------------ Andeler flere var --------------------------
#------------------------------ (Fordelinger) --------------------------


valgtVar <- 'Komorbiditet' #OprIndikSmerter'
outfile <- paste0(valgtVar, '.png')	#''

utdata <- NakkeFigAndeler(RegData=RegData, valgtVar=valgtVar, enhetsUtvalg = 1,
                          datoFra='2017-01-01', datoTil='2020-12-31', reshID = 601161)
                          #, outfile=outfile, myelopati = myelopati, fremBak = 1)

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
valgtVar <- 'KomplStemme3mnd'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, ErstatningPreOp,
		  #Fornoyd12mnd, FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, , NytteOpr3mnd, NytteOpr12mnd
		  #Verre3mnd, Verre12mnd, OprIndikMyelopati, OprIndikSmerter, PerOpEnhverKompl, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning

outfile <- '' #paste0(valgtVar, 'Syn.png')	#''	#Navn angis av Jasper
AndelerTid <- NakkeFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                 datoTil=datoTil, enhetsUtvalg=0, tidsenhet = 'Kvartal', outfile=outfile)

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
NakkeData <- read.table('A:/Nakke/allevarnum2018-06-21.csv', sep=';', header=T, encoding = 'UTF-8') #Nakke18012016, allevarnum2016-01-04Num
RegData <- NakkeData
setwd("C:/ResultattjenesteGIT/Nakke/")
RegData <- NakkePreprosess(NakkeRegDataSQL(datoFra = '2020-01-01'))

valgtVar <- 'NDIendr12mnd35pstKI'	#Må velge... Alder, AndreRelSykdommer, Antibiotika,
          #ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd, ASAgrad, BMI, EnhverKompl3mnd
		  #ErstatningPreOp,
		  #FornoydBeh3mnd,FornoydBeh12mnd, Misfor3mnd,Misfor12mnd, KomplinfekDyp3mnd,
		  #KomplinfekOverfl3mnd, KomplStemme3mnd, KomplSvelging3mnd, NDIendr12mnd, NytteOpr3mnd, NytteOpr12mnd
		  #NRSsmerteArmEndr12mnd,Verre3mnd, Verre12mnd, OprIndikMyelopati, Roker, Saardren,
		  #SmertestillPreOp, SymptVarighetNakkeHode, SymptVarighetSmerterUker, UforetrygdPreOp, Utdanning
outfile <- paste0(valgtVar, '_ShusSyn.pdf')	#''	#Navn angis av Jasper
outfile <- '' #paste0(valgtVar, '_ShusSyn.png')	#''	#Navn angis av Jasper
NakkeFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar)
, fremBak = fremBak, myelopati = myelopati, datoFra=datoFra,
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


load('A:/Nakke/NakkeAarsrapp2018.Rdata') #IKKE preprossessert

#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, preprosess=1, valgtVar='KomplStemme3mnd',
                     myelopati=0, fremBak=1, Ngrense=20, outfile='')

NakkeFigAndeler(NakkeData, valgtVar)

#------------------Kvalitetsindikatorer, Resultatportalen / Interaktive nettsider -----------------------
rm(list=ls())
library(nakke)

#---Interaktive nettsider
RegData <- NakkeRegDataSQL(datoFra = '2014-01-01', datoTil = '2020-12-31')
RegData <- NakkePreprosess(RegData)

aar <- 2016:2020
NakkeKvalInd <- dataTilOffVisning(RegData = RegData, valgtVar='NDIendr12mnd35pst', aar=aar, ResPort=0)

kvalIndParam <- c('KomplSvelging3mnd', 'KomplStemme3mnd', 'Komplinfek', 'NDIendr12mnd35pst')
indikatorID <- c('nakke_komplsvelg3mnd', 'nakke_komplstemme3mnd', 'nakke_komplinfek', 'nakke_ndiendr12mnd35pst')   #c('nakke1', 'nakke2', 'nakke3', 'nakke4')

NakkeKvalInd <- data.frame(NULL) #Aar=NULL, ShNavn=NULL)
for (valgtVar in kvalIndParam){
  NakkeKvalInd1 <- NakkeKvalInd
  NakkeKvalInd <- dataTilOffVisning(RegData = RegData, valgtVar, aar=aar, ResPort=0, lagreFil=0)
  #NakkeKvalInd1 <- dataTilOffVisning(...)
  NakkeKvalInd <- rbind(NakkeKvalInd, NakkeKvalInd1)
}
#table(NakkeKvalInd$orgnr, useNA = 'a')
write.table(NakkeKvalInd, file='NakkeTilSKDE.csv', sep=';', row.names = F)

# NakkeData <- read.table('A:/Nakke/allevarnum2019-09-12.csv', sep=';', header=T) #, encoding = 'UTF-8')
# RegData <- NakkePreprosess(NakkeData)
# RegData <- RegData[RegData$Aar>=2014,]

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

##-------- Nøkkeltall, Resultatportalen---

rm(list=ls())
library(nakke)
NakkeData <- read.table('A:/Nakke/allevarnum2020-08-24.csv', sep=';', header=T) #, encoding = 'UTF-8')
#load('A:/Nakke/NakkeAarsrapp2018.Rdata')
RegData <- NakkePreprosess(NakkeData)
NakkeData <- RegData[RegData$Aar>=2014,]

# Antall sykehusavdelinger	?	9
# Antall operasjoner 2018	5302	1091
# Andel som svarer på oppfølging 3 og 12 mnd.
# Andel >70 år	27%	6%
# Gjennomsnittsalder	57	52
# Andel kvinner operert	47,5%	44%
# Fornøyd med behandlingen de fikk på sykehuset 3 mnd. etter kirurgi	91%	91%
# Suksess: «helt restituert» etter «mye bedre» 3 mnd. etter kirurgi	65%	61%* (Fremre nakkekirurgi)
# Andel som angir at de er verre 3 mnd. etter kirurgi	3%	3%* (Fremre nakkekirurgi)

antSh <- colSums(table(as.character(NakkeData$ShNavn),NakkeData$Aar)>0)
antOp <- table(NakkeData$Aar)
andelSvart3mnd <- tapply(NakkeData$OppFolgStatus3mnd,NakkeData$Aar, FUN=function(x){length(which(x==1))/length(x)})
andelSvart12mnd <- tapply(NakkeData$OppFolgStatus12mnd,NakkeData$Aar, FUN=function(x){length(which(x==1))/length(x)})
NakkeData$over70 <- 0
NakkeData$over70[NakkeData$Alder>=70] <- 1
andel70aar <- tapply(NakkeData$over70,NakkeData$Aar, FUN='mean', na.rm=T)
alderGjsn <- tapply(NakkeData$Alder,NakkeData$Aar, FUN='mean', na.rm=T)
alderMedian <- tapply(NakkeData$Alder,NakkeData$Aar, FUN='median', na.rm=T)
andelKvinner <- 1-tapply(NakkeData$ErMann,NakkeData$Aar, FUN='mean', na.rm=T)

#datoTil <- min(datoTil, as.character(Sys.Date()-100))
#Fornøydhet
NakkeData$Fornoyd <- 0
NakkeDataForn <- NakkeData[intersect(which(NakkeData$OppFolgStatus3mnd==1), which(NakkeData$FornoydBeh3mnd %in% 1:5)),
                         c('FornoydBeh3mnd', 'Fornoyd', 'Aar')]
NakkeDataForn$Fornoyd[NakkeDataForn$FornoydBeh3mnd %in% 1:2] <- 1
andelFornoyd <- tapply(NakkeDataForn$Fornoyd, NakkeDataForn$Aar, FUN='mean', na.rm=T)

andelForn <- function(Data, ktr=1){
  Data$Variabel <- 0
  Data$Utfylt <- switch(ktr, Data$OppFolgStatus3mnd, Data$OppFolgStatus12mnd)
  Data$Fornoyd <- switch(ktr, Data$FornoydBeh3mnd, Data$FornoydBeh12mnd)
  ind <- intersect(which(Data$Utfylt==1), which(Data$Fornoyd %in% 1:5))
  DataDum <- Data[ind, c('Fornoyd', 'Variabel', 'Aar')]
  DataDum$Variabel[DataDum$Fornoyd %in% 1:2] <- 1
  andelFornoyd <- tapply(DataDum$Variabel, DataDum$Aar, FUN='mean', na.rm=T)
}
andelForn3mnd <- andelForn(NakkeData,ktr = 1)
andelForn12mnd <- andelForn(NakkeData,ktr = 2)

#Betydelig endring
andelEndring <- function(Data, ktr=2){
  Data$Bedre <- 0
  Data$Verre <- 0
  Data$Utfylt <- switch(ktr, Data$OppFolgStatus3mnd, Data$OppFolgStatus12mnd)
  Data$NytteOpr <- switch(ktr, Data$NytteOpr3mnd, Data$NytteOpr12mnd)
  DataEndring <-  Data[intersect(intersect(which(Data$Utfylt==1),
                                           which(Data$NytteOpr %in% 1:7)),
                                 which(Data$OprMetodeTilgangFremre==1)),
                       c('NytteOpr', 'Bedre', 'Verre','Aar')]
  DataEndring$Bedre[DataEndring$NytteOpr %in% 1:2] <- 1
  DataEndring$Verre[DataEndring$NytteOpr %in% 6:7] <- 1

  andelSuksess <- tapply(DataEndring$Bedre, DataEndring$Aar, FUN='mean', na.rm=T)
  andelVerre <- tapply(DataEndring$Verre, DataEndring$Aar, FUN='mean', na.rm=T)
  return(list('Suksess'= andelSuksess,  'Verre' = andelVerre))
}
endring <- andelEndring(NakkeData, ktr=1)
andelSuksess3mnd <- andelEndring(NakkeData, ktr=1)$Suksess
andelSuksess12mnd <- andelEndring(NakkeData, ktr=2)$Suksess
andelVerre3mnd <- andelEndring(NakkeData, ktr=1)$Verre
andelVerre12mnd <- andelEndring(NakkeData, ktr=2)$Verre



NokkeltallNakke <- rbind(
  'Antall avdelinger' = antSh,
  'Antall operasjoner' = antOp,
  'Svart på oppfølging, 3 mnd.' = andelSvart3mnd,
  'Svart på oppfølging, 12 mnd.' = andelSvart12mnd,
  'Andel over 70 år'	= andel70aar,
  'Gjennomsnittsalder' = alderGjsn,
  #   'Medianalder' = alderMedian,
  'Andel kvinner' = andelKvinner,
  'Fornøyd med behandlingen, 3 mnd. etter' = andelForn3mnd,
  'Fornøyd med behandlingen, 12 mnd. etter' = andelForn12mnd,
  'Helt restituert/mye bedre, 3 mnd. etter fremre kir.' = andelSuksess3mnd,
  'Helt restituert/mye bedre, 12 mnd. etter fremre kir.' = andelSuksess12mnd,
  'Verre 3 mnd. etter fremre kir.' = andelVerre3mnd,
  'Verre 12 mnd. etter fremre kir.' = andelVerre12mnd
)
tabNokkeltallNakke <- cbind(row.names(NokkeltallNakke),NokkeltallNakke)
#tabNokkeltallNakke[ ,c('2017','2018')]
write.table(NokkeltallNakke, file = 'A:/Resultatportalen/NokkeltallNakke.csv', row.names=T,
            sep=';', fileEncoding = 'UTF-8' )

tabShResh <- unique(NakkeData[,c("ReshId",'ShNavn')])
write.table(tabShResh, file = 'P:/Registerinfo og historie/Nakke/ReshNakke.csv', row.names=T,
            sep=';', fileEncoding = 'UTF-8' )

#------------------ Data til NPR, Dekningsgradsanalyse
'SELECT ForlopsID, Alder, Kjonn, Organisasjon, AvdRESH, SykehusNavn,
OprDato, OprKode, InngrepType, Dagkirurgi
FROM allevarnum
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

