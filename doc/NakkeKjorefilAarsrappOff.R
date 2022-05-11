#**************************************************
#********************* Tilrettelegge filer ****************************
#Nakke
library(nakke)
library(xtable)
setwd('~/speil/aarsrapp')
aarsRappAar <- 2021

startAar <- 2012
datoFra <- paste0(startAar,'-01-01')
datoFra1aar <- paste0(aarsRappAar,'-01-01')
datoFra2aar <- paste0(aarsRappAar-1,'-01-01')
datoFra3aar <- paste0(aarsRappAar-2,'-01-01')
datoTil12mnd <- paste0(aarsRappAar-1,'-12-31')
datoTil <- paste0(aarsRappAar,'-12-31')
aar <- aarsRappAar
aar2 <- (aarsRappAar-1):aarsRappAar
aar2_12mnd <- (aarsRappAar-2):(aarsRappAar-1)
tidlAar <- aarsRappAar-1
tidlAar2 <- (aarsRappAar-3):(aarsRappAar-2)

NakkeDataRaa <- NakkeRegDataSQL(datoTil = datoTil)
NakkeData <- NakkePreprosess(NakkeDataRaa)

#write.table(NakkeData, file = 'NakkeDataAarsrapp2021.csv', sep = ';', na='', row.names = F, fileEncoding = 'latin1') #'UTF-8')




#----------------------------Kvalitetsindikatorer:

#NDI etter fremre nakkekirurgi hos pasienter operert for cervikal radikulopati (ekskl. myelopati)
#per sykehus. Her skal gjennomsnittlig forbedring brukes. Det som er over gj.sn blir grønt, resten gult
# NakkeFigGjsnGrVar(RegData = NakkeData, valgtVar='NDIendr12mnd',
#                   datoFra=datoFra2aar, datoTil=datoTil12mnd,
#                   myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeNDIendr12mnd.pdf')
# NakkeFigGjsnTid(RegData = NakkeData, valgtVar='NDIendr12mnd',
#                   myelopati=0, fremBak=1, outfile='NakkeNDIendr12mndTid.pdf')

NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='NDIendr12mnd35pst', datoFra = datoFra3aar , datoTil = datoTil12mnd,
                     fremBak = 1, myelopati = 0, outfile='' )#'NakkeNDIendr12mnd35pstSh.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='NDIendr12mnd35pstKI', datoTil = datoTil12mnd,
                 fremBak=1, myelopati=0, outfile='NakkeNDIendr12mnd35pstTid.pdf')


#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra1aar, valgtVar='Komplinfek',
                     Ngrense=20, outfile= 'NakkeKomplinfekSh.pdf')
dum <- NakkeFigAndelerGrVarAar(RegData=NakkeData, preprosess=0, valgtVar='Komplinfek',
                        Ngrense=20, ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='NakkeKomplinfekShAar.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='Komplinfek', outfile='NakkeInfekTid.pdf')

#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra1aar,  valgtVar='KomplStemme3mnd',
                   myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplStemme3mndSh.pdf')

NakkeFigAndelerGrVarAar(RegData=NakkeData, preprosess=0, valgtVar='KomplStemme3mnd',
                     myelopati=0, fremBak=1, Ngrense=20,
                     ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='NakkeKomplStemme3mndShAar.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='KomplStemme3mnd',
                 myelopati=0, fremBak=1, outfile='NakkeKomplStemme3mndTid.pdf')


#Svelgvansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra1aar,  valgtVar='KomplSvelging3mnd',
                        myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplSvelging3mndSh.pdf')
NakkeFigAndelerGrVarAar(RegData=NakkeData, preprosess=0, valgtVar='KomplSvelging3mnd',
                   myelopati=0, fremBak=1, Ngrense=20,
                   ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='NakkeKomplSvelging3mndShAar.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='KomplSvelging3mnd',
                 myelopati=0, fremBak=1, outfile='NakkeKomplSvelging3mndTid.pdf')



#---Figurer---------

NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='Alder', datoFra=datoFra1aar, outfile='NakkeAlder70Sh.pdf')

dum <- NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='OprIndikMyelopati',
                            datoFra=datoFra1aar, outfile='NakkeOprIndikMyelopatiSh.pdf')

dum <- NakkeFigAndelTid(RegData=NakkeData, valgtVar='Saardren', fremBak = 1, myelopati = 0,
                        outfile='NakkeSaardrenUmFTid.pdf')

dum <- NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='Saardren', fremBak = 1, myelopati = 0,
                            datoFra=datoFra1aar, outfile='NakkeSaardrenUmFSh.pdf')

dum <- NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='NRSsmerteArmEndr12mnd',
                            datoFra=datoFra3aar, datoTil = datoTil12mnd,
                            fremBak = 1, myelopati = 0, outfile='NakkeNRSsmerteArmEndr12mndUmFSh.pdf')

dum <- NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='FornoydBeh12mnd',
                            datoFra=datoFra3aar, datoTil = datoTil12mnd,
                            fremBak = 1, outfile='NakkeFornoydBeh12mndFremSh.pdf')

#Pasienter med myelopati (ja) (både bakre og fremre tilgang) og lage figur for gj. sn. ODI forbedring per sykehus. (etterbestilling).
NakkeFigGjsnGrVar(RegData = NakkeData, valgtVar='NDIendr12mnd',
                  #datoFra=datoFra3aar, datoTil=datoTil12mnd,
                  myelopati=1, Ngrense=20, outfile='NakkeNDIendr12mnd.pdf')



#-------------------------------------
AndelPst <- function(variabel,teller,nevner){
      sprintf('%.0f' , sum(variabel %in% teller)/
                    sum(variabel %in% nevner)*100)}

#----- TABELLER og tall ------------------------------------------------------------------
NakkeUtvalg <- NakkeUtvalgEnh(RegData=NakkeData, datoFra=datoFra1aar, datoTil=datoTil)
NakkeData1aar <- NakkeUtvalg$RegData


tabAvdNnakke <- addmargins(table(NakkeData[c('ShNavn','Aar')]))
antKol <- ncol(tabAvdNnakke)
tabAvdN5nakke <- tabAvdNnakke[,(antKol-5):antKol]
rownames(tabAvdN5nakke)[dim(tabAvdN5nakke)[1] ]<- 'TOTALT, alle avdelinger:'
colnames(tabAvdN5nakke)[dim(tabAvdN5nakke)[2] ]<- paste0(min(NakkeData$Aar),'-',aarsRappAar)

xtable(tabAvdN5nakke, digits=0, align=c('l', rep('r', 6)),
       caption=paste0('Antall registreringer av nakkeoperasjoner ved hver avdeling siste 5 år, samt totalt siden ',
                      min(NakkeData$Aar, na.rm=T),'.'), label = 'tab:AntRegNakke')


#Totalt antall operasjoner siden 2012: 9326
(NtotNakke <- dim(NakkeData)[1])

#Kjønnsfordeling siden 2012, prosent, kvinner menn: "44.8" "55.2"
(tabKjPstNakke <- sprintf('%.1f',table(NakkeData$ErMann)/NtotNakke*100))

#Tall for 2019:
#Gjennomsnittsalder:
(AlderGjsnNakke <- round(mean(NakkeData1aar$Alder, na.rm = T)))
#Andel kvinner:
(Kvinner <- prop.table(table(NakkeData1aar$ErMann))[1]*100)

#Andel elektive:
(Elektivt <- sprintf('%.0f' , sum(NakkeData1aar$OperasjonsKategori == 1)/
                          sum(NakkeData1aar$OperasjonsKategori %in% 1:3)*100))
#Andel med ASA-grad 3-5: 11%
(ASA345 <- AndelPst(NakkeData1aar$ASAgrad, 3:5, 1:5))

#Forekomst av sårinfeksjon (totalt for bakre og fremre nakkekirurgi): 2,2%
'%i%' <- intersect
ind <- which(NakkeData1aar$OppFolgStatus3mnd == 1) %i%
      union(which(NakkeData1aar$KomplinfekDyp3mnd %in% 0:1),
            which(NakkeData1aar$KomplinfekOverfl3mnd %in% 0:1))
NakkeData1aarDum <- NakkeData1aar[ind, ]
Saarinf <- length(union(which(NakkeData1aarDum$KomplinfekDyp3mnd==1),
                        which(NakkeData1aarDum$KomplinfekOverfl3mnd==1)))
(AndelSaarinf <- round(Saarinf/dim(NakkeData1aarDum)[1]*100, 1))






#----- Testing av data-----
#Dobbeltregistrering
RegData <- NakkeData
testDato <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','OprDato')], drop=TRUE, FUN=length)
print(testDato[which(testDato$x >1), ], row.names = F)
RegData$Mnd <- as.POSIXlt(RegData$InnDato)$mon +1
RegData$Mnd <- RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
testMnd <- aggregate(RegData$OprDato, by=RegData[ ,c('PasientID','Mnd','Aar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
print(duplMnd, row.names = F)
testAar <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','Aar')], drop=TRUE, FUN=length)
sum(testAar$x >1)

