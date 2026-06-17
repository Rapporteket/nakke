  # **************************************************
# ********************* Tilrettelegge filer ****************************
#Nakke
library(nakke)
library(xtable)
setwd('../Aarsrapp/NKR/') #Nakke23/
aarsRappAar <- 2025

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

source("C:/Users/lro2402unn/RegistreGIT/nakke/dev/sysSetenv.R")
NakkeDataRaa <- NakkeHentRegData(datoTil = datoTil) #13927
NakkeData <- NakkePreprosess(NakkeDataRaa)
NakkeData1aar <- NakkeUtvalgEnh(RegData=NakkeData, datoFra=datoFra1aar, datoTil=datoTil)$RegData

#Datasjekk
SykehusNavnResh <- unique(NakkeDataRaa[ ,c('SykehusNavn', 'ReshId')])
ShNavnResh <- unique(NakkeData[ ,c('SykehusNavn', 'ReshId')])
#write.csv2(ShNavnResh[order(ShNavnResh$SykehusNavn), ], file = 'NakkeSykehusNavnAVDResh.csv', row.names = F, fileEncoding = 'latin1')

#Til Tore
# RegData <- NakkePreprosess(RegData = NakkeRegDataSQL(medProm = 1))
# write.table(RegData, file = 'NakkeDataAarsrapp2023alt.csv', sep = ';', na='', row.names = F, fileEncoding = 'latin1') #'UTF-8')


#----------------------------Kvalitetsindikatorer:--------

#NDI etter fremre nakkekirurgi hos pasienter operert for cervikal radikulopati (ekskl. myelopati)

NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='NDIendr12mnd35pst', datoFra = datoFra3aar , datoTil = datoTil12mnd,
                     fremBak = 1, myelopati = 0, Ngrense=20, outfile='NakkeNDIendr12mnd35pstSh.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='NDIendr12mnd35pst', datoTil = datoTil12mnd,
                 fremBak=1, myelopati=0, Ngrense=20,outfile='NakkeNDIendr12mnd35pstTid.pdf')


#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, preprosess=0, datoFra=datoFra2aar,  valgtVar='KomplStemme3mnd',
                   myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplStemme3mndSh.pdf')

NakkeFigAndelTid(RegData=NakkeData, preprosess=0, valgtVar='KomplStemme3mnd',
                 myelopati=0, fremBak=1, outfile='NakkeKomplStemme3mndTid.pdf')

NakkeFigAndelerGrVar(RegData=NakkeData, preprosess=0, datoFra=datoFra3aar, datoTil = datoTil12mnd,
                     valgtVar='KomplStemme12mnd',
                     myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplStemme12mndSh.pdf')


#Svelgvansker, 3 og 12 mnd (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra2aar,  valgtVar='KomplSvelging3mnd',
                        myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplSvelging3mndSh.pdf')
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra3aar, datoTil = datoTil12mnd,
                     valgtVar='KomplSvelging12mnd',
                     myelopati=0, fremBak=1, Ngrense=20, outfile='NakkeKomplSvelging12mndSh.pdf')

NakkeFigAndelTid(RegData=NakkeData, valgtVar='KomplSvelging3mnd',
                 myelopati=0, fremBak=1, outfile='NakkeKomplSvelging3mndTid.pdf')



#---Figurer---------
#Registreringsforsinkelse:
NakkeFigAndeler(RegData = NakkeData1aar, valgtVar = 'regForsinkelse', outfile = 'NakkeRegForsinkFord.pdf')
NakkeFigAndelerGrVar(RegData=NakkeData1aar, valgtVar='regForsinkelse', Ngrense=10, outfile= 'NakkeRegForsinkSh.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='regForsinkelse', datoFra = '2022-01-01', outfile='NakkeRegForsinkTid.pdf')

#Oppf3mnd
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra1aar, valgtVar='Oppf3mnd',
                     Ngrense=20, outfile= 'NakkeOppf3mndSh.pdf')
NakkeFigAndelTid(RegData=NakkeData, valgtVar='Oppf3mnd', outfile='NakkeOppf3mndTid.pdf')


# NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='Alder', datoFra=datoFra1aar, outfile='NakkeAlder70Sh.pdf')
NakkeFigAndelTid(RegData=NakkeData, preprosess=0, valgtVar='Alder', outfile='NakkeAlder70Tid.pdf')

dum <- NakkeFigAndelerGrVar(RegData=NakkeData, valgtVar='OprIndikMyelopati',
                            datoFra=datoFra1aar, Ngrense=20,outfile='NakkeOprIndikMyelopatiSh.pdf')

dum <- NakkeFigAndelTid(RegData=NakkeData, valgtVar='Saardren', fremBak = 1, myelopati = 0,
                        outfile='NakkeSaardrenUmFTid.pdf')
#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav, ikke lenger kval.ind.
NakkeFigAndelerGrVar(RegData=NakkeData, datoFra=datoFra1aar, valgtVar='Komplinfek',
                     Ngrense=20, outfile= 'NakkeKomplinfekSh.pdf')

#LiggeDognPostop
NakkeFigGjsnGrVar(RegData=NakkeData, datoFra=datoFra1aar, valgtVar='LiggeDognPostop',
                     Ngrense=10, myelopati = 0, valgtMaal = 'Med',
                     outfile= 'NakkeLiggeDognPostopMedSh_uMy.pdf')
NakkeFigGjsnGrVar(RegData=NakkeData, datoFra=datoFra1aar, valgtVar='LiggeDognPostop',
                     Ngrense=10, myelopati = 1,valgtMaal = 'Med',
                     outfile= 'NakkeLiggeDognPostopMedSh_mMy.pdf')

#Div figurer:

for (variabler in c('Inngrep','OpAndreEndosk', 'ventetidSpesOp',  'diffPasUtfOp',
                    'ventetidHenvTimePol') ){
  NakkeFigAndeler(RegData = NakkeData1aar, valgtVar = variabler,
                  outfile =paste0('Nakke', variabler, 'Ford.pdf'))
}

for (variabler in c('OpAndreEndosk', 'ventetidSpesOp',  'diffPasUtfOp',
                    'ventetidHenvTimePol', 'trombProfylLett') ){
  NakkeFigAndelerGrVar(RegData=NakkeData1aar, valgtVar=variabler,
                       outfile = paste0('Nakke', variabler, 'Sh.pdf'))
}

NakkeFigAndelTid(RegData=NakkeData, valgtVar='OpAndreEndosk', tidsenhet = 'Aar'
                 ,outfile='NakkeOpAndreEndoskTid.pdf')

for (variabler in c('NDIscore3mnd', 'diffUtf3mnd')){  # diffUtf12mnd'NDIscore12mnd',
  NakkeFigGjsnGrVar(RegData = NakkeData1aar, valgtVar = variabler, valgtMaal='',
                  outfile = paste0('Nakke', variabler, 'GjsnSh.pdf'))
}
NakkeFigGjsnGrVar(RegData = NakkeData, datoFra = datoFra2aar, datoTil = datoTil12mnd,
                  valgtVar = 'diffUtf12mnd', valgtMaal='',
                  outfile = 'NakkediffUtf12mndGjsnSh.pdf')
# Innført sent i 2025:valgtVar <- c('MJOAendr3mnd', 'MJOAendr12mnd')
#
# MedIQR <- plot(NakkeData$SykehusNavn, as.numeric(NakkeData$Alder), notch=TRUE, plot=FALSE)
# c('MJOAsumPre', 'MJOAsum3mnd', 'MJOAsum12mnd')

#-------------------------------------
AndelPst <- function(variabel,teller,nevner){
      sprintf('%.1f' , sum(variabel %in% teller)/
                    sum(variabel %in% nevner)*100)}

#----- TABELLER og tall ------------------------------------------------------------------

tabAvdNnakke <- addmargins(table(NakkeData[c('SykehusNavn','Aar')]))
antKol <- ncol(tabAvdNnakke)
tabAvdN5nakke <- tabAvdNnakke[,(antKol-5):antKol]
rownames(tabAvdN5nakke)[dim(tabAvdN5nakke)[1] ]<- 'TOTALT, alle avdelinger:'
colnames(tabAvdN5nakke)[dim(tabAvdN5nakke)[2] ]<- paste0(min(NakkeData$Aar),'-',aarsRappAar)

xtable(tabAvdN5nakke, digits=0, align=c('l', rep('r', 6)),
       caption=paste0('Antall registreringer av nakkeoperasjoner ved hver avdeling siste 5 år, samt totalt siden ',
                      min(NakkeData$Aar, na.rm=T),'.'), label = 'tab:AntRegNakke')


#Totalt antall operasjoner siden 2012:
(NtotNakke <- dim(NakkeData)[1])

#Kjønnsfordeling siden 2012, prosent, kvinner menn:
(tabKjPstNakke <- sprintf('%.1f',table(NakkeData$ErMann)/NtotNakke*100))

#Tall for årsrapportåret:
#Gjennomsnittsalder:
(AlderGjsnNakke <- round(mean(NakkeData1aar$Alder, na.rm = T)))
#Andel kvinner, årsrapportåret:
(Kvinner <- prop.table(table(NakkeData1aar$ErMann))[1]*100)

#Andel elektive:
(Elektivt <- sprintf('%.0f' , sum(NakkeData1aar$OperasjonsKategori == 1)/
                          sum(NakkeData1aar$OperasjonsKategori %in% 1:3)*100))
#Andel med ASA-grad 3-5:
(ASA345 <- AndelPst(NakkeData1aar$ASAgrad, 3:5, 1:5))

#Forekomst av sårinfeksjon (totalt for bakre og fremre nakkekirurgi): 2,2%
'%i%' <- intersect
ind <- which(NakkeData1aar$StatusUtfyll3mnd == 1) %i%
      union(which(NakkeData1aar$KomplinfekDyp3mnd %in% 0:1),
            which(NakkeData1aar$KomplinfekOverfl3mnd %in% 0:1))
NakkeData1aarDum <- NakkeData1aar[ind, ]
Saarinf <- length(union(which(NakkeData1aarDum$KomplinfekDyp3mnd==1),
                        which(NakkeData1aarDum$KomplinfekOverfl3mnd==1)))
(AndelSaarinf <- round(Saarinf/dim(NakkeData1aarDum)[1]*100, 1))


#-------Data til SKDE interaktive nettsider----------

source("dev/sysSetenv.R")
library(nakke)
#library(xtable)
NakkeDataRaa <- NakkeHentRegData() #Tar også med inneværende år
NakkeData <- NakkePreprosess(NakkeDataRaa)

#Sjekk om det har kommet nye avdelinger:
ny <- setdiff(sort(unique(NakkeData$ReshId)), sort(names(nyID)))
NakkeData$SykehusNavn[match(ny, NakkeData$ReshId)]
# NB: Aktuelle utvalg for fremBak, myelopati osv. er lagt inn i funksjonen (dataTilOffVisning)

setwd('../Aarsrapp/NKR')
#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
#valgtVar='KomplStemme3mnd', myelopati=0, fremBak=1, Ngrense=20
nakke1 <- nakke::dataTilOffVisning(RegData = NakkeData,
                                   valgtVar='KomplStemme3mnd',
                                  # aar=aarTilVisning,
                                   slaaSmToAar=0)


#Svelgevansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
#valgtVar='KomplSvelging3mnd', myelopati=0, fremBak=1, Ngrense=20
nakke2 <- nakke::dataTilOffVisning(RegData = NakkeData,
                                   valgtVar='KomplSvelging3mnd',
                                  # aar=aarTilVisning,
                                   slaaSmToAar=0)

# #Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav FJERNET F.O.M 2022.
# #valgtVar='Komplinfek', Ngrense=20,
#     nakke3 <- nakke::dataTilOffVisning(RegData = NakkeData,
#                                      valgtVar='Komplinfek',
#                                      aar=aarTilVisning,
#                                      slaaSmToAar=0)

#NDI etter fremre nakkekirurgi hos pasienter operert for cervikal radikulopati (ekskl. myelopati)
# valgtVar='NDIendr12mnd35pst', fremBak = 1, myelopati = 0, Ngrense=20, outfile='NakkeNDIendr12mnd35pstSh.pdf')
nakke4 <- nakke::dataTilOffVisning(RegData = NakkeData,
                                   valgtVar='NDIendr12mnd35pst',
                                  # aar=aarTilVisning,
                                   slaaSmToAar=0)

FellesFilNakke <- rbind(nakke1, nakke2, nakke4)
write.table(FellesFilNakke, file = 'NKRnakkeKvalInd.csv', sep = ';', row.names = F)
table(FellesFilNakke$ind_id, FellesFilNakke$year)



#----------------Kompletthet av kvalitetsindikatorvariabler---------
source("C:/Users/lro2402unn/RegistreGIT/nakke/dev/sysSetenv.R")
library(nakke)
NakkeDataRaa <- NakkeHentRegData(datoFra = '2023-01-01', datoTil = '2025-12-31') #13927
NakkeData <- NakkePreprosess(NakkeDataRaa)
NakkeData <- NakkeUtvalgEnh(RegData=NakkeData, fremBak = 1, myelopati = 0)$RegData
NakkeData1aar <- NakkeUtvalgEnh(RegData=NakkeData, datoFra='2025-01-01')$RegData

#Utvalg - alle kvalitetsindikatorer er filtrert på fremBak = 1, myelopati = 0
table(NakkeData$OprMetodeTilgangFremre, useNA = 'a') #fremBak = 1, OprMetodeTilgangFremre==1
#alle registrert.
table(NakkeData$OprIndikMyelopati, useNA = 'a') #myelopati=0
#alle registrert.

#NDI etter fremre nakkekirurgi hos pasienter operert for cervikal radikulopati (ekskl. myelopati)
# valgtVar='NDIendr12mnd35pst', 2023 og 2024
# fremBak = 1, myelopati = 0
NakkeDataNDI <- NakkeUtvalgEnh(RegData=NakkeData, datoTil = '2024-12-31')$RegData
NakkeDataNDI$NDIdiff <- NakkeDataNDI$NDIscorePreOp - NakkeDataNDI$NDIscore12mnd
100*sum(is.na(NakkeDataNDI$NDIdiff))/dim(NakkeDataNDI)[1] #28,1% manglende

#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
#2024 og 2025 valgtVar='KomplStemme3mnd',StatusUtfyll3mnd == 1, KomplStemme3mnd %in% 0:1
100*prop.table(table(NakkeData$KomplStemme3mnd, useNA = 'a'))
#20.3%

#Svelgvansker, 3 og 12 mnd (ikke-myelopati, fremre tilgang) – lav
#2024 og 2025 valgtVar='KomplSvelging3mnd',
100*prop.table(table(NakkeData$KomplSvelging3mnd, useNA = 'a'))
#20.3%


#----------Dekningsgrad---------------------
ReshSh <- unique(NakkeData[ ,c('ReshId', 'SykehusNavn')])
write.csv2(ReshSh, file = 'data/ReshShNavn', row.names = F)





#----- Testing av data-----
#Dobbeltregistrering
RegData <- NakkeData
testDato <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','OprDato')], drop=TRUE, FUN=length)
print(testDato[which(testDato$x >1), ], row.names = F)
RegData$Mnd <- as.POSIXlt(RegData$OprDato)$mon +1
RegData$Mnd <- RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
testMnd <- aggregate(RegData$OprDato, by=RegData[ ,c('PasientID','Mnd','Aar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
print(duplMnd, row.names = F)
testAar <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','Aar')], drop=TRUE, FUN=length)
sum(testAar$x >1)












#-------Traktplott med justering---------------------
library(nakke)
source("dev/sysSetenv.R")


library(ggplot2)
library(tidyr)

#------------------Eksempel, intro til traktplott---------------
# Make up some data, as if it was from a regression model
# with observed and predicted (expected) events.
# Add a ratio (SR) of observed to expected, our indicator
# Scatter plot in ggplot
# Now add a central line, as 1 is the average/expected value in this case.
# Add a 95% Poisson limit, by using the density function to get the
# quantile value for each 'expected'.
#Dette og det under er hentet fra:
#https://cran.r-project.org/web/packages/FunnelPlotR/vignettes/funnel_plots.html

devtools::install_github("https://github.com/nhs-r-community/FunnelPlotR")
library(FunnelPlotR)
library(COUNT)
library(ggplot2)
data(medpar)
medpar$provnum <- factor(medpar$provnum)
medpar$los <- as.numeric(medpar$los)

mod <- glm(los ~ hmo + died + age80 + factor(type)
           , family = "poisson" #modell, siden har count-data
           , data = medpar)
#mod er ei liste med mange objekter, også datasettet.
summary(mod)
# Nå har vi en lineær modell som kan sammenlignes med observert
medpar$prds <- predict(mod, type = "response")
#Build plot
#Now we can build a funnel plot object with standard Poisson limits, and outliers labelled.
#The function returns an S3 object, with various methods including
#print(), outlier(), limits(), source_data() etc. See the help file: ?funnel_plot for more details.
FunnelPlotR::funnel_plot(
  medpar, numerator = los, denominator = prds, group = provnum
  , title = "Length of Stay Funnel plot for `medpar` data"
  , draw_unadjusted = TRUE, draw_adjusted = FALSE
  , label = "outlier", limit = 99
)
# Overdispersion = There is more variation in our data than we would expect. We get too many outliers!
#The following ratio should be 1 if our data are conforming to Poisson distribution assumption (conditional mean = variance).
#If it is greater than 1, we have overdispersion:

  sum(mod$weights * mod$residuals^2) / mod$df.residual

#applying overdispersed limits using either SHMI or Spiegelhalter methods adjust for this by inflating the limits:
  FunnelPlotR::funnel_plot(
    medpar, numerator = los, denominator = prds, group = provnum
    , title = "Length of Stay Funnel plot for `medpar` data"
    , draw_unadjusted = FALSE, draw_adjusted = TRUE
    , data_type = "SR", sr_method = "SHMI"
    , label = "outlier", limit = 99
  )

#----------------------Traktplott for nakkedata
  source("dev/sysSetenv.R")
  RegData1 <- NakkePreprosess(NakkeHentRegData(datoFra = '2024-01-01')) #, medOppf = 1))
  RegData <- NakkeUtvalgEnh(RegData = RegData1, datoTil = '2024-12-31', myelopati = 0)$RegData
  RegData <- RegData[RegData$Inngrep %in% c(1,3), ]
  RegData <- NakkeVarTilrettelegg(RegData = RegData, valgtVar = 'NDIendr12mnd35pst')$RegData
RegData$Ant <- 1
RegData$ASAover2 <- RegData$ASAgrad>2
RegData$ErTidlOpr <- RegData$TidlOpr %in% 1:3
RegData <- RegData[which(RegData$Roker %in% 0:1), ]
RegData <- RegData[which(RegData$Utdanning %in% 1:5), ]
RegData$UtdHoy <- ifelse(RegData$Utdanning %in% 4:5, 1,0)
RegData$Fedme <- ifelse(RegData$BMI >= 30, 1,0)
RegData$Sykepenger <- ifelse(RegData$ArbeidstausPreOp %in% 6:10, 1,0)
RegData <- RegData[which(RegData$SymptVarighetArmer %in% 2:5), ]
RegData$LangArmsm <- ifelse(RegData$SymptVarighetArmer %in% 4:5, 1,0)
RegData <- RegData[which(RegData$EqAngstPreOp %in% 1:5), ]
RegData$Angst <- ifelse(RegData$EqAngstPreOp >1, 1,0)
RegData <- RegData[which(RegData$AntallNivaaOpr > 0), ]
RegData$OpFlereNivaa <- ifelse(RegData$AntallNivaaOpr >1, 1,0)


#plotUjust <-
   FunnelPlotR::funnel_plot(RegData,
    numerator = Variabel, denominator = Ant, group = SykehusNavn,
    title = "NDIendr12mnd35pst, ujustert ",
    draw_unadjusted = TRUE, draw_adjusted = FALSE,
    data_type = "RC", #  sr_method = "SHMI",
    label = "outlier", limit = 95,
    y_label = "NDIendr>35%"
  )
ggsave('plotUjust.pdf')



  # Traktplott, justerte analyser
  # Utvalg:
  #   (operasjon_type= 1 ELLER operasjon_type=3) & OprIndikMyelopati=0

  #Utfall  - avhengig variabel
  # 1.	Mean differanse: NDIscore12mnd – NDIscorePreOp =NDImeanForbedring12Mnd
  # 2.	 ((NDIscore12mnd – NDIscorePreOp)/ NDIscorePreOp)* 100= prosentvis forbedring av NDI: NDI%Forbedring12Mnd.
  #Lag så dikotom variabel: NDI%Forbedring12Mnd ≥ 35% (ja/nei), (suksess ja/nei)



mod <- glm(Variabel ~ Alder + ErMann + NDIscorePreOp + ASAover2 + TidlOpr + Roker +
             UtdHoy + Fedme + Sykepenger + Angst + OpFlereNivaa #LangArmsm +
           , family = "poisson" #modell, poisson for countdata
           , data = RegData, na.action = 'na.exclude')
summary(mod)
test <- predict(mod, type = "response")
RegData$Estimert <- predict(mod, type = "response")
RegData <- RegData[!is.na(RegData$Estimert), ]

FunnelPlotR::funnel_plot(RegData,
                         numerator = Estimert, denominator = Ant, group = SykehusNavn,
                         title = "NDIendr12mnd35pst, justert ",
                         draw_unadjusted = TRUE, draw_adjusted = FALSE,
                         data_type = "RC", #  sr_method = "SHMI",
                         label = "outlier", limit = 95,
                         y_label = "NDIendr>35%",
                         x_range = c(10, max(table(RegData$SykehusNavn)))
)
ggsave('NDI35pst_estimert.pdf')

  # Kovariater («independent variables»):
  Alder
  Kjonn  (mann/kvinne)
  NDIscorePreOp
  ASAgrad: >2 (ja/nei) dikotom variabel
  TidlOpr (ja/nei)
  Roker (1ja, 0 nei), 9=NULL
  Utdanning (1-3=1 (lav utdanning) og 4-5= 2 (Høy utdanning)), 9=NULL
  BMI (BMI ≥ 30.0=1 (fedme)) og <30= 0 (ikke fedme), 9=NULL
  ArbeidstausPreOp (6-10=1 (mottar sykepenger) og 1-5=0 (mottar ikke sykepenger)), 99= NULL
  SymptVarighetArmer (1=NULL, 2-3=1 (symtomvarighet arm under ett år) og 4-5= 2 (symtomvarighet arm over ett år)), 9= NULL.
  EqAngstPreOp (1=1 (ingen angst eller depresjon) og >1=2 (mild til betydelig angst eller depresjon)), 9= NULL.
  AntallNivaaOpr (0=NULL, 1=1 (ett nivå) og >1=2 (mer enn ett nivå)), 9=NULL.


  Presentasjon av resultat/figurer:
    Ønskelig med analyse for hele perioden med og uten justering for kovariater.
  I tillegg splitte output på tidsperioder:
    Hele perioden: operert i perioden 2012-2024 + perioden 2023-2024
  Dette blir til sammen 8 funnel plot.

  Fixed effects (sykehus)



  # Aggregering: antall og andel per sykehus
  agg <- RegData %>%
    group_by(SykehusNavn) %>%
    summarise(
      n = n(),
      rate = mean(Variabel)
    )

  # Traktplott
  funnel_plot(agg,
              numerator   = agg$rate * agg$n,   # antall "1" i Variabel
              denominator = agg$n,              # antall per sykehus
              group       = agg$SykehusNavn,
              limit       = 95,                 # 95% konfidensgrenser
              label       = TRUE,               # vis sykehusnavn
              title       = "Traktplott for indikatorvariabelen 'Variabel'",
              xlab        = "Antall observasjoner per sykehus",
              ylab        = "Andel (Variabel)"
  )




