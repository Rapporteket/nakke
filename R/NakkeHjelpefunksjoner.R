#' Hjelpefunksjoner for Nakke
#' Detaljer. kommer senere
#' Fil som inneholder hjelpefunksjoner.
#'
#'  Tilrettelegge tidsenhetvariabel. Legger til tidsenhetene Aar, Halvaar, Mnd og Kvartal
#' @param RegData dataramme
#' @param tidsenhet valgt tidsenhet for visninga
#' @param tab Hmmm
#'
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar', tab=0) {
      #Lager sorteringsvariabel for tidsenhet:
      RegData$TidsEnhetSort <- switch(tidsenhet,
                                      Aar = RegData$Aar-min(RegData$Aar)+1,
                                      Mnd = RegData$MndNum-min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                          +(RegData$Aar-min(RegData$Aar))*12, #format(RegData$InnDato, '%b%y'), #
                                      Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*4,
                                      Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*2
      )
      # format.Date(seq(from=as.Date('2018-01-01'),
      #                 to=as.Date('2018-09-01'), by='month'), format = '%b%y')

      tidtxt <- switch(tidsenhet,
                       #Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                        #           sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       #Mnd = RegData$MndAar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)],
                       # Mnd = format.Date(seq(from=min(as.Date(RegData$InnDato), na.rm = T),
                       #                       to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%b%y'),
                       #Henter fullt månedsnavn og forkorter etterpå.
                       Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$InnDato), na.rm = T)), 'month'),
                                         to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))

      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[tab], substrRight(tidtxt, 2))}
      #RegData$TidsEnhetSort <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      #RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE, labels=tidtxt)
      #a <- factor(c(1:10,3,2,4,3,7,9,4), levels=1:11, labels = letters[1:11])
#table(a)

      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}


#' Lage tulledata (simulerte data) for åpen publisering
#' @export
lageTulleData <- function(RegData, varBort=NA, antSh=26, antObs=20000) {
  #Må også legge på resh som svarer til sykehusnavn.
      library(synthpop)
      library(dplyr)
      #ForlopsID <- RegData$ForlopsID
  if (!is.na(varBort[1])) {
      RegData <- RegData[,-which(names(RegData) %in% varBort)]}
      #RegData <- RegData[sample(1:dim(RegData)[1], antObs, replace = T),]
      sykehus <- cbind(ShNavn=paste('Sykehus', LETTERS[1:antSh]),
                       ReshId=1:antSh)
      fordelingPasienter <- sample(1:10,antSh, replace = TRUE)
      indSample <-  sample(1:antSh, prob=fordelingPasienter/sum(fordelingPasienter),
                           replace = TRUE, size=antObs)

      RegDataSyn <- synthpop::syn(RegData, method = "sample", k=antObs, seed = 500) #Trekker med tilbakelegging
      RegData <- data.frame(RegDataSyn$syn)
      RegData[c('SykehusNavn','ReshId')] <- sykehus[indSample,]

	  return(RegData)
}




#' Tilrettelegge data for offentlig visning.
#'
#' @param RegData - data
#' @param valgtVar - 'KomplSvelging3mnd', 'KomplStemme3mnd', 'Komplinfek', 'NDIendr12mnd35pst
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param lagreFil automatisk lagre fila som genereres? 0-nei, 1-ja (standard)
#' @inheritParams NakkeFigAndeler
#' @inheritParams NakkeUtvalgEnh
#' @return Datafil til Resultatportalen
#' @export

dataTilOffVisning <- function(RegData = RegData, valgtVar, datoFra = '2014-01-01', aar=0,
                           ResPort=1, filUt='dummy', lagreFil=1){ #hovedkat=99, myelopati=99, fremBak=0, indID = 'indDummy',

  kvalIndParam <- c('KomplSvelging3mnd', 'KomplStemme3mnd', 'Komplinfek', 'NDIendr12mnd35pst')
  myelopati <- ifelse(valgtVar %in% c('KomplStemme3mnd', 'KomplSvelging3mnd', 'NDIendr12mnd35pst'),  0, 99)
  fremBak <- ifelse(valgtVar == 'NDIendr12mnd35pst', 1, 0)

  filUt <- paste0('Nakke', ifelse(filUt=='dummy',  valgtVar, filUt), c('_SKDE', '_ResPort')[ResPort+1],'.csv')
  NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
  NakkeUtvalg <- NakkeUtvalgEnh(RegData=NakkeVarSpes$RegData, aar=aar, datoFra = datoFra,
                                myelopati=myelopati, fremBak=fremBak) #, hovedkat=hovedkat) # #, datoTil=datoTil)


  if (ResPort == 1){
    #Variabler: Aar	ReshId	Teller Ind1	Nevner Ind1	  AarID	   Indikator
    #          2014	103469	  0	          1	       2014103469	  ind1
    RegDataUt <- NakkeUtvalg$RegData[,c('Aar', "ReshId", "ShNavn", "Variabel")]
    RegDataUt<- dplyr::rename(RegDataUt, Teller = Variabel)
    RegDataUt$AarID <- paste0(RegDataUt$Aar, RegDataUt$ReshId)
    indikatorID <- c('nakke1', 'nakke2', 'nakke3', 'nakke4')
    RegDataUt$Indikator <- indikatorID[which(kvalIndParam == valgtVar)]
    RegDataUt$Nevner <- 1
  }

  if (ResPort == 0){
    #Variabler: year, orgnr, var, denominator, ind_id
    RegDataUt <- NakkeUtvalg$RegData[,c('Aar', "ReshId", "Variabel")]
    # nytt navn = gammelt navn
    RegDataUt <- dplyr::rename(RegDataUt,
                               year = Aar,
                               var = Variabel)

    nyID <- c(# ReshId=OrgID
      '114288'='974703300',              #Stavanger USH
      '109820'='974589095',           #OUS, Ullevål USH
      '105783'='974749025',        #Trondheim, St. Olav
      '103469'='874716782',                  #OUS, RH
      '601161'='974795787',                #Tromsø, UNN
      '999920'='913758862',    #Oslofjordklinikken Vest
      '105588'='974557746',              #Haukeland USH
      '999998'='991835083',        #Oslofjordklinikken
      '110771'='973129856',                     #Volvat
      '4212372'='981541499',      #Aleris Colosseum Oslo #Aleris colosseum nobel
      '4211880'='974518821',             #Aleris Nesttun #Aleris sykehus nesttun
      '4211879'='813381192', #Aleris Colosseum Stavanger
      '100407'='983975240')  #Sørlandet sykehus)

    RegDataUt$orgnr <- as.character(nyID[as.character(RegDataUt$ReshId)])
    indikatorID <- c('nakke_komplsvelg3mnd', 'nakke_komplstemme3mnd', 'nakke_komplinfek', 'nakke_ndiendr12mnd35pst')
    RegDataUt$ind_id <- indikatorID[which(kvalIndParam == valgtVar)]
    RegDataUt$denominator <- 1

    RegDataUt <- RegDataUt[ ,c('year', 'orgnr', 'var', 'denominator', 'ind_id')]
}

  if (lagreFil==1) {
    write.table(RegDataUt, file = filUt, sep = ';', row.names = F) } #, fileEncoding = 'UTF-8')

  return(invisible(RegDataUt))
}



#' Funksjon som produserer rapporten som skal lastes ned av mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending, dvs. (\emph{ uten ".Rnw"})
#' @param reshID Brukerens reshid
#' @param filnavn brukes av downloadHandler
#' @param datoFra startdato
#' @param datoTil sluttdato
#' @return Filsti til pdf-rapporten.
#' @export
henteSamlerapporter <- function(filnavn, rnwFil, reshID=0,
                                datoFra=Sys.Date()-180, datoTil=Sys.Date()) {
  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package='nakke'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(tmpFile)

  gc() #Opprydning gc-"garbage collection"
  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
  # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
}


#' Funksjon som produserer rapporten som skal sendes til mottager.
#' (The actual call to this function is made through do.call and
#' has the effect of providing the parameters as class
#' \emph{list}. Verdier gis inn som listeparametre
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis MED filending (\emph{dvs "filnavn.Rnw"})
#' @param reshID Aktuell reshid
#' @param datoFra startdato
#' @param datoTil sluttdato
#'
#' @return Full path of file produced
#' @export
abonnementNakke <- function(rnwFil, brukernavn='tullebukk', reshID=0,
                       datoFra=Sys.Date()-180, datoTil=Sys.Date()) {

  rapbase::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Nakke',
                    reshId = reshID[[1]], msg = "Abonnement: månedsrapport")

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='nakke'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile)

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')
  rapbase::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Nakke',
                    reshId = reshID[[1]], msg = paste("Sendt: ", utfil))
  return(utfil)
}

#' Automatisk linjebryting av lange tekstetiketter
#'
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#' @return automatisk linjebryting
#' @export
delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
{sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
        USE.NAMES = FALSE)
}



#' Finner pasienter med potensielt dobbeltregistrerte skjema
#'
#' @param RegData dataramme fra nakkeregisteret
#' @param tidssavik - maks tidsavvik (dager) mellom to påfølgende registreringer som sjekkes
#'
#' @return
#' @export
PasMdblReg <- function(RegData, tidsavvik=30){

  FlereReg <- RegData %>% dplyr::group_by(PasientID) %>%
    dplyr::summarise(N = length(PasientID), #n(),
              KortTid = ifelse(N>1,
                              ifelse(difftime(InnDato[order(InnDato)][2:N], InnDato[order(InnDato)][1:(N-1)], units = 'days') <= tidsavvik,
                                     1, 0), 0),
              PasientID = PasientID[1]
    )

  PasMdbl <- FlereReg$PasientID[which(FlereReg$KortTid == 1)]
  TabDbl <- RegData[which(RegData$PasientID %in% PasMdbl),
                    c("PasientID", "InnDato", "SykehusNavn", "ReshId", "ForlopsID")] #, 'SkjemaGUID'
  TabDbl <- TabDbl[order(TabDbl$InnDato), ]
  N <- dim(TabDbl)[1]

  if (N>0) {
    indSmTid <- which(difftime(TabDbl$InnDato[2:N], TabDbl$InnDato[1:(N-1)], units = 'days') <= tidsavvik)
    TabDbl <- TabDbl[unique(sort(c(indSmTid, (indSmTid+1)))), ]
    TabDbl$InnDato <- format(TabDbl$InnDato, '%Y-%m-%d') #'%d.%m.%Y')
    tabUt <- TabDbl[order(TabDbl$PasientID, TabDbl$InnDato), ]
  } else {tabUt <- paste0('Ingen registreringer med mindre enn ', tidsavvik, 'minutter mellom registreringene for samme pasient.')}
}

