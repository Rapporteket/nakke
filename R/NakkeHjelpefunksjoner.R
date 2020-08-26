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




#' Generere data til Resultatportalen
#'
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param valgtVar - beinsmLavPre, peropKompDura, sympVarighUtstr
#' @inheritParams NakkeFigAndeler
#' @inheritParams NakkeUtvalgEnh
#' @return Datafil til Resultatportalen
#' @export

dataTilResPort <- function(RegData = RegData, valgtVar, datoFra = '2014-01-01', aar=0,
                           myelopati=99, fremBak=0, filUt='dummy'){ #hovedkat=99,

  if (valgtVar %in% c('KomplStemme3mnd', 'KomplSvelging3mnd')) {myelopati <- 0}
  if (valgtVar == 'NDIendr12mnd35pst') {
    myelopati <- 0
    fremBak<-1}
  filUt <- paste0('NakkeTilRes', ifelse(filUt=='dummy',  valgtVar, filUt), '.csv')
  NakkeVarSpes <- NakkeVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
  NakkeUtvalg <- NakkeUtvalgEnh(RegData=NakkeVarSpes$RegData, aar=aar, datoFra = datoFra,
                                myelopati=myelopati, fremBak=fremBak) #, hovedkat=hovedkat) # #, datoTil=datoTil)
  NakkeTilResvalgtVar <- NakkeUtvalg$RegData[ ,c('Aar', "ShNavn", "ReshId", "Variabel")]

  ##x <- unique(tab$ReshId)
  nyID <- c('109820'='974589095', '105783'='974749025', '114288'='974703300', #'114288'='4000020',
            '103469'='874716782', '601161'='974795787', '999920'='913705440',
            '105588'='974557746', '999998'='991835083', '110771'='973129856',
            '4212372'='4212372', '4211880'='999999003', '4211879'='813381192')

  #999998 (Oslofjordklinikken, either Sandnes 913758862 or Sandvika 991835083)

  NakkeTilResvalgtVar$ID <- as.character(nyID[as.character(NakkeTilResvalgtVar$ReshId)])
  info <- c(NakkeVarSpes$tittel, NakkeUtvalg$utvalgTxt)
  NakkeTilResvalgtVar$info <- c(info, rep(NA, dim(NakkeTilResvalgtVar)[1]-length(info)))

  # ReshId=OrgID
  # 114288=4000020              Stavanger USH
  # 109820=974589095           OUS, Ullevål USH
  # 105783=974749025        Trondheim, St. Olav
  # 103469=874716782                  OUS, RH
  # 601161=974795787                Tromsø, UNN
  # 999920=913705440    Oslofjordklinikken Vest
  # 105588=974557746              Haukeland USH
  # 999998=991835083        Oslofjordklinikken
  # 110771=973129856                     Volvat
  # 4212372=4212372      Aleris Colosseum Oslo
  # 4211880=999999003             Aleris Nesttun
  # 4211879=813381192 Aleris Colosseum Stavanger


  return(invisible(NakkeTilResvalgtVar))
}


#' Tilrettelegge data for offentlig visning.
#'
#' @param RegData - data
#' @param valgtVar -
#' @param datoFra - startdato
#' @param aar - velge hele år (flervalg)
#' @return Datafil til Resultatportalen
#' @export

tilretteleggDataSKDE <- function(RegData = RegData, datoFra = '2014-01-01', aar=0){ #valgtVar,
  #4212372, 999999003, NA
  #103469  105588  105783  109820  110771  114288  601161  999920  999998 4211879 4212372
  nyID <- c('109820'='974589095', '105783'='974749025', '114288'='974703300', #'114288'='4000020',
            '103469'='874716782', '601161'='974795787', '999920'='913705440',
            '105588'='974557746', '999998'='991835083', '110771'='973129856',
            '4212372'='4212372', '4211880'='999999003', '4211879'='813381192')
  RegData$OrgNrShus <- as.character(nyID[as.character(RegData$ReshId)])
  resultatVariable <- c('KvalIndId', 'Aar', "OrgNrShus" , "Variabel") #"ShNavn", "ReshId",
  NakkeKvalInd <- data.frame(NULL) #Aar=NULL, ShNavn=NULL)

  kvalIndParam <- c('KomplSvelging3mnd', 'KomplStemme3mnd', 'Komplinfek', 'NDIendr12mnd35pstKI')
  indikatorID <- c('nakke_komplsvelg3mnd', 'nakke_komplstemme3mnd', 'nakke_komplinfek', 'nakke_ndiendr12mnd35pst')   #c('nakke1', 'nakke2', 'nakke3', 'nakke4')
  #Test <- NakkeUtvalg$RegData
  #Test[ , c('KvalIndId', 'Aar', "ShNavn", "ReshId", "OrgNrShus" , "Variabel")]

  for (valgtVar in kvalIndParam){
    #print(valgtVar)
    NakkeKvalInd1 <- RegData
    NakkeKvalInd1$KvalIndId <- indikatorID[which(kvalIndParam == valgtVar)]
    myelopati <- if (valgtVar %in% c('KomplStemme3mnd', 'KomplSvelging3mnd')) {0} else {99}
    fremBak <- if (valgtVar %in% c('KomplStemme3mnd', 'KomplSvelging3mnd', 'NDIendr12mnd35pstKI')) {1} else {0}
    NakkeVarSpes <- NakkeVarTilrettelegg(RegData=NakkeKvalInd1, valgtVar=valgtVar, figurtype = 'andelGrVar')
    NakkeUtvalg <- NakkeUtvalgEnh(RegData=NakkeVarSpes$RegData, aar=aar, datoFra = datoFra,
                                  myelopati=myelopati, fremBak=fremBak) #, hovedkat=hovedkat) # #, datoTil=datoTil)
    NakkeKvalInd1 <- NakkeUtvalg$RegData[ , resultatVariable]

    NakkeKvalInd <- rbind(NakkeKvalInd, NakkeKvalInd1)
    #info <- c(NakkeVarSpes$tittel, NakkeUtvalg$utvalgTxt)
    #NakkeKvalInd$info <- c(info, rep(NA, dim(NakkeKvalInd)[1]-length(info)))
  }

  NakkeKvalInd <- dplyr::rename(NakkeKvalInd,
                orgnr = OrgNrShus,
                year = Aar,
                var = Variabel,
                ind_id = KvalIndId)
  NakkeKvalInd$denominator <- 1

  return(invisible(NakkeKvalInd))
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

  raplog::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Nakke',
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
  raplog::subLogger(author = brukernavn, registryName = 'NKR: Degenerativ Nakke',
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


