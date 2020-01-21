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


#' Funksjon som produserer rapporten som skal sendes til mottager.
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

