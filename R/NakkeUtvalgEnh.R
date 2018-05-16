#' Funksjon som gjør utvalg i datagrunnlaget til registreringene for Nakke
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams NakkeFigAndeler
#' @param datoFra Operasjonsdato, fra og med. Standard: '2012-01-01'
#' @param datoTil Operasjonsdato, til og med. Standard: '3000-01-01' (siste registreringsdato)
#' @param minald Alder, fra og med
#' @param maxald Alder, til og med
#' @param erMann Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param enhetsUtvalg Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#' @param fremBak Fremre eller bakre tilgang. 0-ikke valgt, 1-fremre, 2-bakre
#' @param myelopati Operert for myelopati. 0-nei, 1-ja, 99-alle (standard)
#' @param fargepalett - Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#'
#' @export

NakkeUtvalgEnh <- function(RegData, datoFra='2012-01-01', datoTil='3000-01-01', minald=0, maxald=130, erMann='', aar=0,
                           myelopati=99, fremBak=0, enhetsUtvalg=0, reshID=0, fargepalett='BlaaOff')	#insttype,
{

  '%i%' <- intersect

  #Enhetsutvalg:
  #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
  #trengs ikke data for hele landet:
  if (enhetsUtvalg == 2) {
    RegData <- RegData[which(RegData$ReshId == as.numeric(reshID)),]	#kun egen enhet
  }

  Ninn <- dim(RegData)[1]
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
  indAar <- if (aar[1] > 2000) {which(RegData$Aar %in% as.numeric(aar))} else {indAar <- 1:Ninn}
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indMyelo <- if (myelopati %in% 0:1) {which(RegData$OprIndikMyelopati == myelopati)} else {indMyelo <- 1:Ninn}
  #Myelopati: NA=0
  indFremBak <- switch(as.character(fremBak),
                       '0' = 1:Ninn,
                       '1' = which(RegData$OprMetodeTilgangFremre==1),
                       '2' = which(RegData$OprMetodeTilgangBakre==1))

  #indTidlOp <- if (tidlOp %in% 1:4) {which(RegData$TidlOpr==tidlOp)} else {indTidlOp <- 1:Ninn}
  indMed <- indAld %i% indDato %i% indAar %i% indKj %i% indMyelo %i% indFremBak
  RegData <- RegData[indMed,]


  TidlOprtxt <-	c('Tidl. operert samme nivå', 'Tidl. operert annet nivå', 'Tidl. operert annet og sm. nivå', 'Primæroperasjon')

  N <- dim(RegData)[1]

  utvalgTxt <- c(paste0('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                        ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
                 if ((minald>0) | (maxald<130)) {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                                                        ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
                 if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
                 if (myelopati %in% 0:1) {paste0('Myelopati: ', c('Nei', 'Ja')[myelopati+1])},
                 if (fremBak %in% 1:2) {paste0('Tilgang: ', c('Fremre','Bakre')[fremBak])}
                 #	if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
  )




  #Enhetsutvalg:
  indEgen1 <- match(reshID, RegData$ReshId)
  if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
    hovedgrTxt <- as.character(RegData$ShNavn[indEgen1])
  } else {
    hovedgrTxt <-'Hele landet'
  }


  ind <- list(Hoved=0, Rest=NULL)
  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    smltxt <- ''      #
    ind$Hoved <- 1:dim(RegData)[1]	#
  }
  if (enhetsUtvalg == 1) {	#Involverer egen enhet
    medSml <- 1
    smltxt <- 'landet forøvrig'
    ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID)
    ind$Rest <- which(as.numeric(RegData$ReshId) != reshID)
  }





  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, ind=ind,
                 medSml=medSml, hovedgrTxt=hovedgrTxt, smltxt=smltxt) #GronnHNpms624,
  return(invisible(UtData))
}
