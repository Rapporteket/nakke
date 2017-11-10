#' Funksjon som gjør utvalg i datagrunnlaget til registreringene for Nakke
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett - Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#'
#' @export

NakkeLibUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', aar=0,
                           myelopati=99, fremBak=0, fargepalett='BlaaOff')	#insttype,
{

'%i%' <- intersect

Ninn <- dim(RegData)[1]
indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indAar <- if (aar[1] > 2000) {which(RegData$Aar %in% as.numeric(aar))} else {indAar <- 1:Ninn}
indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
indMyelo <- if (myelopati %in% 0:1) {which(RegData$OprIndikMyelopati == myelopati)} else {indMyelo <- 1:Ninn}
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
	if (myelopati %in% 0:1) {paste0('Myelopati:', c('Nei', 'Ja')[myelopati+1])},
	if (fremBak %in% 1:2) {paste0('Tilgang:', c('Fremre','Bakre')[fremBak])}
#	if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
	)


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
return(invisible(UtData))
}
