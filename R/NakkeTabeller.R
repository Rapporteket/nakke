#'  Antall opphold siste X (antMnd) mnd
#'
#' @param RegData dataramme med alle dato
#' @param datoTil sluttdato
#' @param antMnd antall måneder som skal vises
#' @param reshID reshID for filtrering
#'
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=6, reshID=0){

  if (reshID!=0){RegData <- RegData[which(RegData$ReshId==reshID), ]}
  datoFra <- lubridate::floor_date(as.Date(datoTil)- months(antMnd, abbreviate = T), unit='month')
  tabAvdMnd <- 0
  if (exists('datoFra')){
    aggVar <-  c('ShNavn', 'InnDato')
    RegDataDum <- RegData[intersect(which(as.Date(RegData$InnDato) <= as.Date(datoTil, tz='UTC')),
                                    which(as.Date(RegData$InnDato, tz='uTC') > as.Date(datoFra, tz='UTC'))), aggVar]

    RegDataDum$Maaned <- format(lubridate::ymd(RegDataDum$InnDato),'%b')
    tabAvdMnd <- table(RegDataDum[ , c('ShNavn', 'Maaned')])

    if (dim(tabAvdMnd)[1]>0) {
      if (reshID==0 ){
        tabAvdMnd <- addmargins(tabAvdMnd)
        }
      tabAvdMnd <- xtable::xtable(tabAvdMnd, digits=0)
    }
  }

  return(tabAvdMnd)
}

#'  Antall opphold siste 5 år
#' @param RegData dataramme med alle data
#' @param datoTil sluttdato
#'
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date()){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
      tabAvdAarN <- 0
      if (length(AarNaa)>0) {
        RegData <- RegData[which(as.Date(RegData$InnDato) <= as.Date(datoTil, tz='UTC')), ]
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      }
      return(tabAvdAarN)
}


#' Hvor mange skjema av hver type
#'
#' @param skjemaversikt tabellen skjemaoversikt fra QReg
#' @param datoFra startdato
#' @param datoTil til og med dato
#' @param skjemastatus status på registreringsskjemaet
#'
#' @export
tabAntSkjema <- function(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1){
  #tabAntSkjema(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1)
  #NB: Denne skal også kunne vise skjema i kladd!
  #Skjemastatus kan være -1, 0 og 1
  SkjemaOversikt$SkjemaRekkeflg <- factor(SkjemaOversikt$SkjemaRekkeflg, levels = 5*(1:4))
  skjemanavn <- c('Pasient preop.','Lege preop.','Oppfølging, 3mnd', 'Oppfølging, 12mnd')

  indDato <- which(as.Date(SkjemaOversikt$InnDato) >= datoFra & as.Date(SkjemaOversikt$InnDato) <= datoTil)
  indSkjemastatus <- which(SkjemaOversikt$SkjemaStatus==skjemastatus)
  SkjemaOversikt <- SkjemaOversikt[intersect(indDato, indSkjemastatus),]

  tab <- table(SkjemaOversikt[,c('ShNavn', 'SkjemaRekkeflg')])
  tab <- rbind(tab,
               'TOTALT, alle enheter:'=colSums(tab))
  colnames(tab) <- skjemanavn
  tab <- xtable::xtable(tab)

return(tab)
}


#'  Vise figurdata som tabell
#'
#' @param UtDataFraFig Liste med beregnede verdier++ fra "figurforberedelse"
#' @param figurtype figurtype, standard 'andeler'
#' @export
lagTabavFig <- function(UtDataFraFig, figurtype='andeler'){ #lagTabavFigAndeler

  attach(UtDataFraFig, warn.conflicts = F)

  if (figurtype %in% c('andeler','gjsnGrVar', 'andelTid')){

  tab <-cbind(Nvar$Hoved,
              Ngr$Hoved,
              AggVerdier$Hoved,
              if (medSml==1){cbind(
                Nvar$Rest,
                Ngr$Rest,
                AggVerdier$Rest)}
              )}

  if (figurtype %in% c('andeler', 'andelTid')) {
    colnames(tab) <- c(paste0('Antall', c(' (n)',
                                          ' (N)')),
                       'Andel (%)',
                     if (medSml==1) {
                       c(paste0('Antall', c(' (n)',
                                            ' (N)')),
                         'Andel (%)')})
    # colnames(tab) <- c(paste0(hovedgrTxt,', Antall (n)'),
    #                    paste0(hovedgrTxt,', Antall (N)'),
    #                  paste0(hovedgrTxt, ', Andel (%)'),
    #                  if (medSml==1) {
    #                    cbind(paste0(smltxt,', Antall (n)'),
    #                          paste0(smltxt,', Antall (N)'),
    #                          paste0(smltxt, ', Andel (%)'))})
                 }

  if (figurtype == 'gjsnTid'){
    tab <- AggVerdier
    colnames(tab) <-  grtxt
    tab <- t(tab)
  }

    if(figurtype=='gjsnGrVar') {
    kolnavn <- c('Antall (N)', SentralmaalTxt)
    if (medSml==1) {
      colnames(tab) <-  c(kolnavn, paste0(smltxt, c(', Antall (N)', ', Andel (%)')))}
    }
  if (figurtype == 'andeler') {rownames(tab) <- grtxt}
  return(tab)
}


#' Vise figurdata som tabell, sentralmål per sykshus
#'
#' @param UtDataFraFig Liste med beregnede verdier ++ fra GjsnGrVar
#'
#' @export
lagTabavFigGjsnGrVar <- function(UtDataFraFig){
  tab <-cbind(UtDataFraFig$Ngr,
              UtDataFraFig$AggVerdier$Hoved
  )
  colnames(tab) <- c('Antall (N)', UtDataFraFig$SentralmaalTxt)
  detach(UtDataFraFig)

  return(tab)
}



