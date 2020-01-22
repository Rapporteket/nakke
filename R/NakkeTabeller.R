#'  Antall opphold siste X (antMnd) mnd
#'
#' @param RegData dataramme med alle dato
#' @param datoTil sluttdato
#' @param antMnd antall måneder som skal vises
#' @param reshID reshID for filtrering
#'
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=6, reshID=0){
      #RegData må inneholde ..
  if (reshID!=0){RegData <- RegData[which(RegData$ReshId==reshID), ]}
      datoFra <- lubridate::floor_date(as.Date(datoTil)- months(antMnd, abbreviate = T), unit='month')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[intersect(which(as.Date(RegData$InnDato) <= as.Date(datoTil, tz='UTC')),
                               which(as.Date(RegData$InnDato, tz='uTC') > as.Date(datoFra, tz='UTC'))), aggVar]
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      if (reshID==0){
        tabAvdMnd1 <- addmargins((tabAvdMnd1))}
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1, digits=0)
	return(tabAvdMnd1)
}


#'  Antall opphold siste 5 år
#' @param RegData dataramme med alle data
#' @param datoTil sluttdato
#'
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date()){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)

    #tabAntOpphSh5Aar(RegData=RegData, datoTil=Sys.Date())
}


#' Hvor mange skjema av hver type
#'
#' @param SkjemaOversikt tabellen skjemaoversikt fra QReg
#' @param datoFra startdato
#' @param datoTil til og med dato
#' @param skjemastatus status på registreringsskjemaet
#'
#' @export
tabAntSkjema <- function(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1){
  #tabAntSkjema(SkjemaOversikt, datoFra = '2019-01-01', datoTil=Sys.Date(), skjemastatus=1)
  #NB: Denne skal også kunne vise skjema i kladd!
  #Skjemastatus kan være -1, 0 og 1
  SkjemaOversikt$SkjemaRekkeflg <- factor(SkjemaOversikt$SkjemaRekkeflg, levels = 1:4)
  skjemanavn <- c('Pasient preop.','Lege preop.','Oppfølging, 3mnd', 'Oppfølging, 12mnd')

  indDato <- which(as.Date(SkjemaOversikt$InnDato) >= datoFra & as.Date(SkjemaOversikt$InnDato) <= datoTil)
  indSkjemastatus <- which(SkjemaOversikt$SkjemaStatus==skjemastatus)
  SkjemaOversikt <- SkjemaOversikt[intersect(indDato, indSkjemastatus),]

  tab <-table(SkjemaOversikt[,c('ShNavn', 'SkjemaRekkeflg')])
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



#' Vise figurdata som tabell, sentralmål per sykshus
#' @param RegData dataramme
#' @param ant antall diagnoser/prosedyrer
#' @param prosdiag vise prosedyrer ('pros', standard) eller diagnoser ('diag')
#'
#' @export
visVanligsteProcDiag <- function(RegData, ant=20, prosdiag='pros'){

ant <- 20
  ProsHys <- c('HysProsedyre1', 'HysProsedyre2', 'HysProsedyre3')
  ProsLap <- c('LapProsedyre1', 'LapProsedyre2', 'LapProsedyre3')
  DiagLap <- c('LapDiagnose1', 'LapDiagnose2', 'LapDiagnose3')
  DiagHys <- c('HysDiagnose1', 'HysDiagnose2', 'HysDiagnose3')

  variable <- switch(prosdiag,
                     diag = c(DiagHys, DiagLap),
                     pros = c(ProsHys, ProsLap))

AlleProsDiag <- as.vector(as.matrix(RegData[ , variable]))
AllePDsort <- sort(table(AlleProsDiag[which(AlleProsDiag != '')]), decreasing = TRUE)

#AlleProsEget <- as.vector(as.matrix(RegData[indEget, c(ProsHys, ProsLap)]))
#AlleProsEgetSort <- sort(table(AlleProsEget[which(AlleProsEget != '')]), decreasing = TRUE)

tab <- cbind( #Må fjerne tomme
  Andel = (AllePDsort[1:ant])/dim(RegData)[1]*100 ,
  Antall = AllePDsort[1:ant] )

# ProcEget <- cbind( #Må fjerne tomme
#   Andel = (AlleProsEgetSort[1:ant])/Neget*100 ,
#   Antall = AlleProsEgetSort[1:ant] )

# AlleDiag <- as.vector(as.matrix(RegData[ , c(DiagHys,DiagLap)]))
# AlleDiagSort <- sort(table(AlleDiag[which(AlleDiag != '')]), decreasing = TRUE)
# AlleDiagEget <- as.vector(as.matrix(RegData[indEget , c(DiagHys,DiagLap)]))
# AlleDiagEgetSort <- sort(table(AlleDiagEget[which(AlleDiagEget != '')]), decreasing = TRUE)


# Diag <- cbind( #Må fjerne tomme
#   Andel = (AlleDiagSort[1:ant])/N*100 ,
#   Antall = AlleDiagSort[1:ant] )

# DiagEget <- cbind( #Må fjerne tomme
#   Andel = (AlleDiagEgetSort[1:ant])/Neget*100 ,
#   Antall = AlleDiagEgetSort[1:ant] )

type <- switch(prosdiag, pros='prosedyr', diag='diagnos')
tittel <- paste0('Vanligste ', type,'er. Andel angir prosent av utførte
                 operasjoner hvor ', type, 'en er benyttet.')

tabUt <- xtable(tab, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(tab)), na.rm=T))),
       caption=tittel,
       linclude.rownames=TRUE, include.colnames=TRUE)

# xtable(Proc, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(Proc)), na.rm=T))),
#        caption='Vanligste prosedyrer. Andel angir andel av antall utførte
#        operasjoner hvor prosedyra er benyttet.',
#        label='tab:Proc', include.rownames=TRUE, include.colnames=TRUE)
#
# xtable(ProcEget, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(ProcEget)), na.rm=T))),
#        caption='Vanligste prosedyrer, eget sykehus. Andel angir andel av antall utførte
#        operasjoner hvor prosedyra er benyttet.',
#        label='tab:ProcEget', include.rownames=TRUE, include.colnames=TRUE)
#
# xtable(Diag, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(Diag)), na.rm=T))),
#        caption='Vanligste diagnoser. Andel angir andel av antall utførte
#        operasjoner hvor diagnosen er benyttet.',
#        label='tab:Diag', include.rownames=TRUE, include.colnames=TRUE)
#
# xtable(DiagEget, digits=c(0,1,0), align=c('l', rep('r', max(c(1,ncol(DiagEget)), na.rm=T))),
#        caption='Vanligste diagnoser, eget sykehus. Andel angir andel av antall utførte
#        operasjoner hvor diagnosen er benyttet.',
#        label='tab:DiagEget', include.rownames=TRUE, include.colnames=TRUE)

return(tabUt)
}
