#' Funksjoner for å lage tabeller Group of functions page title
#'
#' Fil som beregner div tabeller.Group of functions Description section
#'
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer
#' RegData må inneholde InnDato og Aar.
#' Aktuelle tabeller:
#' -tabAntOpphSh12mnd: Antall opphold per måned og enhet siste 12 måneder fram til datoTil.
#' -tabAntOpphSh5Aar:Antall opphold per år og enhet siste 5 år (inkl. inneværende år) fram til datoTil.
#'
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
# @inheritParams NakkeFigAndeler
#' @return Div tabeller
#' @name Nakketabeller
NULL
#' @rdname Nakketabeller
#' @export

#' @section Antall opphold siste X (antMnd) mnd
#' @rdname Nakketabeller
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
#tabAntOpphShMnd(RegData, datoTil=Sys.Date(), antMnd=3)

#' @section Antall opphold siste 5 år
#' @rdname Nakketabeller
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


#' @section Hvor mange skjema av hver type
#' @rdname Nakketabeller
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


#' @section Vise figurdata som tabell
#' @rdname Nakketabeller
#' @export
# lagTabavFig <- function(UtDataFraFig){
#       tab <-cbind(UtDataFraFig$Ngr$Hoved,
#                   UtDataFraFig$AggVerdier$Hoved,
#                   UtDataFraFig$Ngr$Rest,
#                   UtDataFraFig$AggVerdier$Rest)
#       grtxt <- UtDataFraFig$grtxt
#       if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
#             grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
#       rownames(tab) <- grtxt
#       kolnavn <- c('Antall' , 'Andel (%)')
#       colnames(tab) <- c(kolnavn, if(!is.null(UtDataFraFig$Ngr$Rest)){kolnavn})
#
# return(tab)
# }

lagTabavFig <- function(UtDataFraFig, figurtype='andeler'){ #lagTabavFigAndeler

  attach(UtDataFraFig, warn.conflicts = F)
  #medSml==1

  if (figurtype %in% c('andeler','gjsnGrVar', 'andelTid')){
  tab <-cbind(Ngr$Hoved,
              AggVerdier$Hoved,
              if (medSml==1){cbind(
                Ngr$Rest,
                AggVerdier$Rest)})}

  if (figurtype %in% c('andeler', 'andelTid')) {
    colnames(tab) <- c(paste0(hovedgrTxt,', Antall'),
                     paste0(hovedgrTxt, ', Andel (%)'),
                     if (medSml==1) {
                       cbind(paste0(smltxt,', Antall'),
                             paste0(smltxt, ', Andel (%)'))}
                 )}

  if (figurtype == 'gjsnTid'){
    tab <- AggVerdier
    colnames(tab) <-  grtxt
    tab <- t(tab)
  }

    if(figurtype=='gjsnGrVar') {
    kolnavn <- c('Antall (N)', SentralmaalTxt)
    if (medSml==1) {
      colnames(tab) <-  c(kolnavn, paste0(smltxt, c(', Antall', ', Andel (%)')))}
    }
  rownames(tab) <- grtxt
  return(tab)
}


#' @section Vise figurdata som tabell, sentralmål per sykshus
#' @rdname Nakketabeller
#' @export
lagTabavFigGjsnGrVar <- function(UtDataFraFig){
  tab <-cbind(UtDataFraFig$Ngr,
              UtDataFraFig$AggVerdier$Hoved
  )
  colnames(tab) <- c('Antall (N)', UtDataFraFig$SentralmaalTxt)
  return(tab)
}



#' @section Vise figurdata som tabell, sentralmål per sykshus
#' @rdname Nakketabeller
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
