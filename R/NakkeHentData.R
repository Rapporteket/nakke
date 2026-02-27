#' Endre variabelnavn/kolonnenavn til selvvalgte navn
#' @param tabell datatabellnavn i databasen
#' @param tabType REGISTRATION_TYPE
#' @return tabell med selvvalgte variabelnavn spesifisert i friendlyvar. Intern funksjon
#'
#' @export

mappingEgneNavn <- function(tabell, tabType) {

  friendlyVarTab  <-
    rapbase::loadRegData( "data",
                          query = "SELECT FIELD_NAME, REGISTRATION_TYPE, USER_SUGGESTION
                           FROM friendly_vars") #

  indTabType <- which(friendlyVarTab$REGISTRATION_TYPE %in% tabType)
  if (!length(indTabType)==0) {
    friendlyVarTabType <- friendlyVarTab[indTabType,]
    kuttTabPrefiks <- if (tabType == 'PATIENTFOLLOWUP12') {'PATIENTFOLLOWUP_'} else {paste0(tabType, '_')}

    rydd <- which(friendlyVarTabType$USER_SUGGESTION == 'NEINNICHTS')

    #Fjerner variabler merket 'NEINNICHTS'
    if (length(rydd)>0) {
      fjernvar <- gsub(kuttTabPrefiks, "", friendlyVarTabType$FIELD_NAME[rydd])
      indFjern <- which(names(tabell) %in% fjernvar)
      if (length(indFjern) > 0) {
        tabell <- tabell[ , -indFjern]}
      friendlyVarTabType <- friendlyVarTabType[-rydd, ]
    }

    navn <- gsub(kuttTabPrefiks, "", friendlyVarTabType$FIELD_NAME)
    names(navn) <- friendlyVarTabType$USER_SUGGESTION
    tabell <- dplyr::rename(tabell, dplyr::any_of(navn)) #all_of(navn
  }
  return(tabell)
}



#' Hent datatabell fra nakkes database
#'
#' @param tabellnavn Navn på tabell som skal lastes inn.
#' @param egneVarNavn 0 - Qreg-navn benyttes.
#'                    1 - selvvalgte navn fra Friendlyvar benyttes
#' Bare ferdigstilte (status=1) legeskjema og pasientskjema overføres
#' (Sjekket 23.feb 2026)
#'
#' @export

hentDataTabell <- function(tabellnavn = "surgeonform",
                           qVar = '*',
                           datoFra = '2023-01-01', datoTil = Sys.Date(),
                           egneVarNavn = 1) { #  status = 1

  tabType <- toupper(tabellnavn)

  query <- paste0('SELECT ', qVar, ' FROM ', tabellnavn)
  if (tabellnavn == 'surgeonform'){
    query <- paste0(query,
               ' WHERE OPERASJONSDATO >= \'', datoFra,
               '\' AND OPERASJONSDATO <= \'', datoTil, '\' ')
    query1 <- paste0('SELECT * FROM surgeonform
    WHERE OPERASJONSDATO >= \'', datoFra, '\' AND OPERASJONSDATO <= \'', datoTil, '\' ')
    }

  if (tabellnavn == 'patientfollowup3') {
    query <- paste0("SELECT ", qVar, ' FROM patientfollowup
                    WHERE CONTROL_TYPE = 3')
    tabType <- 'PATIENTFOLLOWUP'
  }

  if (tabellnavn == 'patientfollowup12') {
    query <- query <- paste0("SELECT ", qVar, ' FROM patientfollowup
                              WHERE CONTROL_TYPE = 12')}

  tabell <- rapbase::loadRegData(registryName = "data",
                                 query = query)

  if (egneVarNavn == 1) {
    tabell <- mappingEgneNavn(tabell, tabType)}

  return(tabell)
}

#' Henter Nakke-tabeller og kobler sammen
#'
#' @param medPROM: koble på RAND og TSS2-variabler
#' @param alleData 1- alle variabler med, 0 - utvalgte variabler med
#'
#' @return RegData data frame
#'
#' @export



#-----------Endringer i datasettet fra AlleVarNum til egen sammenkobling av skjema:
# ForlopsID -> MCEID, AvdRESH -> ReshId,  PasientSkjemaStatus -> StatusPasSkjema
# LegeskjemaStatus -> StatusLegeSkjema,   ForstLukketMed -> ForstLukketLege,
# StatusKtr3mnd -> StatusUtfyll3mnd, StatusKtr12mnd -> StatusUtfyll12mnd

# Avdod, AvdodDato - ikke i bruk. Har DodsDato

#Ikke i bruk: BasisRegStatus, ForlopsStatus, ForlopsMailStatus, EqType,
#           ForstLukket3mnd, FriskmeldtDato3mnd, InngrepType, OppFolgLaget3mnd
#           TidlSkulderPlager3mnd, TidlSkulderPlager12mnd

#Beregner: TidlOpr, AntallNivaaOpr,
# PerOpEnhverKompl -> KomplPerOp
# EnhverKompl3mnd -> Kompl3mnd
# EnhverKompl12mnd -> Kompl12mnd
#----------------------------------------------------------------



#' Hente data fra Degenerativ Nakke
#'
#' @param datoFra fra og med operasjonsdato, format: 'yyyy-mm-dd'
#' @param datoTil til og med operasjonsdato, format: 'yyyy-mm-dd'
#' @param medOppf ha med oppfølgingsskjema? 0-nei, 1-ja
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
NakkeHentRegData <- function(datoFra = '2013-01-01', datoTil = Sys.Date(),
                             medOppf = 1,  ...) {
  # Få til å fungere med ny sammenkobling av alle data
  # legg på valg av variabler?
  # legg på datofiltrering

  #mce Trenger nok ganske få av disse variablene
  # mce_patient_data # eneste som inneholder kobling mellom mceid og pasientid
  qmce <- 'CENTREID AS ReshId, CREATEDBY, MCEID, PATIENT_ID AS PasientID,
             sendtSMS12mnd, sendtSMS3mnd, TSCREATED, TSUPDATED'

  mceSkjema <- hentDataTabell(tabellnavn = "mce",
                              qVar = qmce,
                              egneVarNavn = 0)

  #Pasientskjema:
  qPas <- 'BIRTH_DATE,
             DECEASED,
             DECEASED_DATE,
             DISTRICTCODE,
             DISTRICTNAME,
             EDUCATION,
             ETNISK,
             GENDER,
             ID,
             MARITAL_STATUS,
             NATIVE_LANGUAGE,
             NATIVE_LANGUAGE_OTHER,
             NO_CHILDREN,
             OWNING_CENTRE,
             REAPER_DATE,
             REGISTERED_DATE,
             SMOKING,
             SNUFF,
             TSCREATED,
             TSUPDATED'

  PasInfoSkjema <- hentDataTabell(tabellnavn = "patient",
                                  qVar = qPas,
                                  egneVarNavn = 1)
  #Legeskjema
  # datovalg lagt til bare for Legeskjema.
  LegeSkjema <- hentDataTabell(tabellnavn = "surgeonform",
                               qVar = '*',
                               datoFra = datoFra, datoTil = datoTil,
                               egneVarNavn = 1)
  LegeSkjema <- dplyr::rename(LegeSkjema,
                              'ForstLukketLege' = 'FIRST_TIME_CLOSED')

  #Pasientens spørreskjema
  PasSkjema <- hentDataTabell(tabellnavn = "patientform",
                              qVar = '*',
                              egneVarNavn = 1)

  #Sykehusnavn
  EnhetsNavn <- hentDataTabell(tabellnavn = "centreattribute",
                               qVar = 'ID, ATTRIBUTEVALUE as SykehusNavn')

  # SAMMENSTILL SKJEMA:
  RegData <-
    merge(mceSkjema,
          PasInfoSkjema, by = "PasientID",
          suffixes = c("", "_pas")) |>
    merge(LegeSkjema, by = "MCEID", all.x=FALSE, suffixes = c("", "_lege")) |>
    merge(PasSkjema,
          by = "MCEID", all.x = TRUE, suffixes = c("", "_oppf0")) |>
    merge(EnhetsNavn,
          by.x = "ReshId", by.y = 'ID', all.x = TRUE)

  if (medOppf == 1) {
    #Oppfølging, 3 mnd
    Oppf3Skjema <- hentDataTabell(tabellnavn = "patientfollowup3",
                                  qVar = '*',
                                  egneVarNavn = 1)
    #Oppfølging, 12 mnd
    Oppf12Skjema <- hentDataTabell(tabellnavn = "patientfollowup12",
                                   qVar = '*',
                                   egneVarNavn = 1)

    # SAMMENSTILL SKJEMA:
    tictoc::tic
    RegData <- RegData |>
      merge(Oppf3Skjema,
            suffixes = c("", "_oppf3"), by = "MCEID", all.x = TRUE) |>
      merge(Oppf12Skjema,
            suffixes = c("", "_oppf12"), by = "MCEID", all.x = TRUE)
    tictoc::toc()

    # #Feil i andel oppfølging etter innføreing av ePROM.
    # StatusUtfyll3mnd=1 betyr ikke lenger at skjemaet er utfylt
    # #Må lage variabelen på nytt
    ePROMadmTab <- rapbase::loadRegData(registryName='data',
                                        query='SELECT * FROM proms')
    ePROMvar <- c("MCEID", "TSSENDT", "TSRECEIVED", "NOTIFICATION_CHANNEL", "DISTRIBUTION_RULE",
                  'REGISTRATION_TYPE')
    # «EpromStatus»:  0 = Created, 1 = Ordered, 2 = Expired, 3 = Completed, 4 = Failed
    ind3mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                       c('PATIENTFOLLOWUP', 'PATIENTFOLLOWUP_3_PiPP', 'PATIENTFOLLOWUP_3_PiPP_REMINDER'))
    ind12mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                        c('PATIENTFOLLOWUP12', 'PATIENTFOLLOWUP_12_PiPP', 'PATIENTFOLLOWUP_12_PiPP_REMINDER'))

    indIkkeEprom3mnd <-  which(!(RegData$MCEID %in% ePROMadmTab$MCEID[ind3mnd]))
    indIkkeEprom12mnd <-  which(!(RegData$MCEID %in% ePROMadmTab$MCEID[ind12mnd]))

    RegData$OppFolg3mndGML <- RegData$StatusUtfyll3mnd
    RegData$StatusUtfyll3mnd <- 0
    RegData$StatusUtfyll3mnd[
      RegData$MCEID %in% ePROMadmTab$MCEID[intersect(ind3mnd, which(ePROMadmTab$STATUS==3))]] <- 1
    RegData$StatusUtfyll3mnd[intersect(which(RegData$OppFolg3mndGML ==1), indIkkeEprom3mnd)] <- 1

    RegData$OppFolg12mndGML <- RegData$StatusUtfyll12mnd
    RegData$StatusUtfyll12mnd <- 0
    RegData$StatusUtfyll12mnd[
      RegData$MCEID %in% ePROMadmTab$MCEID[intersect(ind12mnd, which(ePROMadmTab$STATUS==3))]] <- 1
    RegData$StatusUtfyll12mnd[intersect(which(RegData$OppFolg12mndGML ==1), indIkkeEprom12mnd)] <- 1
  }



  return(invisible(RegData))
}
