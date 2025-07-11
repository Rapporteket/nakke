

library(nakke)

#devtools::install_github('Rapporteket/rapbase', ref = 'ant_linjer_autorapport')
#remotes::install_github('Rapporteket/nakke', ref = 'main')
setwd('c://Users/lro2402unn/RegistreGIT/nakke')

#Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
nakke::kjorNakkeApp(browser = TRUE)


rapbase::runAutoReport(dato = "2025-07-25", group = "nakke", dryRun = TRUE)
#Velg en dato som matcher startDate for rapporten du vil kjøre (som du finner i autoreport-tabellen i db_autoreport-databasen)

data <- NakkeRegDataSQL()
RegData <- NakkePreprosess(data)
test <- tabAntOpphShMnd(RegData, datoTil=Sys.Date(), antMnd=6, reshID=0)

unique(RegData[,c("ReshId", 'ShNavn')])


#Henter tilgangstre og mapper om resh og ShNavn
Sys.setenv(MRS_ACCESS_HIERARCHY_URL="https://app.mrs.qa.nhn.no/intensivregisterservices/AccessHiearchyReport")
#Sys.setenv(MRS_ACCESS_HIERARCHY_URL= 'https://qreg.nhn.no/laparoskopi/api/centre-information')
#Sys.setenv(MRS_ACCESS_HIERARCHY_URL= 'https://qreg.nhn.no/nakke/api/centre-information')
Sys.setenv(MRS_ACCESS_HIERARCHY_URL= 'https://qreg.nhn.no/rygg/api/centre-information')
TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
TilgangstreInt <- jsonlite::fromJSON(TilgJsn)$AccessUnits
varTilg <- c("UnitId", "ParentUnitId", "HasDatabase", "ExternalId", "Title", "TitleWithPath","ExtraData")
IntData <- merge(RegData, Tilgangstre[ ,varTilg],
                 by.x = 'ReshId', by.y = 'UnitId', suffixes = c('Int','Tilg'))
RegData <- dplyr::rename(IntData,
                         Nivaa = ExtraData,
                         ReshIdReg = ReshId,
                         ReshId = ExternalId,
                         ShNavnReg = ShNavn,
                         ShNavn = Title) #newname = oldname
