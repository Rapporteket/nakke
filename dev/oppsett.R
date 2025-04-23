
Sys.setenv(FALK_EXTENDED_USER_RIGHTS= "[{\"A\":85,\"R\":\"SC\",\"U\":601161},{\"A\":85,\"R\":\"LU\",\"U\":601161},{\"A\":85,\"R\":\"LU\",\"U\":108172},{\"A\":85,\"R\":\"LU\",\"U\":103575}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nakke/data-raw/config")
# Sys.setenv(MYSQL_DB_DATA="DegenNakkeReportDataStaging") #"db_data"
Sys.setenv(MYSQL_DB_DATA="nakke") #"db_data"

setwd('c://Users/lro2402unn/RegistreGIT/nakke')
Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
nakke::kjorNakkeApp(browser = TRUE)

data <- NakkeRegDataSQL()
library(nakke)
##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
sship::dec("c://Users/ast046/Downloads/nakke131fd5e4d.sql.gz__20250228_155615.tar.gz", keyfile = "p://.ssh/id_rsa", target_dir = "c://Users/ast046/Downloads/.")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor
Sys.setenv(MYSQL_DB_DATA="nakke")

Sys.setenv(R_RAP_CONFIG_PATH="c://Users/ast046/repo/rapporteket/rygg/dev/config")

nakke::kjorNakkeApp(browser = TRUE)
