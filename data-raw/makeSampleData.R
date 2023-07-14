#' makeSampleData creates and (optionally) stores samle data in the package
#'
#' To be used to create and/or update sample data in the package
#'
#' @param realRegData Data frame to make sample data from
#' @param saveSampleDataInPackage Logical if TRUE will overwrite any existing
#' @return sampleRegData Data frame of sample data

makeSampleData <- function(realRegData, saveSampleDataInPackage=FALSE) {

  # synpop or whatever method to flip and turn orignal data
  sampleRegData <- realRegData
  # not implemented just yet
  if (1==0) {
    RegData <- NakkeRegDataSQL()
    RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500)
    sampleRegData <- RegDataSyn$syn
  }

  # make fake hospital/dept names consistent with reshIDs
  sampleRegData$Avdeling <- NULL
  sampleRegData$SykehusNavn <- NULL
  sampleRegData$Avdeling <-
    rep("navnløsAvd", length(sampleRegData$AVD_RESH))
  sampleRegData$SykehusNavn <-
    rep("navnløsSh", length(sampleRegData$AVD_RESH))
  depts <- levels(factor(sampleRegData$AVD_RESH))
  baseName <- rep("shusAvd", length(depts))
  ownName <- paste0(baseName, LETTERS[1:length(depts)])
  for (i in 1:length(depts)) {
    ind <- which(sampleRegData$AVD_RESH == depts[i])
    sampleRegData$Avdeling[ind] <- ownName[i]
    sampleRegData$SykehusNavn[ind] <- ownName[i]
  }

  # save data if requested
  if (saveSampleDataInPackage) {
    RegData <- sampleRegData
    devtools::use_data(RegData, RegData, overwrite = saveSampleDataInPackage)
  }

  return(sampleRegData)
}
