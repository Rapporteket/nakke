#' Henter data registrert for Degenerativ Nakke
#'
#' Henter data for Degenerativ Nakke fra "staging"-database.
#' Kan benytte datoFra og datoFra som input.
#'
#' @inheritParams NakkeFigAndeler
#' @param medProm Ha med prom-skjema 0-nei, 1-ja (standard)
#'
#' @return Henter dataramma RegData for Degenerativ Nakke
#' @export
#'
NakkeRegDataSQL <- function(datoFra = '2012-01-01', datoTil = Sys.Date(), medProm = 1) {
  registryName <- 'data' # "nakke"

  queryAVN <- paste0('SELECT
	Alder,
	AndreRelSykdommer,
	AntallNivaaOpr,
	AntBarn,
	AntBarnPas,
	Antibiotika,
	Arbeidstaus12mnd,
	Arbeidstaus3mnd,
	ArbeidstausPreOp,
	ASAgrad,
	AvdRESH,
	BakreFusjonSkruer,
	BakreFusjonStag,
	BakreFusjonWire,
	BMI,
	BMIkategori,
	CentreID12mnd,
	CentreID3mnd,
	Dagkirurgi,
	DelvisSykemeldtPros12mnd,
	DelvisSykemeldtPros3mnd,
	DelvisSykemeldtProsPreOp,
	EMSscore12mnd,
	EMSscore3mnd,
	EMSscorePreOp,
	EnhverKompl12mnd,
	EnhverKompl3mnd,
	EqAngstPreOp,
	Eq5DScore12mnd,
	Eq5DScore3mnd,
	Eq5DScorePreOp,
	Erstatning12mnd,
	Erstatning3mnd,
	ErstatningPreOp,
	-- FirstTimeClosed,
	ForlopsID,
	FornoydBeh12mnd,
	FornoydBeh3mnd,
	ForstLukket12mnd,
	ForstLukket3mnd,
	ForstLukketMed,
	ForstLukketPreOp,
	Fylke,
	Helsetilst12mnd,
	Helsetilst3mnd,
	HelsetilstPreOp,
	Hoyde,
	InngrepType,
	Kjonn,
	KnivtidSluttMin,
	KnivtidSluttTimer,
	KnivtidStartMin,
	KnivtidStartTimer,
	KnivtidTotalMin,
	KomplDVT12mnd,
	KomplDVT3mnd,
	KomplinfekDyp12mnd,
	KomplinfekDyp3mnd,
	KomplinfekOverfl12mnd,
	KomplinfekOverfl3mnd,
	KomplKraftsvikt12mnd,
	KomplKraftsvikt3mnd,
	KomplLungeEmboli12mnd,
	KomplLungeEmboli3mnd,
	KomplPneumoni12mnd,
	KomplPneumoni3mnd,
	KomplStemme12mnd,
	KomplStemme3mnd,
	KomplSvelging12mnd,
	KomplSvelging3mnd,
	KomplUVI12mnd,
	KomplUVI3mnd,
	LegeskjemaStatus,
	LiggeDognPostop,
	LiggeDognTotalt,
	Morsmal,
	NDIscore12mnd,
	NDIscore3mnd,
	NDIscorePreOp,
  NRSsmerteArmPreOp,
	NRSsmerteArm12mnd,
  NRSsmerteArm3mnd,
  NRSsmerteNakkePreOp,
  NRSsmerteNakke12mnd,
	NRSsmerteNakke3mnd,
	NytteOpr12mnd,
	NytteOpr3mnd,
	OperasjonsKategori,
	OppFolgStatus12mnd,
	OppFolgStatus3mnd,
	OprDato,
	OprIndikAnnet,
	OprIndikasjon,
	OprIndikMyelopati,
	OprIndikMyelopatiMotorisk,
	OprIndikMyelopatiSensorisk,
	OprIndikParese,
	OprIndikPareseGrad,
	OprIndikSmerteLokArm,
	OprIndikSmerteLokNakke,
	OprIndikSmerter,
	OprKode,
	OprMetodeAndre,
	OprMetodeAnnenBakreDekompr,
	OprMetodeBakreFusjon,
	OprMetodeDiskektomi,
	OprMetodeDiskektomiBenblokk,
	OprMetodeDiskektomiCage,
	OprMetodeDiskektomiPlate,
	OprMetodeDiskektomiSkiveprotese,
	OprMetodeForamenotomiBakreBiLat,
	OprMetodeForamenotomiBakreUniLat,
	OprMetodeKirDekompresjon,
	OprMetodeKorpektomi,
  OprMetodeTilgangBakre,
  OprMetodeTilgangFremre,
  OprMetodeTilgangFremreH,
  OprMetodeTilgangFremreV,
	Parese12mnd,
	Parese3mnd,
	ParesePreOp,
	PareseVarighet,
	PasientID,
	PasientSkjemaStatus,
	PerOpKomplAnafylaksiI,
	PerOpKomplAnnet,
	PerOpKomplBlodning,
	PerOpKomplDurarift,
  PerOpKomplFeilplasseringImplant,
  PerOpKomplKardioVaskulare,
  PerOpKomplMedullaskade,
  PerOpKomplNerverotSkade,
  PerOpKomplAnnenNerveskade,
  PerOpKomplOpFeilNivaa,
  PerOpKomplRespiratorisk,
  PerOpKomplOsofagusSkade,
  PerOpEnhverKompl
	PerOpEnhverKompl,
	RadiologiCt,
	RadiologiMr,
	RadiologiMyelografi,
	RadiologiRtgCcol,
	RadiologiRtgCcolFunkOpptak,
	Reopr90d,
	Roker,
	RtgFunnANNET,
	RtgFunnCervicalSpStenose,
	RtgFunnDegnerasjonNakke,
	RtgFunnIntrMedHoysingnalMR,
	RtgFunnProlaps,
	RtgFunnRotkanalstenose,
	RtgFunnSpondylolistese,
	Saardren,
	SivilStatus,
	SivilStatusPas,
	SmertestillBrukPreOp,
	SmertestillPreOp,
	Snuser,
	SnuserPas,
	StatusKtr12mnd,
	StatusKtr3mnd,
	SykdAnnenendokrin,
	SykdAnnet,
	SykdCarpalTunnelSyndr,
	SykdCerebrovaskular,
  SykdDepresjonAngst,
  SykdHjertekar,
  SykdHodepine,
  SykdHypertensjon,
	SykDiabetesMellitus,
	SykdKreft,
	SykdKroniskLunge,
	SykdKroniskNevrologisk,
	SykdKrSmerterMuskelSkjelSyst,
	SykdOsteoporose,
	SykdSkulderImpigment,
	SykdWhiplashNakke,
	SykehusNavn,
	SymptVarighetArmer,
	SymptVarighetNakkeHode,
	SymptVarighetSmerterUker,
	TidlOpr,
	TidlOprAnnetNiv,
	TidlOprAntall,
	TidlOprNei,
	TidlOprSammeNiv,
	Uforetrygd12mnd,
	Uforetrygd3mnd,
	UforetrygdPreOp,
	UforeTrygdPros3mnd,
	UforeTrygdProsPreOp,
	Utdanning,
	UtdanningPas,
	UtDato,
	VarighetSykeMeld12mnd,
	VarighetSykeMeld3mnd,
	Vekt
FROM allevarnum
                  WHERE OprDato >= \'', datoFra, '\' AND OprDato <= \'', datoTil, '\'')

  #queryAVN <-'select * from allevarnum'
  RegDataAVN <- rapbase::loadRegData(registryName = registryName , query = queryAVN, dbType = "mysql")

  queryForl <- 'SELECT ForlopsID, Kommune, Kommunenr, Fylkenr, Avdod, AvdodDato, BasisRegStatus
               FROM forlopsoversikt'
  RegDataForl <- rapbase::loadRegData(registryName = registryName , query = queryForl, dbType = "mysql")

  RegData <- merge(RegDataAVN, RegDataForl, by='ForlopsID', all.x = TRUE, all.y = FALSE, suffixes = '')

  if (medProm == 1) {

    #Feil i andel oppfølging etter innføreing av ePROM. OppFolgStatus3mnd=1 betyr ikke lenger at skjemaet er utfylt
    #Må lage variabelen på nytt
    ePROMadmTab <- rapbase::loadRegData(registryName=registryName,
                                        query='SELECT * FROM proms')
    ePROMvar <- c("MCEID", "TSSENDT", "TSRECEIVED", "NOTIFICATION_CHANNEL", "DISTRIBUTION_RULE",
                  'REGISTRATION_TYPE')
    # «EpromStatus»:  0 = Created, 1 = Ordered, 2 = Expired, 3 = Completed, 4 = Failed
    ind3mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                       c('PATIENTFOLLOWUP', 'PATIENTFOLLOWUP_3_PiPP', 'PATIENTFOLLOWUP_3_PiPP_REMINDER'))
    ind12mnd <- which(ePROMadmTab$REGISTRATION_TYPE %in%
                        c('PATIENTFOLLOWUP12', 'PATIENTFOLLOWUP_12_PiPP', 'PATIENTFOLLOWUP_12_PiPP_REMINDER'))

    indIkkeEprom3mnd <-  which(!(RegData$ForlopsID %in% ePROMadmTab$MCEID[ind3mnd]))
    indIkkeEprom12mnd <-  which(!(RegData$ForlopsID %in% ePROMadmTab$MCEID[ind12mnd]))

    #indEprom <-  which((RegDataV3$ForlopsID %in% ePROMadmTab$MCEID[ind3mnd]))
    RegData$OppFolg3mndGML <- RegData$OppFolgStatus3mnd
    RegData$OppFolgStatus3mnd <- 0
    RegData$OppFolgStatus3mnd[
      RegData$ForlopsID %in% ePROMadmTab$MCEID[intersect(ind3mnd, which(ePROMadmTab$STATUS==3))]] <- 1
    RegData$OppFolgStatus3mnd[intersect(which(RegData$OppFolg3mndGML ==1), indIkkeEprom3mnd)] <- 1

    RegData$OppFolg12mndGML <- RegData$OppFolgStatus12mnd
    RegData$OppFolgStatus12mnd <- 0
    RegData$OppFolgStatus12mnd[
      RegData$ForlopsID %in% ePROMadmTab$MCEID[intersect(ind12mnd, which(ePROMadmTab$STATUS==3))]] <- 1
    RegData$OppFolgStatus12mnd[intersect(which(RegData$OppFolg12mndGML ==1), indIkkeEprom12mnd)] <- 1
  }

  return(RegData)
}



