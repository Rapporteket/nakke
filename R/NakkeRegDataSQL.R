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

  queryAVN <- paste0('SELECT mce.CENTREID                                                                    AS AvdRESH,
       -- getFriendlyName(mce.CENTREID)                                                   AS SykehusNavn,
       mce.CENTREID                                                  AS SykehusNavn,
       mce.STATUS                                                                      AS ForlopsStatus,
       mce.MAIL_STATUS                                                                 AS ForlopsMailStatus,
       mce.MCEID                                                                       AS ForlopsID,
       patient.ID                                                                      AS PasientID,
       patient.SSN                                                                     AS FodselsNr,
       patient.BIRTH_DATE                                                              AS FodselsDato,
       patient.LASTNAME                                                                AS Etternavn,
       patient.FIRSTNAME                                                               AS Fornavn,
       patient.GENDER                                                                  AS Kjonn,
       patient.MARITAL_STATUS                                                          AS SivilStatus,
       patient.NO_CHILDREN                                                             AS AntBarn,
       patient.NATIVE_LANGUAGE                                                         AS Morsmal,
       patient.SMOKING                                                                 AS Roker,
       patient.SNUFF                                                                   AS Snuser,
       patient.EDUCATION                                                               AS Utdanning,
       patient.DECEASED                                                                AS PasientDod,
       patient.DECEASED_DATE                                                           AS DodsDato,
       patient.ADDR_TYPE                                                               AS AdresseType,
       patient.ADDRESS                                                                 AS Adresse,
       patient.ZIPCODE                                                                 AS Postnummer,
       patient.TOWN                                                                    AS Poststed,
       patient.COUNTY                                                                  AS Fylke,
       patient.PHONE                                                                   AS TlfNummer,
       patient.ALT_PHONE                                                               AS AltTlfNummer,
       patient.EMAIL                                                                   AS Epost,
       patient.CONSENT_STATUS                                                          AS SamtykkeStatus,
       patientform.UTFYLLING_DATO                                                      AS UtfyltDatoPas,
       patientform.MARITAL_STATUS                                                      AS SivilStatusPas,
       patientform.NO_CHILDREN                                                         AS AntBarnPas,
       patientform.EDUCATION                                                           AS UtdanningPas,
       patientform.ZIPCODE                                                             AS PostnrPas,
       patientform.TOWN                                                                AS PoststedPas,
       patientform.ROYKING                                                             AS RokerPas,
       patientform.SNUFF                                                               AS SnuserPas,
       patientform.HOYDE                                                               AS Hoyde,
       patientform.HOYDE_MISS                                                          AS HoydeMangler,
       patientform.VEKT                                                                AS Vekt,
       patientform.VEKT_MISS                                                           AS VektMangler,
       patientform.BMI                                                                 AS BMI,
       patientform.BMI_CATEGORY                                                        AS BMIkategori,
       patientform.YRKE_FYSISKE_KRAV                                                   AS YrkeFysiskeKrav,
       patientform.ARBEIDSSTATUS                                                       AS ArbeidstausPreOp,
       patientform.DELVISSYKEMELDT_PROSENT                                             AS DelvisSykemeldtProsPreOp,
       patientform.UFORETRYGDET_PROSENT                                                AS UforeTrygdProsPreOp,
       patientform.SOKT_UFOR                                                           AS UforetrygdPreOp,
       patientform.SOKT_ERSTATNING                                                     AS ErstatningPreOp,
       patientform.SYMPTOMVARIGHET_NAKKE_HODE                                          AS SymptVarighetNakkeHode,
       patientform.SYMPTOMVARRIGHET_ARMER                                              AS SymptVarighetArmer,
       patientform.SYMPTOMVARIGHET_SMERTER_UKER                                        AS SymptVarighetSmerterUker,
       patientform.SMERTE_VAS_HODET                                                    AS NRSsmerteHodetPreOp,
       patientform.SMERTE_VAS_HODET_MISS                                               AS NRSsmerteHodetPreOpMangler,
       patientform.SMERTE_VAS_NAKKE                                                    AS NRSsmerteNakkePreOp,
       patientform.SMERTE_VAS_NAKKE_MISS                                               AS NRSsmerteNakkePreOpMangler,
       patientform.SMERTE_VAS_ARMER                                                    AS NRSsmerteArmPreOp,
       patientform.SMERTE_VAS_ARMER_MISS                                               AS NRSsmerteArmPreOpMangler,
       patientform.SMERTE_LOKALISERING_SIDE                                            AS SmerteLokSidePreOp,
       patientform.SMERTE_LOKALISERING_OVEREKS                                         AS SmerteLokOverEksPreOp,
       patientform.SMERTE_TILIGERE_SKULDER_PLAGER                                      AS TidlSkulderPlagerPreOp,
       patientform.SMERTESTILLENDE                                                     AS SmertestillPreOp,
       patientform.SMERTESTILLENDE_BRUK                                                AS SmertestillBrukPreOp,
       patientform.REDUSERT_STYRKE                                                     AS ParesePreOp,
       patientform.REDUSERT_STYRKE_VARIGHET                                            AS PareseVarighet,
       patientform.REDUSERT_STYRKE_VAR_TIMER                                           AS PareseTimer,
       patientform.REDUSERT_STYRKE_VAR_DAGER                                           AS PareseDager,
       patientform.REDUSERT_STYRKE_VAR_UKER                                            AS PareseUker,
       patientform.NDI_SMERTE                                                          AS NDIsmertePreOp,
       patientform.NDI_PERSONLIG_STELL                                                 AS NDIpersStellPreOp,
       patientform.NDI_LOFTING                                                         AS NDIloftingPreOp,
       patientform.NDI_LESING                                                          AS NDIlesingPreOp,
       patientform.NDI_HODEPINE                                                        AS NDIhodepinePreOp,
       patientform.NDI_KONSENTRASJON                                                   AS NDIkonsentrasjonPreOp,
       patientform.NDI_ARBEID                                                          AS NDIarbeidPreOp,
       patientform.NDI_BILKJORING                                                      AS NDIbilkjoringPreOp,
       patientform.NDI_SOVN                                                            AS NDIsovnPreOp,
       patientform.NDI_FRITID                                                          AS NDIfritidPreOp,
       patientform.NDI_SCORE                                                           AS NDIscorePreOp,
       patientform.EMS_GANGFUNKSJON                                                    AS EMSgangeFunkPreOp,
       patientform.EMS_HANDFUNKSJON                                                    AS EMShandFunkPreOp,
       patientform.EMS_KOORDINASJON                                                    AS EMSkoordinasjonPreOp,
       patientform.EMS_BLARE_TARM                                                      AS EMSblareTarmPreOp,
       patientform.EMS_NUMMENHET                                                       AS EMSnummenhetPreOp,
       patientform.EMS_SCORE                                                           AS EMSscorePreOp,
       CASE
           WHEN patientform.EQ5D_GANGE IS NOT NULL THEN 3
           WHEN patientform.EQ5D_5L_GANGE IS NOT NULL THEN 5
           ELSE 0
           END                                                                         AS EqType,
       IFNULL(patientform.EQ5D_5L_GANGE, patientform.EQ5D_GANGE)                       AS EqGangePreOp,
       IFNULL(patientform.EQ5D_5L_PERSONLIG_STELL, patientform.EQ5D_PERSONLIG_STELL)   AS EqPersStellPreOp,
       IFNULL(patientform.EQ5D_5L_VANLIGE_GJOREMAL, patientform.EQ5D_VANLIGE_GJOREMAL) AS EqVanlGjMaalPreOp,
       IFNULL(patientform.EQ5D_5L_SMERTE_UBEHAG, patientform.EQ5D_SMERTE_UBEHAG)       AS EqSmertePreOp,
       IFNULL(patientform.EQ5D_5L_ANGST_DEPRESJON, patientform.EQ5D_ANGST_DEPRESJON)   AS EqAngstPreOp,
       IFNULL(patientform.EQ5D_5L_SCORE, patientform.EQ5D_SCORE)                       AS Eq5DScorePreOp,
       patientform.HELSETILSTAND_SCALE                                                 AS HelsetilstPreOp,
       patientform.HELSETILSTAND_SCALE_MISS                                            AS HelsetilstPreOpMangler,
       patientform.STATUS                                                              AS PasientSkjemaStatus,
       patientform.FIRST_TIME_CLOSED_BY                                                AS ForstLukketAVPreOp,
       patientform.FIRST_TIME_CLOSED                                                   AS ForstLukketPreOp,
       surgeonform.OPERASJONSDATO                                                      AS OprDato,
       surgeonform.UTFYLLING_DATO                                                      AS UtfyltDatoLegeskjema,
       surgeonform.ALDER_VED_OPERASJON                                                 AS Alder,
       surgeonform.TIDLIGERE_OPERERT_SAMME                                             AS TidlOprSammeNiv,
       surgeonform.TIDLIGERE_OPERERT_ANNET                                             AS TidlOprAnnetNiv,
       surgeonform.TIDLIGERE_OPERERT_NEI                                               AS TidlOprNei,
       surgeonform.TIDLIGERE_OPERERT_ANTALL                                            AS TidlOprAntall,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER                                           AS AndreRelSykdommer,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_RA                             AS SykdReumatoidartritt,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_BECHTEREW                      AS SykdBechtrew,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_ANNEN_REVMATISK                AS SykdAnnenreumatisk,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_STEROID_IMMUN                  AS SykdImmunSuprBeh,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_MUSKEL_SKJELETT_SMERTER        AS SykdKrSmerterMuskelSkjelSyst,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_CARPAL_SYNDROM                 AS SykdCarpalTunnelSyndr,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_SKULDER_IMPINGMENT             AS SykdSkulderImpigment,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_WHIPLASH_NAKKESKADE            AS SykdWhiplashNakke,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_OSTEOPOROSE                    AS SykdOsteoporose,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_DEPRESJON_ANGST                AS SykdDepresjonAngst,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_HODEPINE                       AS SykdHodepine,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_CEREBROVASKULAERT              AS SykdCerebrovaskular,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_KRONISK_NEVROLOGISK            AS SykdKroniskNevrologisk,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_HYPERTENSJON                   AS SykdHypertensjon,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_HJERTEKAR                      AS SykdHjertekar,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_CLAUDICATIO_VASKULAER          AS SykdVaskularClaudicatio,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_KREFT                          AS SykdKreft,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_ASTHMA_LUNGESYKDOM             AS SykdKroniskLunge,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_DIABETES                       AS SykDiabetesMellitus,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_ANNEN_ENDOKRINOLOGISK          AS SykdAnnenendokrin,
       surgeonform.ANDRE_RELEVANTE_SYKDOMMER_SPESIFISER_ANNET                          AS SykdAnnet,
       surgeonform.TYPE_UNDERSOEKELSE_CT                                               AS RadiologiCt,
       surgeonform.TYPE_UNDERSOEKELSE_MR                                               AS RadiologiMr,
       surgeonform.TYPE_UNDERSOEKELSE_MYELOGRAFI                                       AS RadiologiMyelografi,
       surgeonform.TYPE_UNDERSOEKELSE_EMG_NEVROGRAFI                                   AS UsEmgNevrografi,
       surgeonform.TYPE_UNDERSOEKELSE_ROTBLOKADE                                       AS UsRotblokkade,
       surgeonform.TYPE_UNDERSOEKELSE_RTG_CERVICAL_COLUMNA                             AS RadiologiRtgCcol,
       surgeonform.TYPE_FLEKSJON_FUNKSJONSOPPTAK                                       AS RadiologiRtgCcolFunkOpptak,
       surgeonform.FUNN_NORMAL                                                         AS RtgFunnNormal,
       surgeonform.FUNN_SKIVEPROLAPS                                                   AS RtgFunnProlaps,
       surgeonform.FUNN_CERVICAL_SPINALSTENOSE                                         AS RtgFunnCervicalSpStenose,
       surgeonform.FUNN_DEGENERATIV_NAKKE                                              AS RtgFunnDegnerasjonNakke,
       surgeonform.FUNN_ROTKANALSTENOSE                                                AS RtgFunnRotkanalstenose,
       surgeonform.FUNN_SPONDYLOLISTESE                                                AS RtgFunnSpondylolistese,
       surgeonform.FUNN_INTRAMEDULLAERE_HOEYSIGNAL_FORANDRINGER_VED_MR                 AS RtgFunnIntrMedHoysingnalMR,
       surgeonform.FUNN_ANNET                                                          AS RtgFunnANNET,
       surgeonform.OPERASJONSINDIKASJON_SMERTER                                        AS OprIndikSmerter,
       surgeonform.OPERASJONSINDIKASJON_SMERTER_LOKALISERING_NAKKE                     AS OprIndikSmerteLokNakke,
       surgeonform.OPERASJONSINDIKASJON_SMERTER_LOKALISERING_ARM                       AS OprIndikSmerteLokArm,
       surgeonform.OPERASJONSINDIKASJON_PARESER                                        AS OprIndikParese,
       surgeonform.OPERASJONSINDIKASJON_PARESE_GRAD                                    AS OprIndikPareseGrad,
       surgeonform.OPERASJONSINDIKASJON_MYELOPATI                                      AS OprIndikMyelopati,
       surgeonform.OPERASJONSINDIKASJON_MYELOPATI_TYPE_SENSORISK                       AS OprIndikMyelopatiSensorisk,
       surgeonform.OPERASJONSINDIKASJON_MYELOPATI_TYPE_MOTORISK                        AS OprIndikMyelopatiMotorisk,
       surgeonform.OPERASJONSINDIKASJON_ANNET                                          AS OprIndikAnnet,
       surgeonform.REOP_INNEN_90_DAGER                                                 AS Reopr90d,
       surgeonform.OPERASJONSKATEGORI                                                  AS OperasjonsKategori,
       surgeonform.OPERASJONSKATEGORI_DAGKIRURGISK                                     AS Dagkirurgi,
       surgeonform.ASA_GRAD                                                            AS ASAgrad,
       surgeonform.RANAWAT_KLASSIFIKASJON                                              AS RanawatKlassifikasjon,
       surgeonform.OPERASJONSMETODE_TILGANG_BAKRE                                      AS OprMetodeTilgangBakre,
       surgeonform.OPERASJONSMETODE_TILGANG_FREMRE                                     AS OprMetodeTilgangFremre,
       surgeonform.OPERASJONSMETODE_TILGANG_FREMRE_HOEYRE                              AS OprMetodeTilgangFremreH,
       surgeonform.OPERASJONSMETODE_TILGANG_FREMRE_VENSTRE                             AS OprMetodeTilgangFremreV,
       surgeonform.OPERASJONSMETODE_MIKRO_MAKRO_ENDO                                   AS OprMetodeMikroMakroEndo,
       surgeonform.OPERASJONSMETODE_DISKEKTOMI                                         AS OprMetodeDiskektomi,
       surgeonform.OPERASJONSMETODE_DISKEKTOMI_SPESIFISER_BENBLOKK                     AS OprMetodeDiskektomiBenblokk,
       surgeonform.OPERASJONSMETODE_DISKEKTOMI_SPESIFISER_PLATE                        AS OprMetodeDiskektomiPlate,
       surgeonform.OPERASJONSMETODE_DISKEKTOMI_SPESIFISER_CAGE                         AS OprMetodeDiskektomiCage,
       surgeonform.OPERASJONSMETODE_DISKEKTOMI_SPESIFISER_SKIVEPROTESE                 AS OprMetodeDiskektomiSkiveprotese,
       surgeonform.OPERASJONSMETODE_KIRURGISK_DEKOMPRESJON                             AS OprMetodeKirDekompresjon,
       surgeonform.OPERASJONSMETODE_FORAMENOTOMI_BAKRE_UNILATERAL                      AS OprMetodeForamenotomiBakreUniLat,
       surgeonform.OPERASJONSMETODE_FORAMENOTOMI_BAKRE_BILATERAL                       AS OprMetodeForamenotomiBakreBiLat,
       surgeonform.OPERASJONSMETODE_ANNEN_BAKRE_DEKOMPRESJON                           AS OprMetodeAnnenBakreDekompr,
       surgeonform.OPERASJONSMETODE_KORPEKTOMI                                         AS OprMetodeKorpektomi,
       surgeonform.OPERASJONSMETODE_KORPEKTOMI_SPESIFISER_PLATE                        AS OprMetodeKorpektomiPlate,
       surgeonform.OPERASJONSMETODE_KORPEKTOMI_SPESIFISER_BUR                          AS OprMetodeKorpektomiBur,
       surgeonform.OPERASJONSMETODE_KORPEKTOMI_SPESIFISER_BEINBLOKK                    AS OprMetodeKorpektomiBenblokk,
       surgeonform.OPERASJONSMETODE_ANDRE_OPERASJONSMETODER                            AS OprMetodeAndre,
       surgeonform.BAKRE_FUSJON                                                        AS OprMetodeBakreFusjon,
       surgeonform.INSTRUMENTERING_WIRE                                                AS BakreFusjonWire,
       surgeonform.INSTRUMENTERING_SKRUER                                              AS BakreFusjonSkruer,
       surgeonform.INSTRUMENTERING_STAG                                                AS BakreFusjonStag,
       surgeonform.BAKRE_FUSJON_NIVAA_PROXIMALE                                        AS BakreFusjonProximaltNiv,
       surgeonform.BAKRE_FUSJON_NIVAA_DISTALE                                          AS BakreFusjonDistaltNiv,
       surgeonform.TYPE_GRAFT_AUGOGRAFT                                                AS BenGraftAutograft,
       surgeonform.TYPE_GRAFT_BENSUBSTITUTT                                            AS BenGraftBensubstitutt,
       surgeonform.TYPE_GRAFT_BANK_BEN                                                 AS BenGraftBankben,
       surgeonform.SIDE_NIVAA_C0_C1                                                    AS SideNivaaC0C1,
       surgeonform.SIDE_NIVAA_C0_C1_HOEYRE                                             AS SideNivaaC0C1H,
       surgeonform.SIDE_NIVAA_C0_C1_VENSTRE                                            AS SideNivaaC0C1V,
       surgeonform.SIDE_NIVAA_C1_C2                                                    AS SideNivaaC1C2,
       surgeonform.SIDE_NIVAA_C1_C2_HOEYRE                                             AS SideNivaaC1C2H,
       surgeonform.SIDE_NIVAA_C1_C2_VENSTRE                                            AS SideNivaaC1C2V,
       surgeonform.SIDE_NIVAA_C2_C3                                                    AS SideNivaaC2C3,
       surgeonform.SIDE_NIVAA_C2_C3_HOEYRE                                             AS SideNivaaC2C3H,
       surgeonform.SIDE_NIVAA_C2_C3_VENSTRE                                            AS SideNivaaC2C3V,
       surgeonform.SIDE_NIVAA_C3_C4                                                    AS SideNivaaC3C4,
       surgeonform.SIDE_NIVAA_C3_C4_HOEYRE                                             AS SideNivaaC3C4H,
       surgeonform.SIDE_NIVAA_C3_C4_VENSTRE                                            AS SideNivaaC3C4V,
       surgeonform.SIDE_NIVAA_C4_C5                                                    AS SideNivaaC4C5,
       surgeonform.SIDE_NIVAA_C4_C5_HOEYRE                                             AS SideNivaaC4C5H,
       surgeonform.SIDE_NIVAA_C4_C5_VENSTRE                                            AS SideNivaaC4C5V,
       surgeonform.SIDE_NIVAA_C5_C6                                                    AS SideNivaaC5C6,
       surgeonform.SIDE_NIVAA_C5_C6_HOEYRE                                             AS SideNivaaC5C6H,
       surgeonform.SIDE_NIVAA_C5_C6_VENSTRE                                            AS SideNivaaC5C6V,
       surgeonform.SIDE_NIVAA_C6_C7                                                    AS SideNivaaC6C7,
       surgeonform.SIDE_NIVAA_C6_C7_HOEYRE                                             AS SideNivaaC6C7H,
       surgeonform.SIDE_NIVAA_C6_C7_VENSTRE                                            AS SideNivaaC6C7V,
       surgeonform.SIDE_NIVAA_C7_TH1                                                   AS SideNivaaC7TH1,
       surgeonform.SIDE_NIVAA_C7_TH1_HOEYRE                                            AS SideNivaaC7TH1H,
       surgeonform.SIDE_NIVAA_C7_TH1_VENSTRE                                           AS SideNivaaC7TH1V,
       surgeonform.ANTIBIOTIKA                                                         AS Antibiotika,
       surgeonform.ANTIBIOTIKA_MEDIKAMENT                                              AS AntibiotikaMedikament,
       surgeonform.ANTIBIOTIKA_DOSE                                                    AS AntibiotikaDose,
       surgeonform.ANTIBIOTIKA_ANTALL_DOSER                                            AS AntibiotikaDoseAntall,
       surgeonform.ANTIBIOTIKA_INTERVALL_KUN_OPERASJONSDAGEN                           AS AntibiotikaIntKunOprDag,
       surgeonform.ANTIBIOTIKA_INTERVALL_EVT_ANTALL_DOEGN                              AS AntibiotikaIntEvtAntDogn,
       surgeonform.ANTIBIOTIKA_ANTALL_DOEGN                                            AS AntibiotikaAntDogn,
       surgeonform.SAARDREN                                                            AS Saardren,
       surgeonform.KNIVTID_KLOKKESLETT_START_TIMER                                     AS KnivtidStartTimer,
       surgeonform.KNIVTID_KLOKKESLETT_START_MIN                                       AS KnivtidStartMin,
       surgeonform.KNIVTID_KLOKKESLETT_SLUTT_TIMER                                     AS KnivtidSluttTimer,
       surgeonform.KNIVTID_KLOKKESLETT_SLUTT_MIN                                       AS KnivtidSluttMin,
       surgeonform.KNIVTID_MIN                                                         AS KnivtidTotalMin,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_DURARIFT                                AS PerOpKomplDurarift,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_NERVEROTSKADE                           AS PerOpKomplNerverotSkade,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_OP_FEIL_NIVAA                           AS PerOpKomplOpFeilNivaa,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_FEILPLASSERING_IMPLANTAT                AS PerOpKomplFeilplasseringImplant,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_BLOEDNING                               AS PerOpKomplBlodning,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_RESPIRATORISKE                          AS PerOpKomplRespiratorisk,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_ANAFYLAKSI                              AS PerOpKomplAnafylaksiI,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_MEDULLASKADE                            AS PerOpKomplMedullaskade,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_OESOFAGUS                               AS PerOpKomplOsofagusSkade,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_SKADE_STORE_BLODKAR                     AS PerOpKomplSkadeStoreBlodkar,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_KARDIOVASKULAERE                        AS PerOpKomplKardioVaskulare,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_ANNEN_NERVESKADE                        AS PerOpKomplAnnenNerveskade,
       surgeonform.PEROPERATIVE_KOMPLIKASJONER_ANNET                                   AS PerOpKomplAnnet,
       surgeonform.OP_KODE1                                                            AS OprKode,
       surgeonform.OP_KODE1_VERSION                                                    AS OprKodeVer,
       surgeonform.OP_INDICATION2                                                      AS OprIndikasjon,
       surgeonform.OP_INDICATION2_VERSION                                              AS OprIndikasjonVer,
       surgeonform.UTDATO                                                              AS UtDato,
       surgeonform.LIGGEDOEGN_POSTOPERATIV                                             AS LiggeDognPostop,
       surgeonform.LIGGEDOEGN_TOTAL                                                    AS LiggeDognTotalt,
       surgeonform.DOEDSFALL_UNDER_OPPHOLDET                                           AS DodsfallOpphold,
       surgeonform.STATUS                                                              AS LegeskjemaStatus,
       surgeonform.FIRST_TIME_CLOSED_BY                                                AS ForstLukketAVMed,
       surgeonform.FIRST_TIME_CLOSED                                                   AS ForstLukketMed,

       followup3.CENTREID                                                              AS CentreID3mnd,
       -- getFriendlyName(followup3.CENTREID)                                             AS SykehusNavn3mnd,
       followup3.STATUS_CONTROL                                                        AS StatusKtr3mnd,
       followup3.UTFYLLING_DATO                                                        AS UtfyltDato3mnd,
       followup3.FRISKMELDT_DATO                                                       AS FriskmeldtDato3mnd,
       followup3.VARIGHET_UKER                                                         AS VarighetSykeMeld3mnd,
       followup3.NYTTE_AV_OPERASJON                                                    AS NytteOpr3mnd,
       followup3.FORNOYDHET_MED_OPERASJON                                              AS FornoydBeh3mnd,
       followup3.REDUSERT_STYRKE_ETTER                                                 AS Parese3mnd,
       followup3.KOMPLIKASJONER_URINVEISINFEKSJON                                      AS KomplUVI3mnd,
       followup3.KOMPLIKASJONER_LUNGEBETENNELSE                                        AS KomplPneumoni3mnd,
       followup3.KOMPLIKASJONER_BLODPROPP_BEN                                          AS KomplDVT3mnd,
       followup3.KOMPLIKASJONER_BLODPROPP_LUNGE                                        AS KomplLungeEmboli3mnd,
       followup3.KOMPLIKASJONER_INFEKSJON_OVERFLADISK                                  AS KomplinfekOverfl3mnd,
       followup3.KOMPLIKASJONER_INFEKSJON_DYP                                          AS KomplinfekDyp3mnd,
       followup3.KOMPLIKASJONER_KRAFTSVIKT                                             AS KomplKraftsvikt3mnd,
       followup3.KOMPLIKASJONER_UBEHAG_SVELGING                                        AS KomplSvelging3mnd,
       followup3.KOMPLIKASJONER_STEMME                                                 AS KomplStemme3mnd,
       followup3.ARBEIDSSTATUS                                                         AS Arbeidstaus3mnd,
       followup3.DELVISSYKEMELDT_PROSENT                                               AS DelvisSykemeldtPros3mnd,
       followup3.UFORETRYGDET_PROSENT                                                  AS UforeTrygdPros3mnd,
       followup3.SOKT_UFOR                                                             AS Uforetrygd3mnd,
       followup3.SOKT_ERSTATNING                                                       AS Erstatning3mnd,
       followup3.SMERTE_VAS_HODET                                                      AS NRSsmerteHodet3mnd,
       followup3.SMERTE_VAS_NAKKE                                                      AS NRSsmerteNakke3mnd,
       followup3.SMERTE_VAS_ARMER                                                      AS NRSsmerteArm3mnd,
       followup3.SMERTE_LOKALISERING_SIDE                                              AS SmerteLokSide3mnd,
       followup3.SMERTE_LOKALISERING_OVEREKS                                           AS SmerteLokOverEks3mnd,
       followup3.SMERTE_TILIGERE_SKULDER_PLAGER                                        AS TidlSkulderPlager3mnd,
       followup3.SMERTESTILLENDE                                                       AS Smertestill3mnd,
       followup3.SMERTESTILLENDE_BRUK                                                  AS SmertestillBruk3mnd,
       followup3.NDI_SMERTE                                                            AS NDIsmerte3mnd,
       followup3.NDI_PERSONLIG_STELL                                                   AS NDIpersStell3mnd,
       followup3.NDI_LOFTING                                                           AS NDIlofting3mnd,
       followup3.NDI_LESING                                                            AS NDIlesing3mnd,
       followup3.NDI_HODEPINE                                                          AS NDIhodepineE3mnd,
       followup3.NDI_KONSENTRASJON                                                     AS NDIkonsentrasjon3mnd,
       followup3.NDI_ARBEID                                                            AS NDIarbeid3mnd,
       followup3.NDI_BILKJORING                                                        AS NDIbilkjoring3mnd,
       followup3.NDI_SOVN                                                              AS NDIsovn3mnd,
       followup3.NDI_FRITID                                                            AS NDIfritid3mnd,
       followup3.NDI_SCORE                                                             AS NDIscore3mnd,
       followup3.EMS_GANGFUNKSJON                                                      AS EMSgangeFunk3mnd,
       followup3.EMS_HANDFUNKSJON                                                      AS EMShandFunk3mnd,
       followup3.EMS_KOORDINASJON                                                      AS EMSkoordinasjon3mnd,
       followup3.EMS_BLARE_TARM                                                        AS EMSblareTarm3mnd,
       followup3.EMS_NUMMENHET                                                         AS EMSnummenhet3mnd,
       followup3.EMS_SCORE                                                             AS EMSscore3mnd,
       CASE
           WHEN followup3.EQ5D_GANGE IS NOT NULL THEN 3
           WHEN followup3.EQ5D_5L_GANGE IS NOT NULL THEN 5
           ELSE 0
           END                                                                         AS EqType3mnd,
       IFNULL(followup3.EQ5D_5L_GANGE, followup3.EQ5D_GANGE)                           AS EqGange3mnd,
       IFNULL(followup3.EQ5D_5L_PERSONLIG_STELL, followup3.EQ5D_PERSONLIG_STELL)       AS EqPersStell3mnd,
       IFNULL(followup3.EQ5D_5L_VANLIGE_GJOREMAL, followup3.EQ5D_VANLIGE_GJOREMAL)     AS EqVanlGjMaal3mnd,
       IFNULL(followup3.EQ5D_5L_SMERTE_UBEHAG, followup3.EQ5D_SMERTE_UBEHAG)           AS EqSmerte3mnd,
       IFNULL(followup3.EQ5D_5L_ANGST_DEPRESJON, followup3.EQ5D_ANGST_DEPRESJON)       AS EqAngst3mnd,
       IFNULL(followup3.EQ5D_5L_SCORE, followup3.EQ5D_SCORE)                           AS Eq5DScore3mnd,
       followup3.HELSETILSTAND_SCALE                                                   AS Helsetilst3mnd,
       followup3.STATUS                                                                AS OppFolgStatus3mnd,
       followup3.TSUPDATED                                                             AS OppFolgOppdatert3mnd,    -- Not specified in NAKKE-203
       followup3.UPDATEDBY                                                             AS OppFolgOppdatertAv3mnd,  -- Not specified in NAKKE-203
       followup3.TSCREATED                                                             AS OppFolgLaget3mnd,        -- Not specified in NAKKE-203
       followup3.CREATEDBY                                                             AS OppFolgLagetAv3mnd,      -- Not specified in NAKKE-203
       followup3.FIRST_TIME_CLOSED_BY                                                  AS ForstLukketAV3mnd,
       followup3.FIRST_TIME_CLOSED                                                     AS ForstLukket3mnd,

       followup12.CENTREID                                                             AS CentreID12mnd,
       -- getFriendlyName(followup12.CENTREID)                                            AS SykehusNavn12mnd,
       followup12.STATUS_CONTROL                                                       AS StatusKtr12mnd,
       followup12.UTFYLLING_DATO                                                       AS UtfyltDato12mnd,
       followup12.FRISKMELDT_DATO                                                      AS FriskmeldtDato12mnd,
       followup12.VARIGHET_UKER                                                        AS VarighetSykeMeld12mnd,
       followup12.NYTTE_AV_OPERASJON                                                   AS NytteOpr12mnd,
       followup12.FORNOYDHET_MED_OPERASJON                                             AS FornoydBeh12mnd,
       followup12.REDUSERT_STYRKE_ETTER                                                AS Parese12mnd,
       followup12.KOMPLIKASJONER_URINVEISINFEKSJON                                     AS KomplUVI12mnd,
       followup12.KOMPLIKASJONER_LUNGEBETENNELSE                                       AS KomplPneumoni12mnd,
       followup12.KOMPLIKASJONER_BLODPROPP_BEN                                         AS KomplDVT12mnd,
       followup12.KOMPLIKASJONER_BLODPROPP_LUNGE                                       AS KomplLungeEmboli12mnd,
       followup12.KOMPLIKASJONER_INFEKSJON_OVERFLADISK                                 AS KomplinfekOverfl12mnd,
       followup12.KOMPLIKASJONER_INFEKSJON_DYP                                         AS KomplinfekDyp12mnd,
       followup12.KOMPLIKASJONER_KRAFTSVIKT                                            AS KomplKraftsvikt12mnd,
       followup12.KOMPLIKASJONER_UBEHAG_SVELGING                                       AS KomplSvelging12mnd,
       followup12.KOMPLIKASJONER_STEMME                                                AS KomplStemme12mnd,
       followup12.ARBEIDSSTATUS                                                        AS Arbeidstaus12mnd,
       followup12.DELVISSYKEMELDT_PROSENT                                              AS DelvisSykemeldtPros12mnd,
       followup12.UFORETRYGDET_PROSENT                                                 AS UforeTrygdPros12mnd,
       followup12.SOKT_UFOR                                                            AS Uforetrygd12mnd,
       followup12.SOKT_ERSTATNING                                                      AS Erstatning12mnd,
       followup12.SMERTE_VAS_HODET                                                     AS NRSsmerteHodet12mnd,
       followup12.SMERTE_VAS_NAKKE                                                     AS NRSsmerteNakke12mnd,
       followup12.SMERTE_VAS_ARMER                                                     AS NRSsmerteArm12mnd,
       followup12.SMERTE_LOKALISERING_SIDE                                             AS SmerteLokSide12mnd,
       followup12.SMERTE_LOKALISERING_OVEREKS                                          AS SmerteLokOverEks12mnd,
       followup12.SMERTE_TILIGERE_SKULDER_PLAGER                                       AS TidlSkulderPlager12mnd,
       followup12.SMERTESTILLENDE                                                      AS Smertestill12mnd,
       followup12.SMERTESTILLENDE_BRUK                                                 AS SmertestillBruk12mnd,
       followup12.NDI_SMERTE                                                           AS NDIsmerte12mnd,
       followup12.NDI_PERSONLIG_STELL                                                  AS NDIpersStell12mnd,
       followup12.NDI_LOFTING                                                          AS NDIlofting12mnd,
       followup12.NDI_LESING                                                           AS NDIlesing12mnd,
       followup12.NDI_HODEPINE                                                         AS NDIhodepineE12mnd,
       followup12.NDI_KONSENTRASJON                                                    AS NDIkonsentrasjon12mnd,
       followup12.NDI_ARBEID                                                           AS NDIarbeid12mnd,
       followup12.NDI_BILKJORING                                                       AS NDIbilkjoring12mnd,
       followup12.NDI_SOVN                                                             AS NDIsovn12mnd,
       followup12.NDI_FRITID                                                           AS NDIfritid12mnd,
       followup12.NDI_SCORE                                                            AS NDIscore12mnd,
       followup12.EMS_GANGFUNKSJON                                                     AS EMSgangeFunk12mnd,
       followup12.EMS_HANDFUNKSJON                                                     AS EMShandFunk12mnd,
       followup12.EMS_KOORDINASJON                                                     AS EMSkoordinasjon12mnd,
       followup12.EMS_BLARE_TARM                                                       AS EMSblareTarm12mnd,
       followup12.EMS_NUMMENHET                                                        AS EMSnummenhet12mnd,
       followup12.EMS_SCORE                                                            AS EMSscore12mnd,
       CASE
           WHEN followup12.EQ5D_GANGE IS NOT NULL THEN 3
           WHEN followup12.EQ5D_5L_GANGE IS NOT NULL THEN 5
           ELSE 0
           END                                                                         AS EqType12mnd,
       IFNULL(followup12.EQ5D_5L_GANGE, followup12.EQ5D_GANGE)                         AS EqGange12mnd,
       IFNULL(followup12.EQ5D_5L_PERSONLIG_STELL, followup12.EQ5D_PERSONLIG_STELL)     AS EqPersStell12mnd,
       IFNULL(followup12.EQ5D_5L_VANLIGE_GJOREMAL, followup12.EQ5D_VANLIGE_GJOREMAL)   AS EqVanlGjMaal12mnd,
       IFNULL(followup12.EQ5D_5L_SMERTE_UBEHAG, followup12.EQ5D_SMERTE_UBEHAG)         AS EqSmerte12mnd,
       IFNULL(followup12.EQ5D_5L_ANGST_DEPRESJON, followup12.EQ5D_ANGST_DEPRESJON)     AS EqAngst12mnd,
       IFNULL(followup12.EQ5D_5L_SCORE, followup12.EQ5D_SCORE)                         AS Eq5DScore12mnd,
       followup12.HELSETILSTAND_SCALE                                                  AS Helsetilst12mnd,
       followup12.STATUS                                                               AS OppFolgStatus12mnd,
       followup12.TSUPDATED                                                            AS OppFolgOppdatert12mnd,   -- Not specified in NAKKE-203
       followup12.UPDATEDBY                                                            AS OppFolgOppdatertAv12mnd, -- Not specified in NAKKE-203
       followup12.TSCREATED                                                            AS OppFolgLaget12mnd,       -- Not specified in NAKKE-203
       followup12.CREATEDBY                                                            AS OppFolgLagetAv12mnd,     -- Not specified in NAKKE-203
       followup12.FIRST_TIME_CLOSED_BY                                                 AS ForstLukketAV12mnd,
       followup12.FIRST_TIME_CLOSED                                                    AS ForstLukket12mnd,

--
-- Derived variables.
--

-- NAKKE-190
       CASE
           WHEN (FUNN_SKIVEPROLAPS = 1 AND OPERASJONSMETODE_TILGANG_FREMRE = 1 AND OPERASJONSMETODE_DISKEKTOMI = 1 AND
                 (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL)) THEN 1
           WHEN (FUNN_SKIVEPROLAPS = 0 AND (FUNN_CERVICAL_SPINALSTENOSE = 1 OR FUNN_ROTKANALSTENOSE = 1) AND
                 OPERASJONSMETODE_TILGANG_FREMRE = 1 AND OPERASJONSMETODE_DISKEKTOMI = 1 AND
                 (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL)) THEN 2
           WHEN (OPERASJONSMETODE_KORPEKTOMI = 1 AND (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL)) THEN 3
           WHEN (OPERASJONSMETODE_TILGANG_BAKRE = 1 AND (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL) AND
                 OPERASJONSMETODE_ANNEN_BAKRE_DEKOMPRESJON = 1) THEN 4
           WHEN (OPERASJONSMETODE_TILGANG_BAKRE = 1 AND (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL) AND
                 OPERASJONSMETODE_ANNEN_BAKRE_DEKOMPRESJON = 3) THEN 5
           WHEN (OPERASJONSMETODE_TILGANG_BAKRE = 1 AND (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL) AND
                 OPERASJONSMETODE_ANNEN_BAKRE_DEKOMPRESJON = 4) THEN 6
           WHEN (OPERASJONSMETODE_TILGANG_BAKRE = 1 AND (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL) AND
                 (OPERASJONSMETODE_FORAMENOTOMI_BAKRE_UNILATERAL = 1 OR
                  OPERASJONSMETODE_FORAMENOTOMI_BAKRE_BILATERAL = 1 OR
                  OPERASJONSMETODE_ANNEN_BAKRE_DEKOMPRESJON = 0 OR OPERASJONSMETODE_ANNEN_BAKRE_DEKOMPRESJON = NULL))
               THEN 7
           WHEN (BAKRE_FUSJON = 1) THEN 8
           WHEN (OPERASJONSMETODE_ANDRE_OPERASJONSMETODER > 0 AND (BAKRE_FUSJON = 0 OR BAKRE_FUSJON = NULL)) THEN 9
           ELSE 10
           END                                                                         AS InngrepType,

-- NAKKE-191
       CASE
           WHEN (TIDLIGERE_OPERERT_ANNET = 1 AND TIDLIGERE_OPERERT_SAMME = 1) THEN 3
           WHEN (TIDLIGERE_OPERERT_SAMME = 1) THEN 1
           WHEN (TIDLIGERE_OPERERT_ANNET = 1) THEN 2
           WHEN (TIDLIGERE_OPERERT_NEI) = 1 THEN 4
           ELSE 9
           END                                                                         AS TidlOpr,

-- NAKKE-192
       (SIDE_NIVAA_C7_TH1 +
        SIDE_NIVAA_C6_C7 +
        SIDE_NIVAA_C5_C6 +
        SIDE_NIVAA_C4_C5 +
        SIDE_NIVAA_C3_C4 +
        SIDE_NIVAA_C2_C3 +
        SIDE_NIVAA_C1_C2 +
        SIDE_NIVAA_C0_C1)
                                                                                       AS AntallNivaaOpr,

-- NAKKE-193
       CASE
           WHEN (PEROPERATIVE_KOMPLIKASJONER_DURARIFT +
                 PEROPERATIVE_KOMPLIKASJONER_NERVEROTSKADE +
                 PEROPERATIVE_KOMPLIKASJONER_OP_FEIL_NIVAA +
                 PEROPERATIVE_KOMPLIKASJONER_FEILPLASSERING_IMPLANTAT +
                 PEROPERATIVE_KOMPLIKASJONER_BLOEDNING +
                 PEROPERATIVE_KOMPLIKASJONER_RESPIRATORISKE +
                 PEROPERATIVE_KOMPLIKASJONER_ANAFYLAKSI +
                 PEROPERATIVE_KOMPLIKASJONER_MEDULLASKADE +
                 PEROPERATIVE_KOMPLIKASJONER_OESOFAGUS +
                 PEROPERATIVE_KOMPLIKASJONER_SKADE_STORE_BLODKAR +
                 PEROPERATIVE_KOMPLIKASJONER_KARDIOVASKULAERE +
                 PEROPERATIVE_KOMPLIKASJONER_ANNEN_NERVESKADE +
                 PEROPERATIVE_KOMPLIKASJONER_ANNET) > 0 THEN 1
           ELSE 0
           END                                                                         AS PerOpEnhverKompl,

-- NAKKE-194
       CASE
           WHEN (followup3.KOMPLIKASJONER_URINVEISINFEKSJON +
                 followup3.KOMPLIKASJONER_LUNGEBETENNELSE +
                 followup3.KOMPLIKASJONER_BLODPROPP_BEN +
                 followup3.KOMPLIKASJONER_BLODPROPP_LUNGE +
                 followup3.KOMPLIKASJONER_INFEKSJON_OVERFLADISK +
                 followup3.KOMPLIKASJONER_INFEKSJON_DYP +
                 followup3.KOMPLIKASJONER_KRAFTSVIKT +
                 followup3.KOMPLIKASJONER_UBEHAG_SVELGING +
                 followup3.KOMPLIKASJONER_STEMME) > 0 THEN 1
           ELSE 0
           END                                                                         AS EnhverKompl3mnd,

-- NAKKE-195
       CASE
           WHEN (followup12.KOMPLIKASJONER_URINVEISINFEKSJON +
                 followup12.KOMPLIKASJONER_LUNGEBETENNELSE +
                 followup12.KOMPLIKASJONER_BLODPROPP_BEN +
                 followup12.KOMPLIKASJONER_BLODPROPP_LUNGE +
                 followup12.KOMPLIKASJONER_INFEKSJON_OVERFLADISK +
                 followup12.KOMPLIKASJONER_INFEKSJON_DYP +
                 followup12.KOMPLIKASJONER_KRAFTSVIKT +
                 followup12.KOMPLIKASJONER_UBEHAG_SVELGING +
                 followup12.KOMPLIKASJONER_STEMME) > 0 THEN 1
           ELSE 0
           END                                                                         AS EnhverKompl12mnd

FROM mce mce
         INNER JOIN patient patient ON mce.PATIENT_ID = patient.ID
         INNER JOIN surgeonform surgeonform ON mce.MCEID = surgeonform.MCEID AND surgeonform.STATUS = 1
         LEFT OUTER JOIN patientform patientform ON mce.MCEID = patientform.MCEID
         LEFT OUTER JOIN patientfollowup followup3 on mce.MCEID = followup3.MCEID and followup3.CONTROL_TYPE = 3
         LEFT OUTER JOIN patientfollowup followup12 on mce.MCEID = followup12.MCEID and followup12.CONTROL_TYPE = 12;
')

#                  WHERE OprDato >= \'', datoFra, '\' AND OprDato <= \'', datoTil, '\'')



  message("Before large query")

  RegData <- rapbase::loadRegData(registryName = registryName , query = queryAVN, dbType = "mysql")

  message("After large query")


# 13.03.2025: Kan ikke se at noen av variablene fra Forløps-view er i bruk...
#  'SELECT ForlopsID, Kommune, Kommunenr, Fylkenr, Avdod, AvdodDato, BasisRegStatus FROM forlopsoversikt'
#  RegDataForl <- rapbase::loadRegData(registryName = registryName , query = queryForl, dbType = "mysql")
# RegData <- merge(RegDataAVN, RegDataForl, by='ForlopsID', all.x = TRUE, all.y = FALSE, suffixes = '')

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
# AHS FJERNET FOR Å KJØRE VIDERE PÅ nhn
# AHS    RegData$OppFolg3mndGML <- RegData$OppFolgStatus3mnd
# AHS    RegData$OppFolgStatus3mnd <- 0
# AHS    RegData$OppFolgStatus3mnd[
# AHS      RegData$ForlopsID %in% ePROMadmTab$MCEID[intersect(ind3mnd, which(ePROMadmTab$STATUS==3))]] <- 1
# AHS    RegData$OppFolgStatus3mnd[intersect(which(RegData$OppFolg3mndGML ==1), indIkkeEprom3mnd)] <- 1
# AHS
# AHS    RegData$OppFolg12mndGML <- RegData$OppFolgStatus12mnd
# AHS    RegData$OppFolgStatus12mnd <- 0
# AHS    RegData$OppFolgStatus12mnd[
# AHS      RegData$ForlopsID %in% ePROMadmTab$MCEID[intersect(ind12mnd, which(ePROMadmTab$STATUS==3))]] <- 1
# AHS    RegData$OppFolgStatus12mnd[intersect(which(RegData$OppFolg12mndGML ==1), indIkkeEprom12mnd)] <- 1
  }

  return(RegData)
}



