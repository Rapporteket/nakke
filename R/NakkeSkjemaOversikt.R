SkjemaOversikt <- function() {


  #Skjemanavn,	SkjemaStatus,	ForlopsID,	HovedDato,	Sykehusnavn,	AvdRESH,	SkjemaRekkeflg
 # FROM skjemaoversikt
  surgeonform
  patientform
  patientfollowup
  centre


  indStatus <- grep(x=names(dataNy), pattern = 'status', ignore.case = T)
  names(dataNy)[indStatus]

  "LegeskjemaStatus"      "PasientSkjemaStatus"
  "STATUS_CONTROL"        "STATUS"                "STATUS_CONTROL_oppf12" "STATUS_oppf12"
  StatusPasSkjema
  StatusLegeSkjema
  StatusUtfyll3mnd
  StatusUtfyll12mnd

  qLege <- 'SELECT * from surgeonform'

  Lege <- hentDataTabell(qVar = )

  LegeskjemaStatus
  PasientSkjemaStatus
  STATUS_oppf12

    SELECT REG_DESCRIPTION FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFORM'
  t.LANGUAGEID='no' AS Skjemanavn,
  CAST(patientform.STATUS AS CHAR(3)) AS SkjemaStatus,
  CAST(patientform.MCEID AS CHAR(10)) AS ForlopsID,
  (SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,

  UNION

  CAST(skjema.STATUS AS CHAR(3)) AS SkjemaStatus,
  CAST(skjema.MCEID AS CHAR(10)) AS ForlopsID,
  (SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,
  c.ID AS AvdRESH,
  CAST((SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'SURGEONFORM'
  ) AS CHAR(2)) AS SkjemaRekkeflg
  FROM
  surgeonform skjema,
  centre c
  WHERE skjema.CENTREID = c.ID

  UNION

  SELECT
  '1B:Oppfølging 3 mnd' AS Skjemanavn,
  CAST(patientfollowup.STATUS AS CHAR(3)) AS SkjemaStatus,
  CAST(patientfollowup.MCEID AS CHAR(10)) AS ForlopsID,
  (SELECT surgeonform.OPERASJONSDATO FROM surgeonform WHERE surgeonform.MCEID = skjema.MCEID) AS HovedDato,
  c.ID AS AvdRESH,
  SELECT ORDERNO FROM reg_questionnaires WHERE REGISTRATION_TYPE = 'PATIENTFOLLOWUP'
  ... AS SkjemaRekkeflg
  FROM
  patientfollowup,
  centre
  WHERE patientfollowup.CENTREID = centre.ID AND patientfollowup.CONTROL_TYPE=3

  UNION

  SELECT
  '1B:Oppfølging 12 mnd' AS Skjemanavn,
  patientfollowup.STATUS AS CHAR(3)) AS SkjemaStatus,
  (patientfollowup.MCEID AS CHAR(10)) AS ForlopsID,

SkjemaRekkeflg: SELECT ORDERNO, REGISTRATION_TYPE, TABLENAME FROM reg_questionnaires
  WHERE REGISTRATION_TYPE = 'PATIENTFOLLOWUP12'
  ;









}
