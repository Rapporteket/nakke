% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NakkeFigGjsnTid.R
\name{NakkeFigGjsnTid}
\alias{NakkeFigGjsnTid}
\title{Tidsutvikling av gjennomsnitt/median for en gitt variabel}
\usage{
NakkeFigGjsnTid(
  RegData,
  outfile = "",
  valgtVar = "Alder",
  erMann = "",
  minald = 0,
  maxald = 130,
  datoFra = "2007-01-01",
  datoTil = "3000-01-01",
  myelopati = 99,
  fremBak = 0,
  tidsenhet = "Aar",
  inngrep = 99,
  konfInt = 1,
  valgtMaal = "",
  enhetsUtvalg = 0,
  hentData = 0,
  preprosess = 0,
  reshID = 0,
  ...
)
}
\arguments{
\item{RegData}{Dataramme med alle nødvendige variable fra registeret}

\item{outfile}{Navn på fil figuren skrives ned til}

\item{valgtVar}{- Variabelen det skal vises resultat for.
Alder: alder (år)
EMSendr12mnd: Forbedring av EMS hos myelopati-pasienter, 12 mnd.
EMSendr3mnd: Forbedring av EMS hos myelopati-pasienter, 3 mnd.
EQ5Dendr12mnd: Forbedring av EQ5D, 12 mnd.
EQ5Dendr3mnd: Forbedring av EQ5D, 3 mnd.
Eq5DScorePreOp: EQ5D, før operasjon
KnivtidTotalMin: total knivtid
LiggeDognPostop: liggetid etter operasjon
LiggeDognTotalt: antall liggedøgn, totalt
NDIscorePreOp: NDI før operasjon
NDIendr3mnd: Forbedring av NDI, 3 mnd. etter operasjon
NDIendr12mnd: Forbedring av NDI, 12 mnd. etter operasjon}

\item{erMann}{Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge}

\item{minald}{Alder, fra og med}

\item{maxald}{Alder, til og med}

\item{datoFra}{Operasjonsdato, fra og med. Standard: '2012-01-01'}

\item{datoTil}{Operasjonsdato, til og med. Standard: '3000-01-01' (siste registreringsdato)}

\item{myelopati}{Operert for myelopati. 0-nei, 1-ja, 99-alle (standard)}

\item{fremBak}{Fremre eller bakre tilgang. 0-ikke valgt, 1-fremre, 2-bakre}

\item{inngrep}{Inngrepstype     0-'Ikke klassifiserbar operasjon', 1-'Fremre diketomi for prolaps',
2-'Bakre dekompresjon', 3-'Fremre dekompresjon sp st.uten prolaps', 4-'Bakre fusjon',
5-'Korporektomi', 6-'Andre inngrep', 99-alle (standard)}

\item{konfInt}{konfidensintervall 1: ja (standard), 0: nei}

\item{valgtMaal}{Sentralmål: 'Med' gir median, alt annet gir gjennomsnitt}

\item{enhetsUtvalg}{Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet}

\item{hentData}{Angir om funksjonen skal kjøre spørring for å hente data eller ikke.
0: ikke kjør (standard)
1: kjør}

\item{preprosess}{Skal data preprosesseres, dvs. gjøre standard omregning av variable og beregne nye.
TRUE (standard) / FALSE}

\item{reshID}{Avdelingsid (reshID) for egen avdeling,}
}
\value{
Figur med...
}
\description{
Gjennomsnitt/median per år med konfidensintervall for valgt variabel.
Hvis man har valgt å sammenlikne, vises  konfidensintervall for resten av landet (evt. annen
sammenlikningsgruppe) i bakgrunnen.
}
\details{
Smerte: Skalaen går fra 0 til 10, dvs. at differansen ligger mellom -10 og 10..
EQ5D: Skala fra -0.594 tl 1, jo høyere jo bedre.
Oswestry: Skala fra 0 til 100, hvor lavest er friskest
}
