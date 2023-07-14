#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#'
#' @inheritParams NakkeFigAndeler
#' @inheritParams NakkeUtvalgEnh
#' @param Ngrense Minste antall registreringer for at ei gruppe skal bli vist
#' @param valgtVar Variabelen det skal vises resultat for.
#'             Alder: Aldersfordeling
#'             AndreRelSykdommer: Andre sykdommer
#'             Antibiotika: Fått antibiotika
#'             Arbeidstaus12mnd: Mottar sykepenger, 12 mnd etter operasjon?
#'             Arbeidstaus3mnd: Mottar sykepenger, 3 mnd etter operasjon?
#'             ArbeidstausPreOp: Mottar sykepenger, preoperativt?
#'             ASAgrad: ASA-grad > II
#'             BMI: Pasienter med fedme
#'             EnhverKompl3mnd: Alle komplikasjoner
#'             ErstatningPreOp: Søkt/planlegger å søke erstatning
#'             FornoydBeh12mnd: Fornøyde pasienter, 12 mnd.
#'             FornoydBeh3mnd: Fornøyde pasienter, 3 mnd.
#'             Komplinfek: Pasientrapportert infeksjon (dyp og/eller overfladisk), 3 mnd.
#'             KomplinfekDyp3mnd: Pasientrapportert dyp infeksjon, 3 mnd.
#'             KomplinfekOverfl3mnd: Overfladisk infeksjon, 3 mnd.
#'             KomplStemme3mnd: Stemmevansker, 3 mnd.
#'             KomplStemme12mnd: Stemmevansker, 12 mnd.
#'             KomplSvelging3mnd: Svelgvansker, 3 mnd.
#'             KomplSvelging12mnd: Svelgvansker, 12 mnd.
#'             Misfor12mnd: Misfornøyde pasienter, 12 mnd.
#'             Misfor3mnd: Misfornøyde pasienter, 3 mnd.
#'             NDIendr12mnd: Minst 30% forbedring av NDI, 12 mnd.
#'             NRSsmerteArmEndr12mnd: Minst 30% forbedring av NSR-arm, 12 mnd.
#'             NytteOpr12mnd: Klart bedre, 12 mnd.
#'             NytteOpr3mnd: Klart bedre, 3 mnd.
#'             Verre12mnd: Klart verre, 12 mnd.
#'             Verre3mnd. Klart verre, 3 mnd.
#'             OprIndikMyelopati: Operasjonsårsak, Myelopati
#'             Roker: Røykere
#'             Saardren: Andel som får sårdren
#'             SmertestillPreOp: Bruker smertestillende, preop.
#'             SymptVarighetNakkeHode: Varighet av hode-/nakkesmerter over 1 år
#'             SymptVarighetSmerterUker: Varighet av smerter minst 1 år
#'             UforetrygdPreOp: Søkt eller planlegger å søke uføretrygd?
#'             Utdanning: Andel høyskole-/universitetsutdannede
#'
#' @return Figur med...
#'
#' @export

NakkeFigAndelerGrVarAar <- function(RegData, valgtVar, datoFra='2012-01-01', datoTil='3000-12-31', enhetsUtvalg=0,
                            minald=0, maxald=130, erMann='', myelopati=99, fremBak=0, Ngrense=10,
                            grVar='ShNavn', ktr=0, aar=0, tidlAar=0, tidsenhet='aar',
                            hentData=0, preprosess=0, tittel=1, outfile='') {

     if (hentData == 1) {
          RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
     }

     # Preprosessere data
     if (preprosess==1){
          RegData <- NakkePreprosess(RegData=RegData)
     }

    '%i%' <- intersect
     #----------- Figurparametre ------------------------------
     cexShNavn <- 1 #0.85

     RegData[ ,grVar] <- factor(RegData[ ,grVar])

	 #Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav

#Svelgvansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav


     RegData$Variabel <- 0
     sortAvtagende <- TRUE

     if (valgtVar=='Komplinfek') {
       #3MndSkjema. Andel med KomplinfekDyp3mnd=1
       #Kode 0,1: Nei, Ja +tomme
       ind <- which(RegData$OppFolgStatus3mnd == 1) %i%
         union(which(RegData$KomplinfekDyp3mnd %in% 0:1), which(RegData$KomplinfekOverfl3mnd %in% 0:1))
       RegData <- RegData[ind, ]
       RegData$Variabel[union(which(RegData$KomplinfekDyp3mnd==1), which(RegData$KomplinfekOverfl3mnd==1))] <- 1
       VarTxt <- 'infeksjoner'
       xAkseTxt <- 'Andel infeksjoner (%)'
       TittelUt <- 'Pasientrapportert dyp eller overfladisk infeksjon, 3 mnd.'
     }
     if (valgtVar=='KomplStemme3mnd') {
          #3MndSkjema. Andel med KomplStemme3mnd=1
          #Kode 0,1: Nei, Ja +tomme
          RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1) %i%
                                which(RegData$KomplStemme3mnd %in% 0:1) %i%
                               which(RegData$OprMetodeTilgangFremre==1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          xAkseTxt <- 'Andel stemmevansker (%)'
          TittelUt <- 'Stemmevansker, 3 mnd.'
     }
    if (valgtVar=='KomplSvelging3mnd') {
          #3MndSkjema. Andel med KomplSvelging3mnd=1
          #Kode 0,1: Nei, Ja +tomme
       ind <-
          RegData <- RegData[(which(RegData$OppFolgStatus3mnd == 1) %i%
                                       which(RegData$KomplSvelging3mnd %in% 0:1) %i%
                                       which(RegData$OprMetodeTilgangFremre==1)), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          xAkseTxt <- 'Andel med svelgproblemer (%)'
          TittelUt <- 'Svelgvansker, 3 mnd.'
     }

     #Gjør utvalg
     if (tidlAar[1] != 0 ) { #tidlAar - år det skal sammenliknes med
       AarTxt <- ifelse(length(aar)>1, paste0(min(aar),'-', max(aar)), as.character(aar))
       RegData[,grVar] <- as.character(RegData[,grVar])
       RegData[,grVar] <- factor(RegData[,grVar])
     }


     #Gjør utvalg
     NakkeUtvalg <- NakkeUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                                   erMann=erMann, myelopati=myelopati, fremBak=fremBak, aar=as.numeric(c(tidlAar, aar)))

     RegData <- NakkeUtvalg$RegData
     utvalgTxt <- NakkeUtvalg$utvalgTxt


#--------------------------------Nytt-----------------------------------------------
smltxt <- NakkeUtvalg$smltxt
      medSml <- NakkeUtvalg$medSml
      hovedgrTxt <- NakkeUtvalg$hovedgrTxt
      utvalgTxt <- NakkeUtvalg$utvalgTxt
      ind <- NakkeUtvalg$ind
      RegData <- NakkeUtvalg$RegData


      RegData <- RegData[which(!is.na(RegData[ ,grVar])), ]
      names(RegData)[which(names(RegData) == grVar)] <- 'grVar'
      N <- dim(RegData)[1] #table(RegData$Aar)      #Antall per år

      #----------------------------------------------------------------------------------------------
      if (tidlAar[1] != 0) { #Sammenligne med resultater for tidligere år.
            RegData$grNaa <- 0 #Har fjernet år som ikke skal være med
            RegData$grNaa[which(RegData$Aar %in% aar)] <- 1
            katVariable <- c('grNaa', 'grVar')
            Nvar <- tapply(RegData$Variabel, RegData[ ,katVariable], sum, na.rm=T) #Variabel er en 0/1-variabel.
            if(N > 0) {Ngr <- table(RegData[ ,katVariable])}	else {Ngr <- 0}

			      AndelerGr <- round(100*Nvar/Ngr,2)
            indGrUt <- 0
            GrNavn <- names(Ngr['1', ]) #names(Ngr[AarTxt, ])

			if (sum(which(Ngr['1', ] < Ngrense))>0) {
                  #Må ta bort punkt/søyler for de som har for få registreringer for det aktuelle året.
                  indGrUt2 <- which(Ngr[2,] < Ngrense)#as.numeric() #"Hoved"år

                  pVerdier <- PverdiAndelsDiff(n=t(Nvar[ ,-indGrUt2]),
                                               N=t(Ngr[ ,-indGrUt2]), justMetode='fdr')
                  signDiffInd <- which(pVerdier < 0.05)
                  Ngrtxt <- paste0(' (',Ngr['1', -indGrUt2],')') #Ngr['1', ]	#Ikke sjekket at tidl. år <Ngrense tas ut.
                  Ngrtxt[signDiffInd] <- paste0(Ngrtxt[signDiffInd],'*')
                  signTxt <- ifelse(length(signDiffInd)==0, 'Ingen av avdelingene har signifikant endring',
                                    '* markerer at endringa er signifikant')

                  GrNavn <- GrNavn[-indGrUt2] #c(paste0(length(indGrUt2), ' avd. med N<',Ngrense), GrNavn[-indGrUt2])
                  #Ngrtxt <- c(paste0(' (',sum(Ngr[2,indGrUt2]),')'), Ngrtxt) #c(paste0(' (',sum(Ngr[2,indGrUt2]),')'), Ngrtxt)
                  indGrUt <- indGrUt2

                  #AndelGrUt <- rowSums(Nvar[ ,indGrUt2], na.rm = T)/rowSums(Ngr[ ,indGrUt2])*100
                  AndelerGr <- AndelerGr[,-indGrUt2] #cbind(AndelGrUt, AndelerGr[,-indGrUt2])

                  sortInd <- order(as.numeric(AndelerGr['1',]), decreasing=sortAvtagende)
                  AndelerSisteSort <- AndelerGr['1',sortInd]
                  indGrUt1sort <- as.numeric(which(c(Ngrense, Ngr[1,-indGrUt2])[sortInd] < Ngrense))
            }
            AndelerGrSort <- AndelerGr[ ,sortInd]
            GrNavnSort <- paste0(GrNavn[sortInd], Ngrtxt[sortInd])  #paste0(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd])
            andeltxt <- paste0(sprintf('%.1f',AndelerGrSort['1',]), '%')

            #--------------------------------------------------------------
      } else {	#Hvis vi skal ha resultater for perioden totalt

            N <- dim(RegData)[1]
            Nvar <- tapply(RegData$Variabel, RegData[ ,'grVar'], sum, na.rm=T)
            if(N > 0) {Ngr <- table(RegData[ ,'grVar'])}	else {Ngr <- 0}
            AntGrNgr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
            AndelerGr <- round(100*Nvar/Ngr,2)

            indGrUt <- as.numeric(which(Ngr < Ngrense))
            if (length(indGrUt)==0) { indGrUt <- 0}
            AndelerGr[indGrUt] <- NA #dummy0
            sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE)
            Ngrtxt <- Ngr #paste('N=', as.character(Ngr), sep='')	#

            AndelerGrSort <- AndelerGr[sortInd]
            AndelerSisteSort <- AndelerGrSort
            AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
            GrNavnSort <-  paste0(names(AndelerSisteSort), ' (', Ngrtxt[sortInd],')')

            andeltxt <- paste0(sprintf('%.1f',AndelerSisteSort), '%') 	#round(as.numeric(AndelerSiste),1)
            if (length(indGrUt)>0) {andeltxt[(AntGrNgr+1):(AntGrNgr+length(indGrUt))] <- ''}
      }

#      if (tittel==0) {Tittel<-''} else {Tittel <- NakkeVarSpes$tittel}
      Tittel <- TittelUt

      #-----------Figur---------------------------------------
      # Lager ikke figur hvis ALLE N er mindre enn grensa eller hvis ugyldig parameterkombinasjon.
      if 	( max(Ngr) < Ngrense) {
            FigTypUt <- rapFigurer::figtype(outfile)
            farger <- FigTypUt$farger
            plot.new()
            if (dim(RegData)[1]>0) {
                  tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
            } else {tekst <- 'Ingen registrerte data for dette utvalget'}
            title(main=Tittel)
            text(0.5, 0.6, tekst, cex=1.2)
            legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
            if ( outfile != '') {dev.off()}

      } else {

            #--------------------------FIGUR---------------------------------------------------
            #Innparametre: ...
            #----------- Figurparametre ------------------------------
            cexShNavn <- 1 #0.85
            hoyde <- ifelse(grVar=='BoHF', 3*600, 3*800)
            FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=NakkeUtvalg$fargepalett)
            farger <- FigTypUt$farger
            soyleFarger <- farger[4] #rep(farger[3], AntGrNgr)
            prikkFarge <- farger[3]
            #Hvis Norge egen søyle: soyleFarger[which(names(AndelerSisteSort)=='Norge')] <- farger[4]
            fargerMaalNiva <- c('#ddffcc', '#ffffcc', '#fff0e6') #Grønn, gul, rød
            #Tilpasse marger for å kunne skrive utvalgsteksten
            NutvTxt <- length(utvalgTxt)
            vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.85)
            #NB: strwidth oppfører seg ulikt avh. av device...
            par('fig'=c(vmarg, ifelse(tidlAar[1]!=0,1,1), 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

            xmax <- min(max(AndelerGrSort, na.rm = T),100)*1.15
            #xAkseTxt <- ifelse (AKjust==1, paste0(xAkseTxt, ', justert for alder og kjønn'), xAkseTxt)

            maalGrenser <- c(2,4,xmax)
            pos <- barplot(as.numeric(AndelerSisteSort), horiz=T, border=NA, col=soyleFarger, #add=TRUE , #plot=T,
                           xlim=c(0,xmax), ylim=c(0.05, 1.32)*length(GrNavnSort), font.main=1, #xlab=xAkseTxt,
                           las=1, cex.names=cexShNavn*0.9)
            #Legge på målnivå
            #rect(xleft=c(0, maalGrenser[1:2]), ybottom=0, xright=maalGrenser, ytop=max(pos)+0.4, #pos[AntGrNgr+1],
            #    col = fargerMaalNiva, border = NA) #add = TRUE,
            ybunn <- 0.1
            ytopp <- max(pos)+ 0.4 #pos[2]-pos[1] #pos[AntGrNgr]+ 0.4	#
            if (tidlAar[1] != 0) {
                  #indMed <- 1:AntGrNgr
                  AartxtTidl <- ifelse(length(tidlAar)>1, paste0(min(tidlAar),'-', max(tidlAar)), as.character(tidlAar))
                  Naar <- rowSums(Ngr, na.rm=T)
                  ResAar <- 100*rowSums(Nvar, na.rm=T)/Naar
                  lines(x=rep(ResAar[2], 2), y=c(ybunn, ytopp), col=farger[1], lwd=2)
                  barplot(as.numeric(AndelerSisteSort), horiz=T, border=NA, col=soyleFarger, add=T, #plot=T,
                          xlim=c(0,xmax), ylim=c(0.05, 1.27)*length(GrNavnSort), font.main=1, #xlab=xAkseTxt,
                          las=1, cex.names=cexShNavn*0.9)
                  if (length(indGrUt1sort)>0) {ind <- -indGrUt1sort} else {ind <- 1:length(pos)}
                  points(y=pos[ind]+0.1, x=AndelerGrSort['0', ind], cex=1, pch=16, col=prikkFarge) #pch='|', y=pos[indMed]+0.1, x=AndelerGrSort[AartxtTidl, indMed]
                  legend('top', inset=c(0.1,0), xjust=1, cex=0.85, bty='o', bg='white', box.col='white',
                         lwd=c(NA,NA,2), pch=c(16,15,NA), pt.cex=c(1, 1.9, 1),  #pch=c(124,15,NA)
                         col=c(prikkFarge,soyleFarger,farger[1]),
                         legend=c(#paste0(Aar1txt, ' (', sprintf('%.1f', ResAar[1]), '%, ', 'N=', Naar[1],')'),
                               paste0(AartxtTidl, ' (', sprintf('%.1f', ResAar[1]), '%, ', 'N=', Naar[1],')'),
                               paste0(AarTxt, ' (', sprintf('%.1f', ResAar[2]), '%, ', 'N=', Naar[2],')'),
                               paste0('Hele landet, ',AarTxt))
                  )

                  mtext(signTxt, side=1, las=1, cex=cexShNavn, adj=0, line=3, col='#FF7260')
                  mtext(xAkseTxt, side=1, las=1, cex=cexShNavn, adj=0.5, line=2)
                  overPos <- max(pos)+0.4*log(max(pos))
                  mtext(at=overPos, paste0('(N, ', AarTxt, ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)
            } else {
                  legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
                         legend=paste0(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
                         bty='o', bg='white', box.col='white')
                  mtext(at=max(pos)+0.5*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)
                  lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[1], lwd=2)
            }
            mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
            title(Tittel, line=1, font.main=1, cex.main=1.3)

            text(x=xmax*0.01, y=pos+0.1, andeltxt, #x=AndelerGrSort+xmax*0.01
                 las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus

            #Tekst som angir hvilket utvalg som er gjort
            mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(2.2+0.8*((NutvTxt-1):0)))
            #            mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


            par('fig'=c(0, 1, 0, 1))
            if ( outfile != '') {dev.off()}
            #----------------------------------------------------------------------------------
      }
}
