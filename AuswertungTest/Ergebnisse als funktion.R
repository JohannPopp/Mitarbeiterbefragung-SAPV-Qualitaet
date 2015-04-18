# Mitarbeiter/innenbefragung zur SAPV-Qualität

# Diese Syntax erstellt eine pdf-Datei mit den Ergebnissen der Befragung.

# Johann Popp
# Erstellt:             2015-03-30
# Letzte Überarbeitung: 2015-04-01
#############################################




###### Eingaben #########

### Wie heißt das SAPV-Team, in dem die Befragung durchgeführt wurde?
SAPVteam <- "Testdaten"
### Wer hat die Daten ausgewertet?
Auswerter <- character()
### Datendatei
Eingabe <- "Mitarbeiterinnenbefragung_Dataform_1_15.csv"

Auswertung <- function(daten, team = SAPVteam, auswerter = Auswerter, art = "orig", datEingabe = FALSE, erklAusg = FALSE, erklSynt = FALSE, hist = TRUE, box = TRUE, grid = TRUE, kA = TRUE){
  
  ### Daten einlesen (die in EpiData mit ";" als Trennzeichen als csv exportiert wurden)
  if(datEingabe == FALSE){
    Eingabe <- (file.choose())
    dat <- read.csv2(Eingabe) 
    dat[dat == 99] <- NA
    Pf <- strsplit(Eingabe, "/")[[1]]
    Pfad <- paste(Pf[-length(Pf)], "/", sep = "",  collapse = "")
  }
  
  # Falls die Daten nicht aus einer csv-Datei eingelesen werden sollen, sondern innerhalb von R in eine Datenmatrix eingegeben werden sollen, kann das Argument "datEingabe = TRUE" in die Funktion Ausgwertung eingetragen werden.
  # Die Bewertungen werden numerisch kodiert: 1 = sehr zufriedenstellend; 5 = sehr verbesserungswürdig
  if(datEingabe == TRUE){
    dat <- data.frame(ID = numeric(), QGesamt = numeric(), SympGesamt = numeric(), SympErfass = numeric(), SympDoku = numeric(), SympBehand = numeric(), SympVorbeug = numeric(), SympSchulung = numeric(), SympSchmerz = numeric(), SympLuft = numeric(), SympAngst = numeric(), SympNausea = numeric(), SympObsti = numeric(), SympFatigue = numeric(), SympKogni = numeric(), SympWunde = numeric(), SympAndere = character(), SympAndWert = numeric(), SympAnmerk = character(), SicherGesamt = numeric(), SicherKriseVerm  = numeric(), SicherKriseAuff = numeric(), SicherMedi = numeric(), SicherInfekt = numeric(), SicherVerletz = numeric(), SicherAndere = character(), SicherAndWert = numeric(), SicherAnmerk = character(), AllGesamt = numeric(), AllKrankheit = numeric(), AllPflege = numeric(), AllSozial = numeric(), AllOrga = numeric(), AllSpirit = numeric(), AllKultur = numeric(), AllAnmerk = character(), Freitext = character())
    fix(dat)
    write.csv2(dat, file.choose(), row.names = FALSE)
    Pfad <- ""
  }

  
  ### Daten modifizieren
  if(art == "abwGesamt"){
    dat[,c(2:16, 18, 20:25, 27, 29:35)] <-  t(apply(dat[,c(2:16, 18, 20:25, 27, 29:35)], 1, function(x) x - x[1]))
  }
  
  if(art == "abwMedian"){
    dat[,c(2:16, 18, 20:25, 27, 29:35)] <-  t(apply(dat[,c(2:16, 18, 20:25, 27, 29:35)], 1, function(x) x - median(x, na.rm = TRUE)))
  }
  
  ### Text aus dem Fragebogen heraus kopieren
  txt <- read.csv2("Fragebogen.csv", colClasses = "character")[,1]
  
  
  ### Name in die Fußnoten einfügen.
  Aut <- paste(paste(auswerter, collapse = ", "), if(length(auswerter) >= 1){" & "},"J. Popp", sep = "")
  Fussn <- paste("Mitarbeiter/innenbefragung zur SAPV-Qualität im Team ", team, ". ", Aut, ": ", Sys.Date(),".", sep = "")
  
  
  ### Name der Ausgabedatei erstellen
  Ausgabe <- paste(Pfad, "Ergebnisse der Mitarbeiterbefragung - ", team, " ", Sys.Date(), ".pdf", sep = "")
  
  ### Einstellungen für die verschiedenen Darstellungsformen
  hoehe <- length(dat[,1])
  teiler <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5)
  laenge <- c(0.5, 6)
  if(art == "abwGesamt" | art == "abwMedian"){
    teiler <- seq(-4.5, 4.5)
    laenge <- c(-4.5, 5.5)
  }
  
  
  ######## Funktionen #############
  
  # Funktion zur Erstellung der Grafiken
  hoehe <- length(dat[,1])
  ergebnis <- function(x, farbe = "#a1d99b99", ...){ 
    h <- hist(x, breaks = teiler, xlim = laenge, ylim = c(0,hoehe),
              axes = FALSE, col = "grey", border = "black", main = "", ylab = "", xlab = "")
    segments(x0 = teiler, y0 = rep(0, 6), y1 = rep(hoehe, 6), col = "grey30", lty = 2)
    if(art == "abwGesamt" | art == "abwMedian"){
      lines(c(0, 0), c(0.01, hoehe), lwd = 3, col = "grey40")
    }
    if(sum(is.na(x)) > 0){
      text(laenge[2]*1.03, hoehe*0.1, paste("k.A. =", sum(is.na(x))), adj = c(1, NA))
    }
    boxplot(x, horizontal = TRUE, add = TRUE, axes = FALSE, 
            at = hoehe/2, boxwex = hoehe, 
            lwd = 1.5, col = farbe)
    points(mean(x, na.rm = TRUE), hoehe/2, col = "grey50", cex = 2, lwd = 1.5, pch = 1)
    text(laenge[2]*1.03, hoehe*0.5, paste("n =", sum(!is.na(x))), adj = c(1, NA))
  }
  
  # Funktion für die Wertelabels
  bezeich <- function(x){
    plot(10, xlim = c(0.5, 6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
    text(c(1, 2, 3, 4, 5), 0.1, c("sehr\nzufrieden-\nstellend", 
                                  "zufrieden-\nstellend", "ausreichend", 
                                  "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig"), 
         adj = c(NA, 0))

    lines(c(0, 10), c(0, 0))
  }
  
  if(art == "abwGesamt" | art == "abwMedian"){
    bezeich <- function(x, ...){
      plot(10, xlim = laenge, ylim = c(0,1), axes = FALSE)
      text(seq(-4,4), 0.1, seq(-4,4), adj = c(NA, 0))
      text(c(-4, 4), 0.4, c("besser", "schlechter"))
      if(art == "abwGesamt"){
        text(0, 0.6, "Abweichung von der persönlichen Gesamtbewertung")
      }
      if(art == "abwMedian"){
        text(0, 0.5, "Abweichung vom persönlichen Median")
      }      
      lines(laenge*2, c(0,0))
    }
  }
  
  ########## pdf-Dokument erstellen #########
  
  pdf(Ausgabe, paper = "a4r", width = 10.5, height = 7.5)
  par(mar = c(0,0,0,0))
  
  ############
  # Seite 1
  
  lmatr <- matrix(c( 1, 1,
                     2, 3,
                     4, 5,
                     6, 7,
                     8, 9,
                     10,11,
                     12,13,
                     14,15,
                     16,17,
                     18,19,
                     20,21,
                     22,22), ncol = 2, byrow = TRUE)
  
  layout(lmatr, widths=lcm(c(14.5,11.5)), heights = lcm(c(2, rep(1.7,5), rep(1.2, 5), 2.5)))
  
  # Zeile 1
  plot(10, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
  text(0.5, c(0.5), paste(strwrap(paste("Wie schätzt du die Qualität der Arbeit des SAPV-Teams", team, "ein?"), 60), collapse = "\n", sep = ""), adj = c(0.5,0.5), cex = 3, font = 2)
  
  # Zeile 2
  plot.new()
  text(0.04, 0.1, "Qualität insgesamt:", adj = c(0,0), cex = 2, font = 2)
  lines(c(0, 10), c(0, 0))
  
  bezeich()
  
  # Zeile 3
  plot.new()
  text(0, 0.5, paste(strwrap(paste("Wie schätzt du die Qualität der Arbeit des SAPV-Teams", team, "insgesamt ein?"), 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$QGesamt, farbe = "#2b8cbe99")
  
  # Zeile 4
  plot.new()
  text(0.04, 0.1, "Symptomkontrolle:", adj = c(0,0), cex = 2, font = 2)
  lines(c(0, 10), c(0, 0))
  
  bezeich()
  
  # Zeile 5
  plot.new()
  text(0, 0.5, paste(strwrap("Wie schätzt du die Qualität der Symptomkontrolle insgesamt ein?", 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympGesamt, farbe = "#2b8cbe99")
  
  # Zeile 6
  plot.new()
  text(0.04, 0.1, "Symptomkontrolle nach Teilprozessen:", font = 2, adj = c(0,0), cex = 1.5)
  lines(c(0, 10), c(0, 0))
  
  bezeich()
    
  # Zeile 7
  plot.new()
  text(0, 0.5, paste(strwrap(txt[8], 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympErfass)
  
  # Zeile 8
  plot.new()
  text(0, 0.5, paste(strwrap(txt[9], 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympDoku)
  
  # Zeile 9
  plot.new()
  text(0, 0.5, paste(strwrap(txt[10], 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympBehand)
  
  # Zeile 10
  plot.new()
  text(0, 0.5, paste(strwrap(txt[11], 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympVorbeug)
  
  # Zeile 11
  plot.new()
  text(0, 0.5, paste(strwrap(txt[12], 72), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympSchulung)
  
  # Copyright
  plot.new()
  lines(c(-0.02,3), c(0.25,0.25))
  text(1.04, 0.1, "Seite 1 von 5", adj = c(1,0))
  text(-0.02, 0.1, Fussn, adj = c(0,0))
  
  
  
  ##########
  # Seite 2
  lmatr2 <- matrix(c( 1, 1, 2, # 1
                      3, 3, 4, # 2
                      5, 5, 6, # 3
                      7, 7, 8, # 4
                      9, 9,10, # 5
                      11,11,12, # 6
                      13,13,14, # 7
                      15,15,16, # 8
                      17,17,18, # 9
                      19,20,20, #10
                      21,21,21, #11
                      22,22,22, #12
                      23,23,23 #13
  ), ncol = 3, byrow = TRUE)
  
  layout(lmatr2, widths=lcm(c(4.2,10.5,11.5)), heights = lcm(c(rep(1.2, 9), 2.5, 1.2, 4, 0.5)))
  
  # Zeile 1
  plot.new()
  text(0.04, 0.1, "Symptomkontrolle nach Symptomen:", font = 2, adj = c(0,0), cex = 1.5)
  lines(c(0, 10), c(0, 0))
  
  bezeich()
  
  # Zeile 2
  plot.new()
  text(0, 0.5, paste(strwrap(txt[16], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympSchmerz)
  
  # Zeile 3
  plot.new()
  text(0, 0.5, paste(strwrap(txt[17], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympLuft)
  
  # Zeile 4
  plot.new()
  text(0, 0.5, paste(strwrap(txt[18], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympAngst)
  
  # Zeile 5
  plot.new()
  text(0, 0.5, paste(strwrap(txt[19], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympNausea)
  
  # Zeile 6
  plot.new()
  text(0, 0.5, paste(strwrap(txt[20], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympObsti)
  
  # Zeile 7
  plot.new()
  text(0, 0.5, paste(strwrap(txt[21], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympFatigue)
  
  # Zeile 8
  plot.new()
  text(0, 0.5, paste(strwrap(txt[22], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympKogni)
  
  # Zeile 9
  plot.new()
  text(0, 0.5, paste(strwrap(txt[23], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SympWunde)
  
  # Zeile 10
  plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
  text(0.09, 2, "Andere Symptome:", adj = c(0,1), cex = 1.5)
  
  plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
  text(0, 2, 
       paste(strwrap(paste(dat$SympAndere[grep('[:alphanum:]', dat$SympAndere)], ': ', dat$SympAndWert[grep('[:alphanum:]', dat$SympAndere)], collapse = '  ---  ', sep = ''), 110), collapse = "\n", sep = ""), 
       adj = c(0,1), cex = 1.5)
  
  # Zeile 11
  plot.new()
  text(-0.02, 0.2, txt[25], adj = c(0,NA), cex = 1.5, font = 2)
  
  # Zeile 12
  plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
  text(-0.02, 5, 
       paste(strwrap(paste(dat$SympAnmerk[grep('[:alphanum:]', dat$SympAnmerk)], collapse = '  ---  '), 130), collapse = "\n", sep = ""),
       adj = c(0, 1), cex = 1.5)
  
  
  # Zeile 13
  plot.new()
  lines(c(-0.02,3), c(1,1))
  text(1.04, 0.1, "Seite 2 von 5", adj = c(1,0))
  text(-0.02, 0.1, Fussn, adj = c(0,0))
  
  
  #####################
  # Seite 3
  
  lmatr3 <- matrix(c( 1, 1, 2, # 1
                      3, 3, 4, # 2
                      0, 0, 0,
                      5, 5, 6, # 3
                      7, 7, 8, # 4
                      9, 9,10, # 5
                      11,11,12, # 6
                      13,13,14, # 7
                      15,16,16, # 8
                      17,17,17, # 9
                      18,18,18, #10
                      19,19,19  #11
  ), ncol = 3, byrow = TRUE)
  
  layout(lmatr3, widths=lcm(c(4.2,10.5,11.5)), heights = lcm(c(1.2, 1.7, 1, rep(1.2, 5), 2.5, 1.2, 4, 0.5)))
  
  # Zeile 1
  plot.new()
  text(0.04, 0.17, "Sicherheitsversprechen:", font = 2, adj = c(0,0), cex = 2)
  lines(c(0, 10), c(0, 0))
  
  bezeich()
  
  # Zeile 2
  plot.new()
  text(0, 0.5, paste(strwrap("In wie weit trägt die Versorgung insgesamt dazu bei, Patienten und Angehörigen die nötige Sicherheit zu geben, krisenhafte Krankheitssituationen im häuslichen Umfeld zu bewältigen?", 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SicherGesamt, farbe = "#2b8cbe99")
  
  # Zeile 3
  plot.new()
  text(0, 0.5, paste(strwrap(txt[36], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SicherKriseVerm)
  
  # Zeile 4
  plot.new()
  text(0, 0.5, paste(strwrap(txt[37], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SicherKriseAuff)
  
  # Zeile 5
  plot.new()
  text(0, 0.5, paste(strwrap(txt[38], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SicherMedi)
  
  # Zeile 6
  plot.new()
  text(0, 0.5, paste(strwrap(txt[39], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SicherInfekt)
  
  # Zeile 7
  plot.new()
  text(0, 0.5, paste(strwrap(txt[40], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$SicherVerletz)
  
  # Zeile 8
  plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
  text(0.09, 2, "Andere Gefahren:", adj = c(0,1), cex = 1.5)
  
  plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
  text(0, 2, 
       paste(strwrap(paste(dat$SicherAndere[grep('[:alphanum:]', dat$SicherAndere)], ': ', dat$SicherAndWert[grep('[:alphanum:]', dat$SicherAndere)], collapse = '  ---  ', sep = ''), 110), collapse = "\n", sep = ""), 
       adj = c(0,1), cex = 1.5)
  
  # Zeile 9
  plot.new()
  text(-0.02, 0.2, txt[42], adj = c(0,NA), cex = 1.5, font = 2)
  
  # Zeile 10
  plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
  text(-0.02, 5, 
       paste(strwrap(paste(dat$SicherAnmerk[grep('[:alphanum:]', dat$SicherAnmerk)], collapse = '  ---  '), 130), collapse = "\n", sep = ""),
       adj = c(0, 1), cex = 1.5)
  
  
  # Zeile 11
  plot.new()
  lines(c(-0.02,3), c(1,1))
  text(1.04, 0.1, "Seite 3 von 5", adj = c(1,0))
  text(-0.02, 0.1, Fussn, adj = c(0,0))
  
  
  
  #####################
  # Seite 4
  
  lmatr4 <- matrix(c( 1, 1, 2, # 1
                      3, 3, 4, # 2
                      0, 0, 0,
                      5, 5, 6, # 3
                      0, 0, 0,
                      7, 7, 8, # 4
                      0, 0, 0,
                      9, 9,10, # 5
                      0, 0, 0,
                      11,11,12, # 6
                      0, 0, 0,
                      13,13,14, # 7
                      0, 0, 0,
                      15,15,16, # 8
                      0, 0, 0,
                      17,17,17, # 9
                      18,18,18, #10
                      19,19,19  #11
  ), ncol = 3, byrow = TRUE)
  
  layout(lmatr4, widths=lcm(c(4.2,10.5,11.5)), heights = lcm(c(1.2, 1.7, 0.5, rep(c(1.5,0.2), 6), 1, 3.5, 0.5)))
  
  # Zeile 1
  plot.new()
  text(0.04, 0.17, "Alltagsrahmung:", font = 2, adj = c(0,0), cex = 2)
  lines(c(0, 10), c(0, 0))
  
  bezeich()
  
  # Zeile 2
  plot.new()
  text(0, 0.5, paste(strwrap(txt[50], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  ergebnis(dat$AllGesamt, farbe = "#2b8cbe99")
  
  # Zeile 3
  plot.new()
  text(0, 0.5, paste(strwrap(txt[51], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  par(mar = c(0.5,0,0.5,0))
  ergebnis(dat$AllKrankheit)
  
  # Zeile 4
  par(mar = c(0,0,0,0))
  plot.new()
  text(0, 0.5, paste(strwrap(txt[52], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  par(mar = c(0.5,0,0.5,0))
  ergebnis(dat$AllPflege)
  
  # Zeile 5
  par(mar = c(0,0,0,0))
  plot.new()
  text(0, 0.5, paste(strwrap(txt[53], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  par(mar = c(0.5,0,0.5,0))
  ergebnis(dat$AllSozial)
  
  # Zeile 6
  par(mar = c(0,0,0,0))
  plot.new()
  text(0, 0.5, paste(strwrap(txt[54], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  par(mar = c(0.5,0,0.5,0))
  ergebnis(dat$AllOrga)
  
  # Zeile 7
  par(mar = c(0,0,0,0))
  plot.new()
  text(0, 0.5, paste(strwrap(txt[55], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  par(mar = c(0.5,0,0.5,0))
  ergebnis(dat$AllSpirit)
  
  # Zeile 8
  par(mar = c(0,0,0,0))
  plot.new()
  text(0, 0.5, paste(strwrap(txt[56], 75), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.5)
  
  par(mar = c(0.5,0,0.5,0))
  ergebnis(dat$AllKultur)
  
  # Zeile 9
  par(mar = c(0,0,0,0))
  plot.new()
  text(-0.02, 0.2, txt[57], adj = c(0,NA), cex = 1.5, font = 2)
  
  # Zeile 10
  plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
  text(-0.02, 5, 
       paste(strwrap(paste(dat$AllAnmerk[grep('[:alphanum:]', dat$AllAnmerk)], collapse = '  ---  '), 130), collapse = "\n", sep = ""),
       adj = c(0, 1), cex = 1.5)
  
  
  # Zeile 11
  plot.new()
  lines(c(-0.02,3), c(1,1))
  text(1.04, 0.1, "Seite 4 von 5", adj = c(1,0))
  text(-0.02, 0.1, Fussn, adj = c(0,0))
  
  
  ##################################
  # Seite 5
  
  lmatr5 <- matrix(c(1,2,3), nrow = 3)
  layout(lmatr5, heights = lcm(c(2.5,16,0.5)))
  
  # Zeile 1
  plot.new()
  text(-0.02, 0.2, paste(strwrap(paste("Welche weiteren Qualitätsaspekte sind dir bei der Arbeit des SAPV-Teams", SAPVteam, "wichtig und welche weiteren Anmerkungen hast du?"), 105), collapse = "\n", sep = ""), adj = c(0,NA), cex = 1.8, font = 2)
  
  # Zeile 2
  plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
  text(-0.02, 5.1,
       paste(strwrap(paste("- ", dat$Freitext[grep('[:alphanum:]', dat$Freitext)], sep = '\n'), 130), collapse = "\n", sep = ""), adj = c(0, 1), cex = 1.5)
  
  # Zeile 3
  plot.new()
  lines(c(-0.02,3), c(1,1))
  text(1.04, 0.1, "Seite 5 von 5", adj = c(1,0))
  text(-0.02, 0.1, Fussn, adj = c(0,0))
  
  
  ######## Erstellen abschließen ##########
  
  dev.off()
}

Auswertung(Eingabe)
