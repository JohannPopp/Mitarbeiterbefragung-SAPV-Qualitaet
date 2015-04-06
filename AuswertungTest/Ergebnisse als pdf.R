# Mitarbeiter/innenbefragung zur SAPV-Qualität

# Diese Syntax erstellt eine pdf-Datei mit den Ergebnissen der Befragung.

# Johann Popp
# Erstellt:             2015-03-30
# Letzte Überarbeitung: 2015-03-31
#############################################

###### Eingaben #########

### Wie heißt das SAPV-Team, in dem die Befragung durchgeführt wurde?
SAPVteam <- "Testdaten"
# Name in die Fußnoten einfügen.
Auswerter <- character()
Aut <- paste(paste(Auswerter, collapse = ", "), if(length(Auswerter) >= 1){" & "},"J. Popp", sep = "")

Fussn <- paste("Mitarbeiter/innenbefragung zur SAPV-Qualität im Team", SAPVteam, ". ", Aut, ": ", Sys.Date(),".", sep = "")

### Daten einlesen (die in EpiData mit ";" als Trennzeichen als csv exportiert wurden)
dat <- read.csv2("../Mitarbeiterinnenbefragung_Dataform_1_15.csv") 
dat[dat == 99] <- NA

# Falls die Daten nicht aus einer csv-Datei eingelesen werden sollen, sondern innerhalb von R in eine Datenmatrix eingegeben werden sollen, können die nächsten drei Befehle aktiviert werden. (Das Kommentierungszeichen "#" löschen. Dann ggf. die beiden Befehle zum Einlesen der csv-Datei mit # auskommentieren.)
# Die Bewertungen werden numerisch kodiert: 1 = sehr zufriedenstellend; 5 = sehr verbesserungswürdig
#dat <- data.frame(ID = numeric(), QGesamt = numeric(), SympGesamt = numeric(), SympErfass = numeric(), SympDoku = numeric(), SympBehand = numeric(), SympVorbeug = numeric(), SympSchulung = numeric(), SympSchmerz = numeric(), SympLuft = numeric(), SympAngst = numeric(), SympNausea = numeric(), SympObsti = numeric(), SympFatigue = numeric(), SympKogni = numeric(), SympWunde = numeric(), SympAndere = character(), SympAndWert = numeric(), SympAnmerk = character(), SicherGesamt = numeric(), SicherKriseVerm  = numeric(), SicherKriseAuff = numeric(), SicherMedi = numeric(), SicherInfekt = numeric(), SicherVerletz = numeric(), SicherAndere = character(), SicherAndWert = numeric(), SicherAnmerk = character(), AllGesamt = numeric(), AllKrankheit = numeric(), AllPflege = numeric(), AllSozial = numeric(), AllOrga = numeric(), AllSpirit = numeric(), AllKultur = numeric(), AllAnmerk = character(), Freitext = character())
#fix(dat)
#write.csv2(dat, file.choose(), row.names = FALSE)


# Text aus dem Fragebogen heraus kopieren
txt <- read.csv2("../Fragebogen.csv", colClasses = "character")[,1]


######## Funktionen #############

# Funktion zur Erstellung der Grafiken
ergebnis <- function(x, farbe = "#a1d99b99"){ 
#  par(mar = c(0,0,0,0))
  h <- hist(x, breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), xlim = c(0.5,6.6), ylim = c(0,15),
            axes = FALSE, col = "grey", border = "black", main = "", ylab = "", xlab = "")
  segments(x0 = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), y0 = rep(0, 6), y1 = rep(15, 6), col = "grey30", lty = 2)
  if(sum(is.na(x)) > 0){
    polygon(c(6.4,6.4,6.6,6.6), c(0, rep(sum(is.na(x)),2), 0), col = "grey30", border = NA)
    text(6.5, sum(is.na(x)) + 1, sum(is.na(x)), col = "grey40", cex = 1, adj = c(NA, 0))
  }
  boxplot(x, horizontal = TRUE, add = TRUE, axes = FALSE, 
          at = 7.5, boxwex = 20, 
          lwd = 1.5, col = farbe)
  points(mean(x, na.rm = TRUE), 7.5, col = "grey50", cex = 2, lwd = 1.5, pch = 1)
}


########## pdf-Dokument erstellen #########

pdf("Ergebnis.pdf", paper = "a4r", width = 10.5, height = 7.5)
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

layout(lmatr, widths=lcm(c(13.5,12.5)), heights = lcm(c(2, rep(1.7,5), rep(1.2, 5), 2.5)))

# Zeile 1
plot(10, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(0.5, c(0.5), paste0(strwrap(paste("Wie schätzt du die Qualität der Arbeit des SAPV-Teams", SAPVteam, "ein?"), 60), collapse = "\n"), adj = c(0.5,0.5), cex = 3, font = 2)

# Zeile 2
plot.new()
text(0.04, 0.1, "Qualität insgesamt:", adj = c(0,0), cex = 2, font = 2)
lines(c(0, 10), c(0, 0))

plot(10, xlim = c(0.5, 6.6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(c(1, 2, 3, 4, 5, 6.5), 0.1, c("sehr\nzufrieden-\nstellend", 
                "zufrieden-\nstellend", "ausreichend", 
                "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig", "keine\nAngabe"), adj = c(NA, 0))
lines(c(0, 10), c(0, 0))

# Zeile 3
plot.new()
text(0, 0.5, paste0(strwrap(paste("Wie schätzt du die Qualität der Arbeit des SAPV-Teams", SAPVteam, "insgesamt ein?"), 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$QGesamt, farbe = "#2b8cbe99")

# Zeile 4
plot.new()
text(0.04, 0.1, "Symptomkontrolle:", adj = c(0,0), cex = 2, font = 2)
lines(c(0, 10), c(0, 0))

plot(10, xlim = c(0.5, 6.6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(c(1, 2, 3, 4, 5, 6.5), 0.1, c("sehr\nzufrieden-\nstellend", 
                                   "zufrieden-\nstellend", "ausreichend", 
                                   "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig", "keine\nAngabe"), adj = c(NA, 0))
lines(c(0, 10), c(0, 0))

# Zeile 5
plot.new()
text(0, 0.5, paste0(strwrap("Wie schätzt du die Qualität der Symptomkontrolle insgesamt ein?", 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympGesamt, farbe = "#2b8cbe99")

# Zeile 6
plot.new()
text(0.04, 0.1, "Symptomkontrolle nach Teilprozessen:", font = 2, adj = c(0,0), cex = 1.5)
lines(c(0, 10), c(0, 0))

plot(10, xlim = c(0.5, 6.6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(c(1, 2, 3, 4, 5, 6.5), 0.1, c("sehr\nzufrieden-\nstellend", 
                                   "zufrieden-\nstellend", "ausreichend", 
                                   "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig", "keine\nAngabe"), adj = c(NA, 0))
lines(c(0, 10), c(0, 0))


# Zeile 7
plot.new()
text(0, 0.5, paste0(strwrap(txt[8], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympErfass)

# Zeile 8
plot.new()
text(0, 0.5, paste0(strwrap(txt[9], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympDoku)

# Zeile 9
plot.new()
text(0, 0.5, paste0(strwrap(txt[10], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympBehand)

# Zeile 10
plot.new()
text(0, 0.5, paste0(strwrap(txt[11], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympVorbeug)

# Zeile 11
plot.new()
text(0, 0.5, paste0(strwrap(txt[12], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

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

layout(lmatr2, widths=lcm(c(4.2,9.5,12.5)), heights = lcm(c(rep(1.2, 9), 2.5, 1.2, 4, 0.5)))

# Zeile 1
plot.new()
text(0.04, 0.1, "Symptomkontrolle nach Symptomen:", font = 2, adj = c(0,0), cex = 1.5)
lines(c(0, 10), c(0, 0))

plot(10, xlim = c(0.5, 6.6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(c(1, 2, 3, 4, 5, 6.5), 0.1, c("sehr\nzufrieden-\nstellend", 
                                   "zufrieden-\nstellend", "ausreichend", 
                                   "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig", "keine\nAngabe"), adj = c(NA, 0))
lines(c(0, 10), c(0, 0))

# Zeile 2
plot.new()
text(0, 0.5, paste0(strwrap(txt[16], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympSchmerz)

# Zeile 3
plot.new()
text(0, 0.5, paste0(strwrap(txt[17], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympLuft)

# Zeile 4
plot.new()
text(0, 0.5, paste0(strwrap(txt[18], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympAngst)

# Zeile 5
plot.new()
text(0, 0.5, paste0(strwrap(txt[19], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympNausea)

# Zeile 6
plot.new()
text(0, 0.5, paste0(strwrap(txt[20], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympObsti)

# Zeile 7
plot.new()
text(0, 0.5, paste0(strwrap(txt[21], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympFatigue)

# Zeile 8
plot.new()
text(0, 0.5, paste0(strwrap(txt[22], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympKogni)

# Zeile 9
plot.new()
text(0, 0.5, paste0(strwrap(txt[23], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SympWunde)

# Zeile 10
plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
text(0.09, 2, "Andere Symptome:", adj = c(0,1), cex = 1.5)

plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
text(0, 2, 
     paste0(strwrap(paste(dat$SympAndere[grep('[:alphanum:]', dat$SympAndere)], ': ', dat$SympAndWert[grep('[:alphanum:]', dat$SympAndere)], collapse = '  ---  ', sep = ''), 110), collapse = "\n"), 
     adj = c(0,1), cex = 1.5)

# Zeile 11
plot.new()
text(-0.02, 0.2, txt[25], adj = c(0,NA), cex = 1.5, font = 2)

# Zeile 12
plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
text(-0.02, 5, 
     paste0(strwrap(paste(dat$SympAnmerk[grep('[:alphanum:]', dat$SympAnmerk)], collapse = '  ---  '), 130), collapse = "\n"),
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

layout(lmatr3, widths=lcm(c(4.2,9.5,12.5)), heights = lcm(c(1.2, 1.7, 1, rep(1.2, 5), 2.5, 1.2, 4, 0.5)))

# Zeile 1
plot.new()
text(0.04, 0.17, "Sicherheitsversprechen:", font = 2, adj = c(0,0), cex = 2)
lines(c(0, 10), c(0, 0))

plot(10, xlim = c(0.5, 6.6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(c(1, 2, 3, 4, 5, 6.5), 0.1, c("sehr\nzufrieden-\nstellend", 
                                   "zufrieden-\nstellend", "ausreichend", 
                                   "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig", "keine\nAngabe"), adj = c(NA, 0))
lines(c(0, 10), c(0, 0))

# Zeile 2
plot.new()
text(0, 0.5, paste0(strwrap("In wie weit trägt die Versorgung insgesamt dazu bei, Patienten und Angehörigen die nötige Sicherheit zu geben, krisenhafte Krankheitssituationen im häuslichen Umfeld zu bewältigen?", 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SicherGesamt, farbe = "#2b8cbe99")

# Zeile 3
plot.new()
text(0, 0.5, paste0(strwrap(txt[36], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SicherKriseVerm)

# Zeile 4
plot.new()
text(0, 0.5, paste0(strwrap(txt[37], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SicherKriseAuff)

# Zeile 5
plot.new()
text(0, 0.5, paste0(strwrap(txt[38], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SicherMedi)

# Zeile 6
plot.new()
text(0, 0.5, paste0(strwrap(txt[39], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SicherInfekt)

# Zeile 7
plot.new()
text(0, 0.5, paste0(strwrap(txt[40], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$SicherVerletz)

# Zeile 8
plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
text(0.09, 2, "Andere Gefahren:", adj = c(0,1), cex = 1.5)

plot(10, xlim = c(0,1), ylim = c(0,3), axes = FALSE)
text(0, 2, 
     paste0(strwrap(paste(dat$SicherAndere[grep('[:alphanum:]', dat$SicherAndere)], ': ', dat$SicherAndWert[grep('[:alphanum:]', dat$SicherAndere)], collapse = '  ---  ', sep = ''), 110), collapse = "\n"), 
     adj = c(0,1), cex = 1.5)

# Zeile 9
plot.new()
text(-0.02, 0.2, txt[42], adj = c(0,NA), cex = 1.5, font = 2)

# Zeile 10
plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
text(-0.02, 5, 
     paste0(strwrap(paste(dat$SicherAnmerk[grep('[:alphanum:]', dat$SicherAnmerk)], collapse = '  ---  '), 130), collapse = "\n"),
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

layout(lmatr4, widths=lcm(c(4.2,9.5,12.5)), heights = lcm(c(1.2, 1.7, 0.5, rep(c(1.5,0.2), 6), 1, 3.5, 0.5)))

# Zeile 1
plot.new()
text(0.04, 0.17, "Alltagsrahmung:", font = 2, adj = c(0,0), cex = 2)
lines(c(0, 10), c(0, 0))

plot(10, xlim = c(0.5, 6.6), ylim = c(0,1), axes = FALSE, ylab = "", xlab = "")
text(c(1, 2, 3, 4, 5, 6.5), 0.1, c("sehr\nzufrieden-\nstellend", 
                                   "zufrieden-\nstellend", "ausreichend", 
                                   "ver-\nbesserungs-\nwürdig", "sehr ver-\nbesserungs-\nwürdig", "keine\nAngabe"), adj = c(NA, 0))
lines(c(0, 10), c(0, 0))

# Zeile 2
plot.new()
text(0, 0.5, paste0(strwrap(txt[50], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

ergebnis(dat$AllGesamt, farbe = "#2b8cbe99")

# Zeile 3
plot.new()
text(0, 0.5, paste0(strwrap(txt[51], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

par(mar = c(0.5,0,0.5,0))
ergebnis(dat$AllKrankheit)

# Zeile 4
par(mar = c(0,0,0,0))
plot.new()
text(0, 0.5, paste0(strwrap(txt[52], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

par(mar = c(0.5,0,0.5,0))
ergebnis(dat$AllPflege)

# Zeile 5
par(mar = c(0,0,0,0))
plot.new()
text(0, 0.5, paste0(strwrap(txt[53], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

par(mar = c(0.5,0,0.5,0))
ergebnis(dat$AllSozial)

# Zeile 6
par(mar = c(0,0,0,0))
plot.new()
text(0, 0.5, paste0(strwrap(txt[54], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

par(mar = c(0.5,0,0.5,0))
ergebnis(dat$AllOrga)

# Zeile 7
par(mar = c(0,0,0,0))
plot.new()
text(0, 0.5, paste0(strwrap(txt[55], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

par(mar = c(0.5,0,0.5,0))
ergebnis(dat$AllSpirit)

# Zeile 8
par(mar = c(0,0,0,0))
plot.new()
text(0, 0.5, paste0(strwrap(txt[56], 67), collapse = "\n"), adj = c(0,NA), cex = 1.5)

par(mar = c(0.5,0,0.5,0))
ergebnis(dat$AllKultur)

# Zeile 9
par(mar = c(0,0,0,0))
plot.new()
text(-0.02, 0.2, txt[57], adj = c(0,NA), cex = 1.5, font = 2)

# Zeile 10
plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
text(-0.02, 5, 
     paste0(strwrap(paste(dat$AllAnmerk[grep('[:alphanum:]', dat$AllAnmerk)], collapse = '  ---  '), 130), collapse = "\n"),
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
text(-0.02, 0.2, paste0(strwrap(paste("Welche weiteren Qualitätsaspekte sind dir bei der Arbeit des SAPV-Teams", SAPVteam, "wichtig und welche weiteren Anmerkungen hast du?"), 105), collapse = "\n"), adj = c(0,NA), cex = 1.8, font = 2)

# Zeile 2
plot(10, ylim = c(0,5), xlim = c(0,1), axes = FALSE)
text(-0.02, 5.1,
     paste0(strwrap(paste("- ", 
                          dat$Freitext[grep('[:alphanum:]', dat$Freitext)],
                          sep = '\n')
                    , 130),
            collapse = "\n"),
     adj = c(0, 1), cex = 1.5)

# Zeile 3
plot.new()
lines(c(-0.02,3), c(1,1))
text(1.04, 0.1, "Seite 5 von 5", adj = c(1,0))
text(-0.02, 0.1, Fussn, adj = c(0,0))


######## Erstellen abschließen ##########

dev.off()

