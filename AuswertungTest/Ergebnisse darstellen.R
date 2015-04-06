# Auswertung der Mitarbeiter/innenbefragung mit odfWeave

# Johann Popp

# Erstellt:     2015-03-10
# Überarbeitet: 

##########################################

#Daten einlesen (die in EpiData mit ";" als Trennzeichen als csv exportiert wurden)
dat <- read.csv2("../Mitarbeiterinnenbefragung_Dataform_1_14.csv") 
dat[dat == 99] <- NA

# Funktion zur Erstellung der Grafiken
ergebnis <- function(x, breite = 5, höhe = 0.2, dpi = 200){
  darst <- getImageDefs()
  darst$dispHeight <- höhe; darst$dispWidth <- breite
  darst$plotHeight <- darst$dispHeight * dpi; darst$plotWidth <- darst$dispWidth * dpi 
  setImageDefs(darst)
  par(mar = c(0,0,0,0))
  h <- hist(x, breaks = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), xlim = c(0.5,6.6), ylim = c(0,15),
            axes = FALSE, col = "grey", border = "grey", main = "", ylab = "", xlab = "")
  segments(x0 = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), y0 = rep(0, 6), y1 = rep(15, 6), col = "grey40", lty = 2)
  if(sum(is.na(x)) > 0){
    polygon(c(6.4,6.4,6.6,6.6), c(0, rep(sum(is.na(x)),2), 0), col = "darkred", border = "darkred")
    text(6.5, sum(is.na(x)) + 1, sum(is.na(x)), col = "darkred", cex = 1.5, adj = c(NA, 0))
  }
  boxplot(x, horizontal = TRUE, add = TRUE, axes = FALSE, 
          at = 7.5, boxwex = 20, 
          lwd = 2, col = '#ffff0099')
  points(mean(x, na.rm = TRUE), 7.5, col = "blue", cex = 2, lwd = 2, pch = 4)
}


# Libre Office Dokument erstellen
library(odfWeave)

odfWeave("Ergebnisse.odt", "Ergebnis Mitarbeiterinnenbefragung.odt")
