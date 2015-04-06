---
title: "README"
author: "Johann Popp"
date: "04/06/2015"
output: html_document
---

### Mitarbeiter/innenbefragung zur SAPV-Qualität

Dieses Verzeichnis enthält Dateien, um eine Einschätzung der Mitarbeiter/innen eines SAPV-Teams zur Qualität der Versorgung zu erfragen und auszuwerten. Im Kern ist sie für das PCT-Ost entwickelt. Ich arbeite jetzt daran, Auswertung so zu verallgemeinern, dass es auch für andere Personen möglich ist, ihre Befragungen automatisch auszuwerten.

Ein Konzept zur Befragung liegt im Format von OpenOffice und im Microsoft Office ("Konzept MA Befragung.odt/doc") vor. Der Fragebogen zum Ausdrucken ist in der Datei "Fragebogen.odt" gespeichtert.

Es gibt eine Datenmaske im EpiData-Format ("Mitarbeiterinnenbefragung.epx"). Es ist mir noch nicht gelungen, diese Datenmaske direkt einzulesen. Es gibt ein Skript für diese Aufgabe auf https://github.com/daudi/Epidata-XML-to-R. Das funktioniert bei mir aber leider nicht. Deshalb muss der Datensatz als csv-Datei mit ";" als Trennzeichen gespeichert werden, damit es dann in R eingelesen werden kann.

In einer ersten Version habe ich die Auswertung über odfWeave programmiert "Ergebnisse darstellen.R". In der zweiten Version habe ich die Auswertung als pdf-Grafik erstellt ("Ergebnisse als pdf.R"). Die dritte Version packt das Ganze in eine die Funktion *Auswertung* ("Ergebnisse als funktion.R").
