# Title: Simulation Prüfungsdatensätze
# Autor: Sara Schiesberg
# Ziel: Nimmt eine Excel-Liste mit den Namen der Prüflinge und eine txt mit den Namen der Orks. Gibt für jeden Prüfling eine csv mit Merkmalen zu den verschiedenen Orks. 

# Pakete laden
library(fGarch) # für schiefen Verteilungen
library(readxl)
library(tidyverse)

# Liste der Prüflinge einlesen
prueflinge <- read_excel("Prüflinge.xlsx", range = "B1:C28")
prueflinge$pruefling <- paste(prueflinge$Nachname, "_", prueflinge$Vorname, sep= "")

# Diese Spalten enthalten in jeder Ergebnistabelle dieselben Werte: 
id <- seq(1:100)
# Spalte Namen
orknamen <- read_delim("Orknamen.txt", delim = "\t", escape_double = FALSE, col_names = FALSE,  trim_ws = TRUE)
orknamen <- rename(orknamen, name = X1)
# Dataframe erzeugen
orks <- as.data.frame(cbind(id, orknamen))

# Jede Ergebnistabelle beinhaltet die folgenden Merkmalsausprägungen:
kanni <- c("sehr oft", "oft", "selten", "noch nie")# für Kannibalismus
krumm <- c("X-Beine", "O-Beine", "S-Beine")# für Krummbeinigkeit
clans <- c("Goffs", "Snakebites", "Evil Sunz")# für Clans
geschl <- c("männlich", "weiblich", "divers")

# Für jeden Prüfling wird nun eine Tabelle erzeugt. Die nun definierten Spalten enthalten individuelle Werte: 
for (i in prueflinge$pruefling) {
  # Spalte Clan
  orks$clan <- sample(clans, 100, replace = TRUE) 
  orks$geschlecht <- sample(geschl, 100, replace = TRUE) 
  # Spalte Kannibalismus
  orks$kannibalismus <- sample(kanni, 100, replace = TRUE)
  orks$kannibalismus <- factor(orks$kannibalismus, ordered = TRUE, levels = kanni)
  # Spalte Krummbeinigkeit
  orks$krummbeinigkeit<- sample(krumm, 100, replace = TRUE)
  # Vitamin-D-Status, in Abhängigkeit von dem Kannibalisimus
  orks <- orks %>%
    mutate(HVDS = case_when(kannibalismus == "sehr oft" ~ round(rnorm(100, 60, 5),2),
                            kannibalismus == "oft" ~ round(rnorm(100, 50, 5),2),
                            kannibalismus == "selten" ~ round(rnorm(100, 40, 5),2),
                            kannibalismus == "noch nie" ~ round(rnorm(100, 30, 5),2)))
  # Körperhöhe in Abhängigkeit von der Clanzugehörigkeit 
  orks <- orks %>%
    mutate(koerperhoehe_cm = case_when(clan == "Goffs" ~ round(rsnorm(n =100, mean = 180, sd = 10, xi = 15),2),
                                       clan == "Snakebites" ~ round(rsnorm(n =100, mean = 220, sd = 8, xi = 15),2),
                                       clan == "Evil Sunz" ~ round(rsnorm(n =100, mean = 250, sd = 7, xi = 15),2),
                                       TRUE ~ 0))
  # Sportlichen Disziplinen
  orks$elfwegschiessen_m <- round(orks$HVDS/8 + rnorm(100, 3, 2), 2)
  orks$zwergenweitwurf_m <- round(orks$elfwegschiessen + rnorm(100, 4, 5), 2)
  # Jeder Datensatz wird gespeichert
  name <- paste("Daten_Orks_", i, sep = "")
  write.table(orks, paste("./2022_pruefungsdatensaetze/",name,".csv", sep = ""), sep = ",", dec = ".", fileEncoding = "UTF-8", row.names = FALSE)
}