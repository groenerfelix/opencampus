
###################################################
### Vorbereitung der Umgebung ####

# Falls nicht installiert ggf. folgendes Paket ausführen
#install.packages("fastDummies")

# Umgebungsvariablen löschen
remove(list = ls())

# Einbinden benötigter Funktionsbibliotheken
library(readr)
library(fastDummies)



###################################################
### Funktionsdefinitionen ####

#' Title Fast creation of normalized variables
#' Quickly create normalized columns from numeric type columns in the input data. This function is useful for statistical analysis when you want normalized columns rather than the actual columns.
#'
#' @param .data An object with the data set you want to make normalized columns from.
#' @param norm_values Dataframe of column names, means, and standard deviations that is used to create corresponding normalized variables from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) with same number of rows an dcolumns as the inputted data, only with normalized columns for the variables indicated in the norm_values argument.
#' @export
#'
#' @examples
norm_cols <- function (.data, norm_values = NULL) {
  for (i in 1:nrow(norm_values)  ) {
    .data[[norm_values$name[i]]] <- (.data[[norm_values$name[i]]] - norm_values$mean[i]) / norm_values$sd[i]
  }
  return (.data)
}


#' Title Creation of a Dataframe including the Information to Standardize Variables
#' This function is meant to be used in combination with the function norm_cols
#'
#' @param .data A data set including the variables you want to get the means and standard deviations from.
#' @param select_columns A vector with a list of variable names for which you want to get the means and standard deviations from.
#'
#' @return A data.frame (or tibble or data.table, depending on input data type) including the names, means, and standard deviations of the variables included in the select_columns argument.
#' @export
#'
#' @examples
get.norm_values <- function (.data, select_columns = NULL) {
  result <- NULL
  for (col_name in select_columns) {
    mean <- mean(.data[[col_name]], na.rm = TRUE)
    sd <- sd(.data[[col_name]], na.rm = TRUE)
    result <- rbind (result, c(mean, sd))
  }
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  result <- data.frame (select_columns, result, stringsAsFactors = FALSE)
  names(result) <- c("name", "mean", "sd")
  return (result)
}



###################################################
### Datenimport ####

# Einlesen der Daten
umsatzdaten <- read_csv("umsatzdaten_final.csv")


###################################################
### Datenaufbereitung ####

# Rekodierung von kategoriellen Variablen (zu Dummy-Variablen)
dummy_list <- c("Warengruppe", "Wochentag")
umsatzdaten_dummy = dummy_cols(umsatzdaten, dummy_list)
#potenziell: gefuehlte Temp, Monat

# Definition von Variablenlisten für die Dummies, um das Arbeiten mit diesen zu erleichtern
warengruppe_dummies = c('Brot', 'Brötchen', 'Croissant', 'Konditorei', 'Kuchen', 'Saisonbrot')
wochentag_dummies = c('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag','Freitag', 'Samstag', 'Sonntag')


# Standardisierung aller Feature Variablen und der Label Variable
norm_list <- c("Umsatz", "Warengruppe", "Bewoelkung", "Temperatur", "Windgeschwindigkeit", "KielerWoche",
               "Feiertag", "Kielmachtauf", "Flohmarkt", "gefuehlteTemp", "Monat") # Liste aller Variablen
#### Probleme ####
# "Datum" - falsches Format, 
# "Wochentag" - as.factor kodieren,
# Dummies laufen nicht durch: warengruppe_dummies, wochentag_dummies.... den part insgesamt immer auch unten anpassen!


norm_values_list <- get.norm_values(umsatzdaten_dummy, norm_list)    # Berechnung der Mittelwerte und Std.-Abw. der Variablen
umsatzdaten_norm <- norm_cols(umsatzdaten_dummy, norm_values_list) # Standardisierung der Variablen



###################################################
### Definition der Feature-Variablen und der Label-Variable ####

# Definition der Features (der unabhängigen Variablen auf deren Basis die Vorhersagen erzeugt werden sollen)
features = c("Datum", "Warengruppe", "Bewoelkung", "Temperatur", "Windgeschwindigkeit", "KielerWoche",
             "Feiertag", "Kielmachtauf", "Flohmarkt", "gefuehlteTemp", "Wochentag", "Monat")
# Definition der Label-Variable (der abhaengigen Variable, die vorhergesagt werden soll) sowie
label = 'Umsatz'


###################################################
### Definition von Trainings- und Testdatensatz ####

# Zufallszähler setzen, um die zufällige Partitionierung bei jedem Durchlauf gleich zu halten
set.seed(43879)
# Bestimmung der Indizes des Traininsdatensatzes
train_ind <- sample(seq_len(nrow(umsatzdaten_norm)), size = floor(0.66 * nrow(umsatzdaten_norm)))

# Teilen in Trainings- und Testdatensatz
train_dataset = umsatzdaten_norm[train_ind, features]
test_dataset = umsatzdaten_norm[-train_ind, features]

# Selektion der Variable, die als Label definiert wurde
train_labels = umsatzdaten_norm[train_ind, label]
test_labels = umsatzdaten_norm[-train_ind, label]


