#### TODO 
# Wochentag macht noch Stress!!
# falsche Zuordnung von Cols bei der yolo Berechnung von M und SD für MONAT
# Save die yolo Aktion
  

###################################################
### Vorbereitung der Umgebung ####

# Falls nicht installiert ggf. folgendes Paket ausführen
#install.packages("fastDummies")

# Umgebungsvariablen löschen
remove(list = ls())

# Einbinden benötigter Funktionsbibliotheken
library(readr)
library(fastDummies)
library(dplyr)



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

umsatzdaten$Wochentag <- NULL
umsatzdaten[is.na(umsatzdaten)] <- 0


###################################################
### Datenaufbereitung ####

# Rekodierung von kategoriellen Variablen (zu Dummy-Variablen)
dummy_list <- c("Warengruppe","gefuehlteTemp", "Monat") # "Wochentag"
umsatzdaten_dummy = dummy_cols(umsatzdaten, dummy_list)


## Weitere Datenaufbereitung 
umsatzdaten_dummy <- lapply(umsatzdaten_dummy, as.numeric)
umsatzdaten_dummy$Datum <- as.POSIXct(umsatzdaten$Datum)
umsatzdaten_dummy <- as_tibble(umsatzdaten_dummy)


#umsatzdaten_dummy <- as.data.frame(umsatzdaten_dummy)


# Definition von Variablenlisten für die Dummies, um das Arbeiten mit diesen zu erleichtern
warengruppe_dummies = c('Warengruppe_1', 'Warengruppe_2', 'Warengruppe_3', 'Warengruppe_4', 'Warengruppe_5', 'Warengruppe_6')
#warengruppe_dummies = c('Brot', 'Brötchen', 'Croissant', 'Konditorei', 'Kuchen', 'Saisonbrot')
#wochentag_dummies = c('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag','Freitag', 'Samstag', 'Sonntag')
#temp_dummies = c('frische Brise', 'falsche Kleidung', 'fast schon zu warm', 'tropische Außentemperatur')
#month_dummies = c('Jan', 'Feb', 'Mar', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez')
temp_dummies = c('gefuehlteTemp_0', 'gefuehlteTemp_1', 'gefuehlteTemp_2', 'gefuehlteTemp_3')
month_dummies = c('Monat_1', 'Monat_10', 'Monat_11', 'Monat_12', 'Monat_2', 'Monat_3', 
                  'Monat_4', 'Monat_5', 'Monat_6', 'Monat_7', 'Monat_8', 'Monat_9')


# Standardisierung aller Feature Variablen und der Label Variable
norm_list <- c("Umsatz", "Bewoelkung", "Temperatur", "Windgeschwindigkeit", 
               "gefuehlteTemp", warengruppe_dummies, temp_dummies, month_dummies) # Liste aller Variablen #wochentag_dummies


#### !!Problem!! ####
# Dumme Dummies laufen nicht durch: warengruppe_dummies, wochentag_dummies.... den part insgesamt immer auch unten anpassen!


norm_values_list <- get.norm_values(umsatzdaten_dummy, norm_list)    # Berechnung der Mittelwerte und Std.-Abw. der Variablen

# yolo mean und Sd von Hand
#norm_values_list[6:27,2] <- colMeans(umsatzdaten_dummy[, 13:34], na.rm = TRUE)
#norm_values_list[6:27,3] <- sapply(umsatzdaten_dummy[, 13:34], sd, na.rm = TRUE)

#umsatzdaten_norm <- data.frame(matrix(0, ncol = 34, nrow = 14664))
#umsatzdaten_norm <- NA
#umsatzdaten_norm[14664]
#test_norm <- normalize()

umsatzdaten_norm <- norm_cols(umsatzdaten_dummy, norm_values_list) # Standardisierung der Variablen



###################################################
### Definition der Feature-Variablen und der Label-Variable ####

# Definition der Features (der unabhängigen Variablen auf deren Basis die Vorhersagen erzeugt werden sollen)
features = c("Bewoelkung", "Temperatur", "Windgeschwindigkeit", "KielerWoche",
             "Feiertag", "Kielmachtauf", "Flohmarkt", warengruppe_dummies, temp_dummies, month_dummies)


# Definition der Label-Variable (der abhaengigen Variable, die vorhergesagt werden soll) sowie
label = 'Umsatz'


###################################################
### Definition von Trainings- und Testdatensatz ####

# Zufallszähler setzen, um die zufällige Partitionierung bei jedem Durchlauf gleich zu halten
set.seed(1512)
# Bestimmung der Indizes des Traininsdatensatzes
train_ind <- sample(seq_len(nrow(umsatzdaten_norm)), size = floor(0.66 * nrow(umsatzdaten_norm)))

# Teilen in Trainings- und Testdatensatz
train_dataset = umsatzdaten_norm[train_ind, features]
test_dataset = umsatzdaten_norm[-train_ind, features]

# Selektion der Variable, die als Label definiert wurde
train_labels = umsatzdaten_norm[train_ind, label]
test_labels = umsatzdaten_norm[-train_ind, label]


