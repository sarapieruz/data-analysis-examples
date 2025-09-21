library(haven)
require(ggplot2)
library(sm)
library(MASS)
library(tidyverse)
library(eRm)
library(dplyr)
require(arm)
require(car)
library(ggrepel)
library(xtable)
library(waffle)
library(weights)
library(intsvy)
library(GDAtools)
library(survey)
library(gridExtra)
library(stargazer)
library(MASS)
library(lmtest)

setwd("C:/Users/Utente/OneDrive/Desktop/Tesi")

# Leggi un file SPSS
# STU_Q_22 <- read_spss("STU_Q_22.sav")
# dati <- mutate_if(STU_Q_22, is.labelled, as_factor)
# write.csv(dati, "STU_Q_22.csv")

# ELABORAZIONE DATI

# Estrazione delle variabili necessarie dal dataset orginale

dati <- data.frame(STU_Q_22$CNT,STU_Q_22$ST004D01T, STU_Q_22$REPEAT,
                   STU_Q_22$ST019AQ01T, STU_Q_22$ST019BQ01T, 
                   STU_Q_22$ST019CQ01T,STU_Q_22$PV1MATH, STU_Q_22$PV2MATH, 
                   STU_Q_22$PV3MATH, STU_Q_22$PV4MATH, STU_Q_22$PV5MATH, 
                   STU_Q_22$PV6MATH, STU_Q_22$PV7MATH, STU_Q_22$PV8MATH, 
                   STU_Q_22$PV9MATH, STU_Q_22$PV10MATH, STU_Q_22$ANXMAT, 
                   STU_Q_22$ESCS, STU_Q_22$FAMSUP, STU_Q_22$IMMIG, 
                   STU_Q_22$W_FSTUWT, STU_Q_22$MATHPREF, STU_Q_22$TEACHSUP, 
                   STU_Q_22$BULLIED,STU_Q_22$PERSEVAGR, STU_Q_22$BELONG)

# Salvare il dataset ridotto
# dati <- mutate_if(dati, is.labelled, as_factor)
# write.csv(dati, "PISA.csv")

# Rinomina e ricodifica delle variabili

# Paese

colnames(dati)[colnames(dati) == "STU_Q_22.CNT"] <- "Paese"

# Codifica dei paesi nelle rispettive aree geografiche-

dati$AREA <- ifelse(dati$Paese %in% c("DNK", "EST", "FIN", "ISL", 
            "LVA", "LTU", "NOR", "SWE"), "Europa del Nord",
            ifelse(dati$Paese %in% c("AUT", "BEL", "FRA", "DEU", 
             "NLD", "CHE", "IRL", "GBR"), "Europa Occidentale",
            ifelse(dati$Paese %in% c("ALB", "BGR", "HRV", "GRC", 
             "ITA", "PRT", "ROU", "SRB", "SVN", "ESP", "MNE", 
             "MKD", "MLT", "QCY"), "Europa Meridionale",
            ifelse(dati$Paese %in% c("CZE", "HUN", "MDA", "POL", 
             "SVK"), "Europa Orientale",
            ifelse(dati$Paese %in% c("AUS", "NZL"), "Oceania",
            ifelse(dati$Paese == "MAR", "Africa",
            ifelse(dati$Paese %in% c("JPN", "KOR", "HKG", "MAC", 
             "TAP"), "Asia Orientale",
            ifelse(dati$Paese %in% c("BRN", "KHM", "IDN", "MYS", 
             "PHL", "SGP", "THA", "VNM"), "Asia Sudorientale",
            ifelse(dati$Paese %in% c("KAZ", "UZB", "KSV"), 
             "Asia Centrale",
            ifelse(dati$Paese %in% c("ARM", "AZE", "BHR", "ARE", 
             "GEO", "JOR", "ISR", "QAT", "SAU", "PSE", "TUR"), 
             "Asia Occidentale",
            ifelse(dati$Paese %in% c("CAN", "USA", "MEX"), 
             "America del Nord",
            ifelse(dati$Paese %in% c("CRI", "SLV", "GTM", "PAN", 
             "JAM", "DOM"), "America Centrale e Caraibi",
            ifelse(dati$Paese %in% c("ARG", "BRA", "CHL", "COL", 
            "PER", "PRY", "URY"), "America del Sud", 
              NA)))))))))))))

# REPEAT: indica se lo studente è ripetente

colnames(dati)[colnames(dati) == "STU_Q_22.REPEAT"] <- "Repeat1"

# Ripetente: 0 se non è ripetente, 1 se lo è

dati$REPEAT <- ifelse(dati$Repeat1 == 0, 0, 
                      ifelse(dati$Repeat1 == 1, 1 , NA))
dati$REPEAT <- as.factor(dati$REPEAT)

# Genere

colnames(dati)[colnames(dati) == "STU_Q_22.ST004D01T"] <- "Genere"

# Creazione della variabile Maschio per la ricodifica del genere

dati$Maschio <- ifelse(dati$Genere==1,0,ifelse(dati$Genere==2,1,NA))
dati$Maschio <- as.factor(dati$Maschio)

# Item immigrazione

colnames(dati)[colnames(dati) == "STU_Q_22.ST019AQ01T"] <- "Immig_stu"
colnames(dati)[colnames(dati) == "STU_Q_22.ST019BQ01T"] <- "Immig_ma"
colnames(dati)[colnames(dati) == "STU_Q_22.ST019CQ01T"] <- "Immig_pa"

# 1 se lo studente, la madre o il padre è immigrato, 0 altrimenti

dati$IMMIG_STU <- ifelse(dati$Immig_stu == 2, 1, 0)
dati$IMMIG_STU <- as.factor(dati$IMMIG_STU)
dati$IMMIG_M <- ifelse(dati$Immig_ma == 2, 1, 0)
dati$IMMIG_M <- as.factor(dati$IMMIG_M)
dati$IMMIG_P <- ifelse(dati$Immig_pa == 2, 1, 0)
dati$IMMIG_P <- as.factor(dati$IMMIG_P)

# Plausible Values per il punteggio

colnames(dati)[colnames(dati) == "STU_Q_22.PV1MATH"] <- "PV1"
colnames(dati)[colnames(dati) == "STU_Q_22.PV2MATH"] <- "PV2"
colnames(dati)[colnames(dati) == "STU_Q_22.PV3MATH"] <- "PV3"
colnames(dati)[colnames(dati) == "STU_Q_22.PV4MATH"] <- "PV4"
colnames(dati)[colnames(dati) == "STU_Q_22.PV5MATH"] <- "PV5"
colnames(dati)[colnames(dati) == "STU_Q_22.PV6MATH"] <- "PV6"
colnames(dati)[colnames(dati) == "STU_Q_22.PV7MATH"] <- "PV7"
colnames(dati)[colnames(dati) == "STU_Q_22.PV8MATH"] <- "PV8"
colnames(dati)[colnames(dati) == "STU_Q_22.PV9MATH"] <- "PV9"
colnames(dati)[colnames(dati) == "STU_Q_22.PV10MATH"] <- "PV10"

# Creazione della variabile SCORE

dati$score <- as.numeric(rowMeans(dati[, c(7,8,9,10,11,12,13,14,15,16)], na.rm = T))
dati$SCORE = dati$score
dati$SCORE[dati$SCORE < 233.17] <- NA

# Creazione della variabile Livello SCORE

dati$Livello <- ifelse(!is.na(dati$SCORE) & dati$SCORE >= 669.30, "livello 6", 
                       ifelse(!is.na(dati$SCORE) & dati$SCORE >= 606.99 & dati$SCORE < 669.30, "livello 5",
                              ifelse(!is.na(dati$SCORE) & dati$SCORE >= 544.68 & dati$SCORE < 606.99, "livello 4",
                                     ifelse(!is.na(dati$SCORE) & dati$SCORE >= 482.38 & dati$SCORE < 544.68, "livello 3",
                                            ifelse(!is.na(dati$SCORE) & dati$SCORE >= 420.07 & dati$SCORE < 482.38, "livello 2",
                                                   ifelse(!is.na(dati$SCORE) & dati$SCORE >= 357.77 & dati$SCORE < 420.07, "livello 1a",
                                                          ifelse(!is.na(dati$SCORE) & dati$SCORE >= 295.47 & dati$SCORE < 357.77, "livello 1b", 
                                                                 ifelse(!is.na(dati$SCORE) & dati$SCORE >= 233.17 & dati$SCORE < 295.47, "livello 1c", NA))))))))
# Ordine dei livelli

ordine_livelli <- c("livello 1c", "livello 1b", "livello 1a", "livello 2", "livello 3", "livello 4", "livello 5", "livello 6")
dati$Livello = factor(dati$Livello, levels = ordine_livelli)

# ANXMAT: Indice dell'ansia in matematica

colnames(dati)[colnames(dati) == "STU_Q_22.ANXMAT"] <- "Anxmat1"
dati$ANXMAT <- ifelse(dati$Anxmat1 > 5, NA, dati$Anxmat1)

# FAMSUP: Indice di supporto familiare

colnames(dati)[colnames(dati) == "STU_Q_22.FAMSUP"] <- "Famsup1"
dati$FAMSUP <- ifelse(dati$Famsup1 > 5, NA, dati$Famsup1)

# IMMIG: Indice Immigrazione

colnames(dati)[colnames(dati) == "STU_Q_22.IMMIG"] <- "Immig1"
dati$IMMIG <- ifelse(dati$Immig1 == 1, "Nativo", 
                     ifelse(dati$Immig1 == 2, "Seconda Generazione", 
                            ifelse(dati$Immig1 == 3, "Prima Generazione", NA)))
dati$IMMIG <- as.factor(dati$IMMIG)

# TEACHSUP: supporto del docente di matematica

colnames(dati)[colnames(dati) == "STU_Q_22.TEACHSUP"] <- "Teachsup1"
dati$TEACHSUP <- ifelse(dati$Teachsup1 > 5, NA, dati$Teachsup1)

# PERSEVAGR: perseveranza

colnames(dati)[colnames(dati) == "STU_Q_22.PERSEVAGR"] <- "Persevagr1"
dati$PERSEVAGR <- ifelse(dati$Persevagr1 > 10, NA, dati$Persevagr1)

# ESCS: indice dello status socio-economico-culturale

colnames(dati)[colnames(dati) == "STU_Q_22.ESCS"] <- "Escs"
dati$ESCS <- ifelse(dati$Escs > 10, NA, dati$Escs)
quintili_ESCS <- quantile(dati$ESCS, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
livelli <- c("Livello 1", "Livello 2", "Livello 3", "Livello 4", "Livello 5")
dati$Livello_ESCS <- cut(dati$ESCS, breaks = quintili_ESCS, labels = livelli, include.lowest = TRUE)

# W_FSTUWT: pesi finali

colnames(dati)[colnames(dati) == "STU_Q_22.W_FSTUWT"] <- "W_FSTUWT"


# PARAGRAFO 2.3

# GRAFICO LIVELLI --> 2.6

dati$Livello <- as.factor(dati$Livello)

frequenze <- wtd.table(dati$Livello, weights = dati$W_FSTUWT)

dati$Livello <- as.factor(dati$Livello)
# Risultato in un data.frame
frequenze_df <- as.data.frame(frequenze)
colnames(frequenze_df) <- c("Livello", "Frequenza")

ggplot(frequenze_df, aes(x = factor(Livello, levels = ordine_livelli), y = Frequenza)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Distribuzione dei livelli di proficienza",
       x = "Livello di proficienza",
       y = "Frequenza") +
  theme_minimal()

# PARAGRAFO 2.2

# TABELLA FREQUENZE PER PAESE --> T 2.1

freq_abs <- table(dati$Paese)
freq_perc <- round(prop.table(freq_abs) * 100, 2)
freq_paesi <- data.frame(Paese = names(freq_abs), FrequenzeAssolute = as.integer(freq_abs), FrequenzePercentuali = as.numeric(freq_perc))

latex_paesi <- xtable(freq_paesi, align = "lrrr", caption = "Frequenze dei Paesi") 
print(latex_paesi, include.rownames = FALSE, floating = FALSE, hline.after = NULL, caption.placement = "top")

# Calcolo delle frequenze ponderate per paese

freq_paesi <- wtd.table(dati$Paese, weights = dati$W_FSTUWT)

freq_paesi <- dati %>% group_by(Paese) %>% summarise(FrequenzeAssolute = sum(W_FSTUWT)) %>%
  mutate(FrequenzePercentuali = round(FrequenzeAssolute / sum(FrequenzeAssolute) * 100, 2))

latex_paesi <- xtable(freq_paesi, align = "lrrr", caption = "Frequenze dei Paesi") 
print(latex_paesi, include.rownames = FALSE, floating = FALSE, hline.after = NULL, caption.placement = "top")

# GRAFICO E TABELLA AREE --> T 2.2 e FIG 2.1

freq_aree <- dati %>%
  filter(!is.na(AREA)) %>%
  group_by(AREA) %>%
  summarise(FrequenzePonderate = sum(W_FSTUWT)) %>%
  mutate(FrequenzePercentuali = round(FrequenzePonderate / sum(FrequenzePonderate) * 100, 2))

ggplot(freq_aree, aes(x = AREA, y = FrequenzePercentuali, fill = AREA)) +
  geom_bar(stat = "identity") +
  theme_minimal() + 
  labs(fill = "AREA", title = "Distribuzione percentuale per Area Geografica") +
  theme(
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))

# Nuova codifica di Area
dati$AREA <- ifelse(dati$Paese %in% c("DNK", "EST", "FIN", "ISL", "LVA", "LTU", "NOR", "SWE", "AUT", "BEL", "FRA", "DEU", "NLD", 
                                      "CHE", "IRL", "GBR", "ALB", "BGR", "HRV", "GRC", "ITA", "PRT", "ROU", "SRB", "SVN", "ESP", 
                                      "MNE", "MKD", "MLT", "QCY", "CZE", "HUN", "MDA", "POL", "SVK"), "Europa",
                    ifelse(dati$Paese %in% c("AUS", "NZL"), "Oceania",
                           ifelse(dati$Paese %in% c("JPN", "KOR", "HKG", "MAC", "TAP", "KAZ", "UZB", "KSV", "BRN", "KHM", 
                                                    "IDN", "MYS", "PHL", "SGP", "THA", "VNM"), "Asia Centrale a Orientale",
                                  ifelse(dati$Paese %in% c("ARM", "AZE", "BHR", "ARE", "GEO", "JOR", "ISR", 
                                                           "QAT", "SAU", "PSE", "TUR", "MAR"), "Africa e Medio Oriente",
                                         ifelse(dati$Paese %in% c("CAN", "USA", "MEX"), "America del Nord",
                                                ifelse(dati$Paese %in% c("CRI", "SLV", "GTM", "PAN", "JAM", "DOM", 
                                                                         "ARG", "BRA", "CHL", "COL", "PER", "PRY", "URY"), "America Centrale e del Sud", NA))))))

# FREQUENZE GENERE --> T 2.3

freq_genere <- wtd.table(dati$Maschio, weights = dati$W_FSTUWT)
freq_genere_rel <- prop.table(unlist(freq_genere$sum.of.weights))

table(dati$Maschio)
round(prop.table(table(dati$Maschio)) * 100, 2)

# FREQUENZE IMMIGRAZIONE --> T 2.4

table(dati$IMMIG)
round(prop.table(table(dati$IMMIG)) * 100, 2)

freq_immig <- wtd.table(dati$IMMIG, weights = dati$W_FSTUWT)
freq_immig_rel <- prop.table(unlist(freq_immig$sum.of.weights))

# IMMIGRAZIONE E GENERE --> FIG 2.2

dati_immig <- dati %>%
  filter(!is.na(Maschio) & !is.na(IMMIG) & !is.na(W_FSTUWT))

immig_genere <- dati_immig %>%
  group_by(Maschio, IMMIG) %>%
  summarise(Frequenza = sum(W_FSTUWT)) %>%
  mutate(Proporzione = Frequenza / sum(Frequenza))

ggplot(immig_genere, aes(x = Maschio, y = Proporzione, fill = IMMIG)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(Proporzione, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Distribuzione percentuale di Genere e Immigrazione",
       x = "Genere",
       y = "Percentuale") + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = c("Femmina", "Maschio")) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

# ESCS AREA GEOGRAFICA --> FIG 2.3 e T 2.5

dati_ESCS <- dati %>% filter(!is.na(ESCS))

dati_filtrati <- dati_ESCS %>%
  filter(!is.na(ESCS)) %>%
  mutate(ESCS = as.numeric(as.character(ESCS)))

des <- svydesign(ids = ~1, weights = ~W_FSTUWT, data = dati_ESCS)
min_ESCS <- svyquantile(~ESCS, des, 0.0)
q1_ESCS <- svyquantile(~ESCS, des, 0.25)
median_ESCS <- svyquantile(~ESCS, des, 0.5)
media_ESCS <- svymean(~ESCS, des)
q3_ESCS <- svyquantile(~ESCS, des, 0.75) 
max_ESCS <- svyquantile(~ESCS, des, 1.0)

ggplot(data = subset(dati_filtrati, !is.na(ESCS)), aes(x = AREA, y = ESCS, fill = AREA)) +
  geom_boxplot() +
  geom_hline(yintercept = media_ESCS[1], linetype = "dashed", color = "red", size = 1) + 
  labs(title = "Boxplot dell'Indice ESCS per Area Geografica con Media Generale",
       x = "Area Geografica",
       y = "Indice ESCS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PARAGRAFO 2.4

dati <- dati %>% filter(!is.na(SCORE))

# PUNTEGGI PER GENERE --> FIG 2.7

dati_genere <- dati[!is.na(dati$Maschio), ]

ggplot(dati_genere, aes(x = SCORE, fill = factor(Maschio))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  facet_wrap(~ factor(Maschio, levels = c(0, 1), labels = c("Femmina", "Maschio"))) +
  scale_fill_manual(values = c("0" = "#F781BF", "1" = "#377EB8"), labels = c("Femmina", "Maschio")) +
  labs(title = "Distribuzione dei Punteggi Grezzi per Genere",
       x = "Punteggio",
       y = "Frequenza",
       fill = "Genere") +
  theme_minimal()



# PUNTEGGI MEDI IN BASE A ESCS PER IMMIGRAZIONE STUDENTE --> FIG 2.9

dati_clean <- dati %>%
  filter(!is.na(Livello_ESCS) & !is.na(IMMIG_STU))

dati_clean <- dati_clean %>%
  mutate(IMMIG_STU = factor(IMMIG_STU, levels = c(0, 1), labels = c("Non Immigrato", "Immigrato")))

media_punteggi <- dati_clean %>%
  group_by(Livello_ESCS, IMMIG_STU) %>%
  summarise(Media_Score = mean(SCORE, na.rm = TRUE)) %>% ungroup()


ggplot(media_punteggi, aes(x = Livello_ESCS, y = Media_Score, color = IMMIG_STU, group = IMMIG_STU)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Non Immigrato" = "green", "Immigrato" = "darkgreen")) +
  labs(title = "Relazione tra i punteggi medi e il livello ESCS\nper stato di immigrazione dello studente",
       x = "Livello ESCS",
       y = "Punteggio Medio",
       color = "Stato di Immigrazione dello studente") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# PUNTEGGIO MEDIO PER AREA GEOGRAFICA PER IMMIGRAZIONE --> FIG 2.10

dati_clean <- dati%>%
  filter(!is.na(AREA) & !is.na(IMMIG_STU) & !is.na(SCORE))

dati_clean <- dati_clean %>%
  mutate(IMMIG_STU = factor(IMMIG_STU, levels = c(0, 1), labels = c("Non Immigrato", "Immigrato")))

media_punteggi <- dati_clean %>%
  group_by(AREA, IMMIG_STU) %>%
  summarise(Media_Score = mean(SCORE, na.rm = TRUE)) %>% ungroup()

ggplot(media_punteggi, aes(x = AREA, y = Media_Score, color = IMMIG_STU, group = IMMIG_STU))+
  geom_point(size = 2) +
  scale_color_manual(values = c("Non Immigrato" = "green", "Immigrato" = "darkgreen")) +
  labs(title = "Punteggi medi in base all'area geografica\nper immigrazione dello studente",
       x = "Area Geografica",
       y = "Punteggio Medio",
       color = "Stato di Immigrazione dello studente") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GENDER GAP PER AREA --> FIG 2.8

dati_clean <- dati %>%
  filter(!is.na(AREA) & !is.na(SCORE))

punteggi_paesi <- dati_clean %>%
  group_by(AREA) %>%
  summarise(Media_Punteggi = mean(SCORE, na.rm = TRUE)) %>%
  ungroup()

media_globale_totale <- mean(punteggi_paesi$Media_Punteggi, na.rm = TRUE)

media_globale_per_genere <- dati_clean %>%
  group_by(Maschio) %>%
  summarise(Media_Punteggi = mean(SCORE, na.rm = TRUE))

media_per_area_genere <- dati_clean %>%
  group_by(AREA, Maschio) %>%
  summarise(Media_Punteggi = mean(SCORE, na.rm = TRUE))

ggplot(media_per_area_genere, aes(x = AREA, y = Media_Punteggi, color = factor(Maschio), shape = factor(Maschio))) +
  geom_point(size = 3) +  
  labs(title = "Punteggi medi per area e genere",
       x = "Area",
       y = "Media Punteggi",
       color = "Genere",
       shape = "Genere") +
  scale_color_manual(values = c("#F781BF", "#377EB8"), labels = c("Femmine", "Maschi")) + 
  scale_shape_manual(values = c(16, 17), labels = c("Femmine", "Maschi")) +
  ylim(350, 530) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# LIVELLO E VALORI MEDI INDICI --> FIG 2.11

dati_filtrati <- dati %>%
  filter(!is.na(Livello), !is.na(ANXMAT), !is.na(TEACHSUP), !is.na(FAMSUP), !is.na(PERSEVAGR))

dati_media <- dati_filtrati %>%
  group_by(Livello) %>%
  summarise(
    Media_ANXMAT = mean(ANXMAT, na.rm = TRUE),
    Media_TEACHSUP = mean(TEACHSUP, na.rm = TRUE),
    Media_FAMSUP = mean(FAMSUP, na.rm = TRUE),
    Media_PERSEVAGR = mean(PERSEVAGR, na.rm = TRUE)
  ) %>%
  ungroup()

dati_media_long <- dati_media %>%
  pivot_longer(cols = starts_with("Media_"), names_to = "Indice", values_to = "Media")

dati_media_long$Indice <- factor(dati_media_long$Indice, 
                                 levels = c("Media_ANXMAT", "Media_TEACHSUP", "Media_FAMSUP", "Media_PERSEVAGR"),
                                 labels = c("ANXMAT", "TEACHSUP", "FAMSUP", "PERSEVAGR"))

ggplot(dati_media_long, aes(x = factor(Livello), y = Media, color = Indice, group = Indice)) +
  geom_line() +
  geom_point() +
  labs(title = "Media di alcuni indici per livello di proficienza",
       x = "Livello di proficienza",
       y = "Media degli indici") +
  scale_color_manual(values = c("ANXMAT" = "#377EB8", "TEACHSUP" = "#FF7F00", "FAMSUP" = "#984EA3", "PERSEVAGR" = "#4DAF4A"),
                     labels = c("ANXMAT", "TEACHSUP", "FAMSUP", "PERSEVAGR")) +
  theme_minimal()

# RIPETENTI --> FIG 2.12

dati_filtrati <- dati %>%
  filter(!is.na(REPEAT), !is.na(SCORE))

dati_media <- dati_filtrati %>%
  group_by(REPEAT) %>%
  summarise(Media_SCORE = mean(SCORE, na.rm = TRUE)) %>%
  ungroup()

dati_media$REPEAT <- factor(dati_media$REPEAT, 
                            levels = c(0, 1), 
                            labels = c("Non ripetente", "Ripetente"))

ggplot(dati_media, aes(x = REPEAT, y = Media_SCORE, fill = REPEAT)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Punteggi medi per ripetenti e non ripetenti",
       x = "Stato",
       y = "Punteggio medio") +
  scale_fill_manual(values = c("Non ripetente" = "#377EB8", "Ripetente" = "#FF7F00")) +
  theme_minimal()


# PARAGRAFO 3.1

# ESEMPO SOGLIE --> FIG 3.1

thresholds <- c(-Inf, -1, 0, 1, Inf)
Y_star <- seq(-3, 3, by = 0.01)
density <- dnorm(Y_star)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Category = cut(Y_star, breaks = thresholds, labels = c("1", "2", "3", "4")))

ggplot(data, aes(x = Y_star, y = Density, fill = Category)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("blue", "green", "orange", "red")) +
  geom_vline(xintercept = c(-1, 0, 1), linetype = "dashed", color = "black") +
  labs(title = "Rappresentazione grafica del sistema delle soglie",
       x = "predittore lineare",
       y = "Densità") +
  theme_minimal()


# PARAGRAFO 3.2 --> MODELLO CATEGORICO

dati$Livello_ESCS <- relevel(dati$Livello_ESCS, ref = "Livello 3")
dati$AREA <- as.factor(dati$AREA)
dati$AREA <- relevel(dati$AREA, ref = "Europa")

mod = polr(formula = Livello ~ Maschio  + IMMIG +  Livello_ESCS + AREA + REPEAT +ANXMAT + TEACHSUP + FAMSUP + PERSEVAGR +Maschio*IMMIG, 
           data = dati, method=c("logistic"))
summary(mod)

# TABELLA 3.1

stargazer(mod, type = "text",
          title = "Risultati dei Modelli di Regressione Logistica",
          dep.var.labels = "Livello",
          covariate.labels = c("Maschio", "Immig. Prima Generazione", "Immig. Seconda Generazione",
                               "Livello ESCS 1", "Livello ESCS 2", "Livello ESCS 4", "Livello ESCS  5",
                               "Africa e Medio Oriente", "America Centrale e del Sud", "America del Nord", "Asia Orientale", "Oceania",
                               "REPEAT","ANXMAT", "TEACHSUP", "FAMSUP", "PERSEVAGR", "Maschio:Immig.Prima Generazione", "Maschio:Immig. Seconda Generazione"),
          intercept.bottom = FALSE,
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          out = "results.txt")

# GRAFICO BASELINE --> FIG 3.2

palette_colori <- c("turquoise","#F781BF", "#377EB8", "#4DAF4A","#E41A1C" , "#FF7F00","#984EA3" , "#999999")
thresholds <- c(-Inf, -5.1883, -2.7459, -1.1241, 0.2224, 1.5407, 2.9575, 4.5891, Inf)

predittore <- -0.084
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))

ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Baseline",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# MASCHIO BASELINE --> FIG 3.3

predittore <- 0.101
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))

ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Maschio con altre caratteristiche della baseline",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# I GEN BASELINE --> FIG 3.4

predittore <- -0.027
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))

ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Immigrata Prima Generazione",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# II GEN BASELINE --> FIG 3.5

predittore <- -0.106
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))


ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Immigrata Seconda Generazione",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# LIVELLI ESCS BASELINE --> FIG 3.6, 3.7, 3.8, 3.9, 3.10

# Livello 1

predittore <- -1.016
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))


ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Livello 1",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# Livello 2

predittore <- -0.480
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))


ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Livello 2",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# Livello 3

predittore <- -0.084
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))


ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Livello 3 (Baseline)",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# Livello 4

predittore <- 0.402
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))


ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Livello 4",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# Livello 5

predittore <- 1.165
Y_star <- seq(-4, 4, by = 0.01)
density <- dnorm(Y_star, predittore)
data <- data.frame(Y_star = Y_star, Density = density)
data <- data %>%
  mutate(Livello = cut(Y_star, breaks = thresholds, labels = c("livello 1c", 
                                                               "livello 1b", 
                                                               "livello 1a", 
                                                               "livello 2", 
                                                               "livello 3", 
                                                               "livello 4", 
                                                               "livello 5", 
                                                               "livello 6")))

ggplot(data, aes(x = Y_star, y = Density, fill = Livello)) +
  geom_area(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = palette_colori) +
  geom_vline(xintercept = thresholds[-c(1, length(thresholds))], linetype = "dashed", color = "black") +
  geom_vline(xintercept = predittore, linetype = "solid", color = "black") +
  labs(title = "Livello 5",
       x = "Predittore lineare",
       y = "Densità") +
  theme_minimal()

# MODELLO LINEARE E DIAGNOSTICA

mod1 = lm(SCORE ~ Maschio  + IMMIG +  Livello_ESCS + AREA + REPEAT +ANXMAT + TEACHSUP + FAMSUP + PERSEVAGR +Maschio*IMMIG, 
           data = dati)
summary(mod1)

# TABELLA 3.2

stargazer(mod1, type = "text",
          title = "Risultati del Modello di Resgressione Lineare",
          dep.var.labels = "Score",
          covariate.labels = c("Intercetta", "Maschio", "Immig. Prima Generazione", "Immig. Seconda Generazione",
                               "Livello ESCS 1", "Livello ESCS 2", "Livello ESCS 4", "Livello ESCS  5",
                               "Africa e Medio Oriente", "America Centrale e del Sud", "America del Nord", "Asia Orientale", "Oceania",
                               "REPEAT","ANXMAT", "TEACHSUP", "FAMSUP", "PERSEVAGR", "Maschio:Immig. Prima Generazione", "Maschio:Immig. Seconda Generazione"),
          intercept.bottom = FALSE,
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          out = "results.txt")


# Calcolare la devianza residua del modello completo
devianza_residua <- deviance(mod1)
cat("Devianza residua del modello completo:", devianza_residua, "\n")

modello_nullo <- lm(SCORE ~ 1, data = dati)

devianza_nullo <- deviance(modello_nullo)
cat("Devianza del modello nullo:", devianza_nullo, "\n")

# GRAFICi RESIDUI --> FIG

# Valori previsti vs. residui
plot(mod1$fitted.values, mod1$residuals,
     main = "Valori previsti vs Residui",
     xlab = "Valori previsti",
     ylab = "Residui",
     cex = 0.3,
     pch = 16)
abline(h = 0, col = "red", cex = 1.1)

# 4. Creare un istogramma dei residui con sovrapposta la distribuzione normale
hist(mod1$residuals, breaks = 30, freq = FALSE,
     main = "Istogramma dei residui con curva normale",
     xlab = "Residui",
     ylab = "Densità")
curve(dnorm(x, mean = mean(mod1$residuals), sd = sd(mod1$residuals)), 
      col = "blue", lwd = 2, add = TRUE)

# 5. Costruire un QQ plot dei residui
qqnorm(mod1$residuals, main = "QQ plot dei residui", 
       xlab = "Quantili Teorici",
       ylab = "Quantili Campionari",
       cex = 0.7)
qqline(mod1$residuals, col = "red")

# Test di diagnostica

# Test di Durbin-Watson

dwtest(mod1)

# Test di Breusch-Pagan

bptest(mod1)

# Calcolo del Variance Inflation Factor (VIF)

vif(mod1)
