library(readxl)
library(dplyr)
library(knitr)

labels_AgeGroup <- c("18-26", "11-17")###
labels_Race <- c("White", "African American", "Hispanic", "Other/Unknown")
labels_Shots <- c("Received 1 Shot", "Received 2 Shots", "Received 3 Shots")
labels_Completed <- c("Noncompleter", "Completer")
labels_InsuranceType <- c("Public", "Private payer", "Hospital–based",
                          "Military")
labels_MedAssist <- c("Public", "Private")
labels_Location <- c("Johns Hopkins Odenton", "Johns Hopkins White Marsh",
                     "Johns Hopkins Outpatient Center",
                     "Johns Hopkins Bayview Medical Offices")
labels_LocationType <- c("Urban", "Suburban")
labels_PracticeType <- c("Pediatrics", "Family practice", "Gynecology")

gardasil <- read_excel("./data-raw/gardasil.xls") %>%
mutate(., AgeGroup = factor(AgeGroup, 1:0, labels_AgeGroup)) %>%
mutate(., Race = factor(Race, 0:3, labels_Race)) %>%
mutate(., Shots = factor(Shots, 1:3, labels_Shots)) %>%
mutate(., Completed = factor(Completed, 0:1, labels_Completed)) %>%
mutate(., InsuranceType = factor(InsuranceType, 0:3, labels_InsuranceType)) %>%
mutate(., MedAssist = factor(MedAssist, 1:0, labels_MedAssist)) %>%
mutate(., Location = factor(Location, 1:4, labels_Location)) %>%
mutate(., LocationType = factor(LocationType, 1:0, labels_LocationType)) %>%
mutate(., PracticeType = factor(PracticeType, 0:2, labels_PracticeType))

AgeGroup <- function() {
  column_one <- gardasil %>% group_by(AgeGroup) %>% summarise("n (%)" = length(AgeGroup)) %>% mutate("n (%)" = paste0(`n (%)`, " (", round(`n (%)` / sum(`n (%)`) * 100, 1), ")"))
  column_two <- filter(gardasil, Shots == "Received 1 Shot") %>% group_by(AgeGroup) %>% summarise("Received 1 Shot" = length(AgeGroup)) %>% mutate("Received 1 Shot" = paste0(`Received 1 Shot`, " (", round(`Received 1 Shot` / sum(`Received 1 Shot`) * 100, 1), "% ± ", round(sqrt(1 / `Received 1 Shot` * `Received 1 Shot` / sum(`Received 1 Shot`) * (1 - `Received 1 Shot` / sum(`Received 1 Shot`))) * 100, 1), "%)"))
  column_three <- filter(gardasil, Shots == "Received 2 Shots" | Shots == "Received 3 Shots" & Completed == "Noncompleter") %>% group_by(AgeGroup) %>% summarise("Received 2 Shots" = length(AgeGroup)) %>% mutate("Received 2 Shots" = paste0(`Received 2 Shots`, " (", round(`Received 2 Shots` / sum(`Received 2 Shots`) * 100, 1), "% ± ", round(sqrt(1 / `Received 2 Shots` * `Received 2 Shots` / sum(`Received 2 Shots`) * (1 - `Received 2 Shots` / sum(`Received 2 Shots`))) * 100, 1), "%)"))
  column_four <- filter(gardasil, Completed == "Completer") %>% group_by(AgeGroup) %>% summarise("Completer" = length(AgeGroup)) %>% mutate("Completer" = paste0(`Completer`, " (", round(`Completer` / sum(`Completer`) * 100, 1), "% ± ", round(sqrt(1 / `Completer` * `Completer` / sum(`Completer`) * (1 - `Completer` / sum(`Completer`))) * 100, 1), "%)"))
  column_five <- filter(gardasil, Completed == "Noncompleter") %>% group_by(AgeGroup) %>% summarise("Noncompleter" = length(AgeGroup)) %>% mutate("Noncompleter" = paste0(`Noncompleter`, " (", round(`Noncompleter` / sum(`Noncompleter`) * 100, 1), "% ± ", round(sqrt(1 / `Noncompleter` * `Noncompleter` / sum(`Noncompleter`) * (1 - `Noncompleter` / sum(`Noncompleter`))) * 100, 1), "%)"))
  all_columns <- list(column_one, column_two, column_three, column_four, column_five)
  Reduce(full_join, all_columns) %>%
  rename(., Group = AgeGroup)
}

MedAssist <- function() {
  column_one <- gardasil %>% group_by(MedAssist) %>% summarise("n (%)" = length(MedAssist)) %>% mutate("n (%)" = paste0(`n (%)`, " (", round(`n (%)` / sum(`n (%)`) * 100, 1), ")"))
  column_two <- filter(gardasil, Shots == "Received 1 Shot") %>% group_by(MedAssist) %>% summarise("Received 1 Shot" = length(MedAssist)) %>% mutate("Received 1 Shot" = paste0(`Received 1 Shot`, " (", round(`Received 1 Shot` / sum(`Received 1 Shot`) * 100, 1), "% ± ", round(sqrt(1 / `Received 1 Shot` * `Received 1 Shot` / sum(`Received 1 Shot`) * (1 - `Received 1 Shot` / sum(`Received 1 Shot`))) * 100, 1), "%)"))
  column_three <- filter(gardasil, Shots == "Received 2 Shots" | Shots == "Received 3 Shots" & Completed == "Noncompleter") %>% group_by(MedAssist) %>% summarise("Received 2 Shots" = length(MedAssist)) %>% mutate("Received 2 Shots" = paste0(`Received 2 Shots`, " (", round(`Received 2 Shots` / sum(`Received 2 Shots`) * 100, 1), "% ± ", round(sqrt(1 / `Received 2 Shots` * `Received 2 Shots` / sum(`Received 2 Shots`) * (1 - `Received 2 Shots` / sum(`Received 2 Shots`))) * 100, 1), "%)"))
  column_four <- filter(gardasil, Completed == "Completer") %>% group_by(MedAssist) %>% summarise("Completer" = length(MedAssist)) %>% mutate("Completer" = paste0(`Completer`, " (", round(`Completer` / sum(`Completer`) * 100, 1), "% ± ", round(sqrt(1 / `Completer` * `Completer` / sum(`Completer`) * (1 - `Completer` / sum(`Completer`))) * 100, 1), "%)"))
  column_five <- filter(gardasil, Completed == "Noncompleter") %>% group_by(MedAssist) %>% summarise("Noncompleter" = length(MedAssist)) %>% mutate("Noncompleter" = paste0(`Noncompleter`, " (", round(`Noncompleter` / sum(`Noncompleter`) * 100, 1), "% ± ", round(sqrt(1 / `Noncompleter` * `Noncompleter` / sum(`Noncompleter`) * (1 - `Noncompleter` / sum(`Noncompleter`))) * 100, 1), "%)"))
  all_columns <- list(column_one, column_two, column_three, column_four, column_five)
  Reduce(full_join, all_columns) %>%
  rename(., Group = MedAssist)
}

LocationType <- function() {
  column_one <- gardasil %>% group_by(LocationType) %>% summarise("n (%)" = length(LocationType)) %>% mutate("n (%)" = paste0(`n (%)`, " (", round(`n (%)` / sum(`n (%)`) * 100, 1), ")"))
  column_two <- filter(gardasil, Shots == "Received 1 Shot") %>% group_by(LocationType) %>% summarise("Received 1 Shot" = length(LocationType)) %>% mutate("Received 1 Shot" = paste0(`Received 1 Shot`, " (", round(`Received 1 Shot` / sum(`Received 1 Shot`) * 100, 1), "% ± ", round(sqrt(1 / `Received 1 Shot` * `Received 1 Shot` / sum(`Received 1 Shot`) * (1 - `Received 1 Shot` / sum(`Received 1 Shot`))) * 100, 1), "%)"))
  column_three <- filter(gardasil, Shots == "Received 2 Shots" | Shots == "Received 3 Shots" & Completed == "Noncompleter") %>% group_by(LocationType) %>% summarise("Received 2 Shots" = length(LocationType)) %>% mutate("Received 2 Shots" = paste0(`Received 2 Shots`, " (", round(`Received 2 Shots` / sum(`Received 2 Shots`) * 100, 1), "% ± ", round(sqrt(1 / `Received 2 Shots` * `Received 2 Shots` / sum(`Received 2 Shots`) * (1 - `Received 2 Shots` / sum(`Received 2 Shots`))) * 100, 1), "%)"))
  column_four <- filter(gardasil, Completed == "Completer") %>% group_by(LocationType) %>% summarise("Completer" = length(LocationType)) %>% mutate("Completer" = paste0(`Completer`, " (", round(`Completer` / sum(`Completer`) * 100, 1), "% ± ", round(sqrt(1 / `Completer` * `Completer` / sum(`Completer`) * (1 - `Completer` / sum(`Completer`))) * 100, 1), "%)"))
  column_five <- filter(gardasil, Completed == "Noncompleter") %>% group_by(LocationType) %>% summarise("Noncompleter" = length(LocationType)) %>% mutate("Noncompleter" = paste0(`Noncompleter`, " (", round(`Noncompleter` / sum(`Noncompleter`) * 100, 1), "% ± ", round(sqrt(1 / `Noncompleter` * `Noncompleter` / sum(`Noncompleter`) * (1 - `Noncompleter` / sum(`Noncompleter`))) * 100, 1), "%)"))
  all_columns <- list(column_one, column_two, column_three, column_four, column_five)
  Reduce(full_join, all_columns) %>%
  rename(., Group = LocationType)
}

PracticeType <- function() {
  column_one <- gardasil %>% group_by(PracticeType) %>% summarise("n (%)" = length(PracticeType)) %>% mutate("n (%)" = paste0(`n (%)`, " (", round(`n (%)` / sum(`n (%)`) * 100, 1), ")"))
  column_two <- filter(gardasil, Shots == "Received 1 Shot") %>% group_by(PracticeType) %>% summarise("Received 1 Shot" = length(PracticeType)) %>% mutate("Received 1 Shot" = paste0(`Received 1 Shot`, " (", round(`Received 1 Shot` / sum(`Received 1 Shot`) * 100, 1), "% ± ", round(sqrt(1 / `Received 1 Shot` * `Received 1 Shot` / sum(`Received 1 Shot`) * (1 - `Received 1 Shot` / sum(`Received 1 Shot`))) * 100, 1), "%)"))
  column_three <- filter(gardasil, Shots == "Received 2 Shots" | Shots == "Received 3 Shots" & Completed == "Noncompleter") %>% group_by(PracticeType) %>% summarise("Received 2 Shots" = length(PracticeType)) %>% mutate("Received 2 Shots" = paste0(`Received 2 Shots`, " (", round(`Received 2 Shots` / sum(`Received 2 Shots`) * 100, 1), "% ± ", round(sqrt(1 / `Received 2 Shots` * `Received 2 Shots` / sum(`Received 2 Shots`) * (1 - `Received 2 Shots` / sum(`Received 2 Shots`))) * 100, 1), "%)"))
  column_four <- filter(gardasil, Completed == "Completer") %>% group_by(PracticeType) %>% summarise("Completer" = length(PracticeType)) %>% mutate("Completer" = paste0(`Completer`, " (", round(`Completer` / sum(`Completer`) * 100, 1), "% ± ", round(sqrt(1 / `Completer` * `Completer` / sum(`Completer`) * (1 - `Completer` / sum(`Completer`))) * 100, 1), "%)"))
  column_five <- filter(gardasil, Completed == "Noncompleter") %>% group_by(PracticeType) %>% summarise("Noncompleter" = length(PracticeType)) %>% mutate("Noncompleter" = paste0(`Noncompleter`, " (", round(`Noncompleter` / sum(`Noncompleter`) * 100, 1), "% ± ", round(sqrt(1 / `Noncompleter` * `Noncompleter` / sum(`Noncompleter`) * (1 - `Noncompleter` / sum(`Noncompleter`))) * 100, 1), "%)"))
  all_columns <- list(column_one, column_two, column_three, column_four, column_five)
  Reduce(full_join, all_columns) %>%
  rename(., Group = PracticeType)
}

Race <- function() {
  column_one <- gardasil %>% group_by(Race) %>% summarise("n (%)" = length(Race)) %>% mutate("n (%)" = paste0(`n (%)`, " (", round(`n (%)` / sum(`n (%)`) * 100, 1), ")"))
  column_two <- filter(gardasil, Shots == "Received 1 Shot") %>% group_by(Race) %>% summarise("Received 1 Shot" = length(Race)) %>% mutate("Received 1 Shot" = paste0(`Received 1 Shot`, " (", round(`Received 1 Shot` / sum(`Received 1 Shot`) * 100, 1), "% ± ", round(sqrt(1 / `Received 1 Shot` * `Received 1 Shot` / sum(`Received 1 Shot`) * (1 - `Received 1 Shot` / sum(`Received 1 Shot`))) * 100, 1), "%)"))
  column_three <- filter(gardasil, Shots == "Received 2 Shots" | Shots == "Received 3 Shots" & Completed == "Noncompleter") %>% group_by(Race) %>% summarise("Received 2 Shots" = length(Race)) %>% mutate("Received 2 Shots" = paste0(`Received 2 Shots`, " (", round(`Received 2 Shots` / sum(`Received 2 Shots`) * 100, 1), "% ± ", round(sqrt(1 / `Received 2 Shots` * `Received 2 Shots` / sum(`Received 2 Shots`) * (1 - `Received 2 Shots` / sum(`Received 2 Shots`))) * 100, 1), "%)"))
  column_four <- filter(gardasil, Completed == "Completer") %>% group_by(Race) %>% summarise("Completer" = length(Race)) %>% mutate("Completer" = paste0(`Completer`, " (", round(`Completer` / sum(`Completer`) * 100, 1), "% ± ", round(sqrt(1 / `Completer` * `Completer` / sum(`Completer`) * (1 - `Completer` / sum(`Completer`))) * 100, 1), "%)"))
  column_five <- filter(gardasil, Completed == "Noncompleter") %>% group_by(Race) %>% summarise("Noncompleter" = length(Race)) %>% mutate("Noncompleter" = paste0(`Noncompleter`, " (", round(`Noncompleter` / sum(`Noncompleter`) * 100, 1), "% ± ", round(sqrt(1 / `Noncompleter` * `Noncompleter` / sum(`Noncompleter`) * (1 - `Noncompleter` / sum(`Noncompleter`))) * 100, 1), "%)"))
  all_columns <- list(column_one, column_two, column_three, column_four, column_five)
  Reduce(full_join, all_columns) %>%
  rename(., Group = Race)
}

`Table 1` <- function() {
  AgeGroup <- AgeGroup()
  MedAssist <- MedAssist()
  LocationType <- LocationType()
  PracticeType <- PracticeType()
  Race <- Race()
  bind_rows(AgeGroup, MedAssist, LocationType, PracticeType, Race)
}


filter(gardasil, Race != "Other/Unknown") %>% mutate(., Minority = ifelse(Race == "White", 0, 1)) %>% mutate(., Minority = factor(Minority, 1:0, c("Not Minority", "Minority"))) %>%
glm(Completed ~ AgeGroup + InsuranceType + LocationType + PracticeType + Race, family = binomial, data = .) %>% coef() %>% exp()





