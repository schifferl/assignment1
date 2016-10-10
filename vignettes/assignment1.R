## ----include=FALSE-------------------------------------------------------
library(magrittr)
library(readxl)
library(dplyr)
library(knitr)
library(assignment1)

## ------------------------------------------------------------------------
labels_AgeGroup <- c("18-26", "11-17")
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

## ------------------------------------------------------------------------
gardasil <- read_excel("../inst/extdata/gardasil.xls") %>%
mutate(., AgeGroup = factor(AgeGroup, 1:0, labels_AgeGroup)) %>%
mutate(., Race = factor(Race, 0:3, labels_Race)) %>%
mutate(., Shots = factor(Shots, 1:3, labels_Shots)) %>%
mutate(., Completed = factor(Completed, 0:1, labels_Completed)) %>%
mutate(., InsuranceType = factor(InsuranceType, 0:3, labels_InsuranceType)) %>%
mutate(., MedAssist = factor(MedAssist, 1:0, labels_MedAssist)) %>%
mutate(., Location = factor(Location, 1:4, labels_Location)) %>%
mutate(., LocationType = factor(LocationType, 1:0, labels_LocationType)) %>%
mutate(., PracticeType = factor(PracticeType, 0:2, labels_PracticeType))

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
Table_1() %>%
kable(col.names = c("", "n (%)", "Received 1 Shot", "Received 2 Shots", 
                            "Completer", "Noncompleter"),
      caption = "Table 1. Demographics of Study Cohort")

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
Table_2() %>%
kable(caption = "Table 2. Human Papillomavirus Vaccination Completion Rates, Univariable Analysis (N=1,413)")

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
Table_3() %>% 
kable(caption = "Table 3. Multivariable Logistic Regression Analysis of Predictors of Human Papillomavirus Vaccine Completion")

