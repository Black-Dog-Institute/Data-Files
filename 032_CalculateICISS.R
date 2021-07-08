---
title:  "032_CalculateICISS.Rmd"
authors: "Patrick McElduff, Lisa Sharwood"
date:   "17 January 2018"
output: 
    html_document:
      toc: yes
      css: hpa_style.css
      classoption: landscape
---


```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)

rm(list=ls())


#library(ggplot2)
library(readxl)
library(knitr)
library(tidyverse)
library(lubridate)

```


```{r read data sets, include= FALSE}

# Import the file for those who meet the definition (ISH admitted episodes)
Incident_Episodes <- readRDS("Data\\Incident_Episodes.rds")
APDC <- readRDS("Data\\APDC.rds")

diagnosis <- Incident_Episodes %>%
  left_join(., APDC %>% select(PPN, project_recid, diagnosis_codeP:diagnosis_code50)) %>%
  select(c(PPN, project_recid, starts_with("diagnosis_code"))) 

# check <- diagnosis %>%
#   filter(PPN %in% c("1219293", "442459", "3557143", "3955863"))

ICISS_codes <- read_excel("Source\\Lisa\\TA_76_2_2013_12_22_GEDEBORG_13-01468_SDC7.xlsx") %>%
  select(icd10code, DSP)

# code_list <- as.vector(ICISS_codes$icd10code)
# 
# diagnosis2 <- diagnosis %>%
#   mutate(row = seq_len(n()))
  
ICISS_values = diagnosis %>%
  select(PPN, project_recid, starts_with("diagnosis_code")) %>%
  mutate_at(funs(ifelse(is.na(.)|. == "", "NA", .)), .vars=vars(c(diagnosis_codeP:diagnosis_code50))) %>%
  mutate_at(funs(sub("\\.", "", .)), .vars=vars(c(diagnosis_codeP:diagnosis_code50))) %>%   
  mutate_at(funs(substr(., 1, 4)), .vars=vars(c(diagnosis_codeP:diagnosis_code50))) %>%
  gather(., codes, icd10code, -c(PPN, project_recid)) %>%
  group_by(PPN, project_recid, icd10code) %>%
  tally() %>%
  ungroup() %>%
  left_join(., ICISS_codes) %>%
  filter(is.na(DSP) == FALSE) %>%
  group_by(PPN, project_recid) %>%
  summarise(ICISS_value = prod(DSP)) %>%
  ungroup() %>%
  group_by(PPN) %>%
  summarise(ICISS_value = max(ICISS_value, na.rm = TRUE)) %>%
  ungroup()

check <- ICISS_values %>%
   filter(PPN %in% c("1219293", "442459", "3357143", "3955863"))
  
hist(ICISS_values$ICISS_value)

saveRDS(ICISS_values, "Data\\ICISS_values.rds")

```




