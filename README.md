# BMIN503/EPID600 Final Project

This repository contains templates for your final written report and GitHub repository. Follow the instructions below to clone this repository, and then turn in your final project's code via a pull request to this repository.


1. To start, **fork** this BMIN503_Final_Project repository.
1. **Clone** the forked repository to your computer.
1. Modify the files provided, add your own, and **commit** changes to complete your final project.
1. **Push**/sync the changes up to your GitHub account.
1. Create a **pull request** on this, the original BMIN503_Final_Project, repository to turn in your final project.


Follow the instructions [here][forking] if you are unsure what the above steps mean.

DUE DATE FOR FINAL VERSION: 12/10/21 11:59PM. This is a hard deadline. Turn in whatever you have by this date.

---
title: "Distinguishing some relationships of asthma in the US from 2017-March 2020"
author: "Kelli Williams"
output: 
  html_document:
    theme: paper 
    highlight: tango
---

### Introduction 
Asthma is a chronic inflammatory disease affecting the respiratory system, which causes difficulty in breathing, and therefore requires ongoing medical management.  Although asthma is not curable, it is highly treatable. According to the 2016 National Health Interview Survey data, lifetime asthma prevalence was 13.9% for adults (18+ years of age), which gives the population estimate of 625,000.   Social determinants of health affect health-related outcomes. Since asthma is highly treatable, differences in outcomes could be associated with social factors, and determining which ones are the most significant in asthma can aid in mitigating the burden of asthma on individuals, families, and healthcare systems.  In this study, we will determine relationships of asthma and some social determinants of health in the United States by using the NHANES 2017- March 2020 questionnaire.
	A multidisciplinary approach is necessary to address any association between asthma and social determinants of health.  To treat asthma patients, medical providers will need this valuable information in assessing care, which will involve the fields of social work, psychology, scientific researchers such as environmentalist, etc. In addition, the business community is necessary to determine necessary risk-reward models such as cost of lost of work vs. better work environments, or hospital healthcare costs per emergency room visit vs. community clinics. In determining any relationships between asthma and some social determinants of health, the expansion of the number of questionnaires being used for analysis must include at minimum topics about basic demographics, hospital utilization, access to care, medical conditions (as discussed with advisors).

### Methods
Describe the data used and general methodological approach. Subsequently, incorporate full R code necessary to retrieve and clean data, and perform analysis. Be sure to include a description of code so that others (including your future self) can understand what you are doing and why. 
```{r}
#install.packages("survey")
#install.packages("boot")
library(haven)
library(nhanesA) 
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
#remotes::install_github("ddsjoberg/gtsummary")
library(gtsummary)
#Import data file after making Stata file from SAS file
P_MCQ <- read_dta("NHANES original data files/P_MCQ.dta")
P_DEMO <- read_dta("NHANES original data files/P_DEMO.dta")
P_HIQ <- read_dta("NHANES original data files/P_HIQ.dta")
P_HUQ <- read_dta("NHANES original data files/P_HUQ.dta")

#Redefine variables for MCQ_J/medical conditions
P_MCQ2 <- P_MCQ %>%
  dplyr::mutate(mcq010 = factor(mcq010, levels =  c(1, 2), 
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq035 = factor(mcq035, levels = c(1, 2), 
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq040 = factor(mcq040, levels = c(1, 2), 
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq050 = factor(mcq050, levels = c(1, 2), 
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160b = factor(mcq160b, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160c = factor(mcq160c, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160d = factor(mcq160d, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160e = factor(mcq160e, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160f = factor(mcq160f, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160m = factor(mcq160m, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160m = factor(mcq160p, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq160l = factor(mcq160l, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq500 = factor(mcq500, levels = c(1, 2), 
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq220 = factor(mcq220, levels = c(1, 2), 
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq371a = factor(mcq371a, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq371b = factor(mcq371b, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq371c = factor(mcq371c, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::mutate(mcq371d = factor(mcq371d, levels = c(1, 2), 
                                 labels =c("Yes", "No"))) %>%
  dplyr::rename(id = seqn, everasthma = mcq010, asthma = mcq035, 
                asthmaattack = mcq040, erasthma = mcq050, 
                chf =  mcq160b, chd = mcq160c, angina = mcq160d, 
                mi = mcq160e,stroke = mcq160f, everthyroid = mcq160m, 
                thyroid = mcq170m, lungdx = mcq160p, everliver = mcq160l, liver = mcq170l, 
                cancer = mcq220, losingweight = mcq371a, exercise = mcq371b, 
                reducesalt = mcq371c, reducefat = mcq371d) %>%
  dplyr::select(id, everasthma, asthma, asthmaattack, erasthma, 
                chf, chd, angina, mi, stroke, everthyroid, thyroid, lungdx, 
                everliver, liver, cancer, losingweight, exercise, reducesalt,
                reducefat)

#Redefine variables for P_DEMO/demographics
P_DEMO2 <- P_DEMO %>%
  dplyr::mutate(riagendr = factor(riagendr, 
                                  levels =  c(1, 2), labels =c("Male", "Female"))) %>%
  dplyr::mutate(ridreth3 = factor(ridreth3, 
                                  levels = c(1, 2, 3, 4, 5, 7), 
                                  labels = c("Mexican American", "Other Hispanic",
                                             "Non-Hispanic White", "Non-Hispanic Black",
                                             "Non-Hispanic Asian", 
                                             "Other Race- Including Multiracial"))) %>%
  dplyr::mutate(dmdeduc2 = factor(dmdeduc2, 
                                  levels = c(1, 2, 3, 4, 5), 
                                  labels = c("< 9th grade", "9th-11th grade",
                                             "High school graduate/GED", 
                                             "Some college or AA degree", 
                                             "College graduate or above"))) %>%
  dplyr::mutate(dmdmartz = factor(dmdmartz, 
                                  levels = c(1, 2, 3), 
                                  labels =c("Married/Living with Partner",
                                            "Widowed/Divorced/Separated", 
                                            "Single"))) %>%
  dplyr::rename(id = seqn, gender = riagendr, age = ridageyr, 
           race = ridreth3, education = dmdeduc2,
           maritalstatus = dmdmartz, poverty = indfmpir) %>%
  dplyr::select(id, gender, age, race, education, maritalstatus, poverty)
  
#Redefine variables for P_HIQ/health insurance
P_HIQ2 <- P_HIQ %>%
  dplyr::mutate(hiq011 = factor(hiq011, levels =  c(1, 2),
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(hiq032a = factor(hiq032a, levels = c(1),
                                 labels = c("Yes"))) %>%
  dplyr::mutate(hiq032b = factor(hiq032b, levels = c(2),
                                 labels = c("Yes"))) %>%
  dplyr::mutate(hiq032c = factor(hiq032c, levels = c(3),
                                 labels =c("Yes"))) %>%
  dplyr::mutate(hiq032d = factor(hiq032d, levels = c(4),
                                 labels =c("Yes"))) %>%
  dplyr::mutate(hiq032e = factor(hiq032e, levels = c(5),
                                 labels =c("Yes"))) %>%
  dplyr::mutate(hiq032h = factor(hiq032h, levels = c(8),
                                 labels =c("Yes"))) %>%
  dplyr::mutate(hiq032i = factor(hiq032i, levels = c(9),
                                 labels =c("Yes"))) %>%
  dplyr::mutate(hiq260 = factor(hiq260, levels = c(1, 2),
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(hiq270 = factor(hiq270, levels = c(1, 2),
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(hiq210 = factor(hiq210, levels = c(1, 2),
                                labels =c("Yes", "No"))) %>%
  dplyr::rename(id = seqn, insurancebinary = hiq011, privateinsurance = hiq032a, 
           medicare = hiq032b, medigap = hiq032c, medicaid = hiq032d, 
           chip = hiq032e, statesponsored = hiq032h, othergovernment = hiq032i, 
           havemedicare = hiq260, prescription_insurance = hiq270,
           noinsuranceyear = hiq210) %>%
  dplyr::select(id, insurancebinary, privateinsurance, medicare,
                medigap, medicaid, chip, statesponsored, othergovernment,
                havemedicare, prescription_insurance, noinsuranceyear)

#Redefine variables for P_HUQ/Hospital Utilization & Access to Care
#should I use HUD062 or just HUQ062????????
P_HUQ2 <- P_HUQ %>%
  dplyr::mutate(hud062 = factor(hud062, levels =  c(1, 2, 3, 4),
                                labels =c("anytime less than 12 months ago",
                                          "1 year but less than 2 years ago",
                                          "within 3 years but less than 5 years",
                                          "within 5 years but less than 10 years"))) %>%
  dplyr::mutate(huq010 = factor(huq010, levels = c(1, 2, 3, 4, 5), 
                                labels = c("Excellent", "Very good", "Good",
                                           "Fair", "Poor"))) %>%
  dplyr::mutate(huq030 = factor(huq030, levels = c(1, 2, 3),
                                labels = c("Yes", "There is no place",
                                           "There is more than one place"))) %>%
  dplyr::mutate(huq051 = factor(huq051, levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
                                labels =c("None", "1", "2 to 3", "4 to 5",
                                          "6 to 7", "8 to 9", "10 to 12",
                                          "13 to 15", "16 or more"))) %>%
  dplyr::mutate(huq071 = factor(huq071, levels = c(1, 2),
                                labels =c("Yes", "No"))) %>%
  dplyr::mutate(huq090 = factor(huq090, levels = c(1, 2),
                                labels =c("Yes", "No"))) %>%
  dplyr::rename(id = seqn, combinationdoctor = hud062, healthcondition = huq010, 
           routinehealthcare = huq030, numberhealthcare = huq051, 
           overnighthospital = huq071, mentalhealthvisit = huq090) %>%
  dplyr::select(id, combinationdoctor, healthcondition, routinehealthcare,
                numberhealthcare, overnighthospital, mentalhealthvisit)

#Link datasets by SEQN/ID
nhanes2017 <- Reduce(function(x, y) merge (x = x, y = y, all = TRUE, by = "id"), 
                     list(P_MCQ2, P_DEMO2, P_HIQ2, P_HUQ2))
#na.omit(nhanes2017)
```

```{r}
#How many NA's for each variable?
sapply(nhanes2017, function(x) sum(is.na(x)))

#View of some variables
head(nhanes2017)
count_unique <- rapply(nhanes2017,function(x) length(unique(x)))
#There are 15,560 subjects in dataset
table(nhanes2017$gender) #males = 7721 & females = 7839
#table(nhanes2017$huq062) # "Combination of HUQ061 and HUQ062"
table(nhanes2017$everasthma) #number of "ever been told you have asthma" = 2322
table(nhanes2017$asthmaattack) #number of "Had asthma attack in past year" = 636
table(nhanes2017$asthma) #number of "still have asthma" = 1423
table(nhanes2017$erasthma) #number of "ER visit for asthma/past yr" = 295
summary(nhanes2017$age) #ages

#Table 1: Descriptive table: https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html; https://cran.r-project.org/web/packages/Gmisc/vignettes/Descriptives.html; https://www.danieldsjoberg.com/gtsummary/

nhanes2017_dem <- nhanes2017 %>%
  select(asthma, everasthma, asthmaattack, gender, age, race, education,
         maritalstatus, poverty, insurancebinary, privateinsurance, medicare,
         medigap, medicaid, chip, statesponsored, othergovernment, havemedicare,
         prescription_insurance, noinsuranceyear)
table1 <- tbl_summary(nhanes2017_dem)
nhanes2017_dem$asthma <- forcats::fct_explicit_na(nhanes2017_dem$asthma)
table2 <- tbl_summary(nhanes2017_dem, by = asthma, missing = "no") %>%
  add_n() %>%
  add_p(simulate.p.value=TRUE) %>%
  add_overall() %>%
  add_stat_label() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Table 1. Decriptive Characteristics by Asthma Status**") %>%
  bold_labels
```

<!-- Links -->
[forking]: https://guides.github.com/activities/forking/

