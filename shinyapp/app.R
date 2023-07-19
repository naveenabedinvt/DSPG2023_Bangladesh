# Load Packages ---------------------------------------------------------------
library(dplyr)
library(tidycensus)
library(ggplot2)
library(mapdeck)
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(maps)
library(plotly)
library(DT)
library(tigris)
library(tidyverse)
library(tidycensus)
library(readxl)
library(collapsibleTree)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(rvest)
library(sf)
library(shinydashboard)
library(shinydashboardPlus)
library(tidygeocoder)
library(janitor)
library(ggplot2)
library(magrittr)
options(tigris_use_cache = TRUE)
library(ggwordcloud)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(readr)
library(stringr)
library(mapdata)
library(htmlwidgets)
library(leafpop)
library(lattice)
library(htmltools)
library(leaflegend)
library(ggplotify)
library(grid)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(shinyWidgets)
library(viridis)
library(psych)
library(knitr)
library(stargazer)
library(tidyverse)
library(dplyr)
library(fpp3)
library(gridExtra) 
library(GGally)
library(readxl)
library(haven)
library(foreign)
library(epiDisplay)
library(ggeasy)
library(patchwork)
library (mice)
library(plotly)

#---------------------------data1: Household Memebers---------------------------------------------------------------------
data_mem <- read_dta(paste0(getwd(),"/data/BIHS2018-19Members_Jun3.dta")) 
data <- read_dta(paste0(getwd(),"/data/BIHS2018-19MC_Jun4.dta"))
chirpsdata<- read_dta(paste0(getwd(),"/data/BIHS2018-19_MC_CHIRPS_Jul5_IDFormatted.dta"))


#Age by Gender

g <- data_mem %>%
  count(gender) %>%
  rename(Gender = gender, Total = n) %>%
  mutate(Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender, "0" = "Male", "1" = "Female")) %>%
  bind_rows(data.frame(Gender = "Total", Total = sum(.$Total)))

avg <- data_mem %>%
  rename("Division" = div_name, "Gender" = gender) %>% 
  mutate(Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender,
                         "1" = "Female",
                         "0" = "Male")) %>%
  group_by(Division, Gender) %>%
  summarize(Mean_age = round(mean(age), 2),
            std_dev = round(sd(age), 2),
            count = n(), .groups = "drop")



avg$Gender <- factor(avg$Gender, levels = c("Male", "Female"))

#Male Age Distribution 
age_div_male <- data_mem %>% 
  filter(gender == 0) %>% 
  rename("Division" = div_name, "Gender" = gender, "Age_range" = age_categorical) %>% 
  group_by(Division) %>% 
  count(Age_range) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Age_range = as.character(haven::zap_labels(Age_range)),
         Age_range = recode(Age_range, "0" =  "0-5 yrs",
                            "1" = "6-10 yrs",
                            "2" = "11-17 yrs",
                            "3" = "18-30 yrs",
                            "4" = "31-65 yrs",
                            
                            "5" = "66-80 yrs",
                            "6" = "80+ yrs"))

#Female Age Distribution 

age_div_female<-data_mem%>% 
  filter(gender == 1) %>% 
  rename("Division" = div_name, "Gender" = gender, "Age_range" = age_categorical) %>% 
  group_by(Division) %>% 
  count(Age_range) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Age_range = as.character(haven::zap_labels(Age_range)),
         Age_range = recode(Age_range, "0" =  "0-5 yrs",
                            "1" = "6-10 yrs",
                            "2" = "11-17 yrs",
                            "3" = "18-30 yrs",
                            "4" = "31-65 yrs",
                            
                            "5" = "66-80 yrs",
                            "6" = "80+ yrs"))



edu_div_male <- data_mem %>% 
  filter(gender == 0) %>% 
  rename("Division" = div_name, "Gender" = gender, "Education_level" = educ) %>% 
  group_by(Division) %>% 
  count(Education_level) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Education_level = as.character(haven::zap_labels(Education_level)),
         Education_level = recode(Education_level,  "4" = "Higher",
                                  "3" = "Completed secondary",
                                  "2" = "Completed primary",
                                  "1" = "Less than primary",
                                  "0" = "No education"))


edu_div_male$Education_level <- factor(edu_div_male$Education_level, levels = c("Higher",
                                                                                "Completed secondary",
                                                                                "Completed primary",
                                                                                "Less than primary",
                                                                                "No education"))
## Female education by division

edu_div_female <- data_mem %>% 
  filter(gender == 1) %>% 
  rename("Division" = div_name, "Gender" = gender, "Education_level" = educ) %>% 
  group_by(Division) %>% 
  count(Education_level) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Education_level = as.character(haven::zap_labels(Education_level)),
         Education_level = recode(Education_level,  "4" = "Higher",
                                  "3" = "Completed secondary",
                                  "2" = "Completed primary",
                                  "1" = "Less than primary",
                                  "0" = "No education"))

edu_div_female$Education_level <- factor(edu_div_female$Education_level,
                                         levels = c("Higher",
                                                    "Completed secondary",
                                                    "Completed primary",
                                                    "Less than primary",
                                                    "No education"))

#-----------------------Data: About households ---------------------------------
data_h <- read_dta(paste0(getwd(),"/data/BIHS2018-19HH_Jun3.dta"))
attach(data_h)
#-------------------------------------------------------------------------------

## Household size by division

HH_size <- data_h %>% 
  rename("Division" = div_name,  "HH_size" = hhsize) %>% 
  group_by(Division) %>% 
  summarise("Average_Household_size" = round(mean(HH_size), 2),
            "std_dev" = round(sd(HH_size), 2),
            count = n())

## Dependency ration

dep_r <- data_h %>% 
  rename("Division" = div_name,  "Dependency_ratio" = depratio) %>% 
  group_by(Division) %>% 
  summarise("Dependency_Ratio" = round(mean(Dependency_ratio), 2),
            "std_dev" = round(sd(Dependency_ratio, na.rm = FALSE), 2),
            count = n()) 

## Highest education in the household

hh_edu_div <- data_h %>%
  rename(Division = div_name, Education_level  = maxeduc) %>%
  group_by(Division, Education_level) %>%
  count() %>%
  ungroup() %>%
  group_by(Division) %>%
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Education_level = as.character(Education_level),  # Convert to character
         Education_level = dplyr::recode(Education_level,
                                         "0" = "No education",
                                         "1" = "Less than primary",
                                         "2" = "Completed primary",
                                         "3" = "Completed secondary",
                                         "4" = "Higher"))

hh_edu_div$Education_level <- factor(hh_edu_div$Education_level,
                                     levels = c("Higher",
                                                "Completed secondary",
                                                "Completed primary",
                                                "Less than primary",
                                                "No education"))

## Household invovement in farming activities 

hh_rfarm <- data_h %>%
  rename(Division = div_name, Crop_Farming = cropfarming, Rice_Farming = ricefarming, Livestock_Farming = livestockfarming, Fish_Farming = fishfarming, Farm_HH = farmhh) %>%
  pivot_longer(cols = c(Crop_Farming, Rice_Farming, Livestock_Farming, Fish_Farming, Farm_HH), names_to = "Activity", values_to = "Value") %>%
  group_by(Division, Activity, Value) %>%
  count() %>%
  ungroup() %>%
  group_by(Division, Activity) %>%
  mutate(Total = sum(n),
         Percentage = round(n / Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Activity = as.character(Activity),
         Activity = dplyr::recode(Activity,
                                  "Crop_Farming" = "Crop Farming",
                                  "Rice_Farming" = "Rice Farming",
                                  "Livestock_Farming" = "Livestock Farming",
                                  "Fish_Farming" = "Fish Farming",
                                  "Farm_HH" = "Households in Farming")) %>%
  filter(Value != 0)




hh_rfarm$Activity <- factor(hh_rfarm$Activity, levels = c("Households in Farming", "Crop Farming", "Rice Farming", "Livestock Farming", "Fish Farming"))

## distribution of cultivable land holding, by division

cult_land_div <- data_h %>%
  rename(Division = div_name, Farm_size = farmgroup) %>%
  group_by(Division, Farm_size) %>%
  count() %>%
  ungroup() %>%
  group_by(Division) %>%
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Farm_size = as.character(Farm_size),  # Convert to character
         Farm_size = dplyr::recode(Farm_size,
                                   "0" = "No cultivable land",
                                   
                                   "1" =  "Marginal: <0.5 acres",  
                                   
                                   "2" = "Small: 0.5-1.5 acres",  
                                   
                                   "3" = "Medium: 1.5-2.5 acres",  
                                   
                                   "4" = "Large: 2.5+ acres"
                                   
                                   
         ))

## Poverty rate by division 

under_ppp<-data_h%>% 
  rename("Division" = div_name, "Categories" = poorhh) %>% 
  group_by(Division) %>% 
  count(Categories) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Categories = as.character(haven::zap_labels(Categories)),
         Categories = recode(Categories, "1" =  "< USD 1.96 2011 PPP",
                             "0" = "> USD 1.96 2011 PPP",
         )) %>% 
  filter(Categories == "< USD 1.96 2011 PPP")


## Household access to electricity

eletri<-data_h%>% 
  rename("Division" = div_name, "Electricity" = electricity) %>% 
  
  group_by(Division) %>% 
  count(Electricity) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>% 
  # filter(electricity == 0) %>%
  ungroup() %>%
  
  mutate(Electricity = as.character(haven::zap_labels(Electricity)),
         Electricity = recode(Electricity, "1" =  "With electricity",
                              "0" = "Without electricity",
         )) %>% 
  filter(Electricity == "Without electricity")

## Household access to improved drinking water source

imp_h2o<-data_h%>% 
  rename("Division" = div_name, "Improved_water" = improvedwater) %>% 
  group_by(Division) %>% 
  count(Improved_water) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Improved_water = as.character(haven::zap_labels(Improved_water)),
         Improved_water = recode(Improved_water, "1" =  "Unimproved_water",
                                 "0" = " Improved_water",
         )) %>% 
  filter(Improved_water == "Unimproved_water")

## Households and water treatment 

filt_h2o<-data_h%>% 
  rename("Division" = div_name, "Untreated_water" = untreatedwater) %>% 
  group_by(Division) %>% 
  count(Untreated_water) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Untreated_water = as.character(haven::zap_labels(Untreated_water)),
         Untreated_water = recode(Untreated_water, "1" =  "Untreated Water",
                                  "0" = "Treated Water",
         )) %>% 
  filter(Untreated_water == "Untreated Water")

## Household headship

hhh_gender<-data_h%>% 
  rename("Division" = div_name, "Gender" = headgender) %>% 
  group_by(Division) %>% 
  count(Gender) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender, "0" =  "Male",
                         "1" = "Female",
         ))

hhh_gender$Gender <- factor(hhh_gender$Gender,
                            levels = c("Male",
                                       "Female"))

## HH_head age distribution

hhh_age<-data_h%>% 
  rename("Division" = div_name, "Gender" = headgender, "Age_category" = headagecat) %>% 
  group_by(Division) %>% 
  count(Age_category) %>% 
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Age_category = as.character(haven::zap_labels(Age_category)),
         Age_category = recode(Age_category, "0" =  "0-5 yrs",
                               "1" = "6-10 yrs",
                               "2" = "11-17 yrs",
                               "3" = "18-30 yrs",
                               "4" = "31-65 yrs",
                               
                               "5" = "66-80 yrs",
                               "6" = "80+ yrs"))

## HH head education attainment

hhh_edu_div <- data_h %>%
  rename(Division = div_name, Education_level = headeduc) %>%
  group_by(Division, Education_level) %>%
  count() %>%
  ungroup() %>%
  group_by(Division) %>%
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Education_level = as.character(Education_level),  # Convert to character
         Education_level = dplyr::recode(Education_level,
                                         "0" = "No education",
                                         "1" = "Less than primary",
                                         "2" = "Completed primary",
                                         "3" = "Completed secondary",
                                         "4" = "Higher"))

hhh_edu_div$Education_level <- factor(hhh_edu_div$Education_level,
                                      levels = c("Higher",
                                                 "Completed secondary",
                                                 "Completed primary",
                                                 "Less than primary",
                                                 "No education"))

## HHH main occupation

hhh_main_occup <- data_h %>%
  rename(Division = div_name, Occupations = headoccupation) %>%
  group_by(Division, Occupations) %>%
  count() %>%
  ungroup() %>%
  group_by(Division) %>%
  mutate(Total = sum(n),
         Percentage = round(n/Total * 100, 2)) %>%
  ungroup() %>%
  mutate(Occupations = as.character(Occupations),  # Convert to character
         Occupations = dplyr::recode(Occupations,
                                     "1" =  "Agricultural day labor",
                                     
                                     "2" = "Non-agricultural day labor",
                                     
                                     "3" = "Salaried",
                                     
                                     "4" = "Rickshaw/van puller",
                                     
                                     "5" =  "Self-employed",
                                     
                                     "6" = "Business/trade",
                                     
                                     "7" = "Production business",
                                     
                                     "8" = "Livestock-related work",
                                     
                                     "9" = "Farming",
                                     
                                     "10" = "Non-earning occupations"))

#stunting by division by gender
#create data frame with total percentage of those stunted
stuntdiv1 <- data %>%
  rename("Division" = div_name, 
         "Stunt" = stunted_all0to59) %>% 
  group_by(Division) %>%
  count(Stunt) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Stunt = as.character(haven::zap_labels(Stunt)),
         Stunt = recode(Stunt, "0" = "Not Stunted",
                        "1" = "Stunted")) %>% 
  na.omit() %>% 
  filter(Stunt == "Stunted")

stuntdiv1 = dplyr::select(stuntdiv1, Division, Percentage) 
stuntdiv1 %>% mutate(Gender = "Total") -> stuntdiv1

#to get percentage for male and female children affected, must make seperate table
#create a different data frame divided by children stunt by Gender
stuntdiv <- data %>%
  rename("Division" = div_name, 
         "Stunt" = stunted_all0to59,
         "Gender" = childgender)%>% 
  group_by(Division, Gender) %>%
  count(Stunt) %>% 
  mutate(Total_by_gender = sum(n), Percentage = round(n/Total_by_gender * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Stunt = as.character(haven::zap_labels(Stunt)),
         Stunt = recode(Stunt, "0" = "Not Stunted",
                        "1" = "Stunted"),
         Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender, "1" = "Male",
                         "2" = "Female")) %>% 
  na.omit() %>% 
  filter(Stunt == "Stunted")
# stuntdiv

#combine tables so that Gender column will contain Total(Male and Female), Male, and Female
stuntdiv_combined <- bind_rows(stuntdiv, stuntdiv1)
#factoring for the bar graph to cluster in this order
stuntdiv_combined$Gender <- factor(stuntdiv_combined$Gender, levels = c("Total", "Male", "Female"))

# Underweight by division by gender
#create data frame with total percentage of those underweight
weightdiv1 <- data %>%
  rename("Division" = div_name, 
         "Weight" = underweight_all0to59) %>% 
  group_by(Division) %>%
  count(Weight) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Weight = as.character(haven::zap_labels(Weight)),
         Weight = recode(Weight, "0" = "Not Underweight",
                         "1" = "Underweight")) %>% 
  na.omit() %>% 
  filter(Weight == "Underweight")

weightdiv1 = dplyr::select(weightdiv1, Division, Percentage) 
weightdiv1 %>% mutate(Gender = "Total") -> weightdiv1

#to get percentage for male and female children affected, must make seperate table
#create a different data frame divided by children underweight by Gender
weightdiv <- data %>%
  rename("Division" = div_name, 
         "Weight" = underweight_all0to59,
         "Gender" = childgender)%>% 
  group_by(Division, Gender) %>%
  count(Weight) %>% 
  mutate(Total_by_gender = sum(n), Percentage = round(n/Total_by_gender * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Weight = as.character(haven::zap_labels(Weight)),
         Weight = recode(Weight, "0" = "Not Underweight",
                         "1" = "Underweight"),
         Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender, "1" = "Male",
                         "2" = "Female")) %>% 
  na.omit() %>% 
  filter(Weight == "Underweight")
# weightdiv

#combine tables so that Gender column will contain Total(Male and Female), Male, and Female
weightdiv_combined <- bind_rows(weightdiv, weightdiv1)
#factoring for the bar graph to cluster in this order
weightdiv_combined$Gender <- factor(weightdiv_combined$Gender, levels = c("Total", "Male", "Female"))

#Average birth weight by division by gender
#create data frame with total mean bw and std dev
bw <- data %>%
  drop_na() %>% 
  rename("Division" = div_name, 
         "birthw" = birthweight) %>% 
  group_by(Division) %>%
  count(birthw) %>%
  mutate(Mean_bw = mean(birthw, 2),
         Total_std_dev = round(sd(birthw), 2))

bw = dplyr::select(bw, Division, Mean_bw) 
bw %>% mutate(Gender = "Total") -> bw

#to get percentage for male and female children, must make seperate table
#create a different data frame divided by children bw means and std dev by Gender
bw_gender <- data %>%
  drop_na() %>% 
  rename("Division" = div_name, 
         "birthw" = birthweight,
         "Gender" = childgender) %>% 
  group_by(Division, Gender) %>%
  count(birthw) %>%
  mutate(Mean_bw = mean(birthw, 2),
         gender_std_dev = round(sd(birthw), 2),
         Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender, "1" = "Male",
                         "2" = "Female"))


#combine tables so that Gender column will contain Total(Male and Female), Male, and Female
bwdiv_combined <- bind_rows(bw, bw_gender)
#factoring for the bar graph to cluster in this order
bwdiv_combined$Gender <- factor(bwdiv_combined$Gender, levels = c("Total", "Male", "Female"))

#MOTHER AGE DISTRIBUTION BY DIVISION 
agediv <- data %>%
  drop_na() %>% 
  rename("Division" = div_name, 
         "Agecategory" = mother_age_categorical) %>% 
  group_by(Division) %>%
  count(Agecategory) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>%
  mutate(Agecategory = as.character(haven::zap_labels(Agecategory)),
         Agecategory = recode(Agecategory, "2" = "11-17 yrs",
                              "3" = "18-30 yrs",
                              "4" = "31-65 yrs"))

#MOTHER EDUCATION DISTRIBUTION
edudiv <- data %>%
  drop_na() %>% 
  rename("Division" = div_name, 
         "Educategory" = mothereduc) %>% 
  group_by(Division) %>%
  count(Educategory) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>%
  mutate(Educategory = as.character(haven::zap_labels(Educategory)),
         Educategory = recode(Educategory, "4" = "Higher",
                              "3" = "Completed secondary",                              
                              "2" = "Completed primary",
                              "1" = "Less than primary",
                              "0" = "No education"))
# edudiv

edudiv$Educategory <- factor(edudiv$Educategory, levels = c("Higher","Completed secondary","Completed primary","Less than primary","No education"))

#MOTHER OCCUPATION DISTRIBUTION
ocudiv <- data %>%
  drop_na() %>% 
  rename("Division" = div_name, 
         "Ocucategory" = motheroccupation) %>% 
  group_by(Division) %>%
  count(Ocucategory) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>%
  mutate(Ocucategory = as.character(haven::zap_labels(Ocucategory)),
         Ocucategory = recode(Ocucategory, "1" = "Agricultural day labor",
                              "2" = "Non-agricultural day labor",
                              "3" = "Salaried",
                              "4" = "Rickshaw/van puller",
                              "5" = "Self-employed",
                              "6" = "Business/trade",
                              "7" = "Production business",
                              "8" = "Livestock-related work",
                              "9" = "Farming",
                              "10" = "Non-earning occupations"))
# ocudiv

ocudiv$Ocucategory <- factor(ocudiv$Ocucategory, levels = c("Non-earning occupations", "Farming","Livestock-related work","Production business","Business/trade","Self-employed","Rickshaw/van puller","Salaried","Non-agricultural day labor","Agricultural day labor"))

female_stunted <- data.frame(
  
  z_score_tertile = c("Low", "Low", "Low", "Medium", "Medium", "Medium", "High", "High", "High"),
  
  Trimester = c("Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3"),
  
  Percentage_stunted = c(33.24, 30.97, 29.44, 30.97, 29.6, 33.15, 35.71, 34.44, 37.5)
  
)



female_stunted$z_score_tertile <- factor(female_stunted$z_score_tertile, levels = c("Low", "Medium", "High"))

male_stunted <- data.frame(
  
  z_score_tertile = c("Low", "Low", "Low", "Medium", "Medium", "Medium", "High", "High", "High"),
  
  Trimester = c("Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3"),
  
  Percentage_stunted = c(31.28, 31.77, 33.25, 30.13, 28.28, 29.07, 35.31, 36.9, 34.35)
  
)



male_stunted$z_score_tertile <- factor(male_stunted$z_score_tertile, levels = c("Low", "Medium", "High"))

female_underweight <- data.frame(
  
  z_score_tertile = c("Low", "Low", "Low", "Medium", "Medium", "Medium", "High", "High", "High"),
  
  Trimester = c("Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3"),
  
  Percentage_underweight = c(24.93, 26.48, 21.67, 22.95, 20.63, 22.04, 25.01, 25.82, 29.57)
  
)


female_underweight$z_score_tertile <- factor(female_underweight$z_score_tertile, levels = c("Low", "Medium", "High"))

male_underweight <- data.frame(
  
  z_score_tertile = c("Low", "Low", "Low", "Medium", "Medium", "Medium", "High", "High", "High"),
  
  Trimester = c("Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3", "Trimester1", "Trimester2", "Trimester3"),
  
  Percentage_underweight = c(24.60, 21.41, 25.07, 19.22, 19.54, 19.52, 24.03, 27.01, 23.16)
  
)

male_underweight$z_score_tertile <- factor(male_underweight$z_score_tertile, levels = c("Low", "Medium", "High"))

stun_female <- chirpsdata %>%
  filter(stunted_all == 1, childgender == "2. Female", agemonth < 24,) %>% 
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(   
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>% 
  group_by(Trim, Precipitation)%>%
  count() %>% 
  mutate(Total = sum(n),
         Percentage = round(n / 343 * 100, 2)) %>%
  rename(stunt_n = n) %>% 
  ungroup() 


# stun_female

notstun_female <- chirpsdata %>%
  filter(stunted_all == 0, childgender == "2. Female", agemonth < 24,) %>% 
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(   
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>% 
  group_by(Trim, Precipitation)%>%
  count() %>% 
  mutate(Tot = sum(n),
         Percent = round(n / 343 * 100, 2)) %>%
  rename(nonstunt_n = n, Precip = Precipitation, trimm = Trim) %>% 
  ungroup() 


# notstun_female

total_stunt_female <-cbind(stun_female, notstun_female)


chirps_stunt_girls <- total_stunt_female %>% 
  group_by(Trim, Precipitation) %>% 
  mutate(Total = sum(nonstunt_n, stunt_n),
         Stunt_percent = round(stunt_n/Total*100, 2))

chirps_stunt_girls$Precipitation <- factor(chirps_stunt_girls$Precipitation, levels = c("Low", "Medium", "High"))

stun_male <- chirpsdata %>%
  filter(stunted_all == 1, childgender == "1. Male", agemonth < 24,) %>% 
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(   
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>% 
  group_by(Trim, Precipitation)%>%
  count() %>% 
  mutate(Total = sum(n),
         Percentage = round(n / 343 * 100, 2)) %>%
  rename(stunt_n = n) %>% 
  ungroup() 


# stun_male

notstun_male <- chirpsdata %>%
  filter(stunted_all == 0, childgender == "1. Male", agemonth < 24,) %>% 
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(   
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>% 
  group_by(Trim, Precipitation)%>%
  count() %>% 
  mutate(Tot = sum(n),
         Percent = round(n / 343 * 100, 2)) %>%
  rename(nonstunt_n = n, Precip = Precipitation, trimm = Trim) %>% 
  ungroup() 


# notstun_male

total_stunt_male <-cbind(stun_male, notstun_male)


chirps_stunt_boys <- total_stunt_male %>% 
  group_by(Trim, Precipitation) %>% 
  mutate(Total = sum(nonstunt_n, stunt_n),
         Stunt_percent = round(stunt_n/Total*100, 2))


chirps_stunt_boys$Precipitation <- factor(chirps_stunt_boys$Precipitation, levels = c("Low", "Medium", "High"))

uw_female <- chirpsdata %>%
  filter(underweight_all == 1, childgender == "2. Female", agemonth < 24,) %>% 
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(   
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>% 
  group_by(Trim, Precipitation)%>%
  count() %>% 
  mutate(Total = sum(n),
         Percentage = round(n / 343 * 100, 2)) %>%
  rename(uw_n = n) %>% 
  ungroup() 


# uw_female

notuw_female <- chirpsdata %>%
  filter(underweight_all == 0, childgender == "2. Female", agemonth < 24,) %>% 
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(   
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>% 
  group_by(Trim, Precipitation)%>%
  count() %>% 
  mutate(Tot = sum(n),
         Percent = round(n / 343 * 100, 2)) %>%
  rename(nonuw_n = n, Precip = Precipitation, trimm = Trim) %>% 
  ungroup() 


# notuw_female

total_uw_female <-cbind(uw_female, notuw_female)


chirps_uw_girls <- total_uw_female %>% 
  group_by(Trim, Precipitation) %>% 
  mutate(Total = sum(nonuw_n, uw_n),
         uw_percent = round(uw_n/Total*100, 2))

chirps_uw_girls$Precipitation <- factor(chirps_uw_girls$Precipitation, levels = c("Low", "Medium", "High"))

uw_male <- chirpsdata %>%
  filter(underweight_all == 1, childgender == "1. Male", agemonth < 24,) %>%
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>%
  group_by(Trim, Precipitation)%>%
  count() %>%
  mutate(Total = sum(n),
         Percentage = round(n / 343 * 100, 2)) %>%
  rename(uw_n = n) %>%
  ungroup()


# uw_male

notuw_male <- chirpsdata %>%
  filter(underweight_all == 0, childgender == "1. Male", agemonth < 24,) %>%
  rename( Trimester1 = tertile_firsttrim_z, Trimester2 = tertile_secondtrim_z, Trimester3 = tertile_thirdtrim_z, Gender = childgender) %>%
  
  pivot_longer(cols = c(Trimester1, Trimester2, Trimester3 ), names_to = "Trim", values_to = "Precipitation") %>%
  mutate(Precipitation = as.character(haven::zap_labels(Precipitation)),
         Precipitation = recode(
           Precipitation,       "3" = "High",
           "2" = "Medium",
           "1" = "Low")) %>%
  group_by(Trim, Precipitation)%>%
  count() %>%
  mutate(Tot = sum(n),
         Percent = round(n / 343 * 100, 2)) %>%
  rename(nonuw_n = n, Precip = Precipitation, trimm = Trim) %>%
  ungroup()


# notuw_male

total_uw_male <-cbind(uw_male, notuw_male)


chirps_uw_boys <- total_uw_male %>%
  group_by(Trim, Precipitation) %>%
  mutate(Total = sum(nonuw_n, uw_n),
         uw_percent = round(uw_n/Total*100, 2))


chirps_uw_boys$Precipitation <- factor(chirps_uw_boys$Precipitation, levels = c("Low", "Medium", "High"))


# #DATA FOR ACESS TO HEALTHCARE TAB

# distance data 
dis <- read_dta(paste0(getwd(),"/data/distance to health facility by rain tertile.dta"))

dis_from_med <- dis %>% 
  rename("Tertile" = tertile_totalrain_z) %>% 
  group_by(Tertile) %>% 
  mutate(Tertile = factor(Tertile)) %>% 
  ungroup() %>%
  mutate(Tertile = as.character(haven::zap_labels(Tertile)),
         Tertile = recode(Tertile,
                          "1" = "Low",
                          "2" = "Medium",
                          "3" = "High"))
dis_from_med$Tertile <- factor(dis_from_med$Tertile, levels = c("Low", "Medium", "High" ))

# mode of transport data
Transportation <- c("Boat", "Foot", "Motor Vehicle", "Rickshaw/Cart", "Other")
Low <- c(2.26, 5.85, 42.82, 47.61, 1.46)
Medium <- c(3.59, 5.45, 64.76, 24.47, 1.72)
High <- c(7.05, 7.18, 67.42, 17.02, 1.33)
colors <- c("#cc4778", "#fde725", "#21918c", "#3a528b", "#440154")

data <- data.frame(Transportation, Low, Medium, High)

trdata_long <- tidyr::pivot_longer(data, -Transportation, names_to = "Tertile", values_to = "Percentage")

trdata_long$Transportation <- factor(trdata_long$Transportation, levels = c("Other", "Foot", "Boat", "Motor Vehicle", "Rickshaw/Cart"))
# c("Other", "Foot", "Boat", "Motor Vehicle", "Rickshaw/Cart"))
# c("Rickshaw/Cart", "Motor Vehicle", "Boat", "Foot", "Other"))
trdata_long$Tertile <- factor(trdata_long$Tertile, levels = c("High", "Medium", "Low"))

# antenatal care data
pct_care <- data.frame(
  Care = c("Received Care", "Received Care", "Received Care", "Did not Receive Care", "Did not Receive Care", "Did not Receive Care"),
  rain_intensity = c("Low", "Medium", "High"),
  
  pct = c(10.81, 11.73, 9.92, 89.19, 88.27, 90.08)
)

pct_care$rain_intensity <-  factor(pct_care$rain_intensity, levels = c("Low", "Medium", "High"))

# primary advisor data
Intensity <- c("Low", "Medium", "High")
Licensed <- c(84.86, 84.04, 86.51)
Unlicensed <- c(4.32, 4.23, 3.56)
Other <- c(10.81, 11.73, 9.92)

padata <- data.frame(Intensity, Licensed, Unlicensed, Other)

padata_long <- tidyr::pivot_longer(padata, -Intensity, names_to = "Advisor", values_to = "Percentage")

padata_long$Intensity <- factor(padata_long$Intensity, levels = c("Low", "Medium", "High"))
padata_long$Advisor <- factor(padata_long$Advisor, levels = c("Licensed", "Unlicensed", "Other"))

# frequency visits data
avg_vist <- data.frame(
  Rain = c("Low", "Medium", "High"),
  Percent = c(4.08, 3.23, 3.30))

avg_vist$Rain <-  factor(avg_vist$Rain, levels = c("Low", "Medium", "High"))

# delivery location data
ddata<- read_dta(paste0(getwd(),"/data/BIHS2018-19_Mechanism_Delivery_CHIRPS.dta"))
                 
delA <- ddata %>% 
  group_by(tertile_totalrain_z) %>% 
  count(delivery) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  mutate(delivery = as.character(haven::zap_labels(delivery)),
         delivery = recode(delivery, "0" = "Home/Other",
                                 "1" = "Hospital/Clinic"),
         tertile_totalrain_z = as.character(haven::zap_labels(tertile_totalrain_z)),
         tertile_totalrain_z = recode(tertile_totalrain_z, "1" = "Low",
                                                           "2" = "Medium",
                                                           "3" = "High")) %>% 
  rename(Intensity = tertile_totalrain_z, Location = delivery)
delA

delA$Intensity <- factor(delA$Intensity, levels = c("Low", "Medium", "High"))

# person present data
Intensity <- c("Low", "Medium", "High")
Licensed <- c( 95.79, 93.18,94.7)
Unlicensed <- c(.53, 0.97, 1.52)
Family <- c(38.42, 52.60, 60.10)
Other <- c(1.05, 0.97, 1.26)

pdata <- data.frame(Intensity, Licensed, Unlicensed, Family, Other)

pdata_long <- tidyr::pivot_longer(pdata, -Intensity, names_to = "Person", values_to = "Percentage")

pdata_long$Intensity <- factor(pdata_long$Intensity, levels = c("Low", "Medium", "High"))
pdata_long$Person <- factor(pdata_long$Person, levels = c("Family", "Licensed", "Unlicensed", "Other"))

# type of licensed professional data
Intensity <- c("Low", "Medium", "High")
Public <- c(69.23, 55.40, 48.00)
Community <- c(.55, .35, 0)
NGO <- c(32.42, 45.64, 54.93)
Private <- c(0, .35, .53)

ldata <- data.frame(Intensity, Private, Community, NGO, Public)

ldata_long <- tidyr::pivot_longer(ldata, -Intensity, names_to = "Physician", values_to = "Percentage")

ldata_long$Intensity <- factor(ldata_long$Intensity, levels = c("Low", "Medium", "High"))
ldata_long$Physician <- factor(ldata_long$Physician, levels = c("NGO", "Public", "Private", "Community"))

# calcium data
Months <- c("No Intake", "1-4 Months", "5-7 Months", "8 or more months")
LowPI <- c(30.65, 44.62, 24.19, .54) 
MediumPI <- c(30.94, 52.12, 16.61, .33)
HighPI <- c(33.59, 50.38, 15.52, .51)
colorsPI <- c("#fde725", "#21918c", "#3a528b", "#440154")
LevelsPI <- c("High", "Medium", "Low")

dataC <- data.frame(Months, LowPI, MediumPI, HighPI)

data_longC <- tidyr::pivot_longer(dataC, -Months, names_to = "Tertile", values_to = "Percentage")

data_longC$Months <- factor(data_longC$Months, levels = c("No Intake", "1-4 Months", "5-7 Months", "8 or more months"))
data_longC$Tertile <- factor(data_longC$Tertile, levels = c("HighPI", "MediumPI", "LowPI"))


#iron data 
Months <- c("No Intake", "1-4 Months", "5-7 Months", "8 or more months")
Low <- c(21.51, 50.00, 27.96, .52)
Medium <- c( 30.07, 50.98, 18.30, .65)
High <- c( 28.24, 52.67, 18.58, .51)
colors <- c("#cc4778", "#fde725", "#21918c", "#3a528b", "#440154")

data <- data.frame(Months, Low, Medium, High)

data_long <- tidyr::pivot_longer(data, -Months, names_to = "Tertile", values_to = "Percentage")

data_long$Months <- factor(data_long$Months, levels = c("No Intake", "1-4 Months", "5-7 Months", "8 or more months"))
data_long$Tertile <- factor(data_long$Tertile, levels = c("High", "Medium", "Low"))


#vitamin A data 
vitamin_a_data <- read_dta(paste0(getwd(),"/data/BIHS2018-19_Mechanism_Supplements_CHIRPS.dta"))
vitA <- vitamin_a_data %>% 
  group_by(tertile_totalrain_z) %>% 
  count(vitamina) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  mutate(vitamina = as.character(haven::zap_labels(vitamina)),
         vitamina = recode(vitamina, "0" = "Not taken",
                           "1" = "Taken"),
         tertile_totalrain_z = as.character(haven::zap_labels(tertile_totalrain_z)),
         tertile_totalrain_z = recode(tertile_totalrain_z, "1" = "Low",
                                      "2" = "Medium",
                                      "3" = "High")) %>% 
  rename(Intensity = tertile_totalrain_z, Supplement = vitamina)

vitA

vitA$Intensity <- factor(vitA$Intensity, levels = c("Low", "Medium", "High"))

#IYCF cummulaitve data 
Intensity <- c("Low", "Medium", "High")
From1to3 <- c(3.87, 2.74, 5.12)
From4to5 <- c(32.60, 34.59, 35.31)
From6to8 <- c(63.54, 62.67, 59.57)

cqdata <- data.frame(Intensity, From1to3, From4to5, From6to8)

cqdata_long <- tidyr::pivot_longer(cqdata, -Intensity, names_to = "Questions", values_to = "Percentage")

cqdata_long$Questions <- factor(cqdata_long$Questions, levels = c("From1to3", "From4to5", "From6to8"))
cqdata_long$Intensity <- factor(cqdata_long$Intensity, levels = c("Low", "Medium", "High"))

#IYCF Per question data 
Intensity <- c("Low", "Medium", "High")
Water <- c(68.51, 75.34, 70.62)
Semisolids <- c(70.17, 68.15, 69.81)
Breastfeeding<- c(95.58, 95.55, 93.80)
Colostrum <- c(97.79, 94.86, 92.45)
HotWeather <- c(49.17, 47.60, 46.36)
Exclusive <- c(89.50, 91.44, 88.95)
Iron <- c(100, 100, 100)
Diarrhea <- c(3.31, 2.05, 2.16)

pqdata <- data.frame(Intensity, Water, Semisolids, Breastfeeding, Colostrum, HotWeather, Exclusive, Iron, Diarrhea)

pqdata_long <- tidyr::pivot_longer(pqdata, -Intensity, names_to = "Question", values_to = "Percentage")

pqdata_long$Intensity <- factor(pqdata_long$Intensity, levels = c("Low", "Medium", "High"))


## drinking water sources

drinking_w <- data.frame(
  
  water_sources = c("Piped", "Piped", "Piped", "Piped", "Piped", "Piped", "Piped", "Tube Wells", "Tube Wells", "Tube Wells", "Tube Wells", "Tube Wells", "Tube Wells", "Tube Wells", "Ponds/Rivers/Canals", "Ponds/Rivers/Canals", "Ponds/Rivers/Canals", "Ponds/Rivers/Canals", "Ponds/Rivers/Canals", "Ponds/Rivers/Canals", "Ponds/Rivers/Canals", "Other", "Other", "Other", "Other", "Other", "Other", "Other"),
  
  Divisions = c("Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshahi", "Rangpur", "Sylhet", "Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshahi", "Rangpur", "Sylhet", "Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshahi", "Rangpur", "Sylhet", "Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshahi", "Rangpur", "Sylhet"),
  
  Percentage  = c(1.21, 4.11, 3.48, 2.17, 9.74, 1.57, 1.99, 26.46, 38.50, 73.68, 68.90, 81.82, 92.13, 42.29, 72.33, 54.41, 22.26, 28.75, 6.49, 6.12, 53.32, 0.00, 2.98, 0.58, 0.18, 1.95, 0.17, 2.39))



drinking_w $water_sources <- factor(drinking_w $water_sources, levels = c("Piped", "Tube Wells", "Ponds/Rivers/Canals", "Other"))


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
# jscode <- '
#    var x = document.getElementsByClassName("navbar-brand");
#    var dspgLink = "https://dspg.aaec.vt.edu/";
#    var githubLink = "https://github.com/VT-Data-Science-for-the-Public-Good";
#    var dspgLogoHTML = \'<a href="\' + dspgLink + \'"><img src="DSPG_black-01.png" alt="VT DSPG" style="height:42px;"></a>\';
#    var githubLogoHTML = \'<a href="\' + githubLink + \'"><img src="github_logo.png" alt="GitHub" style="max-height: 30px; max-width: 100%;"></a>\';
#    var logosHTML = dspgLogoHTML + githubLogoHTML;
#    x[0].innerHTML = x[0].innerHTML + " " + logosHTML;
#   '

jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }
           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }
            var mytype = getUrlParam('type','Empty');
            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");
                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }
          var x = document.getElementsByClassName('navbar-brand');
               var dspgLink = 'https://dspg.aaec.vt.edu/';
    var githubLink = 'https://github.com/naveenabedinvt/DSPG2023_Bangladesh';
    var dspgLogoHTML = \"<a href='\" + dspgLink + \"'><img src='DSPG_black-01.png' alt='VT DSPG' style='height:42px;'></a>\";
    var githubLogoHTML = \"<a href='\" + githubLink + \"'><img src='github_logo.png' alt='GitHub' style='max-height: 30px; max-width: 100%;'></a>\";
    var logosHTML = dspgLogoHTML + githubLogoHTML;
    x[0].innerHTML = x[0].innerHTML + ' ' + logosHTML;
         
           "



# user -------------------------------------------------------------
ui <- navbarPage(
  title = "",
  selected = "overview",
  theme = shinytheme("lumen"),
  tags$head(
    tags$style('.selectize-dropdown {z-index: 2000}'),
    tags$style('
                      .carousel-container {
                        height: auto;
                        overflow-y: 100%;
                      }
                    ')
  ),
  useShinyjs(),
  
  # main tab -----------------------------------------------------------
  tabPanel("Project Overview", value = "overview",
           fluidRow(style = "margin: 2px;",
                    align = "center",
                    # br("", style = "padding-top:2px;"),
                    # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                    br(""),
                    h1(strong("Effects Prenatal Exposure to Flooding on Child Health Outcomes: Evidence from Bangladesh"),
                       #h2("") ,
                       br(""),
                       h4("Data Science for the Public Good Program 2023"),
                       h4("Virginia Polytechnic Institute and State University"),
                       #h4("[updat this]"),
                       br("")
                    )
           ),
           fluidRow(style = "margin: 6px;",
                    column(4,
                           h2(strong("Country Profile")),
                           
                           p("Located in South Asia, Bangladesh captivates with its rich tapestry of culture, mesmerizing landscapes, and a vibrant population. Bangladesh shares borders with India to the west, north, and east, while Myanmar borders its southeastern regions. Spanning across flatlands, Bangladesh is crisscrossed by a network of over 700 rivers and tributaries (Banglapedia, 2021).

In addition to its vibrant culture and natural beauty, Bangladesh faces numerous socio-economic challenges. These include high population density, which puts pressure on resources and infrastructure, vulnerability to rising sea levels and frequent flooding, limited access to quality healthcare services, and persistent gender inequality issues that require concerted efforts to overcome.

As of 2023, Bangladesh is the eighth most populous country in the world, with a population of approximately 167,184,465 people (U.S. Census Bureau, 2023). In 2016, 13.47% of the population in Bangladesh lived below the international poverty line of $2.15 per day. However, by 2022, the poverty rate declined to 10.44%, showing improvement (World Bank, 2023). 

The country is characterized by a multi-tiered administrative structure that helps govern the country effectively. This structure encompasses several levels, including divisions, districts, upazilas (sub-districts), unions, and villages. The country has eight divisions: Barisal, Chittagong, Dhaka, Mymensingh, Khulna, Rajshahi, Rangpur, and Sylhet, which serve as the highest level of administrative units. Each division is further divided into 64 districts. Within the divisions, multiple upazilas (sub-districts) play a crucial role in implementing government policies. Upazilas are divided into unions, which are composed of several villages and are responsible for local government functions and community well-being. Villages are the smallest unit of administration and are primarily located in rural areas. They accommodate the majority of the population and play a significant role in the socio-economic development of Bangladesh.
", align = "justify")),
                    column(4,
                           h2(strong("Goals and Objectives")),
                           h3(strong("Research Question:")), 
                           p("Bangladesh is one of the most flood-prone countries in the world. Around 60% of the population reside in rural areas. With limited infrastructure and access to resources flooding poses significant threats to their livelihood and well-being. 

Pregnant women face greater disadvantages when floods strike, increasing health risks, limiting access to healthcare, and causing inadequate nutrition. These factors further jeopardize their well-being and the well being of their unborn child. Studies have shown that prenatal exposure to natural disasters can cause significant health effects on the child such as stunting and underweight. With climate change influencing flooding prevalence, child health outcomes may become worsened if not addressed now. 

Our research focuses on two interconnected questions: (1) Does prenatal exposure to flooding adversely affect child health outcomes, and (2) What are the mechanisms through which floods affect child health outcomes. Floods have been found to have correlations with worsened child health outcomes such as lower birth weight, cognitive functions, and height (Mallett, 2017). Additionally, the timing of flood exposure during pregnancy can have varying effects on fetal development. The second part of our research aims to examine the mechanisms through which flooding affects access to nutrition and healthcare services as street and road access become limited (Aggarwal, 2018). When flooding occurs, accessing clean water becomes increasingly more difficult as water gets contaminated. Therefore, in this study, we will observe the effects of flood exposure during pregnancy on child health outcomes.
", align = "justify"),
                           h3(strong("Objectives:")), 
                           p("During our 10-week program, our objective was to complete a preliminary study of prenatal exposure to flooding and child health outcomes. We accomplished this by combining flood statistics, from remotely sensed satellite data, with household-level survey data to produce a collection of descriptive statistics detecting trends. This allowed us to gain a comprehensive understanding of how flooding impacts a woman at the individual level. Our team also explored the mechanisms through which floods affect child health outcomes by identifying possible channels from past research work. 
", align = "justify"),
                           
                    ), 
                    
                    column(4,
                           h2(strong("Research Design")),
                           p("For our research, we used the Bangladesh Integrated Household Survey (BIHS) data collected by the International Food Policy Research Institute (IFPRI). We only utilized the third round of the panel survey - BIHS 2018-19 - the most recent available survey in the series. The sample is designed to be nationally representative of rural Bangladesh at the divisional level. The BIHS sample also included observations from the Feed The Future (FTF) Zone of Influence (ZOI). However, for the purposes of this study, we excluded this additional subset of the survey. 

To determine the sample size, IFPRI used a two-stage stratified sampling method. In the first stage, they selected Primary Sampling Units (PSUs) or villages using probability proportional to size based on the number of households in each stratum or division. The choice of PSUs resulted in the following distribution: 21 PSUs in Barisal, 48 in Chittagong, 87 in Dhaka, 27 in Khulna, 29 in Rajshahi, 27 in Rangpur, 36 in Sylhet, and 50 in the FTF zone. In the second stage, twenty households were randomly selected from each PSU. The IFPRI sample size was 6,500 households across 325 PSUs. However, our sample size is smaller because we excluded 1,000 households from the FTF Zone of Influence. Additionally, it is important to note that several changes occurred within households between 2011-2012 and 2018-2019, such as household merges or splits. After making all these adjustments, our sample size is 5,604 households.

In addition to the survey data we will use Global Flood Database (GFD), Climate Hazards Group InfraRed Precipitation with Station Data (CHIRPS), and Sentinel-1 data to assess the flood extent. The GFD identifies significant global flood events based on news reports and uses MODIS satellite imagery to capture those flood events at 250 m resolution. CHIRPS provides precipitation data at 5000 m resolution. Sentinel-1 uses radar to gather data on flood extent at 10 m spatial resolution. 
", align = "justify"),  
                    )), 
           
           
           fluidRow(style = "margin: 6px;",
                    
                    column(6, 
                           ##start of carousel code             
                           h3(strong("Inside Look of Bangladesh")), 
                           carousel(
                             id = "myCarousel", width = 80,
                             carouselItem(
                               tags$img(src = "hh.jpg", width ="100%")
                             ),
                             carouselItem(
                               tags$img(src = "hh1.jpg", width ="100%")
                             ),
                             carouselItem (
                               tags$img(src = "hh2.jpg", width = "100%")
                             ),
                             carouselItem(
                               tags$img(src = "hh3.jpg", width ="100%")
                             ),
                             carouselItem(
                               tags$img(src = "hh4.jpg", width ="100%")
                             ),
                             carouselItem(
                               tags$img(src = "hh5.jpg", width ="100%")
                             )
                             ##end of carousel code 
                           )
                           
                    ),
                    column(6,
                           h2(strong("BIHS 2018-19")),
                           p("The survey includes a wide range of questions about demographic characteristics, socioeconomic status, household assets, employment, income, expenditures, education, health, etc. The survey is structured into 29 modules that correspond to different areas. Each module consists of a series of questions designed to collect specific information related to that area. 

In our study we will focus mainly on modules A, B, S, R, W, and Y. Module A provides insight on the sample households and identification; this includes information on coordinates of the household and the total number of members in the household. Coordinates of the household will be used to locate the proximity of these households to the affected flooded areas. Module B covers the Household Composition and Education. This module entails the education levels, occupation, and source of income for individuals of the household. Module S contains data on distance to nearest healthcare facilities and Module R contains data on household water and sanitation practices. Module W focuses on the anthropometry, health, and illnesses of each individual of the household. Module Y has sectional portions containing survey data on child and antenatal care, Infant and Young Child Feeding (IYCF) practices, immunization and health of children younger than the age of two and service use.
", align="justify")
                           #p("")
                    )
           ),
           #fluidRow(align = "center",
           # p(tags$small(em('Last updated: August 2021'))))
  ),
  
  ## Overview--------------------------------------------
  
  navbarMenu("Background",
             tabPanel("Literature Review",
                      fluidRow(style = "margin: 6px;",
                               p("", style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Literature Review ")),
                                      br(""))),
                      column(6, align = "Justify",
                             p("Several studies have investigated the impact of prenatal exposure to flooding on child birth outcomes and the underlying mechanisms involved. Additionally, other papers have used other environmental hazards such as cyclones, tropical storms, and major monsoons which could lead to negative child health outcomes. One study by Ochani et al. (2022) conducted in Pakistan, revealed that unexpected intense floods have negative impacts on pregnant women, including psycho-physiological stress and disruptions in prenatal care. This can have repercussions on fetal development and child health outcomes. Furthermore, the impact of exposure can vary based on sex of the child as a result of differences in hormonal responses and epigenetic mechanisms (Sutherland & Brunwasser, 2018)."),
                             p("A study by Del Ninno, et al. (2005) found that flooding negatively impacts children’s physical and physiological development. The children that were exposed to floods were found to be more stunted than those who were not; these effects were long-lasting as these children were not able to recover after the temporary shock. According to Rosales-Rueda (2018), the impact of flood exposure during pregnancy varies depending on the trimester. When mothers are exposed to floods during the third trimester, their children tend to be shorter. On the other hand, cognitive function deficits are more common among children whose mothers were exposed to floods during the first trimester of pregnancy. Furthermore, in 2008, malnutrition of mothers and poor feeding practices led to 60.9% of children being underweight in Gauda region (Bangladesh-India border) where it experienced flooding. In a comprehensive review study on flood exposures,  Mallett (2018) found that many children suffered from chronic diarrhea, increased risk of mortality, and poor growth."),
                             p("Several papers have also examined the possible reasons why flooding is related to poorer health outcomes for mothers and children. Orderud, H. et al. (2022)'s study investigated the association between flood exposure and maternal healthcare utilization in Bangladesh. ")),
                      
                      column(6, align = "justify",
                             p("They found that women living in flooded areas used maternal healthcare services less than those in non-flooded areas. The same paper suggested that other confounding factors such as low education levels and lower socioeconomic status contributed to reduced healthcare utilization. Hence, increased deficiencies in iron, vitamin A, and vitamin C are prevalent in flood prone regions due to limited access to healthcare facilities (Oskoorouchi & Sousa-Poza, 2021). De Silva and Kawasaki (2020) highlighted disparity in economic status as a driving force to this unequal healthcare access. Their study found that poor Sri Lankan households experience a 48% greater absolute economic loss due to severe flood events than non-poor households. This is the case because individuals with lower economic status are more likely to reside in highly vulnerable areas, often rural parts, due to their limited resources, thereby increasing their exposure to disasters (De Silva, Akiyuki Kawasaki (2020)."),
                             p("Another major cause of detrimental health consequences in flood-prevalent areas is the consumption of contaminated drinking water. In 2004, Sirajul Islam et al. conducted a study in Dhaka focusing on fecal pollution and the effectiveness of point-of-use water treatment during and after a flood. The collected water samples revealed significant contamination levels, including coliforms, fecal coliforms, fecal streptococci, and Vibrio cholerae. Vibrio cholera is a bacterium responsible for causing cholera, a disease characterized by severe diarrhea, dehydration, and potentially life-threatening complications if left untreated.  A study by Mashiro H. et al. (2010) found that incidence of cholera cases increases  during monsoon seasons (25%), compared to spring (8%) in Bangladesh. Cholera cases among pregnant women pose adverse effects on their children's health outcomes. A study in Nepal found that women with diarrhea during pregnancy were more likely to have small gestational age (SGA) infants compared to those without diarrhea (Newman et al., 2019).
                               ")
                             
                             )),
                             
      
             tabPanel("Flood Conditions", value = "Flood Conditions",
                      fluidRow(style = "margin: 6px;",
                               p(style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Flood Conditions")),
                                      br(""))),
                    
                  
                      fluidRow(
                        column(
                          6,
                          align = "justify",
                          
                          h4(strong("Flood Vulnerability in Different Divisions of Bangladesh")),
                          p("Bangladesh is a topographically flat country situated in the delta region of three major rivers: the Ganges, Brahmaputra, and Meghna. With nearly 60% of the country lying below 6 meters above sea level, slow drainage and the risk of overflow are prevalent (Mirza, 2001). Bangladesh experiences a monsoon season every year, spanning from June to September. The Ganges and Brahmaputra rivers originate from the Himalayan region, which experiences heavy snowfall during the winter months. As the snow melts during the warmer seasons, it increases the discharge of water downstream."),
                          p("Some factors that determine whether an area is more susceptible to flooding than others include elevation, geography, and infrastructure (drainage systems, water management systems, etc.)  (Mirza, 2003). River basins near Bangladesh's three major rivers are more prone to flooding due to their extensive drainage areas, high sediment loads, and proximity to the Himalayan snow melt mentioned earlier (Mirza, 2003)."),
                          h5(strong("Barisal Division")),
                          p("The Barisal Division located in the south is known for its extensive river networks and vast coastal areas. The division experiences regular flooding due to heavy rainfall, overflowing rivers, and tidal surges from the Bay of Bengal, especially during the monsoon"),
                          h5(strong("Chittagong Division")),
                          p("The Chittagong Division is located in the southeastern part of the country, and encompasses both hilly terrain and coastal areas. The division experiences floods caused by heavy rainfall, river overflow, and cyclonic storms coming in from the Bay of Bengal. The hilly regions are also susceptible to landslides during heavy downpours."),
                          h5(strong("Dhaka Division")),
                          p("The Dhaka Division includes the capital city, Dhaka, which lies in the central part of Bangladesh. This division experiences river flooding from three major rivers. Urban areas may experience flooding due to insufficient drainage systems and infrastructure (Mirza, 2003)."),
                          h5(strong("Khulna Division")),
                          p("The Khulna Division is situated in the southwestern region of Bangladesh. This division is prone to coastal flooding and storm surges caused by cyclones from the Bay of Bengal. River flooding is also common during monsoon season."),
                          h5(strong("Rajshahi Division")),
                          p("The Rajshahi Division is located in the northwestern part of Bangladesh. This division experiences river flooding, primarily caused by heavy monsoons and overflowing of the Ganges River."),
                          h5(strong("Rangpur Division")),
                          p("The Rangpur Division is located in the northern part of the country. This division is characterized by its predominantly rural areas and agricultural importance. The division experiences river flooding during monsoon season. Flash floods and waterlogging are also common due to heavy rainfall."),
                          h5(strong("Sylhet Division")),
                          p("The Sylhet Division is located in the northeast region of Bangladesh. The division faces various challenges related to flooding such as heavy rainfall and river overflow. The low lying regions are particularly vulnerable to landslides during intense flash floods due to snowmelt from mountainous regions, in India, surrounding Sylhet."),
                          br("")
                          
                          
                          
                        ),
                        column(
                              6,
                              align = "justify",
                              h5(strong("Map of Bangladesh by Division")),
                              h2(strong("")),
                              img(src = 'new_map2.png', align = 'center', width = "80%", height = "auto"),
                              p("Map of flood affected areas in Bangladesh, Bairagi et al. (2018)")
                            )
                          
                          
                      
                      ),
                      fluidRow(
                        column(6,
                               align = "justify",
                               selectInput(
                                 "floodTimeline", 
                                 label = HTML("<strong>Select Flood Timeline:</strong>"), 
                                 choices = c(
                                   "2013" = "2013",
                                   "2014" = "2014",
                                   "2015" = "2015",
                                   "2016" = "2016",
                                   "2017" = "2017",
                                   "2018" = "2018",
                                   "2019" = "2019"
                                 ),
                               ),
                               withSpinner(plotOutput("timeline", height = "40%", width ="40%")),
                               br("")
                        ),
                        
                        column(
                          width = 6,
                          align = "justify",
                          h5(strong("Flood Disasters in Bangladesh (2013-2019)")),
                          p("The susceptibility to flooding varies across the different divisions of Bangladesh, reflecting the diverse geographic and climatic conditions of the country. There have been several disasters that occurred from 2013-2019. Some notable examples include:"),
                          tags$ul(
                            tags$li("In 2013, Cyclone Mahasen hit the coastal region, killing over 17 people and damaging over 60,000 houses (Government of Bangladesh, 2014)."),
                            tags$li("In 2014, severe flooding affected 46 districts and 16 million people, making it the most severe flooding since the “mega-flood” of 2007 (CARE, 2014)."),
                            tags$li("In 2016, Tropical Storm Roanu hit southern Bangladesh, resulting in significant damages (IFRC, 2017)."),
                            tags$li("In 2017, Cyclone Mora caused devastating floods, impacting a large population and vital infrastructure (Shelter Cluster, 2017)."),
                            tags$li("In 2019, Cyclone Titli caused widespread flooding and landslides affecting 4 million individuals (OCHA, 2019).")
                          ),
                          p("Considering the threat of climate change, erosion, and rising sea levels, it is crucial to understand that storms are projected to increase in intensity, amplifying their impacts on vulnerable regions.")
                        ))
                      
                      
             ),
             tabPanel("Household Profile",
                      fluidRow(style = "margin: 4px;",
                               h1(strong("Household Profile"), align = "center"),
                               p("", style = "padding-top:10px;"), 
                               
                               column( 12, 
                                       tabsetPanel(
                                         
                                         tabPanel("Household Formation",
                                                  fluidRow(style = "margin: 4px;",
                                                           p("", style = "padding-top:10px;"),
                                                           
                                                           column(8, align = "left",
                                                                  selectInput("hhformdrop", "Select Household Formation Characteristic:", width = "100%", choices = c(
                                                                    "Average Household Size" = "household_size_division",
                                                                    "Dependency Ratio" = "dependency_ratio_division", 
                                                                    "Household Headship Distribution by Gender" = "household_headship_division"
                                                                    
                                                                  ),
                                                                  ),   
                                                                  br(""),
                                                                  
                                                                  
                                                                  withSpinner(plotlyOutput("hhform", height = "500px", width ="100%"))
                                                           ),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           
                                                           column(4,
                                                                  h4(strong("Description")),
                                                                  textOutput("desc1"))
                                                           
                                                  )
                                         ),
                                         
                                         
                                         
                                         
                                         tabPanel("Age",
                                                  fluidRow(style = "margin: 4px;",
                                                           p("", style = "padding-top:10px;"),
                                                           column(8, align = "left",
                                                                  selectInput("demos1drop", "Select Age Characteristic:", width = "100%", choices = c(
                                                                    "Average Age by Gender" = "age_by_gender",
                                                                    "Male Age Distribution" = "male_age", 
                                                                    "Female Age Distribution" = "female_age",
                                                                    "Household Head Age Distribution" = "head_age"
                                                                    
                                                                  ),
                                                                  ),   
                                                                  br(""),
                                                                  withSpinner(plotlyOutput("demo1", height = "500px", width ="100%"))
                                                           ),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           column(width = 4,
                                                                  h4(strong("Description")),
                                                                  textOutput("desc2"))
                                                  )),
                                         tabPanel("Education",
                                                  fluidRow(style = "margin: 4px;",
                                                           p("", style = "padding-top:10px;"),
                                                           column(8, align = "left",
                                                                  selectInput("edudrop", "Select Educational Characteristic:", width = "100%", choices = c(
                                                                    "Male Education Distribution" = "male_education_division",
                                                                    "Female Education Distribution" = "female_education_division", 
                                                                    "Highest Household Education Distribution" = "hh_highest_education",
                                                                    "Household Head Education Distribution" = "hh_ head_education"
                                                                    
                                                                  ),
                                                                  ),   
                                                                  br(""),
                                                                  withSpinner(plotlyOutput("edu", height = "500px", width ="100%"))
                                                           ),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           column(width = 4,
                                                                  h4(strong("Description")),
                                                                  textOutput("desc3"))
                                                  )),
                                         tabPanel("Economic Status",
                                                  fluidRow(style = "margin: 4px;",
                                                           p("", style = "padding-top:10px;"),
                                                           column(8, align = "left",
                                                                  selectInput("ecodrop", "Select Economic Status Characteristic:", width = "100%", choices = c(
                                                                    "Households Below Poverty Line" = "households_below_poverty_line",
                                                                    "Household Farming Activity Distribution" = "households_farming_activities", 
                                                                    "Cultivable Land Holding" = "cultivable_land",
                                                                    "Electricity Accessibility " = "electricity_accessibility",
                                                                  
                                                                    "Household Head Occupation" = "hhh_occupation"
                                                                    
                                                                    
                                                                  ),
                                                                  ),   
                                                                  br(""),
                                                                  withSpinner(plotlyOutput("eco", height = "500px", width ="100%"))
                                                           ),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           br(""),
                                                           column(width = 4,
                                                                  h4(strong("Description")),
                                                                  textOutput("desc4"))
                                                           
                                                  ))))),
                      fluidPage(column(12, align = "right",
                                       p("Source:",style = "font-size:12px;"))
                      )),
             tabPanel("Mother and Child Profile", value = "overview",
                      fluidRow(style = "margin: 6px;",
                               p("", style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Mother and Child Profile")),
                                      p(""),
                                      br("")),
                               #creating the tab within tab on the left side 
                               column(12,
                                      #setting up the tabs
                                      tabsetPanel(
                                        #first tab panel child profile
                                        tabPanel("Child Profile",
                                                 #setting the stuff under the child profile tab
                                                 fluidRow(style = "margin: 4px;",
                                                          p("", style = "padding-top:10px;"),
                                                          column(8,
                                                                 selectInput("mcdrop1", "Select Child Health Outcome Characteristic:", width = "100%",
                                                                             choices = c("Stunting" = "stunt_div",
                                                                                         "Underweight" = "underweight_div",
                                                                                         "Average Birth Weight" = "avgbw_div")),
                                                                 br(""),
                                                                 withSpinner(plotlyOutput("mc1", height = "500px", width ="100%"))),
                                                          column(4,
                                                                 br(""),
                                                                 br(""),
                                                                 br(""),
                                                                 br(""),
                                                                 h4(strong("Description")),
                                                                 textOutput("mctext1")))),
                                        tabPanel("Mother Profile",
                                                 #setting the-tbh i dont really know
                                                 fluidRow(style = "margin: 4px;",
                                                          p("", style = "padding-top:10px;"),
                                                          column(8,
                                                                 selectInput("mcdrop2", "Select Mother Socioeconomic characteristic:", width = "100%",
                                                                             choices = c("Age Distribution" = "age_dist",
                                                                                         "Education" = "edu_dist", 
                                                                                         "Mother Occupations " = "occu_dist")),
                                                                 br(""),
                                                                 withSpinner(plotlyOutput("mc2", height = "500px", width ="100%"))),
                                                          column(4,
                                                                 br(""),
                                                                 br(""),
                                                                 br(""),
                                                                 br(""),
                                                                 h4(strong("Description")),
                                                                 textOutput("mctext2")
                                                          )))
                                      ))),
                      fluidPage(column(12, align = "right",
                                       p("Source:",style = "font-size:12px;"))
                      )
             )
             
             ),
  navbarMenu("Methodology",
             tabPanel("Global Food Database",
                      fluidRow(style = "margin: 6px;",
                               p("", style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Global Flood Database (GFD)"))),
                               br("")),

                      fluidRow(
                        column(
                          width = 6, align = "justify",
                          h3("What is the Global Flood Database?"),
                          p("The Global Flood Database uses earth-observing satellites to measure and understand global flood exposure. It focuses on mapping the maximum extent of surface-water coverage during 913 significant flood events documented by the Dartmouth Flood Observatory between 2000 and 2018. The database serves as a valuable resource by complementing existing surface-water products that provide monthly or daily observations. It achieves this by providing a geospatial event catalog, which helps with the calibration and comparisons of flood models."),
                          h3("Why are we using the GFD?"),
                          p("Originally, the team was going to use Sentinel 1 satellite data to observe flooding in Bangladesh however, there are large amounts of missing data during various years and months. For example, two divisions, Rangpur and Rajshahi, are missing all flood data in 2016. There are other instances of missing data but due to the extent of missing data, the sample size of children under the age of 5 and pregnant mothers would decrease significantly and the sample would no longer be representative."),
                          h3("How to mapping flood extent using GFD?"),
                          p("We utilized a two-step process for mapping the flood extent and locating affected households. First, we used data from the Bangladesh Integrated Household Survey (BIHS) to identify all of the surveyed households. These households were represented by dark pink circles in our analysis. Secondly, the Global Flood Database provided us with flood extent during a specific time period which we choose. The flood extent was visualized using bright pink pixels. We created 20 kilometers buffer zones in order to represent the average distance a household has to travel to reach the nearest medical facility. By overlaying the buffer zones onto the surveyed households, we were able to identify the flood extent. This allowed us to pinpoint the households that were potentially impacted by the floods."),
                          h3("Benefits of using the GFD"),
                          p("The database has data from 2000 to 2018, which aligns with the timeframe of our study. This coverage allows us to include all of our intended samples without excluding data due to missing data or any other issues."),
                          h3("Challenges and Limitations"),
                          p("The Global Flood Database has a few limitations when compared to Sentinel-1 satellite data. The spatial resolution of the GFD is 250 meteres which can not capture the details of a smaller-scale area or flood. This resolution is beneficial in capturing large, slow moving flood events while Sentinel-1’s higher resolution allows for more precise and detailed flood mapping. Sentinel-1 offers near-real-time observations with a temporal resolution of 12 days while the Global Flood Database relies on historical records from 2000 to 2018. 
Another limitation is the sensitivity of the GFD to weather conditions such as cloud cover. Since the database is reliant on earth-observing satellites, there is a potential for missing or incomplete data due to dense could cover while the Sentinel-1’s radar technology allows for precise data collection regardless of weather conditions. 
Out of the 913 recorded flood events globally, 134 involved Bangladesh, but only 23 fall within the timeframe of our study."),
                         h3("Severity Levels"),
                          tags$ul(
                            tags$li("1 - Large flood events, significant damage to structure or agriculture, fatalities, and/or 5-15 year reported interval since the last similar event"),
                            tags$li("1.5 - Very large events: >15 year but <100 year recurrence interval"),
                            tags$li("2 - Extreme events: recurrence interval >100 years")
                          )  
                           
                        ),
column(width = 6, align = "center",
       img(src = 'put image1', align = 'right', width = "100%", height = "auto"),
       img(src = 'put image2', align = 'right', width = "100%", height = "auto")


                      ))),
                      
tabPanel("CHIRPS",
         fluidRow(style = "margin: 6px;",
                  p("", style = "padding-top:10px;"),
                  column(12, align = "center",h1(strong("Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS)"))),
                  br("")),
      
         fluidRow(
           column(width = 6, align = "justify",
                  h3("About CHIRPS"),
                  p("CHIRPS, known as the Climate Hazards Group InfraRed Precipitation with Station data, is a globally gridded rainfall dataset. This dataset, originating in 1981, was developed by scientists from the United States Geological Survey (USGS) and the Climate Hazards Center (CHC). It continues to be actively supported by prominent organizations such as the United States Agency for International Development (USAID), National Aeronautics and Space Administration (NASA), and National Oceanic and Atmospheric Administration (NOAA)."),
                  
                  h3("Data Integration"),
                  p("The CHIRPS dataset integrates satellite imagery and station data, to generate comprehensive and detailed rainfall maps. Satellite imagery is collected at a frequency of every five days, while station data provide ground-based observations. By combining these sources, CHIRPS effectively covers regions where station data is scarce, ensuring a more complete representation of rainfall patterns."),
                  
                  h3("Utilization in Research"),
                  p("In our research, we are utilizing CHIRPS to assess rainfall levels in rural areas of Bangladesh, recognizing the strong correlation between precipitation and flood occurrences. Although CHIRPS exhibits a relatively lower spatial resolution of 5566 meters per pixel, in contrast to the finer 10-meter pixel resolution of Sentinel 1, it allows us to address the limitations associated with capturing localized flood events in rural Bangladesh. By employing CHIRPS, we can bridge the data gaps left by Sentinel 1, particularly in terms of flood-related observations within rural regions of Bangladesh.")
           ),
           column(width = 6, align = "center",
                  img(src = 'CHIRPS.png', align = 'right', width = "100%", height = "auto"),
                  img(src = 'CHIRPS.png', align = 'right', width = "100%", height = "auto")
                  
           )
         )
),
             
             tabPanel("Sentinel 1",
                      fluidRow(style = "margin: 6px;",
                               p("", style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Sentinel-1"))),
                               br("")),
                   
                      fluidRow(
                        column(6, align = "justify",
                               h3("What is Sentinel-1?"),
                               p(""),
                               h3("How we used Sentinel-1 in our analysis?"),
                               p(""),
                               p(""),
                               h3("Benefits"),
                               p(""),
                               h3("Limitations"),
                               p("")
                               ),
                        column(6, align = "center",
                               img(src = 'put image1', align = 'right', width = "100%", height = "auto"),
                               img(src = 'put image2', align = 'right', width = "100%", height = "auto")
                               )
                      )
             )
             
             
  ),
  navbarMenu("Results",
             tabPanel("Stunting",
                      fluidRow(style = "margin: 6px;",
                               p("", style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Stunting")),
                                      p("")
                               )),
                      tabsetPanel(
                        tabPanel("GFD Results",
                                 fluidRow(style = "margin: 2px;",
                                          br(""),
                                          column(
                                            7,
                                            selectInput("agdrop2", "Age Categories:", width = "100%",
                                                        choices = c("Children Under 2 Years Old" = "1under2",
                                                                    "Children Under 5 Years Old" = "2under5")))
                                          ,  
                                          
                                          column(
                                            8,
                                            p(h5("Gender Disaggregation")),
                                            withSpinner(plotlyOutput("graph1", height = "500px", width ="100%")),
                                            withSpinner(plotlyOutput("graph2", height = "500px", width ="100%")),
                                          ),
                                          
                                          br(""),
                                          
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          
                                          column(
                                            4,
                                            p(h4(strong("Description"))),
                                            
                                            p("Stunting, a condition characterized by below-average height for a given age, can have profound and lasting effects on individuals, even extending into adulthood. It is a matter of great concern, and to better understand its dynamics, we turn our attention to the gender-disaggregated graphs for children under two years old on the left, which shed light on the impact of flood events occurring during a mother's pregnancy on the prevalence of stunted children."),
                                            
                                            p("The graphs suggest a striking pattern: stunting prevalence shows variations across different trimesters, and it becomes evident that girls are the most adversely affected by flood exposure. This finding emphasizes the need for focused attention on this vulnerable group"),
                                            
                                            p("In the top graphs, we observe that girls experience the highest percentage of stunting, reaching a concerning rate of 40.58%, during the third trimester. In contrast, their male counterparts exhibit a slightly lower prevalence of 33.73% during the same trimester. These figures highlight the gender disparity in the impact of flood events, with girls experiencing a disproportionately higher risk of stunting compared to boys."),
                                            
                                            p("However, when we shift our focus to all children under five years old, the picture changes slightly. While there isn't a significant difference in stunting prevalence across trimesters, the gender disparity in stunting remains evident. Girls continue to exhibit a higher prevalence of stunting, reaching 39.05%, during the third trimester. On the other hand, boys experience their highest rate of stunting (36.55%) during the second trimester."),
                                            
                                            p("Taken together, these findings underscore the critical role of the third trimester in determining the occurrence of stunting among children. It becomes increasingly evident that this period of prenatal development is particularly vulnerable to the adverse effects of flood exposure, particularly for girls.
"), 
                                          ),align = "Justify"
                                 )),
                        tabPanel("CHIRPS Results",
                                 fluidRow(style = "margin: 2px;",
                                          br(""),
                                          column(8,
                                                 
                                                 selectInput("agdrop3", "Age Categories:", width = "100%",
                                                             choices = c("Children Under 2 Years Old" = "3under2",
                                                                         "Children Under 5 Years Old" = "4under5"))),
                                          br(""),
                                          column(
                                            8,
                                            p(h5("Gender Disaggregation")),
                                            withSpinner(plotlyOutput("graph3", height = "500px", width ="100%")),
                                            withSpinner(plotlyOutput("graph4", height = "500px", width ="100%")),
                                          ),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          column(
                                            4,
                                            p(h4(strong("Description"))),
                                            p("Considering the correlation between heavy rains and severe flooding, we conducted an analysis to examine the impact of rainfall during pregnancy, categorized by its intensity, on the occurrence of stunting among children. Additionally, we examined precipitation data by breaking it down by age and gender to facilitate a more comprehensive interpretation.
"),
                                            
                                            p("The graphs on the left do not reveal any noticeable distinctions between children under two years old. However, higher intensity rain is associated with stunting among all children under 5 years old from both genders. High intensity rainfall during pregnancy is associated with a staggering stunting rate of 37.50% observed among girls. Such findings underscore the urgent need for appropriate measures and interventions to mitigate the detrimental consequences of intense rainfall on child health and well-being.
"), 
                                          ),align = "Justify"
                                 )),
                        tabPanel("Regression Results",
                                 p("This is sub-tab 2.2.")
                        )
                      )
             ),
             tabPanel("Underweight",
                      fluidRow(style = "margin: 6px;",
                               p("", style = "padding-top:10px;"),
                               column(12, align = "center",h1(strong("Underweight")),
                                      p("")
                               )),
                      tabsetPanel(
                        tabPanel("GFD Results",
                                 fluidRow(style = "margin: 2px;",
                                          br(""),
                                          
                                          
                                          column(8,
                                                 selectInput("agdrop5", "Age Categories:", width = "100%",
                                                             choices = c("Children Under 2 Years Old" = "5under2",
                                                                         "Children Under 5 Years Old" = "6under5"))),
                                          br(""),
                                          column(
                                            8,
                                            p(h5("Gender Disaggregation")),
                                            withSpinner(plotlyOutput("graph5", height = "500px", width ="100%")),
                                            withSpinner(plotlyOutput("graph6", height = "500px", width ="100%")),
                                          ),
                                          column(
                                            4,
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            p(h4(strong("Description"))),
                                            p("Underweight refers to individuals who have a lower weight compared to their age. It is a significant factor in the health and development of children, making it an important birth outcome. Poor fetal development is among the various factors that can contribute to a child being underweight."),
                                            
                                            p("To assess how flooding affects the occurrence of underweight children, we conducted an analysis of our data considering gender and age. The graphs illustrate that there are no noticeable differences observed across trimesters for boys and girls under the ages of two and as well as under the age of five. Nevertheless, it is apparent that girls are at a greater risk of experiencing underweight conditions if they are exposed to flooding during prenatal stages. This is evident from their higher rates of underweight in both age groups.")
                                            
                                            
                                          ),align = "Justify"
                                 )),
                        tabPanel("CHIRPS Results",
                                 fluidRow(style = "margin: 2px;",
                                          br(""),
                                          column(8,
                                                 
                                                 selectInput("agdrop7", "Age Categories:", width = "100%",
                                                             choices = c("Children Under 2 Years Old" = "7under2",
                                                                         "Children Under 5 Years Old" = "8under5"))),
                                          br(""),
                                          column(
                                            8,
                                            p(h5("Gender Disaggregation")),
                                            withSpinner(plotlyOutput("graph7", height = "500px", width ="100%")),
                                            withSpinner(plotlyOutput("graph8", height = "500px", width ="100%")),
                                          ),
                                          column(
                                            4,
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            br(""),
                                            p(h4(strong("Description"))),
                                            p("Precipitation intensities during each trimester does not reveal any evident correlation with the prevalence of underweight among children of both genders. Moreover, disaggregating the data by age does not appear to significantly alter the findings. Despite the lack of association between rainfall intensity and underweight, it is alarming to observe high rates of underweight in both genders, surpassing 20 percent as depicted in the graphs on the left. This is a matter of great concern."), 
                                          ),align = "Justify"
                                 )),
                        tabPanel("Regression Results",
                                 p("This is sub-tab 2.2.")
                        )
                        
                        
                      )
             ),
             tabPanel("Mechanisms",
                      fluidRow(style = "margin: 2px;",
                               
                               column(
                                 12,align = "center",
                                 h1(strong("Mechanisms")))),
                      fluidRow(style = "margin: 2px;",
                               br(""),
                               column(
                                 12,
                                 # 
                                 # h4(strong("1. Access to healthcare facilities")), 
                                 # p(""),
                                 # br(),
                                 # br(),
                                 # h4(strong("2.Consumption patterns and nutrition")),
                                 # p(""),
                                 # br(),
                                 # br(),
                                 # h4(strong("3. Agricultural and industrial disruptions")), 
                                 # p(""),
                                 # br(),
                                 # br(),
                                 # h4(strong("4.Water quality and consumption")),
                                 # p(""),
                                 # br(),
                                 # br(),
                                 # h4(strong("5.Displacement and living substandard conditions")),
                                 # p("")
                                 # 
                                 
                                 tabsetPanel(
                                   tabPanel("Access to healthcare facilities",

                                            #setting the stuff under the child profile tab HERE JADE
                                            fluidRow(style = "margin: 4px;",
                                                     p("", style = "padding-top:10px;"),
                                                     column(8,
                                                            selectInput("ahcdrop", "Select Access to Healthcare:", width = "100%",
                                                                        choices = c("Household Distance from Health Center " = "dist_med_ahc",
                                                                                    "Mode of Transportation" = "trnsprt_ahc",
                                                                                    "Antenatal Care" = "ant_care_ahc",
                                                                                    "Primary Advisor" = "prim_adv_ahc",
                                                                                    "Frequency of Antenatal Care Visits" = "num_visit_ahc",
                                                                                    "Delivery Location" = "deliv_loc_ahc",
                                                                                    "Person Present During Delivery" = "person_present_ahc",
                                                                                    "Type of Licensed Professional Present During Delivery" = "lpp_ahc")),
                                                            br(""),
                                                            withSpinner(plotlyOutput("ahc", height = "500px", width ="100%"))),
                                                     column(4,
                                                            br(""),
                                                            br(""),
                                                            br(""),
                                                            br(""),
                                                            h4(strong("Description")),
                                                            textOutput("ahctext"),
                                                            align = "justify"))),
                                  
                                   
                                   tabPanel("Consumption Patterns and Nutrition",
                                            fluidRow(style = "margin: 4px;",
                                                     p("", style = "padding=top:10px;"),
                                                     
                                                     column(width = 8, align = "left",
                                                            selectInput("cpndrop", "Select Consumption Patterns and Nutrition Variable Below:", width = "100%", 
                                                                        choices = c("Duration of Calcium Intake" = "calcium_intake",
                                                                                    "Duration of Iron Intake" = "iron_intake", 
                                                                                    "Vitamin A Supplement Intake" = "vitamin_A",
                                                                                    "Infant and Young Child Feeding Practices Test Score: Cumulative" = "cq_fig",
                                                                                    "Infant and Young Child Feeding Practices Test Score: Per Question" = "pq_fig"),
                                                                        
                                                            ),   
                                                            br(""),
                                                            withSpinner(plotlyOutput("cpn", height = "500px", width ="100%"))
                                                     ),
                                                     br(""),
                                                     br(""),
                                                     br(""),
                                                     br(""),
                                                     br(""),
                                                     
                                                     column(4,
                                                            h4(strong("Description")),
                                                            textOutput("cpntext"), align = "justify")
                                                     
                                            )),
                                   
                
                                   
                                   
                                   tabPanel("Water Quality and Consumption",
                                            fluidRow(style = "margin: 4px;",
                                                     p("", style = "padding-top:10px;"),
                                                     
                                                     column(8, align = "justified",
                                                            selectInput("waterdrop", "Select water quality and consumption:", width = "100%", choices = c(
                                                              "Sources of Drinking Water" = "source_drinking_water",
                                                              "Water Treatment" = "water_treatment"
                                                              
                                                              
                                                            ),
                                                            ),   
                                                            br(""),
                                                            
                                                            
                                                            withSpinner(plotlyOutput("waters", height = "500px", width ="100%"))
                                                     ),
                                                     br(""),
                                                     br(""),
                                                     br(""),
                                                     br(""),
                                                     br(""),
                                                     
                                                     column(4,
                                                            h4(strong("Description")),
                                                            textOutput("descw"), align = "Justify")
                                                     
                                            )
                                            
                                            
                                          
                                   )
                                )
                                 
                                 # column(
                                 #   4,
                                 #   align = "justify",
                                 #   h5(strong("Techniques used to fetch groundwater in Bangladesh")),
                                 #   h2(strong("")),
                                 #   img(src = "bangladeshwater.jpg", align = 'right', width = "100%", height = "auto"),
                                 #   p("Source: IAEA/Bangladesh Atomic Energy Commission (BAEC)", style = "font-size:12px;")
                                 #   
                                 
                               )))),
  tabPanel("Discussion/Conclusion", value = "overview",
           fluidRow(style = "margin: 6px;",
                    p("", style = "padding-top:10px;"),
                    column(12, align = "center",h1(strong("Discussion/Conclusion")),
                           p(""),
                           br("")))),
  tabPanel("References", value = "overview",
           fluidRow(style = "margin: 6px;",
                    p("", style = "padding-top:10px;"),
                    column(12, align = "center",h1(strong("References"))),
                    column(6, align = "justify",
                           
                           p("Abiona, O., & Ajefu, J. B. (2022). The impact of timing of in utero drought shocks on birth outcomes in rural households: evidence from Sierra Leone. Journal of Population Economics, 36(3), 1333–1362. https://doi.org/10.1007/s00148-022-00926-w"),
                           p("Aggarwal, S. (2021). The long road to health: Healthcare utilization impacts of a road pavement policy in rural India. Journal of Development Economics, 151, 102667. https://doi.org/10.1016/j.jdeveco.2021.102667"),
                           p("Angeles, I. T., Schultink, W. J., Matulessi, P., Gross, R., & Sastroamidjojo, S. (1993). Decreased rate of stunting among anemic Indonesian preschool children through iron supplementation. The American journal of clinical nutrition, 58(3), 339-342."),
                           p("Asadullah, M. N., & Rahman, S. (2009). Farm productivity and efficiency in rural Bangladesh: the role of education revisited. Applied economics, 41(1), 17-33."),
                           p("Baten, A., Wallemacq, P., van Loenhout, J. A. F., & Guha-Sapir, D. (2020). Impact of recurrent floods on the utilization of maternal and newborn healthcare in Bangladesh. Maternal and child health journal, 24, 748-758."),
                           p("Butte, N., Cobb, K., Dwyer, J., Graney, L., Heird, W., & Rickard, K. (2004). The start healthy feeding guidelines for infants and toddlers. Journal of the American Dietetic Association, 104(3), 442-454."),
                           p("Castillo-Duran, C., Vial, P., & Uauy, R. (1988). Trace mineral balance during acute diarrhea in infants. The Journal of pediatrics, 113(3), 452-457."),
                           p("De Silva, M. M. G. T., & Kawasaki, A. (2020). A local-scale analysis to understand differences in socioeconomic factors affecting economic loss due to floods among different communities. International Journal of Disaster Risk Reduction: IJDRR, 47(101526), 101526. https://doi.org/10.1016/j.ijdrr.2020.101526"),
                           p("Del Ninno, C., & Lundberg, M. (2005). Treading water. Economics and Human Biology, 3(1), 67–96. https://doi.org/10.1016/j.ehb.2004.12.002"),
                           p("Del Ninno, C., & Lundberg, M. (2005). Treading water: the long-term impact of the 1998 flood on nutrition in Bangladesh. Economics & Human Biology, 3(1), 67-96."),
                           p("Dimitrova, A., & Bora, J. K. (2020). Monsoon weather and early childhood health in India. PloS One, 15(4), e0231479. https://doi.org/10.1371/journal.pone.0231479"),
                           p("Floyd, A., & Sakellariou, D. (2017). Healthcare access for refugee women with limited literacy: layers of disadvantage. International Journal for Equity in Health, 16(1). https://doi.org/10.1186/s12939-017-0694-8"),
                           p("Gilson, L., Sen, P. D., Mohammed, S., & Mujinja, P. (1994). The potential of health sector non-governmental organizations: policy options. Health policy and planning, 9(1), 14-24."),
                           p("Gogia, S., & Sachdev, H. S. (2010). Maternal postpartum vitamin A supplementation for the prevention of mortality and morbidity in infancy: a systematic review of randomized controlled trials. International Journal of Epidemiology, 39(5), 1217–1226. https://doi.org/10.1093/ije/dyq080"),
                           p("Guideline: Iron Supplementation in Postpartum Women. Geneva: World Health Organization; 2016. EXECUTIVE SUMMARY. Available from: https://www.ncbi.nlm.nih.gov/books/NBK379995/"),
                           p("Guideline: Iron Supplementation in Postpartum Women. Geneva: World Health Organization; 2016. BACKGROUND. Available from: https://www.ncbi.nlm.nih.gov/books/NBK379991/"),
                           p("Guiteras, R., Jina, A., & Mobarak, A. M. (2015). Satellites, self-reports, and submersion: Exposure to floods in Bangladesh. The American Economic Review, 105(5), 232–236. https://doi.org/10.1257/aer.p20151095"),
                           p("Hasan, M. K., Shahriar, A., & Jim, K. U. (2019). Water pollution in Bangladesh and its impact on public health. Heliyon, 5(8), e02145. https://doi.org/10.1016/j.heliyon.2019.e02145."),
                           p("Hashizume, M., Faruque, A. S. G., Wagatsuma, Y., Hayashi, T., & Armstrong, B. (2010). Cholera in Bangladesh. Epidemiology, 21(5), 706–710. https://doi.org/10.1097/ede.0b013e3181e5b053"),
                           p("Heather C Stobaugh and others, Children with Poor Linear Growth Are at Risk for Repeated Relapse to Wasting after Recovery from Moderate Acute Malnutrition, The Journal of Nutrition, Volume 148, Issue 6, June 2018, Pages 974–979, https://doi.org/10.1093/jn/nxy033"),
                           p("Hilmert, C. J., Kvasnicka-Gates, L., Teoh, A. N., Bresin, K., & Fiebiger, S. (2016). Major flood related strains and pregnancy outcomes. Health Psychology, 35(11), 1189."),
                           p("Hoque, M. E., Khokan, M. R., & Bari, W. (2014). Impact of stature on non-communicable diseases: evidence based on Bangladesh Demographic and Health Survey, 2011 data. BMC Public Health, 14(1), 1007. https://doi.org/10.1186/1471-2458-14-1007"),
                           p("Institute of Medicine (US) Committee on Nutritional Status During Pregnancy, & Lactation. (1990). Iron nutrition during pregnancy. National Academies Press."),
                           p("Khan, J., Vesel, L., Bahl, R., & Martines, J. C. (2015). Timing of breastfeeding initiation and exclusivity of breastfeeding during the first month of life: effects on neonatal mortality and morbidity—a systematic review and meta-analysis. Maternal and child health journal, 19, 468-479."),
                           p("Kumar, A., & Kaur, S. (2017). Calcium: A nutrient in pregnancy. Journal of Obstetrics and Gynaecology of India, 67(5), 313–318. https://doi.org/10.1007/s13224-017-1007-2"),
                           p("Leroy, J. L., & Frongillo, E. A. (2019). Perspective: what does stunting really mean? A critical review of the evidence. Advances in Nutrition, 10(2), 196-204."),
                           p("Luby, S. P., Gupta, S., Sheikh, M. S., Johnston, R., Ram, P. C., & Islam, M. S. (2008). Tubewell water quality and predictors of contamination in three flood-prone areas in Bangladesh. Journal of Applied Microbiology, 105(4), 1002–1008. https://doi.org/10.1111/j.1365-2672.2008.03826.x"),
                           
                           br("")),
                    column(6, align = "justify",
                           
                           
                           p("Lutter, C. K. (2008). Iron deficiency in young children in low-income countries and new approaches for its prevention. The Journal of nutrition, 138(12), 2523-2528."),
                           p("M.M.G.T. De Silva, Akiyuki Kawasaki (2020). A local-scale analysis to understand differences in socioeconomic factors affecting economic loss due to floods among different communities. International Journal of Disaster Risk Reduction, 47, 101526. https://doi.org/10.1016/j.ijdrr.2020.101526"),
                           p("MacDonald, R. (2001). Providing clean water: lessons from Bangladesh. BMJ, 322(7287), 626–627. https://doi.org/10.1136/bmj.322.7287.626"),
                           p("Mallett, L. H., & Etzel, R. A. (2017). Flooding: what is the impact on pregnancy and child health? Disasters, 42(3), 432–458. https://doi.org/10.1111/disa.12256"),
                           p("Moss, K. M., Simcock, G., Cobham, V. E., Kildea, S., Laplante, D. P., & King, S. (2018). Continuous, emerging, and dissipating associations between prenatal maternal stress and child cognitive and motor development: The QF2011 Queensland Flood Study. Early Human Development, 119, 29–37. https://doi.org/10.1016/j.earlhumdev.2018.02.022"),
                           p("National Strategy on Prevention and Control of Micronutrient Deficiencies (2015-2024)- UNICEF. (2015, December). https://www.unicef.org/bangladesh/media/4646/file/NMDCS%20Final.pdf%20.pdf "),
                           p("Newman, K. L., Gustafson, K., Englund, J. A., Magaret, A., Khatry, S., LeClerq, S. C., Tielsch, J. M., Katz, J., & Chu, H. Y. (2019). Effect of Diarrheal Illness During Pregnancy on Adverse Birth Outcomes in Nepal. Open forum infectious diseases, 6(2), ofz011. https://doi.org/10.1093/ofid/ofz011"),
                           p("Ochani, S., Aaqil, S. I., Nazir, A., Athar, F. B., & Ullah, K. (2022). Effect of recent floods on women's reproductive health in Pakistan: an alarming situation. International Journal of Surgery, 6(1), e98. https://doi.org/10.1097/gh9.0000000000000098"),
                           p("Odei Obeng-Amoako, G. A., Stobaugh, H., Wrottesley, S. V., Khara, T., Binns, P., Trehan, I., Black, R. E., Webb, P., Mwangome, M., Bailey, J., Bahwere, P., Dolan, C., Boyd, E., Briend, A., Myatt, M. A., & Lelijveld, N. (2023). How do children with severe underweight and wasting respond to treatment? A pooled secondary data analysis to inform future intervention studies. Maternal & Child Nutrition, 19(1). https://doi.org/10.1111/mcn.13434"),
                           p("Oliveira, J. M., Allert, R., & East, C. E. (2016). Vitamin A supplementation for postpartum women. Cochrane Database of Systematic Reviews, 3(4), CD005944. https://doi.org/10.1002/14651858.CD005944.pub3"),
                           p("Orderud, H., Härkönen, J., Hårsaker, C. T., & Bogren, M. (2022). Floods and maternal healthcare utilisation in Bangladesh. Population and Environment, 44(3–4), 193–225. https://doi.org/10.1007/s11111-022-00410-3"),
                           p("Oskorouchi, H. R., & Sousa-Poza, A. (2021). Floods, food security, and coping strategies: Evidence from Afghanistan. Agricultural Economics, 52(1), 123–140. https://doi.org/10.1111/agec.12610"),
                           p("Perez-Escamilla, R., Segura-Pérez, S., & Lott, M. (2017). Feeding guidelines for infants and young toddlers: a responsive parenting approach. Nutrition Today, 52(5), 223-231."),
                           p("Prell, C., & Koletzko, B. (2016). Breastfeeding and complementary feeding: recommendations on infant nutrition. Deutsches Ärzteblatt International, 113(25), 435"),
                           p("Rosales-Rueda, M. (2018). The impact of early life shocks on human capital formation: Evidence from El Niño floods in Ecuador. Journal of health economics, 62, 13-44."),
                           p("Sarah L. Smiley, Herbert Hambati; Impacts of flooding on drinking water access in Dar es Salaam, Tanzania: implications for the Sustainable Development Goals. Journal of Water, Sanitation and Hygiene for Development 1 June 2019; 9 (2): 392–396. doi: https://doi.org/10.2166/washdev.2019.168"),
                           p("Schatz, J. J. (2008). Floods hamper health-care delivery in southern Africa. The Lancet, 371(9615), 799-800. https://doi.org/10.1016/S0140-6736(08)60362-1"),
                           p("Sharma, D., Shastri, S., & Sharma, P. (2016). Intrauterine growth restriction: Antenatal and postnatal aspects. Clinical Medicine Insights. Pediatrics, 10, 67–83. https://doi.org/10.4137/CMPed.S40070"),
                           p("Sialubanje, C., Kaiser, J. L., Ngoma, T., Mwananyanda, L., Fong, R. M., Hamer, D. H., & Scott, N. A. (2023). Postnatal care services in rural Zambia: a qualitative exploration of user, provider, and community perspectives on quality of care. BMC Pregnancy and Childbirth, 23(1). https://doi.org/10.1186/s12884-023-05350-w"),
                           p("Sirajul Islam, M., Brooks, A., Kabir, M. S., Jahid, I. K., Shafiqul Islam, M., Goswami, D., Nair, G. B., Larson, C., Yukiko, W., &amp; Luby, S. (2007). Faecal contamination of drinking water sources of Dhaka city during the 2004 flood in Bangladesh and use of disinfectants for water treatment. Journal of Applied Microbiology, 103(1), 80–87. https://doi.org/10.1111/j.1365-2672.2006.03234.x "),
                           p("Sutherland, S., & Brunwasser, S. M. (2018). Sex Differences in Vulnerability to Prenatal Stress: a Review of the Recent Literature. Current Psychiatry Reports, 20(11). https://doi.org/10.1007/s11920-018-0961-4"),
                           p("Svefors, P., Pervin, J., Islam Khan, A., Rahman, A., Ekström, E.-C., El Arifeen, S., Ekholm Selling, K., & Persson, L.-Å. (2020). Stunting, recovery from stunting and puberty development in the MINIMat cohort, Bangladesh. Acta Paediatrica (Oslo, Norway: 1992), 109(1), 122–133. https://doi.org/10.1111/apa.14929"),
                           p("Uruakpa, F. O., Ismond, M. A. H., & Akobundu, E. N. (2002). Colostrum and its benefits: a review. Nutrition research, 22(6), 755-767."),
                           p("Verma, P., & Prasad, J. B. (2021). Stunting, wasting and underweight as indicators of under-nutrition in under five children from developing Countries: A systematic review. Diabetes & Metabolic Syndrome, 15(5), 102243. https://doi.org/10.1016/j.dsx.2021.102243"),
                           p("Wai, K. M., Mar, O., Kosaka, S., Umemura, M., & Watanabe, C. (2017). Prenatal Heavy Metal Exposure and Adverse Birth Outcomes in Myanmar: A Birth-Cohort Study. International journal of environmental research and public health, 14(11), 1339. https://doi.org/10.3390/ijerph14111339"),
                           p("Yusof, A., Siddique, A. K., Baqui, A. H., Eusof, A., & Zaman, K. (1991). 1988 floods in Bangladesh: pattern of illness and causes of death. Journal of diarrhoeal diseases research, 310-314."),
                           
                           ))),
  tabPanel("Our Team", value = "overview",
           fluidRow(style = "margin: 6px;",
                    p("", style = "padding-top:10px;"),
                    column(12, align = "center",h1(strong("Meet the Team")),
                           p(""),
                           br(""),
                           h4(strong("VT Data Science for the Public Good"), align = "center"),
                           p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                             "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics. '), 
                             "In its third year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                     around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                     determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                     how to apply, and our annual symposium, please visit", 
                             a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                           p("", style = "padding-top:10px;")
                    ),
                    fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                             column(4, align = "center",
                                    h4(strong("DSPG Graduate Fellow Member")),
                                    img(src = "naveennew.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                    p("", style = "padding-top:10px;"), 
                                    p(a(href = 'https://www.linkedin.com/in/naveen-abedin-0ab1089a', 'Naveen Abedin', target = '_blank'), " is a second-year Ph.D. student in Economics at Virginia Tech, specializing in Agricultural and Applied Economics."),
                                    
                                    
                                    p("", style = "padding-top:10px;"), 
                                    img(src = "Nandini_Das.JPG", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                    
                                    p("", style = "padding-top:10px;"), 
                                    p(a(href = 'https://www.linkedin.com/in/nandini-das-390577104/', 'Nandini Das', target = '_blank'), " is a third year Ph.D. candidate in Economics at Virginia Tech. "),
                                    
                                    
                                    p("", style = "padding-top:10px;") 
                             ),
                             column(4, align = "center",
                                    h4(strong("DSPG Undergraduate Members")),
                                    img(src = "Sotaire_Kwizera (1).jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                    p(a(href = 'https://www.linkedin.com/in/sotairekwizera', 'Sotaire Kwizera', target = '_blank'), " is a junior at Berea College pursuing an undergraduate degree in Economics & Computer Science with a concentration in Data Analytics & Modeling.

"),
                                    img(src = "Jade_Nguyen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                    p(a(href = 'https://www.linkedin.com/in/ngoc-anh-nguyen-a8a590219', 'Jade Nguyen', target = '_blank'), " is a junior at Virginia Polytechnic Institute and State University pursuing an undergraduate degree in Business Information Technology with a concentration in Decision Support Systems."),
                                    img(src = "Riya_Pulla.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "200px"),
                                    p(a(href = 'https://www.linkedin.com/in/riyapulla', 'Riya Pulla', target = '_blank'), " is a junior at Virginia Commonwealth University pursuing an undergraduate degree in Bioinformatics with a concentration in Computational Sciences."),
                                    
                             ),
                             column(4, align = "center",
                                    h4(strong("Faculty Advisor")),
                                    img(src = "dr.chen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                    
                                    p("", style = "padding-top:10px;"), 
                                    p(a(href = "https://aaec.vt.edu/people/faculty/holmes-chanita.html", 'Dr. Susan Chen,', target = '_blank'), " Ph.D., is an Associate Professor in the Department of Agricultural and Applied Economics (AAEC) at Virginia Tech. She serves as the Director of the Data Science for Public Good (DSPG) program, overseeing its operations and initiatives. Additionally, she holds the position of Graduate Program Director, guiding and supporting students in their academic and research endeavors. ") , 
                                    
                                    p("", style = "padding-top:10px;")
                             )) ,
                    fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                             h4(strong("Project Stakeholder")),
                             img(src = "IFPRILogo.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                             p(a(href = 'https://www.lcps.org/outreachservices', 'Dr. Mehrab Bakhtiar', target = '_blank'), "(International Food Policy Research Institute (IFPRI))"),
                             
                             p("", style = "padding-top:10px;"),
                             h4(strong("Acknowledgments")) ,
                             p("We would like to thank (---) officials for providing us with data for our project. "),
                             p("", style = "padding-top:10px;")
                    )
           )
           
  )
  
)


# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  Var_hhform <- reactive({
    input$hhformdrop
  })
  
  output$hhform <- renderPlotly({
    if (Var_hhform() == "household_size_division") {
      
      p_hh<- ggplot(HH_size, 
                    aes(Division, Average_Household_size, fill = Division))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Average Household Size: ", Average_Household_size)))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Average Household Size by Division",
             x= "Division",
             y = "Average Household Size")+
        easy_remove_legend()+
        scale_fill_viridis_d() +
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        
        ylim(0,7)
      
      ggplotly(p_hh, tooltip = c("text"))
      
    }
    
    else if (Var_hhform() == "dependency_ratio_division") {
      
      p_ma<- p_depr<- ggplot(dep_r, 
                             aes(Division, Dependency_Ratio, fill = Division))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Dependency Ratio: ", Dependency_Ratio)))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Dependency Ratio by Division",
             x= "Division",
             y = "Dependency Ratio")+
        scale_fill_viridis_d() + 
        easy_remove_legend()+
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ylim(0, 0.5)
      
      ggplotly(p_depr, tooltip = c("text"))
      
    }
    
    else if (Var_hhform() == "household_headship_division") {
      
      p_h2o<-ggplot (hhh_gender,
                     aes(Division, Percentage, fill = Gender))+
        geom_bar(position="dodge", stat="identity")+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Household Headship by Division by Gender",
             x= "Division",
             y = "Percentage")+
        scale_fill_manual(values = c("Male" = "#21918c", "Female" = "#cc4778")) +
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ylim(0,100)
      ggplotly(p_h2o)
      
    }  
  })
  
  
  output$desc1 <- renderText({
    " Description"
  })
  
  
  output$desc1 <- renderText({
    if (Var_hhform() == "household_size_division") {
      
      "The average household size is around 4.2 members across all divisions. Sylhet has relatively large households with an average household size of 4.9 members."
      
    }
    
    else if (Var_hhform() == "dependency_ratio_division") {
      
      "The dependency ratio is a demographic measure that compares the working-age population (15-65 years) to the non-working-age population (<15 and >65 years). A higher dependency ratio indicates a greater number of dependents per working-age person who may require support. In our analysis, the dependency ratio is below 40% across all divisions."
      
    }
    
    else if (Var_hhform() == "household_headship_division") {
      
      "The predominance of male household heads in rural Bangladesh reflects traditional gender roles and cultural norms, where men are often perceived as the primary decision-makers and main income generators. This gender disparity may have significant implications for women's empowerment and access to resources, as female-headed households might face additional challenges in accessing economic opportunities and social support.The graph shows Chittagong stands out with a relatively higher percentage (33.57%) of female household heads, suggesting some regional variation in household leadership dynamics."
      
    }  
  })
  
  
  Var3 <- reactive({
    input$demos1drop
  })
  
  output$demo1 <- renderPlotly({
    
    if (Var3() == "age_by_gender") {
      
      pgg <- ggplot(avg, aes(Division, Mean_age, fill = Gender))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Average Age: ", Mean_age)))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Average Age by Division by Gender",
             x= "Division",
             y = "Average Age")+
        scale_fill_manual(values = c("Male" = "#21918c", "Female" = "#cc4778")) +
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ylim(0, 35)
      
      ggplotly(pgg, tooltip = c("text"))
      
    }
    
    else if (Var3() == "male_age") {
      
      p_ma<- ggplot(age_div_male, 
                    aes(Division, Percentage, fill = Age_range))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Average Range: ", Age_range, "\n", "Percentage: ", Percentage)))+
        scale_fill_viridis_d() +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        scale_fill_manual(values = c("0-5 yrs" = "#440154", "6-10 yrs" = "#fde725",
                                     "11-17 yrs" = "#3a528b", "18-30 yrs" = "#21918c", 
                                     "31-65 yrs" = "#ff7f00","66-80 yrs" = "#bd93f5", 
                                     "#5ec962", "#cc4778",
                                     "80+ yrs" = "#f6c2f8"))+
        labs(title = "Male Age Distribution by Division",
             x= "Division",
             y = "Percentage")+
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ggeasy::easy_add_legend_title("Age Range")+
        ylim(0,50)
      ggplotly(p_ma, tooltip = c("text"))
    }
    
    else if (Var3() == "female_age") {
      
      p_fe <- ggplot(age_div_female, aes(Division, Percentage, fill = Age_range)) +
        geom_bar(position = "dodge", stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Average Range: ", Age_range, "\n", "Percentage: ", Percentage))) +
        scale_fill_viridis_d() +  # Set colors to viridis defaults
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        scale_fill_manual(values = c("0-5 yrs" = "#440154", "6-10 yrs" = "#fde725",
                                     "11-17 yrs" = "#3a528b", "18-30 yrs" = "#21918c", 
                                     "31-65 yrs" = "#ff7f00","66-80 yrs" = "#bd93f5", 
                                     "#5ec962", "80+ yrs" = "#f6c2f8"))+
        # easy_all_text_colour("#630031") +
        labs(title = "Female Age Distribution by Division", x = "Division", y = "Percentage") +
        ggeasy::easy_rotate_labels(which = "x", angle = 300) +
        ggeasy::easy_add_legend_title("Age Range")+
        ylim(0, 50)
      ggplotly(p_fe, tooltip = c("text"))
    }
    else if (Var3() == "head_age") {
      p_hhh<-ggplot (hhh_age,aes(Division, Percentage, fill = Age_category))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Average Range: ", Age_category, "\n", "Percentage: ", Percentage)))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Household Head Age Distribution by Division",
             x= "Division",
             y = "Percentage")+
        scale_fill_viridis_d() + 
        scale_fill_manual(values = c("0-5 yrs" = "#440154", "6-10 yrs" = "#fde725",
                                     "11-17 yrs" = "#3a528b", "18-30 yrs" = "#21918c", 
                                     "31-65 yrs" = "#ff7f00","66-80 yrs" = "#bd93f5", 
                                     "#5ec962", "80+ yrs" = "#f6c2f8"))+
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ggeasy::easy_add_legend_title("Age Range")+
        ylim(0,100)
      ggplotly(p_hhh, tooltip = c("text"))
      
    }
    
    
  })
  
  output$desc2 <- renderText({
    if (Var3() == "age_by_gender") {
      
      "There are no significant age variations between males and females. However, females tend to be slightly older than males. In Khulna, the average age for males is 30.44 years old, while for females, it is 31.77 years, making it the division with the highest average age for both genders. Conversely, in Chittagong, the average age is the lowest, with males averaging 26.13 years and females averaging 27.79 years."
      
    }
    
    else if (Var3() == "male_age") {
      
      "Across all divisions, the modal age range is 31-65 years. Khulna has the highest percentage of people in that category at 40.76%. It is also important to note that around 15% of the male population consists of children under the age of 5 across all divisions."
      
    }
    
    else if (Var3() == "female_age") {
      
      "Similar to their male counterparts, a significant proportion of the female population in rural Bangladesh falls within the 31-65 years age range. Khulna division stands out with the highest percentage (43.74%) of females falling within this age category. Additionally, female children aged 5 years constitute approximately 10% of the population across all divisions."
      
    }
    else if(Var3() == "head_age"){
      "The most frequent household head age range across all of the divisions is 31-65 years, and the least is 80+ years. Some factors that contribute to this could be that people under 31 years old migrate to cities or abroad in search of better educational and employment opportunities." 
    }
  }) 
  
  
  hedu <- reactive({
    input$edudrop
  })
  
  output$edu <- renderPlotly({
    
    if (hedu() == "male_education_division") {
      
      p_m <- ggplot(edu_div_male, aes(Division, Percentage, fill = Education_level)) +
        geom_bar(position = "stack", stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Education Level: ", Education_level, "\n", "Percentage: ", Percentage))) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        scale_fill_viridis_d() +
        scale_fill_manual(values = c("No education" = "#440154", "Less than primary" = "#fde725",
                                     "Completed primary" = "#3a528b", "Completed secondary" = "#21918c", 
                                     "Higher" = "#ff7f00"))+
        labs(
          title = "Educational Attainment of Males by Division",
          x = "Division",
          y = "Percentage") +
        ylim(0, 100) +
        coord_flip()+
        ggeasy::easy_add_legend_title("Education Level")
      
      ggplotly(p_m, tooltip = c("text"))
      
    }
    
    else if (hedu() == "female_education_division") {
      
      p_f <- ggplot(edu_div_female, aes(Division, Percentage, fill = Education_level)) +
        geom_bar(position = "stack", stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Education Level: ", Education_level, "\n", "Percentage: ", Percentage))) + 
        scale_fill_viridis_d() +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        scale_fill_manual(values = c("No education" = "#440154" , "Less than primary" = "#fde725",
                                     "Completed primary" = "#3a528b", "Completed secondary" = "#21918c", 
                                     "Higher" = "#ff7f00"))+
        # easy_all_text_colour("#630031") +
        labs(title = "Educational Attainment of Females by Division",
             x = "Division",
             y = "Percentage") +
        coord_flip()+
        ggeasy::easy_add_legend_title("Education Level")
      
      ggplotly(p_f, tooltip = c("text"))
    }
    
    else if (hedu() == "hh_highest_education") {
      
      p_h <- ggplot(hh_edu_div, aes(Division, Percentage, fill = Education_level)) +
        geom_bar(stat = "identity", position = "dodge", aes(text = paste0("Division: ", Division, "\n", "Education Level: ", Education_level, "\n", "Percentage: ", Percentage))) +
        labs(title = "Highest Household Education Level by Division",
             x = "Division",
             y = "Percentage") +
        scale_fill_manual(values = c("No education" = "#440154", "Less than primary" = "#fde725",
                                     "Completed primary" = "#3a528b", "Completed secondary" = "#21918c", 
                                     "Higher" = "#ff7f00"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        ggeasy::easy_rotate_labels(which = "x", angle = 300) +
        ggeasy::easy_add_legend_title("Education Level")+
        ylim(0, 70)
      
      ggplotly(p_h, tooltip = c("text"))
    }
    else if (hedu() == "hh_ head_education") {
      p_hh <- ggplot(hhh_edu_div, aes(Division, Percentage, fill = Education_level)) +
        geom_bar(position = "dodge", stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Education Level: ", Education_level, "\n", "Percentage: ", Percentage))) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(
          title = "Household Head Educational Attainment by Division",
          x = "Division",
          y = "Percentage"
        ) +
        scale_fill_viridis_d() +
        scale_fill_manual(values = c("No education" = "#440154", "Less than primary" = "#fde725",
                                     "Completed primary" = "#3a528b", "Completed secondary" = "#21918c", 
                                     "Higher" = "#ff7f00"))+
        ggeasy::easy_rotate_labels(which = "x", angle = 300) +
        ggeasy::easy_add_legend_title("Education Level")+
        ylim(0, 50)  
      
      ggplotly(p_hh, tooltip = c("text"))}
  }
  )
  
  output$desc3 <- renderText({
    if (hedu() == "male_education_division") {
      
      "In general, there are more males with no educational attainment than females. Despite this, a higher percentage of males attain higher education when compared to females. Sylhet, a more flood-prone division, has the lowest percentage of males in higher education at 4.16%."
      
    }
    
    else if (hedu() == "female_education_division") {
      
      "In general across all divisions, most females have no formal education or only completed primary education. Rangpur has the highest percentage of uneducated females with 42.55%. The percentage range of females who manage to go past secondary school is between 2.83% to 5.15%."
      
    }
    
    else if (hedu() == "hh_highest_education") {
      
      "In this graph, more than half of the sampled households report completing primary education as their highest household educational attainment across all divisions. Rangpur, one of the more flood prone divisions, has the highest percentage of households whose members have no formal education.
"
      
    }
    else if(hedu() == "hh_ head_education"){
      "Household heads are primarily responsible for making income for the family and major decisions, but based on the head’s educational attainment by division, it is evident that a majority of household heads across all divisions either do not have education or discontinued their education after primary school which is equivalent to the fifth grade in the US. These low levels of education could potentially have negative impacts on household members." 
    }
  }) 
  
  
  hheco <- reactive({
    input$ecodrop
  })
  
  output$eco <- renderPlotly({
    
    if (hheco() == "households_below_poverty_line") {
      
      p_ppp<-ggplot (under_ppp,
                     aes(Division, Percentage, fill = Categories))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "Percentage: ", Percentage)))+ 
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Households Living Below Poverty Line by Division",
             x= "Division",
             y = "Percentage")+
        scale_fill_viridis_d() +
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        easy_remove_legend()+
        ylim(0,20)
      ggplotly(p_ppp, tooltip = c("text")) 
      
      
    }
    
    else if (hheco() == "households_farming_activities") {
      
      ag <- ggplot(hh_rfarm, aes(x = Division, y = Percentage, fill = Activity)) +
        geom_bar(position = position_dodge(width = 0.7), width = 0.7, stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Agricultural Activity: ", Activity, "\n", "Percentage: ", Percentage))) +
        xlab("Division") +
        ylab("Percentage") +
        ggtitle("Household Agricultural Activities by Division") +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        scale_fill_viridis_d() +
        ggeasy::easy_add_legend_title("Agricultural Activities")+
        coord_flip()
      ggplotly(ag, tooltip = c("text"))
    }
    
    else if (hheco() == "cultivable_land") {
      
      p_land <- ggplot(cult_land_div, aes(Division, Percentage, fill = Farm_size)) +
        geom_bar(position = "dodge", stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Farm Size: ", Farm_size, "\n", "Percentage: ", Percentage))) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(
          title = "Cultivable Land Holding by Division",
          x = "Division",
          y = "Percentage"
        ) +
        # scale_fill_viridis_d() + 
        scale_fill_manual(values = c("No cultivable land" = "#440154", "Marginal: <0.5 acres" = "#fde725",
                                     "Small: 0.5-1.5 acres" = "#3a528b", "Medium: 1.5-2.5 acres" = "#21918c", 
                                     "Large: 2.5+ acres" = "#ff7f00"))+
        ggeasy::easy_add_legend_title("Farm Size")+
        # ggeasy::easy_rotate_labels(which = "x", angle = 300) +
        ylim(0, 50, tooltip = c("text"))  
      
      ggplotly(p_land)
    }
    else if (hheco() == "electricity_accessibility") {
      p_el<-ggplot (eletri,
                    aes(Division, Percentage, fill = Electricity))+
        geom_bar(position="dodge", stat="identity")+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Households Without Electricity by Division",
             x= "Division",
             y = "Percentage")+
        
        scale_fill_viridis_d() +
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        easy_remove_legend()+
        ylim(0,30)
      ggplotly(p_el)
      
    }
    
    else if (hheco() == "water_improvement") {
      
      p_ih2o<-ggplot (imp_h2o,
                      aes(Division, Percentage, fill = Improved_water))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Water Source: ", Improved_water, "\n", "Percentage: ", Percentage)))+
        scale_fill_viridis_d() + 
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Unimproved Water Usage by Division",
             x= "Division",
             y = "Percentage")+
        easy_remove_legend()+
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ylim(0,100)
      ggplotly(p_ih2o, tooltip = c("text"))
    }
    else if (hheco() == "hhh_occupation") {
      p_hm <- ggplot(hhh_main_occup, aes(Division, Percentage, fill = Occupations)) +
        geom_bar(position = "stack", stat = "identity", aes(text = paste0("Division: ", Division, "\n", "Occupation: ", Occupations, "\n", "Percentage: ", Percentage))) + #, aes(text = paste0(Percentage, "%"))
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(
          title = "Household Head Occupation by Division",
          x = "Division",
          y = "Percentage"
        ) +
        scale_fill_viridis_d() +
        scale_fill_manual(values = c("Agricultural day labor" = "#440154", "Non-agricultural day labor" = "#fde725",
                                     "Salaried" = "#3a528b", "Rickshaw/van puller" = "#21918c", 
                                     "Self-employed" = "#ff7f00", "Business/trade" = "#bd93f5", 
                                     "Production business" = "#5ec962", "Livestock-related work" = "#cc4778",
                                     "Farming" = "#f6c2f8", "Non-earning occupations" = "#b26600"))+
        coord_flip()+
        ylim(0, 110)
      
      ggplotly(p_hm, tooltip = c("text"))
      
    }
    
  }
  )
  
  output$desc4 <- renderText({
    if (hheco() == "households_below_poverty_line") {
      
      "Using the international poverty line of $1.96 adjusted for 2011 purchasing power parity, our graph shows more flood prone divisions have a higher percentage of households that live below the poverty line in Bangladesh. For example, Rangpur, known to be a more flood prone region, had 17.13% of households surveyed living under the poverty line whereas Chittagong, a less flood prone division, had 3.29% of households surveyed living under the poverty line."
      
    }
    
    else if (hheco() == "households_farming_activities") {
      
      "Most households primarily engage in farming activities with approximately 90% of them relying on subsistence farming. Livestock and crop farming is the most prevalent agricultural activity compared to the other agricultural activities listed. Note that some households will participate in more than one agricultural activity.
"
      
    }
    
    else if (hheco() == "cultivable_land") {
      
      "Across divisions, the most common farm size categories are the “Marginal” and “No cultivable land” categories. Chittagong has the highest percent (44.97%) of households without cultivable land. It is followed by Sylhet at 43.09% without cultivable land."
      
    }
    else if(hheco() == "electricity_accessibility"){
      "This graph shows that the more flood prone divisions, Barisal and Rangpur have a higher percentage of surveyed households that do not have access to electricity within their household. Specifically, Barisal with 29.61% and Rangpur with 26.40%."
    }
    else if(hheco() == "water_treatment"){
      "In Bangladesh, 95.06% of surveyed households across divisions do not filter their drinking water. Water filtration is crucial in rural areas without access to improved water sources. During floods, available drinking water sources become contaminated with chemicals and waterborne pathogens. Filtering water is important to eliminate these pollutants. Rangpur and Barisal have the highest percentage of households not filtering water, while Sylhet has the lowest percentage." 
    }
    else if(hheco() == "water_improvement"){
      "The graph shows that a majority of surveyed households lack access to improved water sources, relying instead on hand pump tube wells that tap into groundwater. However, during flooding, these water sources become unreliable and unsafe for consumption without proper precautions. In Bangladesh, an average of 97.05% of surveyed households reported not having access to improved water sources across all divisions." 
    }
    else if(hheco() == "hhh_occupation"){
      "In each division, farming is the dominant occupation, with the highest concentration of farmers in Khulna. Non-earning occupations, including household chores, unpaid family labor, and others, are the second most common occupations across most divisions. Livestock-related work has the lowest frequency, accounting for less than 1% combined across all seven divisions. This is likely due to the prioritization of crop cultivation and the vulnerability of livestock to natural disasters.
"
    }
    
  }) 
  Var_regress <- reactive({
    input$regressiondrop
  })
  
  output$regression<- renderPlotly({
    if (Var_regress() == "D") {
      
      
    }
    
    else if (Var_regress() == "E") {
      
    }
    
    else if (Var_regress() == "F") {
      
    } 
    else if (Var_regress() == "F") {
      
    }  
  })
  
  
  output$desc6 <- renderText({
    " Description"
  })
  
  
  output$desc6 <- renderText({
    if (Var_regress() == "D") {
      
      " "
      
    }
    
    else if (Var_regress() == "E") {
      
      ""
      
    }
    
    else if (Var_regress() == "F") {
      
      ""
      
    } 
    else if (Var_regress() == "G") {
      
      ""
      
    }
  })
  
  Var_FT <- reactive({
    input$floodTimeline
  })
  
  output$timeline <- renderImage({
    
    if (Var_FT() == "2013") { 
      
      # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/new2013.jpg"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
    else if (Var_FT() == "2014") {    # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/2014.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
    else if (Var_FT() == "2015") {    # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/2015.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
    else if (Var_FT() == "2016") {    # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/2016.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
    else if (Var_FT() == "2017") {    # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/2017.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
    else if (Var_FT() == "2018") {    # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/2017.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
    else if (Var_FT() == "2019") {    # Return a list containing the filename and alt text
      list(src = paste0(getwd(),"/www/2019.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
    }
    
  })
  
  # Render map 
  var4 <- reactive({input$mcdrop1})
  #matching with ui for mc1
  output$mc1 <- renderPlotly({
    
    if (var4() == "stunt_div") {
      stuntplot <- ggplot(stuntdiv_combined, aes(x = Division, y = Percentage, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Children Stunted by Division and Gender",
             x = "Division",
             y = "Percentage",
             fill = "Gender") +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        scale_fill_manual(values = c("#65cb5e", "#21918c", "#cc4778")) 
      # stuntplot 
      
      stunt_gender_div <- ggplotly(stuntplot)}
    else if (var4() == "underweight_div") {
      # Create the bar plot using ggplot
      weightplot <- ggplot(weightdiv_combined, aes(x = Division, y = Percentage, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Children Underweight by Division by Gender",
             x = "Division",
             y = "Percentage",
             fill = "Gender") +
        scale_fill_manual(values = c("#65cb5e", "#21918c", "#cc4778")) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()
      
      underweight_gender_div <- ggplotly(weightplot)}
    else if (var4() == "avgbw_div") {
      # Create the bar plot using ggplot
      bwplot <- ggplot(bwdiv_combined, aes(x = Division, y = Mean_bw, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge", aes(text = paste0("Division: ", Division, "\n", "Gender: ", Gender, "\n", "Average Birth Weight: ", Mean_bw))) +
        labs(title = "Average Birth Weight by Division by Gender",
             x = "Division",
             y = "Birth Weight (kg)",
             fill = "Gender") +
        scale_fill_manual(values = c("#65cb5e", "#21918c", "#cc4778")) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()
      
      bw_gender_div <- ggplotly(bwplot, tooltip = c("text"))}
  })
  #TEXT OUPUT PER GRAPH
  
  output$mctext1 <- renderText({
    if (var4() == "stunt_div") {
      "Stunting, defined by the World Health Organization, is having a height-for-age z score (HAZ) that is more than 2 SD below the WHO growth standard mean for 2006. The HAZ score is calculated by subtracting a median value, adjusted by sex and age, from a standard population and dividing by the standard deviation of the standard population (Leroy & Frongillo, 2019). The graph shows the percentage of stunting among children under the age of five in Bangladesh by division and gender. Sylhet has the highest percentage of stunting at 42.12%, followed by Barisal at 33.33%. Rajshahi has the lowest percentage of stunting at 27.04%."}
    else if (var4() == "underweight_div") {
      "Underweight, defined by the World Health Organization, is having a weight-for-age z score (WAZ) that is more than 2 SD below the WHO growth standard mean for 2006. The WAZ score is calculated by subtracting a median value, adjusted by sex and age, from a standard population and dividing by the standard deviation of the standard population (Leroy & Frongillo, 2019). The graph depicts the percentage of children under the age of five who are underweight by gender and division in Bangladesh. Sylhet has the highest percentage of underweight children at 32.27%, followed by Chittagong at 24.19%. Rajshahi has the lowest percentage of underweight children at 16.84%."}
    else if (var4() == "avgbw_div") {
      "This graph shows the average birth weight of children born by division and gender.Rajshahi has the highest birth weight among the divisions at 3.25 kg and Chittagong being the lowest at 2.7 kg. Referring back to the percentage of children underweight, Rajshahi has the lowest percentage among the divisions reflected in this graph as it has the highest average birth weight. Similarly, Sylhet has a lower relative birth weight when compared to the other divisions. It is important to note that birth weight information was available for less than 50 percent of the children in the sample. As a result, this variable is not used in subsequent analysis"}
  })
  
  # Render maps for mother profile 
  var5 <- reactive({input$mcdrop2})
  #matching with ui for mc2
  output$mc2 <- renderPlotly({
    if (var5() == "age_dist") {
      # Create the bar plot using ggplot
      ageplot <- ggplot(agediv, aes(x = Percentage, y = Division, fill = Agecategory)) +
        geom_bar(stat = "identity", position = "stack", aes(text = paste0("Division: ", Division, "\n", "Age Category: ", Agecategory, "\n", "Percentage: ", Percentage))) +
        labs(title = "Age Distribution of Mothers by Division",
             x = "Percentage",
             y = "Division",
             fill = "Agecategory") +
        scale_fill_manual(values = c( "#3a528b", "#21918c","#ff7f00" )) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 20)+
        easy_add_legend_title("Age Category")+
        easy_center_title()
      
      # ageplot
      
      agedist_div <- ggplotly(ageplot, tooltip = c("text"))}
    else if (var5() == "edu_dist") {
      # Create the bar plot using ggplot
      eduplot <- ggplot(edudiv, aes(x = Percentage, y = Division, fill = Educategory)) +
        geom_bar(stat = "identity", position = "stack", aes(text = paste0("Division: ", Division, "\n", "Education Category: ", Educategory, "\n", "Percentage: ", Percentage))) +
        labs(title = "Educational Attainment of Mothers by Division",
             x = "Percentage",
             y = "Division",
             fill = "Education category") +
        scale_fill_manual(values =c("No education" = "#440154", "Less than primary" = "#fde725",
                                    "Completed primary" = "#3a528b", "Completed secondary" = "#21918c", 
                                    "Higher" = "#ff7f00")) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_add_legend_title("Education Category")+
        easy_center_title()
      
      # eduplot
      
      edudist_div <- ggplotly(eduplot, tooltip = c("text"))}
    else if (var5() == "occu_dist") {
      # Create the bar plot using ggplot
      ocuplot <- ggplot(ocudiv, aes(x = Percentage, y = Division, fill = Ocucategory)) +
        geom_bar(stat = "identity", position = "stack", aes(text = paste0("Division: ", Division, "\n", "Occupation Category: ", Ocucategory, "\n", "Percentage: ", Percentage))) +
        labs(title = "Occupation of Mothers by Division",
             x = "Percentage",
             y = "Division",
             fill = "Occupation category") +
        scale_fill_manual(values = c("Agricultural day labor" = "#440154", "Non-agricultural day labor" = "#fde725",
                                     "Salaried" = "#3a528b", "Rickshaw/van puller" = "#21918c", 
                                     "Self-employed" = "#ff7f00", "Business/trade" = "#bd93f5", 
                                     "Production business" = "#5ec962", "Livestock-related work" = "#cc4778",
                                     "Farming" = "#f6c2f8", "Non-earning occupations" = "#b26600"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_add_legend_title("Occupation Category")+
        easy_center_title()
      
      # ocuplot
      
      ocudist_div <- ggplotly(ocuplot, tooltip = c("text"))}
    
  })
  
  #TEXT OUTPUT FOR MOTHER GRAPHS
  output$mctext2 <- renderText({
    if (var5() == "age_dist") {
      "The graph displays the average distribution of mothers' ages across the seven divisions in Bangladesh. Generally, the modal age falls within the range of 18-30 years old. While the percentage of mothers aged 11-17 years old is low, early pregnancy can impact birth outcomes. Younger mothers may face challenges in terms of empowerment and health care for themselves and their children. Insufficient nutrient intake during this stage can pose risks to both the mother and child's growth and development."}
    else if (var5() == "edu_dist") {
      "Education is an indicator of empowerment and knowledge. Therefore, the more education a woman has, the more likely she will be able to provide for her own and child's health. This graph shows the educational attainment levels among the mothers within the divisions. In general, the modal category of education is completing primary school. In Bangladesh, primary school ends at the fifth grade in the US. This means that most mothers discontinue their education after achieving fifth grade knowledge."}
    else if (var5() == "occu_dist") {
      "The graph displays the distribution of occupations among mothers in Bangladesh. Non-earning occupations, including roles such as primary caregivers, maids and nannies, comprise the largest percentage across all divisions. A mother's level of education and societal norms greatly influence her ability to engage in income-generating occupations that support the household."}
  })
  
  output$graph1 <- renderPlotly({
    stunt_g <- ggplot(read.csv("data/stunt_female.csv"), aes(x = Trim, y = Percentage, fill = factor(Value))) +
      geom_bar(stat = "identity", position = "dodge") +
      
      labs(title = "Percentage of Stunted Girls < 5 Years Old by Incidence of Flood Event",
           x = "Trimesters", 
           y = "Percentage of Stunted Girls < 5 Years Old") +
      easy_add_legend_title("Flood Events")+
      theme(plot.title = element_text(size = 15)) +
      scale_fill_viridis_d()+
      theme_classic()+ 
      easy_y_axis_title_size(size = 13)+
      scale_y_continuous(limits = c(0, 100))+
      easy_x_axis_title_size(size = 13)+
      easy_plot_legend_title_size(size = 13)+
      easy_plot_legend_size(size = 10)+
      easy_plot_title_size(size = 15)+
      coord_cartesian(ylim = c(0, 40))
    
    ggplotly(stunt_g)})
  
  output$graph2 <- renderPlotly({
    stunt_b <- ggplot(read.csv("data/stunt_male.csv"), aes(x = Trim, y = Percentage, fill = factor(Value))) +
      geom_bar(stat = "identity", position = "dodge") +
      
      labs(title = "Percentage of Stunted Boys < 5 Years Old by Incidence of Flood Event",
           x = "Trimesters", 
           y = "Percentage of Stunted Boys < 5 Years Old") +
      easy_add_legend_title("Flood Events")+
      theme(plot.title = element_text(size = 15)) +
      scale_fill_viridis_d()+
      theme_classic()+ 
      easy_y_axis_title_size(size = 13)+
      scale_y_continuous(limits = c(0, 100))+
      easy_x_axis_title_size(size = 13)+
      easy_plot_legend_title_size(size = 13)+
      easy_plot_legend_size(size = 10)+
      easy_plot_title_size(size = 15)+
      coord_cartesian(ylim = c(0, 40))
    
    ggplotly(stunt_b)})
  
  output$graph5 <- renderPlotly({ underw_g <- ggplot(read.csv("data/underwe_female.csv"), aes(x = Trim, y = Percentage, fill = factor(Value))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Percentage of Underweight Girls < 5 Years Old by Incidence of Flood Event",
         x = "Trimesters", 
         y = "Percentage of Underweight Girls < 5 Years Old") +
    easy_add_legend_title("Flood Events")+
    theme(plot.title = element_text(size = 15)) +
    scale_fill_viridis_d()+
    theme_classic()+
    easy_y_axis_title_size(size = 13)+
    scale_y_continuous(limits = c(0, 100))+
    easy_x_axis_title_size(size = 13)+
    easy_plot_legend_title_size(size = 13)+
    easy_plot_legend_size(size = 10)+
    easy_plot_title_size(size = 15)+
    # easy_center_title()+
    coord_cartesian(ylim = c(0, 30))
  
  ggplotly(underw_g)})
  
  ## underweight boys
  
  output$graph6 <- renderPlotly({underw_b <- ggplot(read.csv("data/underwe_male.csv"), aes(x = Trim, y = Percentage, fill = factor(Value))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Percentage of Underweight Boys < 5 Years Old by Incidence of Flood Event",
         x = "Trimesters", 
         y = "Percentage of Underweight Boys < 5 Years Old") +
    easy_add_legend_title("Flood Events")+
    theme(plot.title = element_text(size = 15)) +
    scale_fill_viridis_d()+
    theme_classic()+
    # easy_center_title()+
    easy_y_axis_title_size(size = 13)+
    scale_y_continuous(limits = c(0, 100))+
    easy_x_axis_title_size(size = 13)+
    easy_plot_legend_title_size(size = 13)+
    easy_plot_legend_size(size = 10)+
    easy_plot_title_size(size = 15)+
    coord_cartesian(ylim = c(0, 30))
  
  ggplotly(underw_b)})
  
  
  
  
  output$graph3 <- renderPlotly({stunt_female_by_Rainfall <- ggplot(female_stunted, aes(x = Trimester, y = Percentage_stunted, fill = factor(z_score_tertile))) +
    
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    
    labs(title = "Percentage of Stunted Girls < 5 Years Old by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "Percentage of Stunted Girls < 5 Years Old") +
    
    theme(plot.title = element_text(size = 14)) +
    
    guides(fill = guide_legend(title = "Precipitation Intensity")) +
    
    scale_fill_viridis_d() +
    
    theme_classic() +
    easy_y_axis_title_size(size = 13)+
    scale_y_continuous(limits = c(0, 100))+
    easy_x_axis_title_size(size = 13)+
    easy_plot_legend_title_size(size = 13)+
    easy_plot_legend_size(size = 10)+
    easy_plot_title_size(size = 15)+
    # easy_center_title()+
    coord_cartesian(ylim = c(0, 40))
  ggplotly(stunt_female_by_Rainfall)})
  
  output$graph4 <- renderPlotly({stunt_male_by_Rainfall <- ggplot(male_stunted, aes(x = Trimester, y = Percentage_stunted, fill = factor(z_score_tertile))) +
    
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    
    labs(title = "Percentage of Stunted Boys < 5 Years Old by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "Percentage of Stunted Boys < 5 Years Old") +
    
    theme(plot.title = element_text(size = 14)) +
    
    guides(fill = guide_legend(title = "Precipitation Intensity")) +
    
    scale_fill_viridis_d() +
    
    theme_classic() +
    easy_y_axis_title_size(size = 13)+
    scale_y_continuous(limits = c(0, 100))+
    easy_x_axis_title_size(size = 13)+
    easy_plot_legend_title_size(size = 13)+
    easy_plot_legend_size(size = 10)+
    easy_plot_title_size(size = 15)+
    # easy_center_title()+easy_center_title()+
    easy_plot_legend_size(size = 13)+
    
    
    coord_cartesian(ylim = c(0, 40))
  ggplotly(stunt_male_by_Rainfall)})
  
  
  output$graph7 <- renderPlotly({underw_female_by_Rainfall <- ggplot(female_underweight, aes(x = Trimester, y = Percentage_underweight, fill = factor(z_score_tertile))) +
    
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    
    labs(title = "Percentage of Underweight Girls < 5 Years Old by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "Percentage of Underweight Girls < 5 Years Old") +
    
    theme(plot.title = element_text(size = 14)) +
    
    guides(fill = guide_legend(title = "Precipitation Intensity")) +
    
    scale_fill_viridis_d() +
    
    theme_classic() +
    easy_y_axis_title_size(size = 13)+
    scale_y_continuous(limits = c(0, 100))+
    easy_x_axis_title_size(size = 13)+
    easy_plot_legend_title_size(size = 13)+
    easy_plot_legend_size(size = 10)+
    easy_plot_title_size(size = 15)+
    # easy_center_title()+
    
    coord_cartesian(ylim = c(0, 30))
  ggplotly(underw_female_by_Rainfall)})
  
  output$graph8 <- renderPlotly({underw_male_by_Rainfall <- ggplot(male_underweight, aes(x = Trimester, y = Percentage_underweight, fill = factor(z_score_tertile))) +
    
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    
    labs(title = "Percentage of Underweight Boys < 5 Years Old by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "Percentage of Underweight Boys < 5 Years Old") +
    
    theme(plot.title = element_text(size = 14)) +
    
    guides(fill = guide_legend(title = "Precipitation Intensity")) +
    
    scale_fill_viridis_d() +
    
    theme_classic() +
    easy_y_axis_title_size(size = 13)+
    scale_y_continuous(limits = c(0, 100))+
    easy_x_axis_title_size(size = 13)+
    easy_plot_legend_title_size(size = 13)+
    easy_plot_legend_size(size = 10)+
    easy_plot_title_size(size = 15)+
    
    coord_cartesian(ylim = c(0, 30))
  ggplotly(underw_male_by_Rainfall)})
  
  
  output$graph1 <- renderPlotly({
    if (input$agdrop2 == "2under5") {
      # Generate graph 1 - Condition: Under 5 years old
      stunt_g <- ggplot(read.csv("data/stunt_female.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        
        labs(title = "Percentage of Stunted Girls < 5 Years Old by Incidence of Flood Event",
             x = "Trimesters",
             y = "Percentage of Stunted Girls < 5 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        coord_cartesian(ylim = c(0, 40))
      
      ggplotly(stunt_g)  
    } else if (input$agdrop2 == "1under2") {
      # Generate graph 1 - Condition: Under 2 years old
      stunt_g <- ggplot(read.csv("data/below2_stunt_female.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Stunted Girls <2 Years Old by Incidence of a Major Flood Event",
             x = "Trimesters", 
             y = "Percentage of Stunted Girls <2 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        coord_cartesian(ylim = c(0, 42))
      
      ggplotly(stunt_g)  
    }
  })
  
  output$graph2 <- renderPlotly({
    if (input$agdrop2 == "2under5") {
      # Generate graph 2 - Condition: Under 5 years old
      stunt_b <- ggplot(read.csv("data/stunt_male.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        
        labs(title = "Percentage of Stunted Boys < 5 Years Old by Incidence of Flood Event",
             x = "Trimesters",
             y = "Percentage of Stunted Boys < 5 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        coord_cartesian(ylim = c(0, 40))
      
      ggplotly(stunt_b)
      
    } else if (input$agdrop2 == "1under2") {
      # Generate graph 2 - Condition: Under 2 years old
      stunt_b <- ggplot(read.csv("data/below2_stunt_male.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        
        labs(title = "Percentage of Stunted Boys <2 Years Old by Incidence of a Major Flood Event",
             x = "Trimesters", 
             y = "Percentage of Stunted Boys <2 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        coord_cartesian(ylim = c(0, 40))
      
      ggplotly(stunt_b)  
    }
  }) 
  
  
  
  
  
  
  
  
  ## Stunting CHIRPS
  
  output$graph3 <- renderPlotly({
    if (input$agdrop3 == "4under5") {
      # Generate graph 3 - Condition: Under 5 years old
      stunt_female_by_Rainfall <- ggplot(female_stunted, aes(x = Trimester, y = Percentage_stunted, fill = z_score_tertile)) +
        
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
        
        labs(title = "Percentage of Stunted Girls < 5 Years Old by Precipitation Intensity",
             
             x = "Trimesters", 
             
             y = "Percentage of Stunted Girls < 5 Years Old") +
        theme(plot.title = element_text(size = 14)) +
        
        easy_add_legend_title("Precipitation Intensity") +
        
        scale_fill_viridis_d() +
        
        theme_classic() +
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        # easy_center_title()+
        coord_cartesian(ylim = c(0, 40))
      ggplotly(stunt_female_by_Rainfall)  
    } else if (input$agdrop3 == "3under2") {
      # Generate graph 3 - Condition: Under 2 years old
      stunt_g <- ggplot(chirps_stunt_girls, aes(x = Trim, y = Stunt_percent, fill = Precipitation)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Stunted Girls < 2 Years Old by Precipitation Intensity",
             x = "Trimesters", 
             y = "Percentage of Stunted Girls < 2 Years Old") +
        easy_add_legend_title("Precipitation Intensity")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+ 
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        # easy_center_title()+
        coord_cartesian(ylim = c(0, 35))
      
      ggplotly(stunt_g)    
    }
  })
  
  output$graph4 <- renderPlotly({
    if (input$agdrop3 == "4under5") {
      # Generate graph 4 - Condition: Under 5 years old
      stunt_male_by_Rainfall <- ggplot(male_stunted, aes(x = Trimester, y = Percentage_stunted, fill = z_score_tertile)) +
        
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
        
        
        labs(title = "Percentage of Stunted Boys < 5 Years Old by Precipitation Intensity",
             
             x = "Trimesters", 
             
             y = "Percentage of Stunted Boys < 5 Years Old") +
        
        theme(plot.title = element_text(size = 14)) +
        
        easy_add_legend_title("Precipitation Intensity") +
        
        scale_fill_viridis_d() +
        
        theme_classic() +
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        
        
        coord_cartesian(ylim = c(0, 40))
      ggplotly(stunt_male_by_Rainfall)   
      
    } else if (input$agdrop3 == "3under2") {
      # Generate graph 4 - Condition: Under 2 years old
      stunt_b <- ggplot(chirps_stunt_boys, aes(x = Trim, y = Stunt_percent, fill = Precipitation)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Stunted Boys < 2 Years Old by Precipitation Intensity",
             x = "Trimesters", 
             y = "Percentage of Stunted Boys < 2 Years Old") +
        easy_add_legend_title("Precipitation Intensity")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+ 
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        # easy_center_title()+
        coord_cartesian(ylim = c(0, 35))
      
      ggplotly(stunt_b)   
    }
  }) 
  
  ## Underweight GFD 
  
  output$graph5 <- renderPlotly({
    if (input$agdrop5 == "6under5") {
      # Generate graph 5 - Condition: Under 5 years old
      underw_g <- ggplot(read.csv("data/underwe_female.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Underweight Girls < 5 Years Old by Incidence of Flood Event",
             x = "Trimesters", 
             y = "Percentage of Underweight Girls < 5 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        # easy_center_title()+
        coord_cartesian(ylim = c(0, 30))
      
      ggplotly(underw_g) 
    } else if (input$agdrop5 == "5under2") {
      # Generate graph 7 - Condition: Under 2 years old
      underw_g <- ggplot(read.csv("data/below2_underwe_female.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Underweight Girls < 2 Years Old by Incidence of a Major Flood Event",
             x = "Trimesters", 
             y = "Percentage of Underweight Girls <2 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        coord_cartesian(ylim = c(0, 30))
      ggplotly(underw_g)  
    }
  })
  
  output$graph6 <- renderPlotly({
    if (input$agdrop5 == "6under5") {
      # Generate graph 6 - Condition: Under 5 years old
      underw_b <- ggplot(read.csv("data/underwe_male.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Underweight Boys < 5 Years Old by Incidence of Flood Event",
             x = "Trimesters", 
             y = "Percentage of Underweight Boys < 5 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        # easy_center_title()+
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        coord_cartesian(ylim = c(0, 30))
      
      ggplotly(underw_b)
    } else if (input$agdrop5 == "5under2") {
      # Generate graph 6 - Condition: Under 2 years old
      underw_b <- ggplot(read.csv("data/below2_underwe_male.csv"), aes(x = Trim, y = Percentage, fill = Value)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Underweight Boys <2 Years Old by Incidence of a Major Flood Event",
             x = "Trimesters", 
             y = "Percentage of Underweight Boys < 2 Years Old") +
        easy_add_legend_title("Flood Events")+
        theme(plot.title = element_text(size = 15)) +
        scale_fill_viridis_d()+
        theme_classic()+
        coord_cartesian(ylim = c(0, 30))
      
      ggplotly(underw_b)   
    }
  })
  
  ## Underweight CHIRPS
  
  output$graph7 <- renderPlotly({
    if (input$agdrop7 == "8under5") {
      # Generate graph 7 - Condition: Under 5 years old
      underw_female_by_Rainfall <- ggplot(female_underweight, aes(x = Trimester, y = Percentage_underweight, fill = z_score_tertile)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
        labs(title = "Percentage of Underweight Girls < 5 Years Old by Precipitation Intensity",
             x = "Trimesters",
             y = "Percentage of Underweight Girls < 5 Years Old") +
        theme(plot.title = element_text(size = 14)) +
        scale_fill_viridis_d() +
        theme_classic() +
        easy_add_legend_title("Precipitation Intensity") +
        coord_cartesian(ylim = c(0, 30))
      
      ggplotly(underw_female_by_Rainfall)
    } else if (input$agdrop7 == "7under2") {
      # Generate graph 7 - Condition: Under 2 years old
      uw_g <- ggplot(chirps_uw_girls, aes(x = Trim, y = uw_percent, fill = Precipitation)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Underweight Girls < 2 Years Old by Precipitation Intensity",
             x = "Trimesters",
             y = "Percentage of Underweight Girls < 2 Years Old") +
        scale_fill_viridis_d() +
        theme_classic() +
        coord_cartesian(ylim = c(0, 25))
      
      ggplotly(uw_g)
    }
  })
  
  output$graph8 <- renderPlotly({
    if (input$agdrop7 == "8under5") {
      # Generate graph 8 - Condition: Under 5 years old
      underw_male_by_Rainfall <- ggplot(male_underweight, aes(x = Trimester, y = Percentage_underweight, fill = z_score_tertile)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
        labs(title = "Percentage of Underweight Boys < 5 Years Old by Precipitation Intensity",
             x = "Trimesters",
             y = "Percentage of Underweight Boys < 5 Years Old") +
        theme(plot.title = element_text(size = 14)) +
        scale_fill_viridis_d() +
        theme_classic() +
        easy_add_legend_title("Precipitation Intensity") +
        coord_cartesian(ylim = c(0, 30))
      
      ggplotly(underw_male_by_Rainfall)
    } else if (input$agdrop7 == "7under2") {
      # Generate graph 8 - Condition: Under 2 years old
      uw_b <- ggplot(chirps_uw_boys, aes(x = Trim, y = uw_percent, fill = Precipitation)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Percentage of Underweight Boys < 2 Years Old by Precipitation Intensity",
             x = "Trimesters",
             y = "Percentage of Underweight Boys < 2 Years Old") +
        scale_fill_viridis_d() +
        theme_classic() +
        coord_cartesian(ylim = c(0, 25))
      
      ggplotly(uw_b)
    }
    
  })
  
  # drop down output for access to healthcare tab NOW HERE JADE
  ahc1 <- reactive({input$ahcdrop})
  #matching with ui for ahc
  output$ahc <- renderPlotly({
    
    if (ahc1() == "dist_med_ahc") {
      graph_distance <- ggplot(dis_from_med, aes(x = Tertile, y = distance_medical, fill = Tertile)) +
        geom_boxplot() +
        scale_fill_viridis_d() +
        theme_classic() +
        labs(title = "Household Distance from Health Center/Hospital by Precipitation Intensity",
             x = "Precipitation Intensity",
             y = "Distance (Km) ") +
        ylim(0, 75) +
        theme(plot.title = element_text(size = 15)) +
        theme_classic() +
        easy_y_axis_title_size(size = 13) +
        easy_x_axis_title_size(size = 13) +
        easy_plot_title_size(size = 15) +
        guides(fill = FALSE)
      
      graph_distance <- ggplotly(graph_distance)} 
      
      # mode of transport
    else if (ahc1() == "trnsprt_ahc") {
      # Create the bar plot using ggplot
      figg <- ggplot(trdata_long, aes(x = Tertile, y = Percentage, fill = Transportation)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = colors) +
        labs(x = "Precipitation Intensity", y = "Percentage", fill = "Tranportation") +
        ggtitle("Transportation Distribution by Precipitation Intensity") +
        coord_flip()+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        guides(fill = guide_legend(reverse = TRUE))
      
      figgplty1 <- ggplotly(figg)
      
      figgplty <- layout(figgplty1, legend = list(traceorder = "reversed"))}
      #antenatal care
    else if (ahc1() == "ant_care_ahc") {
      # Create the bar plot using ggplot
      carep <- ggplot(pct_care, aes(x = rain_intensity, y = pct, fill = Care)) +
        geom_bar(stat = "identity", position = "stack", aes(text = paste0("Rain Intensity: ", rain_intensity, "\n", "Percentage: ", pct, "\n", "Care: ", Care))) +
        labs(
          title = "Percentage of Healthcare Provision by Precipitation Intensity",
          x = "Precipitation intensity",
          y = "Percent"
        ) +
        easy_add_legend_title("Antenatal Care") +
        scale_fill_viridis_d() +
        theme_classic() +
        easy_y_axis_title_size(size = 13) +
        scale_y_continuous(limits = c(0, 100)) +
        easy_x_axis_title_size(size = 13) +
        easy_plot_title_size(size = 15) +
        coord_cartesian(ylim = c(0, 100))
      ggplotly(carep, tooltip = c("text"))}
    else if (ahc1() == "prim_adv_ahc") {
      Intensity <- c("Low", "Medium", "High")
      Licensed <- c(84.86, 84.04, 86.51)
      Unlicensed <- c(4.32, 4.23, 3.56)
      Other <- c(10.81, 11.73, 9.92)
      
      padata <- data.frame(Intensity, Licensed, Unlicensed, Other)
      
      padata_long <- tidyr::pivot_longer(padata, -Intensity, names_to = "Advisor", values_to = "Percentage")
      
      padata_long$Intensity <- factor(padata_long$Intensity, levels = c("Low", "Medium", "High"))
      padata_long$Advisor <- factor(padata_long$Advisor, levels = c("Licensed", "Unlicensed", "Other"))
      
      pa_fig <- ggplot(padata_long, aes(x = Intensity, y = Percentage, fill = Advisor)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Primary Advisor Type by Precipitation Intensity",
             x = "Precipitation Intensity",
             y = "Percentage") +
        scale_fill_manual(values = c("Licensed" = "#440154",
                                     "Other" = "#21918c",
                                     "Unlicensed" = "#3a528b"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 90))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)
      
      # pa_fig
      
      ggplotly(pa_fig)}
    else if (ahc1() == "num_visit_ahc") {
      visits <- ggplot(avg_vist , aes(x = Rain, y = Percent , fill = Rain)) +
        geom_col() +
        labs(title = "Average Antenatal Care Visits",
             x = "Rain intensity",
             y = "Average") +
        scale_fill_viridis_d() +
        theme_classic() +
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_remove_legend()+
        easy_plot_title_size(size = 15)+
        coord_cartesian(ylim = c(0, 5))
      ggplotly(visits)}
    else if (ahc1() == "deliv_loc_ahc") {
      delAplot <- ggplot(delA, aes(x= Intensity, y= Percentage, fill = Location))+
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Delivery Location by Preciptation Intensity",
             x = "Precipitation Intensity",
             y = "Percentage of Children",
             fill = "Delivery Location") +
        # geom_text(aes(label = paste0(Percentage, "%")), 
        #           position = position_stack(vjust = 0.5), 
        #           vjust = -0.5, 
        #           color = ifelse( delA$delivery == "Home/Other", "white", "black"),
        #           size = 4) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        # easy_center_title()+
        scale_fill_manual(values = c("#440154","#fde725")) 
      
      ggplotly(delAplot)}
    else if (ahc1() == "person_present_ahc") {
      person_deliv <- ggplot(pdata_long, aes(x = Intensity, y = Percentage, fill = Person)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Person Present During Delivery by Precipitation Intensity",
             x = "Precipitation Intensity",
             y = "Percentage") +
        scale_fill_manual(values = c("Family" = "#440154",
                                     "Other" = "#fde725",
                                     "Licensed" = "#3a528b",
                                     "Unlicensed" = "#21918c"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)
      
      ggplotly(person_deliv)}
    else if (ahc1() == "lpp_ahc") {
      ptype_deliv <- ggplot(ldata_long, aes(x = Intensity, y = Percentage, fill = Physician)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Licensed Physician Type by Precipitation Intensity",
             x = "Precipitation Intensity",
             y = "Percentage") +
        scale_fill_manual(values = c("NGO" = "#440154",
                                     "Community" = "#fde725",
                                     "Public" = "#3a528b",
                                     "Private" = "#21918c"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 75))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)
      
      ptype_deliv
      
      ggplotly(ptype_deliv)}
  })
  #TEXT OUPUT PER GRAPH

  output$ahctext <- renderText({
    if (ahc1() == "dist_med_ahc") {
      "The distance people travel to reach the nearest medical facility can vary significantly based on factors like geographic location, seasons, and available infrastructure. Rural areas, on average, face farther distances than urban areas.  Due to the intensity of precipitation, some areas will have more health facilities than others where low precipitation regions will have more facilities. The graphs on the left depict this scenario, showing that in areas with intense precipitation, some individuals may have to travel up to 12 km to access the nearest medical facility."}
    else if (ahc1() == "trnsprt_ahc") {
      "Bangladesh's transportation options vary based on location and seasons. In major cities like Dhaka, Chittagong, and Khulna, people primarily use buses, taxis, and ride-sharing services. In rural areas with limited road infrastructure, non-motorized modes like bicycles, cycle rickshaws, and bullock carts are common for local transportation. During the monsoon season (June to September), when heavy flooding occurs, boats become essential for navigating through submerged areas.

The graphs on the left categorize transportation into five groups: Motor vehicles, Rickshaw/cart, Foot, Boat, and other. When examining the data based on precipitation intensity, it is evident that during heavy rain seasons, the usage of motor vehicles increases to 64.4%, while boat usage rises to 7.05%. This trend is due to impassable roads for small and non-motorized vehicles during floods. Consequently, healthcare access becomes more expensive as reliance on motorized vehicles increases."}
    else if (ahc1() == "ant_care_ahc") {
      "Our analysis indicates that there is little variation in the frequency of mothers receiving antenatal care across different levels of rainfall intensities. This suggests that women consistently choose to seek antenatal care, regardless of how intense the rain is. Despite potential challenges posed by varying weather conditions, such as heavy rainfall during certain periods, women continue to prioritize their health and the health of their unborn children by seeking antenatal care. This trend underscores the importance of antenatal healthcare among women, reflecting their proactive approach in ensuring the well-being and proper development of their pregnancies."}
    else if (ahc1() == "prim_adv_ahc") {
      "Approximately 85Percentage of women consistently select licensed healthcare workers as their primary advisors, a preference unaffected by changes in precipitation intensity. This highlights the significant role licensed professionals play in women's healthcare decisions. It emphasizes the value women place on receiving proper care from these qualified experts."}
    else if (ahc1() == "num_visit_ahc") {
      "As precipitation intensity increases, there is a noticeable rise in home or non-facility births. The difficulty in organizing delivery locations and securing skilled personnel during or after disasters contributes to this trend. Consequently, maternal healthcare in rural Bangladesh faces disruptions during flood events. To address this issue, implementing measures for disaster preparedness and ensuring healthcare continuity becomes essential to provide adequate support to expectant mothers and newborns."}
    else if (ahc1() == "deliv_loc_ahc") {
      "As precipitation intensity increases, the number of home/other births increase. It is difficult to organize the place and person for delivery especially during or after a disaster period. Maternal healthcare in rural Bangladesh is disrupted during flood events."}
    else if (ahc1() == "person_present_ahc") {
      "As rainfall intensity increases, we observe a rise in the number of family members present during deliveries. Despite this increase, licensed healthcare workers remain the most prominent individuals attending these deliveries. Interestingly, when referring to the rise in home births correlating with the consistent percentage of licensed health workers present during deliveries, it suggests that these professionals are being called to homes for deliveries instead of attending to them in health facilities."}
    else if (ahc1() == "lpp_ahc") {
      "In summary, licensed doctors were consistently present during home deliveries. Data was grouped into Public, Community, NGO, and Private services. Public services, government-funded with doctors from public hospitals, are highly subsidized. Surprisingly, higher precipitation intensity leads to fewer public health workers during deliveries, but NGOs step in to fill the gap. However, NGO services may face quality challenges due to limited funding, equipment, and facilities compared to public doctors. Healthcare access evaluation should consider these factors across regions."}
    })

  
  
  
  
  
  
  Varw <- reactive({
    input$waterdrop
  })
  
  output$waters <- renderPlotly({
    
    if (Varw() == "source_drinking_water") {
      
      
      d_water <- ggplot(drinking_w , aes(x = Divisions, y = Percentage , fill = water_sources)) +
        geom_bar(stat = "identity", position = "dodge",  aes(text = paste0("Divisions: ", Divisions, "\n", "Percentage: ", Percentage, "\n", "Water Sources: ", water_sources)))+
        
        # geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
        
        labs(title = "Source of Drinking Water by Division",
             
             x = "Divisions", 
             
             y = "Percentage") +
        
        easy_add_legend_title("Water Sources") +
        
        scale_fill_viridis_d() +
        
        theme_classic() +
        easy_y_axis_title_size(size = 13)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 13)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 17)+
        coord_cartesian(ylim = c(0, 95))
      ggplotly(d_water, tooltip = c("text"))
      
    }
    
    else if (Varw() == "water_treatment") {
      
      p_h2o<-ggplot (filt_h2o,
                     aes(Division, Percentage, fill = Untreated_water))+
        # geom_col(position = position_dodge(width = 0.6), width = 0.6) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6,  aes(text = paste0("Division: ", Division, "\n", "Percentage: ", Percentage, "\n", "Quality: ", Untreated_water)))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        # easy_center_title()+
        labs(title = "Households Without Treated Drinking Water by Division",
             x= "Division",
             y = "Percentage")+
        scale_fill_viridis_d() +
        # ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        easy_remove_legend()+
        ylim(0,100)
      ggplotly(p_h2o, tooltip = c("text"))  
      
      
    }
    
  })
  
  
  output$descw <- renderText({
    " Description"
  })
  
  
  output$descw <- renderText({
    if (Varw() == "source_drinking_water") {
      
      "The graph above shows that tube wells are the primary source of drinking water in four of the  divisions of Bangladesh, constituting around 80 percent, on average, of their consumed water. In the remaining divisions, ponds, rivers, and canals are the main sources of water. Open water sources like rivers and ponds are particularly vulnerable to floods because floodwaters can directly run into them and contaminate them with pathogens. Conversely, although tube well water is less susceptible to mixing with floodwaters, polluted runoff can enter these supposedly clean underground water sources. Many factors such as the location and depth of the tube well, the type of soil around it, and the level of maintenance it receives dictate the risk of contamination for well water. In fact,  a paper by Luby et al. (2008) revealed that 258 surveyed tube wells in flood prone districts (Comilla, Brahmanbaria and Sirajgan) of Bangladesh had an average depth of of 21 meters and 86 percent of them were located within 10 m of a latrine while 70 percent were within 10 m of other types of pollution sources. Consequently, there is a high likelihood of floodwater breaching tube well structures through unsealed cracks and introducing external pollutants from surrounding pollution sources. 
"
      
    }
    
    else if (Varw() == "water_treatment") {
      
      "Despite concerns about flood water contaminating drinking water sources the graph to the left suggests that  95.06% of surveyed households across divisions do not treat their drinking water, such as through boiling or filtration.  Water treatment is crucial in rural areas without access to piped water sources to mitigate the exposure to harmful pollutants. Rangpur and Barisal have the highest percentage of households not treating water, while Sylhet has the lowest percentage.
"
      
    }
    
  }) 
  
  #Consumption and Nutrition output Fart
  cpn1 <- reactive({input$cpndrop})
  #matching with ui for cpn
  output$cpn <- renderPlotly({
    
    if (cpn1() == "calcium_intake") {
      fig_cal <- ggplot(data_longC, aes(x = Tertile, y = Percentage, fill = Months)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = colorsPI) +
        labs(x = "Precipitation Intensity", y = "Percent", fill = "Months") +
        ggtitle("Duration of Calcium Intake by Precipitation Intensity") +
        theme_classic() +
        theme(legend.position = "right")+
        easy_y_axis_title_size(size = 15) +
        scale_y_continuous(limits = c(0, 100)) +
        easy_x_axis_title_size(size = 15) +
        easy_plot_legend_title_size(size = 13) +
        easy_plot_legend_size(size = 10) +
        easy_plot_title_size(size = 15) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_x_discrete(labels = LevelsPI) +
        coord_flip()
      
      ggplotly(fig_cal)
    } else if (input$cpndrop == "iron_intake") {
      #generate iron stacked bar graph
      fig_iron <- ggplot(data_long, aes(x = Tertile, y = Percentage, fill = Months)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = colors) +
        labs(x = "Precipitation Intensity", y = "Percent", fill = "Months") +
        ggtitle("Duration of Iron Intake by Precipitation Intensity") +
        coord_flip()+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        guides(fill = guide_legend(reverse = TRUE))
      ggplotly(fig_iron)

    } else if (input$cpndrop == "vitamin_A") {
      vit_a_graph <- ggplot(vitA, aes(x= Intensity, y= Percentage, fill = Supplement))+
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Mother's Vitamin A Intake by Preciptation Intensity",
             x = "Precipitation Intensity",
             y = "Percentage of Mothers",
             fill = "Supplement Consumption") +
        # geom_text(aes(label = paste0(Percentage, "%")), 
        #           position = position_dodge(width = 0.9), 
        #           vjust = -0.5, 
        #           color = "black",
        #           size = 4) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 80))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)+
        # easy_center_title()+
        scale_fill_manual(values = c("#440154","#fde725")) 
      ggplotly(vit_a_graph)
    } else if (input$cpndrop == "cq_fig") {
      cq_fig <- ggplot(cqdata_long, aes(x = Intensity, y = Percentage, fill = Questions)) +
        geom_bar(stat = "identity", position = "dodge",  aes(text = paste0("Cumulative Score : ", Questions, "\n", "Precipitation Intensity: ", Intensity, "\n", "Percentage: ", Percentage))) +
        labs(title = "Cumulative Score by Precipitation Intensity",
             x = "Precipitation Intensity",
             y = "Percentage") +
        scale_fill_manual(values = c("From1to3" = "#21918c", 
                                     "From4to5" = "#3a528b", 
                                     "From6to8" = "#440154"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 65))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)
      ggplotly(cq_fig, tooltip = c("text"))
    } else if (input$cpndrop == "pq_fig") {
      
      pq_fig <- ggplot(pqdata_long, aes(x = Question, y = Percentage, fill = Intensity)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Correct Score by Precipitation Intensity",
             x = "Question",
             y = "Percentage") +
        scale_fill_manual(values = c("High" = "#440154",
                                     "Medium" = "#3a528b",
                                     "Low" = "#21918c"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        scale_y_continuous(limits = c(0, 100))+
        easy_x_axis_title_size(size = 15)+
        easy_plot_legend_title_size(size = 13)+
        easy_plot_legend_size(size = 10)+
        easy_plot_title_size(size = 15)
      ggplotly(pq_fig)
    }
    #TEXT OUPUT PER GRAPH
  })
  
  output$cpntext <- renderText({
    if (cpn1() == "calcium_intake") {
      "Main sources of dietary calcium include eggs, milk, meat, and green leafy vegetables. However, it is found that young woman in Bangladesh experience significant calcium inadequacy (Bromage, S., Ahmed, T., & Fawzi, W. W., 2016). It is recommended to an average woman to take calcium supplements of at least 1 g daily during mid-pregnancy, as studies have demonstrated their potential to reduce pre-eclampsia which is caused by calcium deficiency (Hofmeyr, G. J., et al., 2017). Another study found that the total body bone mineral content was significantly greater in infants born to calcium-supplemented mothers than the placebo group which prevents stunting and micronutrient deficiencies (Kumar, A., & Kaur, S., 2017). A woman with low calcium should take 600-1000 mg supplements during pregnancy  in order to improve maternal and infant bone health  (Kumar, A., & Kaur, S., 2017)."
    }
    else if (cpn1() == "iron_intake") {
      "Maternal anemia, particularly if severe or long-lasting, can impair the delivery of oxygen and nutrients to the developing fetus. This can result in intrauterine growth restriction (IUGR), which means that the baby may not grow to its full potential in the womb (Sharma, D., Shastri, S., & Sharma, P., 2016). IUGR is associated with an increased risk of low birth weight and developmental complications in the newborn. 

According to BDHS in 2011, nearly half of pregnant (49.6 per cent) and lactating (48 per cent) women in Bangladesh were anemic (Mitra 2011). The appropriate time of a 30 mg/day iron supplementation is after about week 12 of gestation because this is when iron requirements for pregnancy begin to increase (Institute of Medicine (US) Committee on Nutritional Status During Pregnancy and Lactation, 1990). There is a program in Bangladesh, the IFA (Iron and Folic Acid) supplementation program for pregnant women, which provides iron supplements and testing to vulnerable populations and remote areas. 
"
    }
    else if (cpn1() == "vitamin_A") {
      "Vitamin A deficiency has been recognized as a significant public health issue in Bangladesh since the 1960s (UNICEF, 2015). To address this concern, the Government of Bangladesh mandates the administration of a single high dose of vitamin A (200,000 IU) to all postpartum women within six weeks after delivery. This is crucial for promoting maternal health, ensuring the production of high-quality breast milk, and supporting the development of newborns (UNICEF, 2015). A 2016 study revealed enhanced levels of retinol in the breast milk of mothers who received vitamin A supplementation, enabling them to meet their own nutritional needs as well as those of their breastfed infants (Oliveira et al., 2016). The Bangladeshi government encourages immediate breastfeeding initiation following birth and exclusive breastfeeding until the age of six months. Incorporating vitamin A supplements into the postnatal period significantly contributes to the nutritional support provided during breastfeeding."
    }
    else if (cpn1() == "cq_fig") {
      "In the BIHS, the Infant and Young Child Feeding profile (IYCFP) in Modules Y2 and Y3 assessed the knowledge of mothers through suggested infant and young child feeding practices along with illness recovery practices. In our analysis, we identified eight questions and created a maternal knowledge score. On this relative scale, we see that as precipitation intensity increases, the higher score category declines and disperses among the lower score categories seen through the increase of cumulative score categories “1 to 3” and “4 to 5.”"
    } else if (cpn1() == "pq_fig") {
      "This graph contains the eight questions assessed in the maternal knowledge score. On the relative scale, knowledge about proper breastfeeding related questions decreases. This includes the questions about how soon a mother should breastfeed a newborn child, what to do with colostrum, and how long a woman should exclusively breastfeed. A woman should breastfeed her child within the first hour (Khan J., 2015) as delayed breastfeeding of the mother’s colostrum is associated with a higher risk of neonatal mortality. Additionally, the child should be exclusively breastfed six months after birth (Butte N., 2004).
We also find that the knowledge score about introducing liquids and foods besides breastmilk is relatively lower than other questions. 
Among the lowest of scores lie the situational questions asking about diarrhea and if a child under 6 months should be given water in hot weather conditions. We see that the general trend in these questions, keeping in mind they are already low, declines as precipitation intensity increases. This becomes a risk for the child’s immune system  as they are already drinking water before the recommended age  and additionally when flooding increases and could contaminate drinking water without proper filtration."
    }
  })
}

shinyApp(ui = ui, server = server)


