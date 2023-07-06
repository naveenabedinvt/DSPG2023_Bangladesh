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

#WASTING BY DIVISION BY GENDER
#create data frame with total percentage of those wasted
wastediv1 <- data %>%
  rename("Division" = div_name, 
         "Waste" = wasting_all0to59) %>% 
  group_by(Division) %>%
  count(Waste) %>% 
  mutate(Total = sum(n), Percentage = round(n/Total * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Waste = as.character(haven::zap_labels(Waste)),
         Waste = recode(Waste, "0" = "Not Wasted",
                        "1" = "Wasted")) %>% 
  na.omit() %>% 
  filter(Waste == "Wasted")

wastediv1 = dplyr::select(wastediv1, Division, Percentage) 
wastediv1 %>% mutate(Gender = "Total") -> wastediv1

#to get percentage for male and female children affected, must make seperate table
#create a different data frame divided by children wasted by Gender
wastediv <- data %>%
  rename("Division" = div_name, 
         "Waste" = wasting_all0to59,
         "Gender" = childgender)%>% 
  group_by(Division, Gender) %>%
  count(Waste) %>% 
  mutate(Total_by_gender = sum(n), Percentage = round(n/Total_by_gender * 100, 2)) %>% 
  ungroup() %>% 
  mutate(Waste = as.character(haven::zap_labels(Waste)),
         Waste = recode(Waste, "0" = "Not Wasted",
                        "1" = "Wasted"),
         Gender = as.character(haven::zap_labels(Gender)),
         Gender = recode(Gender, "1" = "Male",
                         "2" = "Female")) %>% 
  na.omit() %>% 
  filter(Waste == "Wasted")
# wastediv

#combine tables so that Gender column will contain Total(Male and Female), Male, and Female
wastediv_combined <- bind_rows(wastediv, wastediv1)
#factoring for the bar graph to cluster in this order
wastediv_combined$Gender <- factor(wastediv_combined$Gender, levels = c("Total", "Male", "Female"))

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
                    tags$style('.selectize-dropdown {z-index: 1000}'),
                    tags$style('
                      .carousel-container {
                        height: 100%;
                        overflow-y: auto;
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
                                   h1(strong("Effects of Prenatal Exposure to Flooding on Child Health Outcomes: Evidence from Bangladesh"),
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
                                          
                                          p("Located in the heart of South Asia, Bangladesh captivates with its rich tapestry of culture, mesmerizing landscapes, and a vibrant population. Bangladesh shares borders with India to the west, north, and east, while Myanmar borders its southeastern regions. Spanning across flatlands, Bangladesh is crisscrossed by a network of over 700 rivers and tributaries (Banglapedia, 2021).

In addition to its vibrant culture and natural beauty, Bangladesh faces numerous socio-economic challenges. These include high population density, which puts pressure on resources and infrastructure, vulnerability to rising sea levels and frequent flooding, limited access to quality healthcare services, and persistent gender inequality issues that require concerted efforts to overcome.

As of 2023, Bangladesh is the eighth most populous country in the world, with a population of approximately 169,469,771 people (Country Profile, 2021). In 2016, 13.47% of the population in Bangladesh lived below the international poverty line of $2.15 per day. However, by 2022, the poverty rate declined to 10.44%, showing improvement. However, it is important to recognize that a little over 10% of the population still lives below the national poverty line (World Bank Group, 2023). Agriculture is a significant contributor to the economy (Embassy of the People’s Republic of Bangladesh, 2020).

The country is characterized by a multi-tiered administrative structure that helps govern the country effectively. This structure encompasses several levels, including divisions, districts, upazilas (sub-districts), unions, and villages. The country has seven divisions: Barisal, Chittagong, Dhaka, Khulna, Rajshahi, Rangpur, and Sylhet, which serve as the highest level of administrative units. Each division is further divided into 64 districts, comparable to counties in the USA. Within the divisions, multiple upazilas (sub-districts) play a crucial role in implementing government policies. Upazilas are divided into unions, which are composed of several villages and are responsible for local government functions and community well-being. Villages are the smallest unit of administration and are primarily located in rural areas. They accommodate the majority of the population and play a significant role in the socio-economic development of Bangladesh.
", align = "justify")),
column(4,
       h2(strong("Goals and Objectives")),
       h3(strong("Research Question:")), 
       p("Our research focuses on two interconnected questions: (1) Does prenatal exposure to flooding adversely affect child health outcomes, and (2) What are the mechanisms through which floods affect child health outcomes? Floods have been found to have correlations with worsened child health outcomes, such as lower birth weight, cognitive functions, and height (Mallett, 2017). Additionally, the timing of flood exposure during pregnancy affects the severity of fetal growth. The second part of our research aims to examine the mechanisms through which flooding affects societal access to sources of nutrition and healthcare services. When flooding occurs, accessing clean water becomes increasingly more difficult as water gets contaminated. Street and road access also become inaccessible (Aggarwal, 2018). These factors directly affect mothers and their accessibility to adequate nutrition and healthcare. In this paper, we will further observe the effects of flood exposure during pregnancy on child health outcomes.
", align = "justify"),
       h3(strong("Objectives:")), 
       p("Throughout our ten-week study, we conducted a literature review on prenatal exposure to flooding and child health outcomes. We are using Google Earth Engine and the BIHS to collect flood data and birth outcomes in Bangladesh from 2018-2019. To address our two important research questions, we are integrating large-scale household survey data with flood-related remotely sensed satellite data. This allows us to obtain information from affected households and satellite imagery, providing us with a comprehensive understanding of how flooding impacts women at the individual level. Based on our findings, we will present results via a poster shared at the Virginia Tech Symposium. We will also create an interactive Shiny App dashboard so that our research is accessible to the public.
", align = "justify"),

), 

column(4,
       h2(strong("Research Design")),
       p("For our research, we will use the Bangladesh Integrated Household Survey (BIHS) data collected by the International Food Policy Research Institute (IFPRI). We utilized cross-sectional data for the years 2018-2019 from round 3 of the BIHS survey, which is the most recent available household survey. The sample design had two main objectives: gathering national representative data from the rural areas of Bangladesh and including all seven administrative divisions for divisional representation. The BIHS sample also included observations from the Feed The Future (FTF) zone of influence, which we will exclude from our sample to avoid over-sampling issues.

To determine the sample size, the IFPRI survey team used a careful 2-stage stratified statistical sampling method. In the first stage, they selected Primary Sampling Units (PSUs) or villages using probability proportional to size based on the number of households in each stratum or division. The choice of PSUs resulted in the following distribution: 21 PSUs in Barisal, 48 in Chittagong, 87 in Dhaka, 27 in Khulna, 29 in Rajshahi, 27 in Rangpur, 36 in Sylhet, and 50 in the FTF zone. In the second stage, twenty households were randomly selected from each PSU. The IFPRI sample size was 6,500 households across 325 PSUs. However, our sample size is smaller because we excluded 1,000 households from the FTF Zone of Influence. Additionally, it is important to note that several changes occurred within households between 2011-2012 and 2018-2019, such as household mergers or splits. After making all these adjustments, our sample size is 5,604 households.

In addition to the survey data we will use Global Flood Database (GFD), CHIRPS, and Sentinel-1 data to assess  the flood extent. The GFD identifies significant global flood events based on news reports and uses MODIS satellite imagery to capture those flood events to output flood extent data. CHIRPS provides precipitation data with detailed rainfall maps at 5000m resolution. Sentinel-1 is a satellite launched by the European Space Agency in 2014. It uses radar to gather data information from the earth's surface and generates high-resolution images as radar sensors are not sensitive to atmospheric conditions. 
", align = "justify"),  
)), 


         fluidRow(style = "margin: 6px;",
                  
                  column(8, 
                         ##start of carousel code             
                         h3(strong("Inside Look of Bangladesh")), 
                         carousel(
                           id = "myCarousel",
                           carouselItem(
                             img(src = "hh.png")
                           ),
                           carouselItem(
                             img(src = "hh1.png")
                           ),
                           carouselItem (
                             img(src = "hh2.png")
                           )
                           ##end of carousel code 
                         )
                         
                         ),
                column(4,
h2(strong("Survey")),
p("The survey includes a wide range of questions about demographic characteristics, socioeconomic status, household assets, employment, income, expenditures, education, health, etc. The survey is structured into 29 modules that correspond to different areas. Each module consists of a series of questions designed to collect specific information related to that area. Each response is assigned to a numerical code which allows for efficient data management and analysis. There are also quality control measures embedded in the survey to ensure accuracy and integrity of the collected information. 

In our study we will focus mainly on modules A, B, W, and Y. Module A will provide insight on the sample households and identification; this includes information on coordinates of the household and the total number of members in the household. Coordinates of the household will be used to locate the proximity of these households to the affected flooded areas. Module B covers the Household Composition and Education. This module entails the education levels, occupation, and source of income for individuals of the household. Module W focuses on the anthropometry, health, and illnesses of each individual of the household. Module Y has sectional portions containing survey data on child and antenatal care, Infant and child feeding practices, immunization and health of children younger than the age of two and service use.

", align="justify")
#p("")
)
                          ),
#fluidRow(align = "center",
# p(tags$small(em('Last updated: August 2021'))))
                 ),

## Overview--------------------------------------------

navbarMenu("Background",
           
           tabPanel("Flood Conditions", value = "Flood Conditions",
                    fluidRow(
                      style = "margin: 6px;",
                      column(
                        12,
                        align = "justify",
                        h4(strong("Flood Vulnerability in Different Divisions of Bangladesh")),
                        p("Bangladesh is a topographically flat country situated in the delta region of three major rivers: the Ganges, Brahmaputra, and Meghna. With nearly 60% of the country lying below 6 meters above sea level, slow drainage and the risk of overflow are prevalent (Mirza, 2001). Bangladesh experiences a monsoon season every year, spanning from June to September. The Ganges and Brahmaputra rivers originate from the Himalayan region, which experiences heavy snowfall during the winter months. As the snow melts during the warmer seasons, it increases the discharge of water downstream."),
                        p("Some facors that determine whether an area is more susceptible to flooding than others include elevation, geography, and infrastructure (drainage systems, water management systems, etc.) (Mirza, 2003). River basins near Bangladesh’s three major rivers are more prone to flooding due to their extensive drainage areas, high sediment loads, and proximity to the Himalayan snowmelt mentioned earlier (Mirza, 2003).")
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        align = "justify",
                        h5(strong("Barisal Division")),
                        p("The Barisal Division is located in the southern part of Bangladesh and is known for its extensive river networks and vast coastal areas. It is prone to river flooding, especially during the monsoon season. The division experiences regular flooding due to heavy rainfall, overflowing rivers, and tidal surges from the Bay of Bengal.")
                      ),
                      column(
                        6,
                        align = "justify",
                        h5(strong("Chittagong Division")),
                        p("The Chittagong Division is located in the southeastern part of the country and encompasses both hilly terrain and coastal areas. The division experiences floods caused by heavy rainfall, river overflow, and cyclonic storms coming in from the Bay of Bengal. The hilly regions are also susceptible to landslides during heavy downpours.")
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        align = "justify",
                        h5(strong("Dhaka Division")),
                        p("The Dhaka Division includes the capital city, Dhaka, which lies in the central part of Bangladesh. This division experiences river flooding from three major rivers. Urban areas may experience flooding due to insufficient drainage systems and infrastructure (Mirza, 2003). The most severe flooding in Dhaka Division typically occurs from July to September when the monsoon rainfall is at its peak.")
                      ),
                      column(
                        6,
                        align = "justify",
                        h5(strong("Khulna Division")),
                        p("The Khulna Division is situated in the southwestern region of Bangladesh. This division is prone to coastal flooding and storm surges caused by cyclones from the Bay of Bengal. River flooding is also common during the monsoon season. This division has its highest flood risk from June to September due to coastal flooding, storm surges, and river flooding.")
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        align = "justify",
                        h5(strong("Rajshahi Division")),
                        p("The Rajshahi Division is located in the northwestern part of Bangladesh. This division experiences river flooding, primarily caused by heavy monsoons and overflowing of the Ganges River. The most significant flooding in the region typically occurs during the monsoon season, affecting both rural and urban areas.")
                      ),
                      column(
                        6,
                        align = "justify",
                        h5(strong("Rangpur Division")),
                        p("The Rangpur Division is situated in the northern part of Bangladesh. This division experiences river flooding, particularly from the Teesta River and its tributaries. The region is vulnerable to flooding during the monsoon season, with the highest risk occurring from July to August. Flash floods can also occur due to heavy rainfall in the hilly regions of neighboring India.")
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        align = "justify",
                        h5(strong("Sylhet Division")),
                        p("The Sylhet Division is located in the northeastern part of Bangladesh and is known for its hilly terrain and numerous rivers. The division experiences river flooding, flash floods, and landslides during the monsoon season. The hilly areas are prone to landslides, particularly after heavy rainfall.")
                      ),
                      column(
                        6,
                        align = "justify",
                        h5(strong("Map of Bangladesh by Division")),
                        h2(strong("")),
                        img(src = 'Bangladesh_map.png', align = 'right', width = "100%", height = "auto")
                      ),
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
                             #withSpinner(img(src="timeline", height = "500px", width = "100"))
                             withSpinner(plotOutput("timeline", height = "10px", width ="10%")),
                             #div(id = "year_timeline")
                      ),
                      
                      column(
                        width = 6,
                        align = "justify",
                        h5(strong("Flood Disasters in Bangladesh (2013-2019)")),
                        p("The susceptibility to flooding varies across the different divisions of Bangladesh, reflecting the diverse geographic and climatic conditions of the country. There have been several disasters that occurred from 2013-2019. Some notable examples include:"),
                        tags$ul(
                          tags$li("In 2013, the cyclone MAHASEN hit the coastal region, killing over 17 people and damaging over 60,000 houses (Government of Bangladesh, 2014)."),
                          tags$li("In 2014, severe flooding affected 46 districts and 16 million people, making it the most severe flooding since the “mega-flood” of 2007 (CARE, 2014)."),
                          tags$li("In 2016, tropical storm Roanu hit southern Bangladesh, resulting in significant damages (IFRC, 2017)."),
                          tags$li("In 2017, the cyclone storm Mora caused devastating floods, impacting a large population and vital infrastructure (Shelter Cluster, 2017)."),
                          tags$li("In 2019, Cyclone Titli caused widespread flooding and landslides, affecting 4 million individuals (OCHA, 2019).")
                        ),
                        p("Considering the threat of climate change, erosion, and rising sea levels, it is crucial to understand that storms are projected to increase in intensity, amplifying their impacts on vulnerable regions.")
                      )),
                    
                    fluidRow(
                      column(
                        12,
                        style = "padding: 30px;",
                        align = "justify",
                        h5(strong("Flood Write Up and Methodology")),
                        p("Gathering an accurate assessment of flood impacts is essential for determining the devastating consequences of flooding in Bangladesh. However, relying solely on self-reported data collected through the BIHS survey creates challenges due to underreporting and subjective interpretations of flood shocks and impacts. The variability in individual definitions of “shock” and different acceptance levels of the severity of flood events complicates the accuracy of self-reported data."),
                        p("To increase consistency, this research paper proposes the use of Sentinel-1 data, a powerful remote sensing satellite that uses synthetic aperture radar (SAR). Unlike self-reported data, which is prone to human errors and cognitive biases, Sentinel-1 data provides an unbiased and measurable representation of flood extents and duration."),
                        p("In a study done by Guiteras, Jina, and Mobarak in 2015, it was revealed that households reporting being affected by floods actually experienced higher objective flood exposure than what they reported. The team was able to determine this by comparing self-reported data with measurements from satellite data."),
                        p("There are multiple ways to use Sentinel-1 data for generating precise flood maps. The RGB method takes images before and after a flood event and combines them to create a visual representation of temporal changes. A study done by Conde and Muñoz in 2019 used Sentinel-1 (SAR) images to focus on mapping floods that occurred in the Ebro River in Spain in April 2018. They concluded that Sentinel-1 data should be utilized as a new source of input to enhance the precision of flood mapping, especially in weather conditions where floods occur.")
                      ),
                      
                    ),
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
                                                                  "Water Treatment" = "water_treatment",
                                                                  "Improved Water Accessibility" = "water_improvement",
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
                                                                                       "Average Birth Weight" = "avgbw_div",
                                                                                       "Wasting" = "wasting_div")),
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
           )),
navbarMenu("Methodology",
           tabPanel("Global Food Database",
                    # Overview section
                    fluidRow(
                      column(
                        width = 12,
                        h3("What is the Global Flood Database?"),
                        p("The Global Flood Database uses earth-observing satellites to measure and understand global flood exposure. It focuses on mapping the maximum extent of surface-water coverage during 913 significant flood events documented by the Dartmouth Flood Observatory between 2000 and 2018. The database serves as a valuable resource by complementing existing surface-water products that provide monthly or daily observations. It achieves this by providing a geospatial event catalog, which helps with the calibration and comparisons of flood models.
")
                      )
                    ),
                    
                    # Importance of GFD section
                    fluidRow(
                      column(
                        width = 12,
                        h3("Why are we using the GFD?"),
                        p("Originally, the team was going to use Sentinel 1 satellite data to observe flooding in Bangladesh however, there are large amounts of missing data during various years and months. For example, two divisions, Rangpur and Rajshahi, are missing all flood data in 2016. There are other instances of missing data but due to the extent of missing data, the sample size of children under the age of 5 and pregnant mothers would decrease significantly and the sample would no longer be representative. 
")
                      )
                    ),
                    
                    # Data Extraction Process section
                    fluidRow(
                      column(
                        width = 12,
                        h3("How to mapping flood extent using GFD?
"),
                        p("We utilized a two-step process for mapping the flood extent and locating affected households. First, we used data from the Bangladesh Integrated Household Survey (BIHS) to identify all of the surveyed households. These households were represented by dark pink circles in our analysis. Secondly, the global flood database provided us with flood extent during a specific time period which we choose. The flood extent was visualized using bright pink pixels. We created 20 kilometers buffer zones in order to represent the average distance a household has to travel to reach the nearest medical facility. By overlaying the buffer zones onto the surveyed households, we were able to identify the flood extent. This allowed us to pinpoint the households that were potentially impacted by the floods.
")
                      )
                    ),
                    
                    # Benefits of GFD section
                    fluidRow(
                      column(
                        width = 12,
                        h3("Benefits of using the GFD"),
                        p("The database has data from 2000 to 2018, which aligns with the timeframe of our study. This coverage allows us to include all of our intended samples without excluding data due to missing data or any other issues.
")
                      )
                    ),
                    
                    # Challenges and Limitations section
                    fluidRow(
                      column(
                        width = 12,
                        h3("Challenges and Limitations"),
                        p("The Global Flood Database has a few limitations when compared to Sentinel-1 satellite data. The spatial resolution of the GFD is 250 meteres which can not capture the details of a smaller-scale area or flood. This resolution is beneficial in capturing large, slow moving flood events while Sentinel-1’s higher resolution allows for more precise and detailed flood mapping. Sentinel-1 offers near-real-time observations with a temporal resolution of 12 days while the Global Flood Database relies on historical records from 2000 to 2018. 
Another limitation is the sensitivity of the GFD to weather conditions such as cloud cover. Since the database is reliant on earth-observing satellites, there is a potential for missing or incomplete data due to dense could cover while the Sentinel-1’s radar technology allows for precise data collection regardless of weather conditions. 
Out of the 913 recorded flood events globally, 134 involved Bangladesh, but only 23 fall within the timeframe of our study.
")
                      )
                    ),
                    
                    # Severity Levels section
                    fluidRow(
                      column(
                        width = 12,
                        h3("Severity Levels"),
                        tags$ul(
                          tags$li("1 - Large flood events, significant damage to structure or agriculture, fatalities, and/or 5-15 year reported interval since the last similar event"),
                          tags$li("1.5 - Very large events: >15 year but <100 year recurrence interval"),
                          tags$li("2 - Extreme events: recurrence interval >100 years")
                        )
                      )
                    )
           ),
           tabPanel("CHIRPS",
                    h3("About CHIRPS"),
                    p("CHIRPS, known as the Climate Hazards Group InfraRed Precipitation with Station data, is a globally gridded rainfall dataset. This dataset, originating in 1981, was developed by scientists from the United States Geological Survey (USGS) and the Climate Hazards Center (CHC). It continues to be actively supported by prominent organizations such as the United States Agency for International Development (USAID), National Aeronautics and Space Administration (NASA), and National Oceanic and Atmospheric Administration (NOAA).
"),
                    
                    h3("Data Integration"),
                    p("The CHIRPS dataset integrates satellite imagery and station data, to generate comprehensive and detailed rainfall maps. Satellite imagery is collected at a frequency of every five days, while station data provide ground-based observations. By combining these sources, CHIRPS effectively covers regions where station data is scarce, ensuring a more complete representation of rainfall patterns.
"),
                    
                    h3("Utilization in Research"),
                    p("In our research, we are utilizing CHIRPS to assess rainfall levels in rural areas of Bangladesh, recognizing the strong correlation between precipitation and flood occurrences. Although CHIRPS exhibits a relatively lower spatial resolution of 5566 meters per pixel, in contrast to the finer 10-meter pixel resolution of Sentinel 1, it allows us to address the limitations associated with capturing localized flood events in rural Bangladesh. By employing CHIRPS, we can bridge the data gaps left by Sentinel 1, particularly in terms of flood-related observations within rural regions of Bangladesh.
"),
          img(src = 'CHIRPS.png', align = 'right', width = "100%", height = "auto")
           ),

           tabPanel("Sentinel 1",
                    # Content for sub-tab 2
                    p("This is sub-tab 3.")
           )
           
           
),
navbarMenu("Results",
           tabPanel("Stunting",
                    tabsetPanel(
                      tabPanel("GFD Results",
                               fluidRow(style = "margin: 2px;",
                                        br(""),
                                        column(
                                          8,
                                          p(h5("Gender Disaggregation")),
                               plotlyOutput("graph1"),
                               plotlyOutput("graph2"),
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
                                 p("Stunting refers to when individuals have low height for their age which can have long-lasting effects even into adulthood. The graphs depict gender-disaggregated data for the percentage of children under five years old who are stunted on the y axis. We look at whether a flood event occurred during each trimester of the mother’s pregnancy given in the x-axis. Stunting prevalence differs across trimesters, with higher rates for children whose mothers experienced flooding during the third trimester. Additionally, the graphs highlight the vulnerability of girls to stunting, as their percentages surpass those of boys. This vulnerability may stem from neglect in rural Bangladesh due to patriarchal societal norms.
"), 
                               ),align = "Justify"
                      )),
                      tabPanel("CHIRPS Results",
                               fluidRow(style = "margin: 2px;",
                                        br(""),
                                        column(
                                          8,
                                          p(h5("Gender Disaggregation")),
                                          plotlyOutput("graph3"),
                                          plotlyOutput("graph4"),
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
                                          p(" we observed a positive correlation between precipitation intensity and stunting. In each trimester, higher rainfall intensities during pregnancy were associated with increased stunting after birth for both genders."), 
                                        ),align = "Justify"
                               )),
                      tabPanel("Regression",
                               p("This is sub-tab 2.2.")
                      )
                    )
           ),
           tabPanel("Underweight",
                    tabsetPanel(
                      tabPanel("GFD Results",
                               fluidRow(style = "margin: 2px;",
                                        br(""),
                                        column(
                                          8,
                                          p(h5("Gender Disaggregation")),
                                          plotlyOutput("graph5"),
                                          plotlyOutput("graph6"),
                                        ),
                                        column(
                                          4,
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          p(h4(strong("Description"))),
                                          p("Underweight refers to having a lower weight for their age. The data do not reveal a clear trend in children being underweight based on flood exposure during specific trimesters. The mixed results are expected, as literature suggests, since weight is a short-term indicator of health which can be recovered with proper nutrition over time
"), 
                                        ),align = "Justify"
                               )),
                      tabPanel("CHIRPS Results",
                               fluidRow(style = "margin: 2px;",
                                        br(""),
                                        column(
                                          8,
                                          p(h5("Gender Disaggregation")),
                                          plotlyOutput("graph7"),
                                          plotlyOutput("graph8"),
                                        ),
                                        column(
                                          4,
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          br(""),
                                          p(h4(strong("Description"))),
                                          p("When examining the graphs depicting percentages of underweight boys and girls per trimester based on rainfall intensity, no clear trend was evident, except that girls generally exhibited higher levels of underweight.
"), 
                                        ),align = "Justify"
                               )),
                      tabPanel("Regression",
                               p("This is sub-tab 2.2.")
                      )
                      
                      
                    )
           ),
           tabPanel("Mechanisms",
                    tabsetPanel(
                      tabPanel("Sub-tab 3.1",
                               p("This is sub-tab 3.1.")
                      ),
                      tabPanel("Sub-tab 3.2",
                               p("This is sub-tab 3.2.")
                      ),
                      tabPanel("Sub-tab 2.2",
                               p("This is sub-tab 2.2.")
                      )
                    )
           )
),
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
                  column(12, align = "left",
                         p("Aggarwal, Shilpa. “The Long Road to Health: Healthcare Utilization Impacts of a Road Pavement Policy in Rural India.” Journal of Development Economics, vol. 151, Elsevier BV, June 2021, p. 102667, doi:10.1016/j.jdeveco.2021.102667."),
                         p("Del Ninno, Carlo, and Mattias Lundberg. “Treading Water.” Economics and Human Biology, vol. 3, no. 1, Elsevier BV, Mar. 2005, pp. 67–96, doi:10.1016/j.ehb.2004.12.002."),
                         p("Guiteras, R., Jina, A., & Mobarak, A. M. (2015). Satellites, Self-reports, and Submersion: Exposure to Floods in Bangladesh. The American Economic Review, 105(5), 232–236. https://doi.org/10.1257/aer.p20151095 "),
                         p("Mallett, Lea H., and Ruth A. Etzel. “Flooding: What Is the Impact on Pregnancy and Child Health?” Disasters, vol. 42, no. 3, Wiley-Blackwell, July 2018, pp. 432–58, doi:10.1111/disa.12256."),
                         p(""),
                         p(""),
                         p(""),
                         p(""),
                         p(""),
                         p(""),
                         br("")))),
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
                                  img(src = "naveen.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                  p("", style = "padding-top:10px;"), 
                                  p(a(href = 'https://www.linkedin.com/in/naveen-abedin-0ab1089a', 'Naveen Abedin', target = '_blank'), " is a second-year Ph.D. student in Economics at Virginia Tech, specializing in Agricultural and Applied Economics."),
                                  
                                  
                                  p("", style = "padding-top:10px;"), 
                                  img(src = "Nandini_Das.JPG", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
                                  
                                  p("", style = "padding-top:10px;"), 
                                  p(a(href = 'https://www.linkedin.com/in/nandini-das-390577104/', 'Nandini Das', target = '_blank'), " Nandini Das is a Ph.D. candidate in Economics at Virginia Tech. "),
                                  
                                  
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
                                  p(a(href = "https://aaec.vt.edu/people/faculty/holmes-chanita.html", 'Dr. Susan Chen', target = '_blank'), ", Ph.D., is an Associate Professor in the Department of Agricultural and Applied Economics (AAEC) at Virginia Tech. She serves as the Director of the Data Science for Public Good (DSPG) program, overseeing its operations and initiatives. Additionally, she holds the position of Graduate Program Director, guiding and supporting students in their academic and research endeavors. ") , 
                                  
                                  p("", style = "padding-top:10px;")
                           )) ,
                  fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                           h4(strong("Project Stakeholder")),
                           img(src = "dr.bakhtiari.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "200px"),
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
      
      "The average household size is around 4.2 people across all divisions. Sylhet has relatively large households with an average household size of 4.93 members."
      
    }
    
    else if (Var_hhform() == "dependency_ratio_division") {
      
      "The dependency ratio is a demographic measure that compares the working-age population to the non-working-age population. A higher dependency ratio indicates a greater number of dependents per working-age person who may require support. It is important to note that dependency ratios can vary across different regions or divisions. In our analysis, the dependency ratio is below 40% across all divisions, indicating a relatively lower proportion of dependents. However, it's essential to consider other factors that may affect household composition and the level of independence of household members."
      
    }
    
    else if (Var_hhform() == "household_headship_division") {
      
      "The predominance of male household heads in rural Bangladesh reflects traditional gender roles and cultural norms, where men are often perceived as the primary decision-makers and main income generators. This gender disparity may have significant implications for women's empowerment and access to resources, as female-headed households might face additional challenges in accessing economic opportunities and social support. It is important to consider gender-specific interventions to promote inclusivity and socioeconomic equality in rural communities. However, it is worth noting that Chittagong stands out with a relatively higher percentage (33.57%) of female household heads, suggesting some regional variation in household leadership dynamics."
      
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
      
      "There are no significant age variations between males and females. However, females tend to be slightly older than males. In Khulna, the average age for males is 30.44 years old old, while for females, it is 31.77 years old, making it the division with the highest average age for both genders. Conversely, in Chittagong, the average age is the lowest, with males averaging 26.13 years old and females averaging 27.79 years old."
      
    }
    
    else if (Var3() == "male_age") {
      
      "Across all divisions, the age range with the highest male population is 31-65 years. Khulna has the highest percent of people in that category at 40.76%. It is also important to note that around 15% of the male population consists of children under the age of 5 across all divisions."
      
    }
    
    else if (Var3() == "female_age") {
      
      "Similar to their male counterparts, a significant proportion of the female population in rural Bangladesh falls within the 31-65 years age range. Khulna division stands out with the highest percentage (43.74%) of females falling within this age category. Additionally, children aged 5 years constitute approximately 10% of the population across all divisions."
      
    }
    else if(Var3() == "head_age"){
      "The most frequent household age across all of the divisions is “31-65 yrs”. The least frequent household age is “80+ yrs”. ’ Some factors that contribute to this could be that people under 31 years old migrate to cities or more developed areas in search of better educational and employment opportunities." 
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
      
      "In general, there are more males with no education than females. Despite this, a higher percentage of males attain higher education when compared to females. Sylhet, a more flood-prone division, has the lowest percentage of males in higher education at 4.16%."
      
    }
    
    else if (hedu() == "female_education_division") {
      
      "In general across all divisions, most females have no formal education or only completed primary education across. Rangpur has the highest percentage of uneducated females with 42.55%. The percentage range of females who manage to go past secondary school is between 2.83% to 5.15%."
      
    }
    
    else if (hedu() == "hh_highest_education") {
      
      "In this graph, more than half of the sampled households report “Completed primary” as their highest household educational attainment across all divisions. Rangpur, one of the more flood prone divisions, has the highest percentage of households whose members have no formal education."
      
    }
    else if(hedu() == "hh_ head_education"){
      "Household heads are primarily responsible for making income for the family and big decisions, but based on the Household educational attainment by division, it is evident that a majority of household heads across all divisions either do not have education or discontinued their education after primary school which is equivalent to the fifth grade in the US.  These low levels of education could potentially have negative impacts on household members." 
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
    else if (hheco() == "water_treatment") {
      
      p_h2o<-ggplot (filt_h2o,
                     aes(Division, Percentage, fill = Untreated_water))+
        geom_bar(position="dodge", stat="identity", aes(text = paste0("Division: ", Division, "\n", "Water Treatment: ", Untreated_water, "\n", "Percentage: ", Percentage)))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        labs(title = "Households Without Treated Drinking Water by Division",
             x= "Division",
             y = "Percentage")+
        scale_fill_viridis_d() +
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        easy_remove_legend()+
        ylim(0,100)
      ggplotly(p_h2o, tooltip = c("text"))
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
      
      "Most households primarily engage in farming activities with approximately 90% of them relying on subsistence farming. Livestock farming is the most prevalent agricultural activity compared to the other agricultural activities listed. Note that some households will participate in more than one agricultural activity.
"
      
    }
    
    else if (hheco() == "cultivable_land") {
      
      "Across divisions, the most relative farm size categories are the “Marginal” and “No cultivable land” categories. Chittagong has the highest percent (44.97%) of households without cultivable land. It is followed by Sylhet at 43.09% without cultivable land."
      
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
      "In each division, farming is the dominant occupation, with the highest concentration of farmers in Khulna. Non-earning occupations, including subsistence farming, household chores, unpaid family labor, and others, are the second most common occupations across most divisions. Livestock-related work has the lowest frequency, accounting for less than 1% combined across all seven divisions. This is likely due to the prioritization of crop cultivation and the vulnerability of livestock to natural disasters."
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
      list(src = paste0(getwd(),"/www/2013.png"), align = 'center', width = "650px", height = "320px", deleteFile=FALSE)
      
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
    else if (var4() == "wasting_div") {
      # Create the bar plot using ggplot
      wasteplot <- ggplot(wastediv_combined, aes(x = Division, y = Percentage, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Children Wasted by Division by Gender",
             x = "Division",
             y = "Percentage",
             fill = "Gender") +
        scale_fill_manual(values = c("#65cb5e", "#21918c", "#cc4778")) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 20)+
        easy_center_title()
      
      waste_gender_div <- ggplotly(wasteplot)}
  })
  #TEXT OUPUT PER GRAPH
  
  output$mctext1 <- renderText({
    if (var4() == "stunt_div") {
      "The graph shows the percentage of stunting among children under the age of five in Bangladesh by division and gender. Stunting refers to individuals having low height for their age, which can have long-lasting effects. Sylhet has the highest percentage of stunting at 42.12%, followed by Barisal at 33.33%. Rajshahi has the lowest percentage of stunting at 27.04%."}
    else if (var4() == "underweight_div") {
      "The graph depicts the percentage of children under the age of five who are underweight by gender and division in Bangladesh. Underweight refers to individuals having a low weight for their age. Sylhet has the highest percentage of underweight children at 32.27%, followed by Chittagong at 24.19%. Rajshahi has the lowest percentage of underweight children at 16.84%."}
    else if (var4() == "avgbw_div") {
      "This graph shows the average birth weight of children born by Division and Gender. Rajshahi has the highest birth weight among the divisions at 3.25kg and Chittagong being the lowest at 2.7kg. Referring back to the percentage of children underweight, Rajshahi has the lowest percentage among the divisions reflected in this graph as it has the highest average birth weight. Similarly, Sylhet has a lower relative birth weight when compared to the other divisions. "}
    else if (var4() == "wasting_div") {
      "The graph examines the percentage of children under the age of five who are wasted, categorized by gender and division in Bangladesh. Wasting, as defined by the World Health Organization (WHO), indicates a condition where a person's weight is significantly low for their height, indicating acute malnutrition. Sylhet has the highest percentage of wasted children at 10.59%, followed by Chittagong at 10.58%. Barisal has the lowest percentage at 7.14%."}
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
      "Education is an indicator of empowerment and knowledge. Therefore, the more education a woman has, the more likely she will be able to provide for her own and child's health. This graph shows the educational attainment levels among the mothers within the Divisions. In general, the modal category of education is completing primary school. In Bangladesh, primary school ends at the fifth grade in the US. This means that most mothers discontinue their education after achieving fifth grade knowledge."}
    else if (var5() == "occu_dist") {
      "The graph displays the distribution of occupations among mothers in Bangladesh. Non-earning occupations, including roles such as primary caregivers, maids, street vendors, subsistence farmers, and nannies, comprise the largest percentage across all divisions. A mother's level of education and societal norms greatly influence her ability to engage in income-generating occupations that support the household."}
  })
  
  output$graph1 <- renderPlotly({
    stunt_g <- ggplot(read.csv("data/stunt_female.csv"), aes(x = Trim, y = Percentage, fill = factor(Value))) +
      geom_bar(stat = "identity", position = "dodge") +

      labs(title = "Percentage of Stunted Girls < 5 y/o by Incidence of Flood Event",
           x = "Trimesters", 
           y = "% of Stunted Girls < 5 y/o") +
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
      
      labs(title = "Percentage of Stunted Boys < 5 y/o by Incidence of Flood Event",
           x = "Trimesters", 
           y = "% of Stunted Boys < 5 y/o") +
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
    labs(title = "Percentage of Underweight Girls < 5 y/o by Incidence of Flood Event",
         x = "Trimesters", 
         y = "% of Underweight Girls < 5 y/o") +
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
    labs(title = "Percentage of Underweight Boys < 5 y/o by Incidence of Flood Event",
         x = "Trimesters", 
         y = "% of Underweight Boys < 5 y/o") +
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
    
    labs(title = "Percentage of Stunted Girls < 5 y/o by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "% of Stunted Girls < 5 y/o") +
    
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
    
    labs(title = "Percentage of Stunted Boys < 5 y/o by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "% of Stunted Boys < 5 y/o") +
    
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
    
    labs(title = "Percentage of Underweight Girls < 5 y/o by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "% of Underweight Girls < 5 y/o") +
    
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
    
    labs(title = "Percentage of Underweight Boys < 5 y/o by Precipitation Intensity",
         
         x = "Trimesters", 
         
         y = "% of Underweight Boys < 5 y/o") +
    
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

    
}

shinyApp(ui = ui, server = server)











