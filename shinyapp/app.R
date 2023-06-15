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

#---------------------------data---------------------------------------------------------------------

data_mem <- read_dta(paste0(getwd(),"/data/BIHS2018-19Members_Jun3.dta")) 
data <- read_dta(paste0(getwd(),"/data/BIHS2018-19MC_Jun4.dta"))

#---------------------------Household data---------------------------------------------------------------------
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
            count = n())



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
                            
                            "5" = "65-80 yrs",
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
                            
                            "5" = "65-80 yrs",
                            "6" = "80+ yrs"))

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
         Ocucategory = recode(Ocucategory, "1" = "agricultural day labor",
                              "2" = "non-agricultural day labor",
                              "3" = "salaried",
                              "4" = "rickshaw/van puller",
                              "5" = "self-employed",
                              "6" = "business/trade",
                              "7" = "production business",
                              "8" = "livestock-related work",
                              "9" = "farming",
                              "10" = "non-earning occupations"))
# ocudiv

ocudiv$Ocucategory <- factor(ocudiv$Ocucategory, levels = c("non-earning occupations", "farming","livestock-related work","production business","business/trade","self-employed","rickshaw/van puller","salaried","non-agricultural day labor","agricultural day labor"))


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
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
           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/node/451\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('economic');
           }
           "

# user -------------------------------------------------------------
ui <- navbarPage(title = "DSPG Bangladesh 2023",
                 selected = "Overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # main tab -----------------------------------------------------------
                 tabPanel("Project Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Effects of prenatal exposure to flooding on birth outcomes: Evidence from Bangladesh"),
                                      #h2("") ,
                                      br(""),
                                      h4("Data Science for the Public Good Program 2023"),
                                      h4("Virginia Polytechnic Institute and State University"),
                                      #h4("[updat this]"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Country Profile")),
                                          
                                          p("Bangladesh is a country located in South Asia. It is bordered by India in its West, North, and East regions, and Myanmar to its Southeast regions. The country has a flat geography with over 700 rivers and tributaries that originate from three main river systems: the Brahmaputra-Jamuna, Ganges-Padma, and Surma-Meghna (. As of 2020, Bangladesh was the ninth most densely populated country in the world; its present day population is 169,469,771. In 2016, 13.47% of the population in Bangladesh lived below the international poverty line of $2.15 per day. However, by 2022, the poverty rate had declined to 10.44% which shows improvement however it is important to recognize that a little over 10% of the population still lives below the national poverty line. Agriculture is the country's primary economic sector, which makes up 37.6% of the total work force and 13.1% of the GDP. 

The country is characterized by a multi-tiered administrative structure that helps in governing the country effectively. This structure encompasses several levels, including divisions, districts, upazilas (sub-districts), unions, and villages. The country has 8 divisions: Barisal, Chittagong, Dhaka, Khulna, Mymensingh, Rajshahi, Rangpur, and Sylhet, which serve as the highest level of administrative units. Each division is further divided into 64 districts, comparable to counties in the USA. Within the divisions, multiple upazilas (sub-districts) play a crucial role in implementing government policies. The number of upazilas varies based on population density and administrative requirements, with a recorded count of 544 upazilas until 2022. These upazilas are divided into unions, which are composed of several villages and are responsible for local government functions and community well-being. Villages are the smallest unit of administration and are primarily located in rural areas. They accommodate the majority of the population and play a significant role in the socio-economic role of Bangladesh. 

While there is a systematic administrative structure in place, the country still faces many socio-economic challenges such as a high population density, the country's vulnerability to rising sea levels, flooding, limited access to healthcare, and gender inequality. ", align = "justify"),
                                          
                                          h2(strong("Flood Conditions")),
                                          p("Bangladesh is a topographically flat country lying at the delta region of three major rivers: the Ganges, Biahmaputra, and Meghna. Almost 60% of the country is lower than 6m above sea level which contributes to slow draining and increases the risk of overflow. The country also experiences a monsoon season that spans from June-September. The Ganges and Biahmaputra originate from the Himalayan region which experiences heavy snowfall during the winter. The snow melts during the warmer months and increases river discharge downstream. The combination of heavy rainfall, the flat geography, and the increased river discharge from snowmelt contributes to the severity of floods in Bangladesh.

Some factors that determine whether an area is more susceptible to flooding than others include elevation, geography, and infrastructure (drainage systems, water management systems, etc..) . River basins near Bangladesh’s three major rivers are more susceptible to flooding due to their extensive drainage areas, high sediment loads, and proximity to the Himalayan snow melt that was mentioned earlier. 

The coastal regions of Bangladesh, including the South-Western and South-Central parts, are highly susceptible to flooding. The combination of cyclones, rising sea levels, and excessive rainfall increases the risk of tidal storms and flooding on the flat areas. The central and eastern parts of Bangladesh are also susceptible to flooding, though the intensity and duration will vary. Flooding in this region is caused by heavy monsoon rainfall and limited drainage capacity of the Padma river. 

While the north/northeastern parts of Bangladesh are far from large bodies of water, this region floods the most due to its proximity to the Himalayas. Snow melt and rainfall causes water to flow rapidly downstream and onto Bangladesh's flat land leading to flash floods. ", align = "justify")
                                          #p("During the 2018 – 2019 school year, the Community school model provided the families with clothes, shoes, and other basic supplies 538 times; enabled 135 families to receive weekend meals throughout the school year; supported 6 academic programs for 323 students; and provided 9 after-school enrichment programs for 373 students. Funds have provided these Community Schools with additional resources, such as full-time parent liaisons, a full-time social worker, and programs that keep families engaged in their child’s education. The Community Schools initiative focuses on bolstering these schools in six areas: academies, health and social services, youth and community engagement, building stronger families, and healthier communities."),
                                          
                                   ),
                                   column(4,
                                          h2(strong("Goals and objectives")),
                                          h3(strong("Research Question:")), 
                                          p("The research question we are centering our study on are the effects of prenatal exposure to flooding on birth outcomes and the mechanisms through which floods affect birth outcomes. Flooding is found to have a correlation between worsened birth outcomes including lower birth weight, cognitive function, and height (Mallett, 2017). Additionally, the timing of  flood exposure on pregnant women will affect the severity of fetal growth. The second part of our research aims to look at the mechanisms of which flooding affects societal access to sources of nutrition and healthcare services. When flooding occurs, the availability of obtaining clean water becomes increasingly more difficult as water gets contaminated. Street and road access also becomes inaccessible. These factors directly affect mothers and their availability to access adequate nutrition and health care. In this paper, we will further observe the effects of flood exposure on prenatal expire on birth outcomes.", align = "justify"),
                                          h3(strong("Objectives:")), 
                                          p("During our 10-week study, we will conduct literature review on prenatal exposure to flooding and birth outcomes. To collect flooding data and birth outcomes in Bangladesh from the year 2018 - 19, we will be using satellite data on google earth engine and the BIHS survey. With the satellite data, we will download flood statistics and generate maps based on our findings. In order to accurately capture the effects of flooding, the flood statistics will be joined with the survey data leading to a multivariate analysis of main outcomes and flooding. Our team will explore the mechanisms through which floods affect birth outcomes. Based on our research, we will result in a poster and presentation which will be delivered at the Virginia Tech Symposium and an interactive Shiny App webpage.", align = "justify"),
                                          h3(strong("Background:")), 
                                          p("Due to the short discourse of our 10-week program, we will be reviewing other relevant and credible literature relating to research questions. A number of research papers converge to the point that flood exposure negatively affects birth outcomes. Most of them highlight food insecurity and healthcare  inaccessibility during flood periods to be the main cause of poor birth outcomes that include lower birth weight, child stuntedness which in most cases lead to higher infant mortality, morbidity, developmental problems and diseases in adulthood (Wilcox, 2001).
For example,  the 2021 paper by Oskorouchi, et al t, suggest that exposure to flooding during a 12-month period decreased daily calorie con-sumption by approximately 60 kcal while increasing the probability of iron, vita-min A, and vitamin C deficiency by 11, 12, and 27 percentage points, respectively. This emphasizes the fact that if a pregnant mother is exposed to flood, she would be more likely to have fetal development issues and hence negative birth outcomes. However, this paper has a problem, for their analysis, they solely relied on self-reported data to assess flood exposure. Self-reported data are susceptible not only to the inaccuracies of memory but also to various cognitive biases such as reference dependence (Guiteras et al., 2015).
", align = "justify")
                                          
                                          
                                          
                                   ), 
                                   
                                   column(4,
                                          h2(strong("Research Design")),
                                          p("In our research, we will use the Bangladesh Integrated Household Survey (BIHS) data collected by IFPRI. We will utilize cross-sectional data for the year 2018-2019,  from round 3 of the BIHS survey, which is the most recent available household survey.  The sample design had two main objectives: capturing the rural areas of Bangladesh (national representation) and including all seven administrative divisions in Bangladesh, divisional representation. The BIHS sample also included observations from the Feed The Future (FTF) zone of influence, which we are going to discard from our sample to avoid over-sampling issues
To determine the sample size meeting the above-mentioned conditions, the IFPRI survey team used a careful 2-stage stratified statistical sampling method to satisfy the above mentioned conditions. In the first stage, they selected Primary Sampling Units (PSUs), which are villages in this context, using probability proportional to size based on the number of households in each stratum or division. The allocation of PSUs resulted in the following distribution: 21 PSUs in Barisal, 48 in Chittagong, 87 in Dhaka, 27 in Khulna, 29 in Rajshahi, 27 in Rangpur, and 36 in Sylhet, and 50 in the FTF zone. In the second stage, 20 households were randomly selected from each PSU. The IFPRI sample size was 6,500 households across 325 PSUs. Our sample size, however,  is a bit smaller than the 6,500 households because we excluded 1000  households from the FTF Zone of Influence. Also, it is important to note that, since we are using the data from the 3rd round of the survey,  several changes happened within households between 2011-12 and 2018-19: households from the first round have either merged, or split. After making  all those adjustments our sample size turns out to be 5604 households.", align = "justify"),  
p("In addition to the survey data we will use Sentinel-1 data to assess  the flood extent 
Sentinel-1 is a satellite by the European Space Agency launched in 2014. It uses radar to gather data information from the earth's surface and generates high-resolution images as radar sensors are not sensitive to atmospheric conditions. There are a few steps we followed to gather our flood data. First we chose our study area: this is setting a buffer zone around each sampled household so that we examine how much it was affected by the flood.  Second, we chose the timeframe based on when the mother was pregnant so that we can birth outcomes based on the mother’s flood exposure ", align = "justify"),
                                          h2(strong("Survey")),
                                          p("To gather more information about the health and living conditions of Bangladesh, we will be using the BIHS, conducted by our stakeholders, IFPRI, stationed in Bangladesh. The survey is composed of multiple modules. However, in our study we will focus mainly on modules A, B, W, and Y. Module A will provide insight on the sample households and identification; this includes information on coordinates of the household and the total number of members in the household. Coordinates of the household will be used to locate the proximity of these households to the affected flooded areas. Module B covers the Household Composition and Education. THis module entails the education levels, occupation, and source of income for individuals of the household. Module W focuses on the anthropometry, health, and illnesses of each individual of the household. Module Y has sectional portions containing survey data on child and antenatal care, Infant and child feeding practices, immunization and health of children younger than the age of two and service use.
", align="justify")
                                          #p("")
                                   )
                          ),
                          #fluidRow(align = "center",
                          # p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("Flooding in Bangladesh")),
                                          p(""),
                                          br("")
                                   ))
                 ), 
                  tabPanel("Household Profile", value = "overview",
                           # this is the title of the HH tab 
                            fluidRow(style = "margin: 6px;",
                             p("", style = "padding-top:10px;"),
                             column(12, align = "center",h4(strong("Household Profile")),
                               p(""),
                               br(""))
                             ),
                            # this is the body of the HH prof tab
                            fluidPage(
                              column(6, align = "left", 
                                     selectInput("demos1drop","Select Socioeconomic Characteristic:", 
                                                 width = "100%", 
                                                 choices = c("Age by Gender" = "age_by_gender",
                                                             "Male Age Distribution" = "male_age", 
                                                             "Female Age Distribution" = "female_age")),
                                     br(""),
                                     withSpinner(plotlyOutput("demo1", height = "500px", width ="100%")),
                              column(12, align = "right",
                                     p("Source:",style = "font-size:12px;"))
                              )
                            )
                  ),

                  tabPanel("Mother and Child", value = "overview",
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
                                                               selectInput("mcdrop1", "Select Birth Outcome characteristic:", width = "100%",
                                                                           choices = c("Stunting by Divsion" = "stunt_div",
                                                                                       "Underweight by Division" = "underweight_div",
                                                                                       "Average Birth Weight by Division" = "avgbw_div",
                                                                                       "Wasting by Division " = "wasting_div")),
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
                                                                                       "Occupations " = "occu_dist")),
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
)


## Sundarbans Region--------------------------------------------

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  Var3 <- reactive({input$demos1drop})
  
  output$demo1 <- renderPlotly({
    
    if (Var3() == "age_by_gender") {
      
      pgg <- ggplot(avg, aes(Division, Mean_age, fill = Gender))+
        geom_bar(position="dodge", stat="identity")+
        theme_classic()+
        labs(title = "Average Age by Division and Gender",
             x= "Division",
             y = "Mean age")+
        scale_fill_manual(values = c("Male" = "#21918c", "Female" = "#cc4778")) +
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        ylim(0, 35)
      
      ggplotly(pgg)
      
    }
    
    else if (Var3() == "male_age") {
      
      p_ma<- ggplot(age_div_male, 
                    aes(Division, Percentage, fill = Age_range))+
        geom_bar(position="dodge", stat="identity")+
        scale_fill_viridis_d() +
        theme_classic()+
        scale_fill_manual(values = c("0-5 yrs" = "#440154", "6-10 yrs" = "#fde725",
                                     "11-17 yrs" = "#3a528b", "18-30 yrs" = "#21918c", 
                                     "31-65 yrs" = "#ff7f00","65-80 yrs" = "#bd93f5", 
                                     "#5ec962", "#cc4778",
                                     "80+ yrs" = "#f6c2f8"))+
        labs(title = "Age Distribution of Male by Division",
             x= "Division",
             y = "Percentage")+
        easy_plot_title_size(size = 16)+
        easy_center_title()+
        ggeasy::easy_rotate_labels(which = "x", angle = 300)+
        ylim(0,50)
      ggplotly(p_ma)
    }
    
    else if (Var3() == "female_age") {
      
      p_fe <- ggplot(age_div_female, aes(Division, Percentage, fill = Age_range)) +
        geom_bar(position = "dodge", stat = "identity") +
        scale_fill_viridis_d() +  # Set colors to viridis defaults
        theme_classic() +
        scale_fill_manual(values = c("0-5 yrs" = "#440154", "6-10 yrs" = "#fde725",
                                     "11-17 yrs" = "#3a528b", "18-30 yrs" = "#21918c", 
                                     "31-65 yrs" = "#ff7f00","65-80 yrs" = "#bd93f5", 
                                     "#5ec962", "#cc4778",
                                     "80+ yrs" = "#f6c2f8"))+
        # easy_all_text_colour("#630031") +
        labs(title = "Age Distribution of Female by Division", x = "Division", y = "Percentage") +
        easy_plot_title_size(size = 16) +
        easy_center_title() +
        ggeasy::easy_rotate_labels(which = "x", angle = 300) +
        ylim(0, 50)
      ggplotly(p_fe)
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
        geom_bar(stat = "identity", position = "dodge") +
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
      
      bw_gender_div <- ggplotly(bwplot)}
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
  #TEXT OUPUT PER GRAPH work on this tomorrow

  output$mctext1 <- renderText({
    if (var4() == "stunt_div") {
      "This graph looks at the percentage of stunting among children under the age of five in Bangladesh by division by gender. Among the divisions, Sylhet has the highest percentage of stunting at 42.12%. Barisal, following Sylhet, has the second highest percentage of stunting at 33.33%. Rajshahi has the lowest at 27.04%. "}
    else if (var4() == "underweight_div") {
      "This graph looks at the percentage of children < 5 years old that are underweight by gender and division. Among the divisions, Sylhet has the highest percentage of underweight children at 32.27%  and Chittagong being the second highest percentage at 24.19%. Rajshahi has the lowest at 16.84%. "}
    else if (var4() == "avgbw_div") {
      "This graph shows the average birth weight of children born by Division and Gender. Rajshahi has the highest birth weight among the divisions at 3.25kg and Chittagong being the lowest at 2.7kg. Referring back to the percentage of children underweight, Rajshahi has the lowest percentage among the divisions reflected in this graph as it has the highest average birth weight. Similarly, Sylhet has a lower relative birth weight when compared to the other divisions. "}
    else if (var4() == "wasting_div") {
      "This graph looks at the percentage of children < 5 years old that are wasted by gender and division. Among the divisions, Sylhet has the highest percentage of wasted children at 10.59%  and Chittagong being the second highest percentage at 10.58%. Barisal has the lowest at 7.14%. "}
  })
    
  # Render maps for mother profile 
  var5 <- reactive({input$mcdrop2})
  #matching with ui for mc2
  output$mc2 <- renderPlotly({
    if (var5() == "age_dist") {
      # Create the bar plot using ggplot
      ageplot <- ggplot(agediv, aes(x = Percentage, y = Division, fill = Agecategory)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Age Distribution of Mothers by Division",
             x = "Percentage",
             y = "Division",
             fill = "Agecategory") +
        scale_fill_manual(values = c( "#3a528b", "#21918c","#ff7f00" )) +
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 20)+
        easy_center_title()
      
      # ageplot
      
      agedist_div <- ggplotly(ageplot)}
    else if (var5() == "edu_dist") {
      # Create the bar plot using ggplot
      eduplot <- ggplot(edudiv, aes(x = Percentage, y = Division, fill = Educategory)) +
        geom_bar(stat = "identity", position = "stack") +
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
        easy_center_title()
      
      # eduplot
      
      edudist_div <- ggplotly(eduplot)}
    else if (var5() == "occu_dist") {
      # Create the bar plot using ggplot
      ocuplot <- ggplot(ocudiv, aes(x = Percentage, y = Division, fill = Ocucategory)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Occupation of Mothers by Division",
             x = "Percentage",
             y = "Division",
             fill = "Occupation category") +
        scale_fill_manual(values = c("agricultural day labor" = "#440154", "non-agricultural day labor" = "#fde725",
                                     "salaried" = "#3a528b", "rickshaw/van puller" = "#21918c", 
                                     "self-employed" = "#ff7f00", "business/trade" = "#bd93f5", 
                                     "production business" = "#5ec962", "livestock-related work" = "#cc4778",
                                     "farming" = "#f6c2f8", "non-earning occupations" = "#b26600"))+
        theme_classic()+
        easy_y_axis_title_size(size = 15)+
        easy_x_axis_title_size(size = 15)+
        easy_plot_title_size(size = 16)+
        easy_center_title()
      
      # ocuplot
      
      ocudist_div <- ggplotly(ocuplot)}
    
  })
  
  #TEXT OUTPUT FOR MOTHER GRAPHS
  output$mctext2 <- renderText({
    if (var5() == "age_dist") {
      "This graph shows the average distribution of mothers' ages across the seven divisions. In general, the modal age is between 18-30 years old. Though the percentage of mothers aged 11-17 years old are low, a younger pregnancy is a possible determinant to birth outcomes. The younger a mother is, the less voice and empowerment she will have to take care of own and her child's health. At this stage of growth, having enough nutrients becomes a risk for the mother and child. Without receiving a proper amount of nutrients, the mother and/or the child's growth will be affected."}
    else if (var5() == "edu_dist") {
      "Education is an indicator of empowerment and knowledge. Therefore, the more education a woman has, the more likely she will be able to provide for her own and child's health. This graph shows the educational attainment levels among the mothers within the Divisions. In general, the modal category of education is completing primary school. In Bangladesh, primary school ends at the fifth grade in the US. This means that most mothers discontinue their education after achieving fifth grade knowledge."}
    else if (var5() == "occu_dist") {
      "\\This graph shows the percentage distribution of occupations mothers in Bangladesh participate in. Among all divisions, the non-earning occupations category is the highest percentage. Non-earning occupations consist of roles such as primary care givers, maids, street vendors, subsistence farmers, and nannies. The level of education a woman receives and the societal culture will also affect a mother's ability to participate in an occupation that will allow her to earn income for the household.\""}
  })
}


#sociodemo tabset ----------------------------------------------------
shinyApp(ui = ui, server = server)


