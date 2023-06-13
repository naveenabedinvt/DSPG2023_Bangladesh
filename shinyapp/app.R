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


#-----------Diagrams-------------------------------------------

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
                 selected = "overview",
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
                                      h4("Data Science for the Public Good Program"),
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
Sentinel-1 is a satellite by the European Space Agency launched in 2014. It uses radar to gather data information from the earth's surface and generates high-resolution images as radar sensors are not sensitive to atmospheric conditions. There are a few steps we followed to gather our flood data. First we chose our study area: this is setting a buffer zone around each sampled household so that we examine how much it was affected by the flood.  Second, we chose the timeframe based on when the mother was pregnant so that we can birth outcomes based on the mother’s flood exposure ", align = "justify")
                                          #p(""),
                                          #p("")
                                   )
                          ),
                          #fluidRow(align = "center",
                          # p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 ## Sterling Area--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 6px;",
                                   p("", style = "padding-top:10px;"),
                                   column(12, align = "center",h4(strong("Map of Sterling")),
                                          p("This map shows the Sterling area and the 6 schools."),
                                          br("")
                                          
                                          
                                          
                                   )),
                          
                          fluidPage(
                            column(12, align = "center", plotlyOutput("age_by_gender", width = "60%")
                                   #fluidRow(align = "center",
                                   #    p(tags$small(em('Last updated: August 2021'))))
                            ) 
                          )
                 ), 
                  tabPanel("Sociodemographics", value = "overview",
                            fluidRow(style = "margin: 6px;",
                             p("", style = "padding-top:10px;"),
                             column(12, align = "center",h4(strong("")),
                               p(""),
                                br("")
                         
                         
                         
                  ))
)
)


## Sundarbans Region--------------------------------------------

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # Render map 
  output$age_by_gender <- renderPlotly({
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
  })
  
}

#sociodemo tabset ----------------------------------------------------

shinyApp(ui = ui, server = server)