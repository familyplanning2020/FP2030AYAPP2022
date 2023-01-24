# 
# list.of.packages <- c("shiny", "dplyr", "ggplot2", "tidyr",
#                       "readxl", "formattable", "plotly", "gt",
#                       "plyr", "factorial2x2", "shinyBS")
# 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(formattable)
library(plotly)
library(gt)
library(factorial2x2)
library(shinyBS)
library(DT)
library(cowplot)

#setwd("C:/Users/sfarid/OneDrive - United Nations Foundation/Documents/FP2030AYAPP/ay")
aypopdata <- read_excel("data/CleanedAYData_2022_ProgressReport.xlsx", sheet = "AYPOP")
aypopdata$sum_10_49 =rowSums(aypopdata[,2:3])
aypopdata$prop10_14 =round((aypopdata$`Young Adolescents (10-14)`/aypopdata$sum_10_49)*100,1)
aypopdata$prop15_19 =round((aypopdata$`Older Adolescents (15-19)`/aypopdata$sum_10_49)*100,1)
aypopdata$prop20_24 =round((aypopdata$`Older Youth (20-24)`/aypopdata$sum_10_49)*100,1)
aypopdata$round_sum_10_49=round(aypopdata$sum_10_49,-4)
aypopdata$round_sum_10_14=round(aypopdata$`Young Adolescents (10-14)`,-4)
aypopdata$round_sum_15_19=round(aypopdata$`Older Adolescents (15-19)`,-4)
aypopdata$round_sum_20_24=round(aypopdata$`Older Youth (20-24)`,-4)

aypopdata.prop <- aypopdata %>% gather(Age_Group,Count,prop10_14, prop15_19, prop20_24)
aypopdata.sum <- aypopdata %>% gather(Age_Group,Round_Total,round_sum_10_14, round_sum_15_19, round_sum_20_24)
aypopdata.sum=aypopdata.sum[, 13]
aypopdata.long <- cbind(aypopdata.prop, aypopdata.sum)
aypopdata.long$survey = sample(100, size = nrow(aypopdata.long), replace = TRUE)
#aypopdata.long =aypopdata.long[, -16]
aypopdata.long2=aypopdata.long

kle_age <- read_excel("data/CleanedAYData_2022_ProgressReport.xlsx", sheet = "KLEAgeEvents")
kle_marriage <- read_excel("data/CleanedAYData_2022_ProgressReport.xlsx", sheet = "KLEMarriage")
kle_marriage$`% of 15-19 year olds who are married`=round(kle_marriage$`% of 15-19 year olds who are married`*100,1)
kle_marriage$`% of 20-24 year olds who are married`=round(kle_marriage$`% of 20-24 year olds who are married`*100,1)
kle_marriage$`% of adolescent and youth (15-24) who are married`=round(kle_marriage$`% of adolescent and youth (15-24) who are married`*100,1)
kle_marriage$`% of 20-24 year olds married before 18`=round(kle_marriage$`% of 20-24 year olds married before 18`*100,1)
kle_marriage$`% of 25-29 year olds married before 18`=round(kle_marriage$`% of 25-29 year olds married before 18`*100,1)
data <- subset(kle_marriage, select=c(5))
colnames(data)[1]=""
kle_marriage=kle_marriage[, -c(5)]
kle_marriage$`% of adolescent and youth (15-24) who are married`=data
colnames(kle_marriage)[6]="% of 25-29 year olds married before 18"
kle_age_compare=kle_age
kle_marriage_compare=kle_marriage

ayfp <- read_excel("data/CleanedAYData_2022_ProgressReport.xlsx", sheet = "AYFPUse")
ayfp$`MCPR for married adolescent and youth (15-24)`= round(ayfp$`MCPR for married adolescent and youth (15-24)`, 1)
ayfp_compare=ayfp

# Round the counts of AY popualtion & WRA Population  ==> SHIZA DID THIS <== 
# aypopdata.long$Round_Count <- round(aypopdata.long$Count, -5)
aypopdata$Round_Count_WRA <- round(aypopdata$`Women of Reproductive Age (15-49)`, -5)

res <- aypopdata.long %>% filter(aypopdata.long$Country == "India")


countries <- unique(aypopdata.long$Country)

#FP2030 Color Palette - Graphs 
#AY Population Graph = cbp1 
#Key Life Events Graph = cbp2
#Prevalence of sexual activity in the last month (Sexual activity and never had sex) = cbp3
#Modern Contraceptive Method Prevalence (Unmarried) = cbp3 
#Modern Contraceptive Method Prevalence (Married) = cbp5
#Modern Contraceptive Method Prevalence (Condom use) = cbp4
#Traditional Contraceptive Method Prevalance (Unmarried) = cbp3
#Traditional Contraceptive Method Prevalence (Married) = chp5

#FP2030 Color Hex Codes
#cbp1 <- c("#bdd1ff", "#82d816", "#73d8bf", "#248c85", "#f7bc1b", "#ff7314", "#4fb3ff", "#00158a")
cbp2 <- c("#21b1fe",  "#1bce9b", "#1a7158", "#ffb636", "#ff7140", " #f2f0ff")
#cbp2 <- c("#bdd1ff", "#73d8bf", "#2a977c")
cbp1 <- c("#ffb636", "#ff7140", "#89427b")
cbp3 <- c( "#1bce9b", "#1a7158")
cbp4<- c("#97aed4")
cbp5 <- c("#1bce9b","#97aed4", "#1a7158")

### NEED TO FIGURE OUT WHY IT'S NOT WORKING WITH GITHUB FOLDER ### 
b64 <- base64enc::dataURI(file="www/FP2030_Logo_primary_color.png", mime="image/png")

# Define UI for application 
ui <- navbarPage(                  
  #the line of code places the logo on the left hand side before the tabs start. See image below.
  title = div(img(src=b64,style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
  #builds Tab for Profile Page
  tabPanel("How to Use",
           HTML(
             paste(
               h1("Adolescent and Youth Data App"),'<br/>',
               h2("What is this?"),'<br/>',
               p("This is an interactive data app created by Family Planning 2030 (FP2030). FP2030 
                 is a global partnership to empower women and girls by investing in rights-based family planning. 
                 You will be able to view, compare, and analyze the adolescent and youth data that 
                 was released with the FP2030 Measurement Report 2022 through different graphics and tables."),'<br/>',
               p("All estimates are calculated using a country's latest Demographic Health Survey (DHS), Multiple Indicator Cluster Survey (MICS),
                   or Performance Monitoring for Action Surveys (PMA). These surveys provide nationally representative data on health and population 
                   in developing countries; all data is publicly available."),
               h2("Profile, Compare, and Analyze Pages"),'<br/>',
               p("The Profile Page includes individual country data on adolescents and youth population, key life events, prevalence of sexual activity,
                  modern contraceptive method prevalence, and traditional contraceptive method prevalence. The Compare Page provides the opportunity to view
                  data from the profile page for multiple countries. The Analyze Page allows you to further analyze this data."),'<br/>',
               h2("Have Questions?"),
               p("Contact us at info@fp2030.org"),'<br/>'),
             #tags$img(src = b64, align = "right", height = '100px', width = '100px')),
           )
           
  ),
  
  tabPanel("Profile",
           HTML(
             paste(
               h1("Profile"),
               h3("Select a Country to Learn about its Adolescent and Youth Data"), '<br/>', '<br/>'
               
             )
           ),
           fluidRow(
             column(6,
                    wellPanel(
                      selectInput("country",
                                  "Select Country",
                                  choices = as.list(aypopdata.long$Country))
                    )
             ),
             column(6,
             )), 
           fluidRow( 
             HTML(
               paste('<br/>',
                     h3("Adolescent & Youth Population", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info0", inline = TRUE), downloadButton("downloadGraph", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'), '<br/>'
               )
             ),
             column(6,
                    plotOutput("graph", width = "75%", height = "150px")    
             ),
             column(6,
             )
           ),
           fluidRow(
             HTML(
               paste('<br/>',
                     h3("Key Life Events", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info0a", inline=TRUE), downloadButton("downloadGraph1", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'), '<br/>')
             ),
             column(6,
                    plotOutput("linegraph", width = "75%", height = "150px")
             ),
             column(6,
                    tableOutput("table")
             )
           ),
           fluidRow(
             HTML(
               paste('<br/>',
                     h3("Prevalence of Sexual Activity in the Last Month", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("infoActivity", inline=TRUE), '<br/>')
             ),
             
             column(6,  downloadButton("downloadGraph2", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoRecent"),# 
                    plotOutput("sex_activity_graph", width = "60%", height = "200px")
             ),
             column(6, downloadButton("downloadGraph3", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoNever"),#
                    plotOutput("never_sex_graph", width = "60%", height = "200px")
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>', '<br/>',
                     h3("Modern Contraceptive Prevalence", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("infoMCP", inline=TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph4", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoUnMarr"),#
                    plotOutput("mod_con", width = "60%", height = "200px")
             ),
             column(6,downloadButton("downloadGraph5", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoMarr"),#
                    plotOutput("mod_marr",  width = "60%", height = "200px")
             ),
             
           ),
           fluidRow(
             HTML(
               paste('<br/>', '<br/>',
                     h3("Traditional Contraceptive Prevalence", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("infoTCP", inline=TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph6", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoTUnmarr"),#
                    plotOutput("trad_unmarr", width = "60%", height = "200px")
             ),
             column(6, downloadButton("downloadGraph7", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoTMarr"),#
                    plotOutput("trad_marr", width = "60%", height = "200px")
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>', '<br/>',
                     h3("Unmet Need", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("infoUnmet", inline=TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph8", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoUUnMarr"),#
                    plotOutput("unmet_unmarr", width = "60%", height = "200px")
             ),
             column(6, downloadButton("downloadGraph9", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:left; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), #uiOutput("infoUMarr"),#
                    plotOutput("unmet_marr", width = "60%", height = "200px")
             ),
           ),
           fluidRow(
             HTML(paste('<br/>','<br/>',
                        h3("Condom Use at Last Sex", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("infoCondom", inline=TRUE), downloadButton("downloadGraph10", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'), '<br/>')
             ),
             column(6, 
                    plotOutput("con_use", width = "60%", height = "120px")
             ),
           ), 
           fluidRow(
             HTML(paste()
             ),
             column(12,
                    tags$hr(height = "2px"),
                    tags$h4("More Info"),
                    tags$p(""), "If you woud like to learn more, the A&Y dataset used to create this App can be found on the ",
                    tags$a(href = https://www.fp2030.org/data-hub/progress, "FP2030 Site"),
                    tags$p(""), "The code used to create this App can be found on our",
                    tags$a(href = https://github.com/familyplanning2020/FP2030AYAPP, "GitHub Account"),
                    tags$p(""), "This app only includes data for low and lower-middle income countries as classified by the World Bank." ,
             ),  
           )
  ),
  #builds Tab for Compare Page
  tabPanel("Compare",
           HTML(
             paste(
               h1("Compare"),
               h3("Select Up to 4 Countries to Compare its Adolescent and Youth Data"), '<br/>', '<br/>'
             )
           ),
           fluidRow(
             column(6,
                    wellPanel(
                      selectizeInput("country_compare",
                                     "Select Up to 4 Countries to Compare",
                                     choices = as.list(aypopdata.long$Country),
                                     multiple = TRUE,
                                     options = list(maxItems = 4))
                    )
                    
             ),
             column(6,
             )),
           fluidRow( 
             HTML(
               paste('<br/>',
                     h3("Adolescent & Youth Population", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info12", inline = TRUE), downloadButton("downloadGraph12", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'), '<br/>'
               )
             ),
             column(6,plotOutput("graph12", width = "75%", height = "150px")    
             ),
             column(6,
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>',
                     h3("Key Life Events", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info0a-compare", inline=TRUE), downloadButton("downloadGraph0acomp", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'), '<br/>')
             ),
             column(6,
                    plotOutput("linegraph2", width = "75%", height = "300px")
             ),
             column(6,
                    DT::dataTableOutput("table1")
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>',
                     h3("Prevalence of Sexual Activity in the Last Month", style = 'display:inline; margin: 0px 0px 10px 17px'),uiOutput("info13", inline = TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph13", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'),
                    plotOutput("graph13", width = "75%", height = "400px") 
             ),
             column(6, downloadButton("downloadGraph14", "Download Graph", style='display:block; height: 30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'),
                    plotOutput("graph14", width = "75%", height = "400px")
             ),
             column(6,
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>',
                     h3("Modern Contraceptive Prevalence", style='display:inline; margin: 0px 0px 10px 17px'),uiOutput("info14", inline = TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph15", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), 
                    plotOutput("graph15", width = "60%", height = "400px")
             ),
             column(6,downloadButton("downloadGraph16", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), 
                    plotOutput("graph16",  width = "60%", height = "400px")
             ),
             column(6,
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>', 
                     h3("Traditional Contraceptive Prevalence", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info15", inline = TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph17", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), 
                    plotOutput("graph17", width = "60%", height = "400px")
             ),
             column(6, downloadButton("downloadGraph18", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), 
                    plotOutput("graph18", width = "60%", height = "400px")
             ),
             column(6,
             ),
           ),
           fluidRow(
             HTML(
               paste('<br/>', 
                     h3("Unmet Need", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info16", inline=TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph19", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), 
                    plotOutput("graph19", width = "60%", height = "400px")
             ),
             column(6, downloadButton("downloadGraph20", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 10px 0px; font-size:90%'), 
                    plotOutput("graph20", width = "60%", height = "400px")
             ),
           ),
           fluidRow(
             HTML(paste('<br/>',
                        h3("Condom Use at Last Sex", style='display:inline; margin: 0px 0px 10px 17px'), uiOutput("info17", inline=TRUE), '<br/>')
             ),
             column(6, downloadButton("downloadGraph21", "Download Graph", style='display:block; height:30px; width:125px; color:#636b6f; align:center; padding:5px 4px 4px 4px; margin:20px 0px 0px 17px; font-size:90%'),
                    plotOutput("graph21", width = "60%", height = "300px")
             ), 
             column(6,
                    DT::dataTableOutput("table2")
             ),
           ),
           
           fluidRow(
             HTML(paste(h5("More Info"), '<br/>')
             ),
             column(12,
                    tags$h1(""), "If you woud like to learn more, the A&Y dataset used to create this App can be found on the ",
                    tags$a(href = https://www.fp2030.org/data-hub/progress, "FP2030 Site"),
                    tags$h2(""), "The code used to create this App can be found on our",
                    tags$a(href = https://github.com/familyplanning2020/FP2030AYAPP, "GitHub Account"),
                    tags$h3(""), "This app only includes data for low and lower-middle income countries as classified by the World Bank.",
             ),  
           )
           
  ),
  #builds Tab for Analyze Page
  tabPanel("Analyze",
           HTML(
             paste(
               h1("Analzye"),
               h3("Info"), '<br/>', '<br/>'
             )
           ),
           fluidRow(
             HTML(paste(h5("More Info"), '<br/>')
             ),
             column(12,
                    tags$h1(""), "If you woud like to learn more, the A&Y dataset used to create this App can be found on the ",
                    tags$a(href = https://www.fp2030.org/data-hub/progress, "FP2030 Site"),
                    tags$h2(""), "The code used to create this App can be found on our",
                    tags$a(href = https://github.com/familyplanning2020/FP2030AYAPP, "GitHub Account")
             ),  
             
           )
  )
)

# Draw Bargraphs and Figures
server <- function(input, output) {
  #add reactive for plots to change for each panel 
  
  #for graph reactive downloads 
  vals <- reactiveValues()
  
  output$instructions <- renderText("Some text")
  
  #Information Buttons 
  output$info0 <- renderUI({
    tags$span(
      popify(bsButton("info0", icon("info"),size = "extra-small"), 
             "Definition",
             "Total and Percentage of 10-14, 15-19, and 20-24 out of women aged 10-49. Women aged 15-49 are considered women of reproductive age. Blue represents 10-14, light green represents 15-19, and dark green represents 20-24."),
    )
  })
  
  output$info0a <- renderUI({
    tags$span(
      popify(bsButton("info0a", icon("info"), size = "extra-small"), 
             "Definition",
             "Order of key life events that indicate when most adolescents and young people first marry, engage in sex, give birth. Yellow represents first marriage, orange represents first sex, and purple represents first birth."),
    )
  })
  
  output$infoTUnmarr <- renderUI({
    tags$span(
      popify(bsButton("infoTUnMarr", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of unmarried sexually active women using a traditional contraceptive method. Light green represents 15-19 and dark green represents 20-24."),
    )
  })
  output$infoTMarr <- renderUI({
    tags$span(
      popify(bsButton("infoTMarr", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of married women using a traditional contraceptive method. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."),
    )
  })
  
  output$infoActivity <- renderUI({
    tags$span(
      popify(bsButton("infoActivity", icon("info"), size = "extra-small"), 
             "Definitions",
             "<strong>Sexually Active % :</strong> Percentage of women who were sexually activity in the four weeks preceding the survey. Light green represents 15-19 and dark green represents 20-24. <br/> <strong>Never Had Sex % :</strong> Percentage of women who never had intercourse. Light green represents 15-19 and dark green represents 20-24."
      ),
    )
  })
  
  output$infoMCP <- renderUI({
    tags$span(
      popify(bsButton("infoMCP", icon("info"), size = "extra-small"), 
             "Definitions",
             "<strong>Unmarried Sexually Active % :</strong> Percentage of unmarried sexually active women age 15-24 who reported using a modern contraceptive method. Light green represents 15-19 and dark green represents 20-24. <br/> <strong>Married % :</strong> Percentage of married women age 15-24 who reported using a modern contraceptive method. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."
      ),
    )
  })
  
  output$infoTCP <- renderUI({
    tags$span(
      popify(bsButton("infoTCP", icon("info"), size = "extra-small"), 
             "Definitions",
             "<strong>Unmarried Sexually Active % :</strong> Percentage of unmarried sexually active women using a traditional contraceptive method. Light green represents 15-19 and dark green represents 20-24. <br/> <strong>Married % :</strong> Percentage of married women using a traditional contraceptive method. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."
      ),
    )
  })
  
  output$infoNever <- renderUI({
    tags$span(
      popify(bsButton("infoNever", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of women who never had intercourse. Light green represents 15-19 and dark green represents 20-24."),
    )
  })
  output$infoRecent <- renderUI({
    tags$span(
      popify(bsButton("infoRecent", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of women who were sexually activity in the four weeks preceding the survey. Light green represents 15-19 and dark green represents 20-24."),
    )
  })
  output$infoUnMarr <- renderUI({
    tags$span(
      popify(bsButton("infoUnMarr", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of unmarried sexually active women age 15-24 who reported using a modern contraceptive method. Light green represents 15-19 and dark green represents 20-24."),
    )
  })
  output$infoMarr <- renderUI({
    tags$span(
      popify(bsButton("infoMarr", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of married women age 15-24 who reported using a modern contraceptive method. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."),
    )
  })
  output$infoCondom <- renderUI({
    tags$span(
      popify(bsButton("infoCondom", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of young women age 15-24 who reported using a condom at last sexual intercourse, of all young women who had sex with more than one partner in the 12 months preceding the survey. Pastel purple represents 15-24."),
    )
  })
  output$infoUnmet <- renderUI({
    tags$span(
      popify(bsButton("infoUnmet", icon("info"), size = "extra-small"), 
             "Definitions",
             "<strong>Unmarried Sexually Active % :</strong> Percentage of unmarried sexually active women with unmet need. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24. <br/> <strong>Married % :</strong> Percentage of married women with unmet need. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."
      ),
    )
  })
  output$infoUUnMarr <- renderUI({
    tags$span(
      popify(bsButton("infoUUnMarr", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of unmarried sexually active women with unmet need. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."),
    )
  })
  output$infoUMarr <- renderUI({
    tags$span(
      popify(bsButton("infoUMarr", icon("info"), size = "extra-small"), 
             "Definition",
             "Percentage of women women with unmet need. Light green represents 15-19, pastel purple represents 15-24, and dark green represents 20-24."),
    )
  })
  
  #Code for Information Buttons End
  
  
  #Start Code for Graphs  
  #New Plot: Population by Age Groups
  ay_res <- reactive({
    res <- aypopdata.long %>% filter(aypopdata.long$Country == input$country)
    req(nrow(res) > 0)
    res
  })
  
  output$graph <- renderPlot({
    bar_one <- (ggplot(ay_res(), aes(fill=Age_Group, y=Country, x=Count)) + 
                  geom_bar(position="stack", stat="identity") +
                  geom_text(aes(label=paste0(Count,"%", " ", "(", (round(Round_Total/1000000,1))," ", "Million", ")")), position=position_stack(vjust=0.5),colour = "black")+
                  theme_classic() +
                  scale_fill_manual(values = cbp2, labels = c("Young Adolescents (10-14)", "Older Adolescents (15-19)","Older Youth (20-24)"), name = "Age Group") +
                  theme(axis.line.y=element_blank(),
                        #axis.text.y=element_blank(),
                        axis.title.y=element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.y=element_blank(),
                        axis.text.x =element_blank(),
                        axis.ticks.x =element_blank(),
                        axis.line.x =element_blank(),
                        legend.position = "right") +
                  labs(caption="Source: UN Population Division 2022", size=7))
    
    vals$bar_one <- bar_one
    print(bar_one)
    
    
  })
  output$downloadGraph <- downloadHandler(
    filename = function() {
      paste("Adolescents and Youth", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$bar_one)
      dev.off()
    })
  
  
  ### END PLOT
  
  
  #Key Life Events Table
  kle_mar_res <- reactive({
    res3 <- kle_marriage %>% filter(kle_marriage$Country == input$country)
    req(nrow(res3) > 0)
    table1 <- matrix(c(res3[[1,3]], NA, res3[[1,4]], res3[[1,5]], NA, res3[[1,6]],  res3[[1,7]], NA, res3[[1,2]], res3[[1,2]]), ncol = 2, byrow = TRUE)
    colnames(table1) <- c("% Married", "% Married before 18 Years Old")
    rownames(table1) <- c( "15-19", "20-24", "25-29","15-24", "Source")
    table2 <- as.matrix(table1)
    `Age Group`=c( "15-19", "20-24", "25-29","15-24", "Source")
    table2=cbind(`Age Group`, table2)
    
    table2 
    
  })
  
  output$table <- renderTable(kle_mar_res(),hover =TRUE, bordered = TRUE, colnames= TRUE,digits = 1)
  
  #Recent Sexual Activity 
  ayfp_sex_res <- reactive({
    res4 <- ayfp %>% select(2,3,6,7)  %>% filter(ayfp$Country == input$country)
    req(nrow(res4) > 0)
    res4$"15-19" <- res4$"Recent sex older adolescents aged 15-19"
    res4$"20-24" <- res4$"Recent sex older youth aged 20-24"
    #res4 <- res4[,-1:-3]
    res4.long <- res4 %>% gather("Age.Group", "Percent","15-19" , "20-24") 
    res4.long
    res4.long %>% filter(res4.long$Percent>0)
    
  })
  
  output$sex_activity_graph <- renderPlot({
    source<- ayfp_sex_res()
    sex_act <- (ggplot(subset(ayfp_sex_res(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))+
      coord_flip() + theme_classic() + 
      geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      scale_fill_manual(values = cbp3, name = "Age Group") + 
      labs(title = "% Sexually Active (Last Month)") +  theme(axis.line.y=element_blank(),
                                                              axis.text.y=element_blank(),
                                                              axis.title.x=element_blank(),
                                                              axis.title.y=element_blank(),
                                                              axis.ticks.y=element_blank(),
                                                              axis.text.x =element_blank(),
                                                              axis.ticks.x =element_blank(),
                                                              axis.line.x =element_blank(),
                                                              legend.position = "bottom") + labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$sex_act<- sex_act
    print(sex_act)
    
  })
  output$downloadGraph2 <- downloadHandler(
    filename = function() {
      paste("Sexual Activity", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$sex_act)
      dev.off()
    })
  
  
  #END PLOT
  
  #Ever Sexual Activity 
  ayfp_never_res <- reactive({
    res5 <- ayfp %>% select(2,3,4,5) %>% filter(ayfp$Country == input$country)
    req(nrow(res5) > 0)
    ayfp_never_res
    res5$"15-19" <- res5$"Never have had sex older youth aged 15-19"
    res5$"20-24" <- res5$"Never have had sex older youth aged 20-24"
    res5.long <- res5 %>% gather("Age.Group", "Percent",  "15-19", "20-24")
    res5.long
    res5.long %>% filter(res5.long$Percent>0)
    
  })
  
  output$never_sex_graph <- renderPlot({
    source<- ayfp_never_res() 
    never_sex<- (ggplot(subset(ayfp_never_res(), `Percent`>0), aes(x= reorder(`Age.Group`,`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity")) +
      coord_flip() + theme_classic() + 
      geom_text(aes(label=`Percent`), color="black", size=3.5) + scale_fill_manual(values = cbp3, name = "Age Group") + 
      labs(title= "% Never Had Sex") + theme(axis.line.y=element_blank(),
                                             axis.text.y=element_blank(),
                                             axis.title.x=element_blank(),
                                             axis.title.y=element_blank(),
                                             axis.ticks.y=element_blank(),
                                             axis.text.x =element_blank(),
                                             axis.ticks.x =element_blank(),
                                             axis.line.x =element_blank(),
                                             legend.position = "bottom") +labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$never_sex<- never_sex
    print(never_sex)
    
  })
  output$downloadGraph3 <- downloadHandler(
    filename = function() {
      paste("Never Had Sex", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$never_sex)
      dev.off()
    })
  
  #END PLOT
  
  #NEW PLOT
  #timeline of key life events
  kle_age_res <- reactive({
    res2 <- kle_age %>% filter(kle_age$Country == input$country)
    req(nrow(res2) > 0)
    res2.long <- res2 %>% gather(Event, Age, `First Marriage`,`First Sex`, `First Birth`)
    res2.long$Event<- factor(res2.long$Event, levels = c("First Marriage", "First Sex", "First Birth"))
    res2.long
    
  })
  
  output$linegraph <- renderPlot({
    source<- kle_age_res() 
    #Create Plot
    timeline_plot<- ggplot(kle_age_res(), aes(x=Age, y=0.25, col=Event, label=""))+
      theme_classic() + 
      #labs(col="Events") + 
      geom_hline(yintercept = 0.25, color = '#000000', size = 0.25) +
      geom_point(aes(y=0.25), size=6) +
      geom_text(aes (x = Age, y = 0.65, label = Age), size = 3.5, color = "#000000",check_overlap = TRUE) +  
      scale_color_manual(values = cbp1) +
      scale_x_continuous(name="Median Age at Event (Among 25-29 Year Olds)", breaks=seq(15, 25, 2), labels=c("15", "17", "19", "21", "23", "25"), limits=c(15, 25)) + 
      scale_y_continuous(limits=c(0,1)) +
      theme(plot.background = element_rect(fill = '#f0f1f2'),
            panel.background = element_rect(fill = '#f0f1f2'),
            axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(size = 12, margin = margin(0, 0, 10, 0)),
            axis.line.x = element_line(size = 1, color = '#121212'),
            axis.text.x = element_text(size = 12, margin = margin(1, 0, 15, 0)),
            axis.ticks.x =element_line(),
            axis.ticks.length.x = unit(0.25, "cm"),
            legend.position = "top",
            legend.background = element_rect(fill = '#f0f1f2'),
            legend.text = element_text(size = 12),
            legend.title = element_blank()) + labs(caption=paste0("Source: ",source$Source), size=7)
    
    timeline_plot
    
    vals$timeline_plot<- timeline_plot
    print(timeline_plot)
    
    
  })
  output$downloadGraph1 <- downloadHandler(
    filename = function() {
      paste("Key Life Events", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$timeline_plot)
      dev.off()
    })
  
  
  
  #NEW PLOT
  # Modern Contraceptive Use: Unmarried Women
  
  ayfp_mod_res <- reactive({
    res <- ayfp %>% select(2,3,8, 9) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    res$`15-19` <- res$`MCPR for unmarried sexually active adolescents (15-19)**`
    res$`20-24` <- res$`MCPR for unmarried sexually active youth (20-24)**`
    res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24")
    res.long
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$mod_con <- renderPlot({
    source <- ayfp_mod_res()
    mcp_aw <- (ggplot(subset(ayfp_mod_res(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))+
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs( subtitle = "MCP(%) Among Unmarried Sexually Active") +  theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp3, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") +labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$mcp_aw <- mcp_aw
    print(mcp_aw)
    
    
  })
  output$downloadGraph4 <- downloadHandler(
    filename = function() {
      paste("MCP- UMSA", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$mcp_aw)
      dev.off()
    })
  
  #END PLOT
  
  # Modern Contraceptive Use: Married Women
  ayfp_mod_marr<- reactive({
    res <- ayfp %>% select(2,3,10, 11, 12) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    
    res$`15-19` <- res$`MCPR for married adolescents (15-19)`
    res$`20-24` <- res$`MCPR for married youth (20-24)`
    res$`15-24` <- res$`MCPR for married adolescent and youth (15-24)`
    
    res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24", "15-24")
    res.long
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$mod_marr <- renderPlot({
    source<- ayfp_mod_marr()
    mcp_mw <- (ggplot(subset(ayfp_mod_marr(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity")) +
      coord_flip() + theme_classic() + geom_text(aes(label= round(`Percent`,1)), color="black", size=3.5) + 
      labs(subtitle = "MCP(%) Among Married") +  theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") +
      labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$mcp_mw <- mcp_mw
    print(mcp_mw)
    
    
  })
  output$downloadGraph5 <- downloadHandler(
    filename = function() {
      paste("MCP- Married Women", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$mcp_mw)
      dev.off()
    })
  
  
  #END PLOT
  
  #Condom Use at Last Sex
  ayfp_con_res<- reactive({
    res <- ayfp %>% select(2,3,29) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    
    res$`15-24` <- res$`Condom use during last sex: 15-24 year olds`
    res.long <- res %>% gather("Age.Group", "Percent", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$con_use <- renderPlot({
    source <- ayfp_con_res()
    fig <- (ggplot(subset(ayfp_con_res(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))+
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "% Condom Use During Last Sex") +  theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp4, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") +labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$fig <- fig
    print(fig)
    
    
  })
  output$downloadGraph10 <- downloadHandler(
    filename = function() {
      paste("Condom Use at Last Sex", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$fig)
      dev.off()
    })
  
  #Traditional Method Use 
  #UMSA
  ayfp_trad_unmarr<- reactive({
    res <- ayfp %>% select(2,3,13,14) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    res$`15-19` <- res$`% of unmarried sexually active** older adolescents aged 15-19 using a traditional method`
    res$`20-24` <- res$`% of unmarried sexually active** older youth aged 20-24 using a traditional method`
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$trad_unmarr <- renderPlot({
    source <- ayfp_trad_unmarr()
    tcp_aw <- (ggplot(subset(ayfp_trad_unmarr(),`Percent`>0),aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity")) +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "TCP(%) Among Unmarried Sexually Active") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp3, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") +labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$tcp_aw <- tcp_aw
    print(tcp_aw)
    
    
  })
  output$downloadGraph6 <- downloadHandler(
    filename = function() {
      paste("TCP- UMSA", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$tcp_aw)
      dev.off()
    })
  
  #Married
  ayfp_trad_marr<- reactive({
    res <- ayfp %>% select(2,3,15,16,17) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    res$`15-19` <- res$`% of married older adolescents aged 15-19 using a traditional method`
    res$`20-24` <- res$`% of married older youth aged 20-24 using a traditional method`
    res$`15-24` <- res$`% of married youth aged 15-24 using a traditional method`
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$trad_marr <- renderPlot({
    source<- ayfp_trad_marr()
    tcp_mw <- (ggplot(subset(ayfp_trad_marr(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity")) +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "TCP(%) Among Married") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom")+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$tcp_mw <- tcp_mw
    print(tcp_mw)
    
    
  })
  output$downloadGraph7 <- downloadHandler(
    filename = function() {
      paste("TCP- Married Women", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$tcp_mw)
      dev.off()
    })
  
  #Unmet Need 
  #UMSA
  ayfp_unmet_unmarr<- reactive({
    res <- ayfp %>% select(2,3,19,20,21) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    res$`15-19` <- res$"Unmet need 15-19 UMSA"
    res$`20-24` <- res$"Unmet need 20-24 UMSA"
    res$`15-24` <- res$"Unmet need 15-24 UMSA"
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$unmet_unmarr <- renderPlot({
    source <- ayfp_unmet_unmarr()
    un_aw <- (ggplot(subset(ayfp_unmet_unmarr(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity")) +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "% Unmet Need Among Unmarried Sexually Active") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom")+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$un_aw <- un_aw
    print(un_aw)
    
    
  })
  output$downloadGraph8 <- downloadHandler(
    filename = function() {
      paste("Unmet Need - UMSA", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$un_aw)
      dev.off()
    })
  
  #Married
  ayfp_unmet_marr<- reactive({
    res <- ayfp %>% select(2,3,22,23,24) %>% filter(ayfp$Country == input$country)
    req(nrow(res) > 0)
    res$`15-19` <- res$"Unmet need 15-19 Married"
    res$`20-24` <- res$"Unmet need 20-24 Married"
    res$`15-24` <- res$"Unmet need 15-24 Married"
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$unmet_marr <- renderPlot({
    source <- ayfp_unmet_marr()
    un_mw <- (ggplot(subset(ayfp_unmet_marr(), `Percent`>0), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity")) +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "% Unmet Need Among Married") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + 
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom")+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$un_mw <- un_mw
    print(un_mw)
    
    
  })
  output$downloadGraph9 <- downloadHandler(
    filename = function() {
      paste("Unmet Need - Married Women", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$un_mw)
      dev.off()
    })
  
  #ALL THE GRAPHS BELOW ARE THE COMPARE GRAPHS 
  
  #AYPop Graph 
  ay_res_compare <- reactive({
    res <- aypopdata.long2 %>% filter(Country %in% input$country_compare) # we need to this
    req(nrow(res) > 0)
    res
  })
  
  
  output$graph12 <- renderPlot({
    bar_one_new <- (ggplot(ay_res_compare(), aes(fill=Age_Group, y=Country, x=Count)) + 
                      geom_bar(position="stack", stat="identity") +
                      geom_text(aes(label=paste0(Count,"%", " ", "(", (round(Round_Total/1000000,1))," ", "Million", ")")), position=position_stack(vjust=0.5),colour = "black")+
                      theme_classic() +
                      scale_fill_manual(values = cbp2, labels = c("Young Adolescents (10-14)", "Older Adolescents (15-19)","Older Youth (20-24)"), name = "Age Group") +
                      theme(axis.line.y=element_blank(),
                            #axis.text.y=element_blank(),
                            axis.title.y=element_blank(),
                            axis.title.x = element_blank(),
                            axis.ticks.y=element_blank(),
                            axis.text.x =element_blank(),
                            axis.ticks.x =element_blank(),
                            axis.line.x =element_blank(),
                            legend.position = "right") +
                      labs(caption="Source: UN Population Division 2022", size=7))
    
    vals$bar_one_new <- bar_one_new
    print(bar_one_new)
    
    
  })
  output$downloadGraph12 <- downloadHandler(
    filename = function() {
      paste("Adolescents and Youth_Compare", "png", sep = ".")
    },
    content = function(file) {
      png(file, width = 980, height = 400)
      print(vals$bar_one_new)
      dev.off()
    })
  
  
  #Recent Sexual Activity 
  ayfp_sex_res_compare <- reactive({
    res4 <- ayfp_compare %>% select(2,3,6,7)  %>% filter(Country %in% input$country_compare)
    req(nrow(res4) > 0)
    res4$"15-19" <- res4$"Recent sex older adolescents aged 15-19"
    res4$"20-24" <- res4$"Recent sex older youth aged 20-24"
    #res4 <- res4[,-1:-3]
    res4.long <- res4 %>% gather("Age.Group", "Percent","15-19" , "20-24") 
    res4.long
    res4.long %>% filter(res4.long$Percent>0)
    
  })
  
  output$graph13 <- renderPlot({
    source<- ayfp_sex_res_compare()
    sex_act_compare <- (ggplot(ayfp_sex_res_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity")+
      theme_classic() + coord_flip() +
      geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      scale_fill_manual(values = cbp3, name = "Age Group") + 
      labs(title = "% Sexually Active (Last Month)") +  
      facet_grid(vars(Country)) +theme(axis.line.y=element_blank(), 
                                       axis.text.y=element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.title.y=element_blank(),
                                       axis.ticks.y=element_blank(),
                                       axis.text.x =element_blank(),
                                       axis.ticks.x =element_blank(),
                                       axis.line.x =element_blank(),
                                       legend.position = "bottom") #+ labs(caption=paste0("Source: ",source$Source), size=7) 
    
    
    
    vals$sex_act_compare <- sex_act_compare
    print(sex_act_compare)
    
    
  })
  output$downloadGraph13 <- downloadHandler(
    filename = function() {
      paste("Sexual Activity Compare", "png", sep = ".")
    },
    content = function(file) {
      png(file, width = 980, height = 400)
      print(vals$sex_act_compare)
      dev.off()
    })
  
  #Ever Sexual Activity -- COMPARE
  ayfp_never_res_compare <- reactive({
    res5 <- ayfp_compare %>% select(2,3,4,5) %>% filter(Country %in% input$country_compare)
    req(nrow(res5) > 0)
    ayfp_never_res
    res5$"15-19" <- res5$"Never have had sex older youth aged 15-19"
    res5$"20-24" <- res5$"Never have had sex older youth aged 20-24"
    res5.long <- res5 %>% gather("Age.Group", "Percent",  "15-19", "20-24")
    res5.long
    res5.long %>% filter(res5.long$Percent>0)
    
  })
  
  output$graph14 <- renderPlot({
    source<- ayfp_never_res_compare() 
    never_sex_compare<- (ggplot(ayfp_never_res_compare(), aes(x= reorder(`Age.Group`,`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity") +
      coord_flip() + theme_classic() + 
      geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      scale_fill_manual(values = cbp3, name = "Age Group") + 
      labs(title= "% Never Had Sex") +
      facet_grid(vars(Country)) + theme(axis.line.y=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.title.y=element_blank(),
                                        axis.ticks.y=element_blank(),
                                        axis.text.x =element_blank(),
                                        axis.ticks.x =element_blank(),
                                        axis.line.x =element_blank(),
                                        legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$never_sex_compare<- never_sex_compare
    print(never_sex_compare)
    
  })
  output$downloadGraph14 <- downloadHandler(
    filename = function() {
      paste("Never Had Sex Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$never_sex_compare)
      dev.off()
    })
  
  #MCP UMSA -- COMPARE
  ayfp_mod_res_compare <- reactive({
    res <- ayfp_compare %>% select(2,3,8, 9) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    res$`15-19` <- res$`MCPR for unmarried sexually active adolescents (15-19)**`
    res$`20-24` <- res$`MCPR for unmarried sexually active youth (20-24)**`
    res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24")
    res.long
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph15 <- renderPlot({
    source <- ayfp_mod_res_compare()
    mcp_aw_compare <- (ggplot(ayfp_mod_res_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity")+
      coord_flip() + theme_classic() + 
      geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs( subtitle = "MCP(%) Among Unmarried Sexually Active") +  theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp3, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$mcp_aw_compare <- mcp_aw_compare
    print(mcp_aw_compare)
    
  })
  output$downloadGraph15 <- downloadHandler(
    filename = function() {
      paste("MCP- UMSA Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$mcp_aw_compare)
      dev.off()
    })
  #END PLOT
  
  #MCP Married -- COMPARE
  ayfp_mod_marr_compare<- reactive({
    res <- ayfp_compare %>% select(2,3,10, 11, 12) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    
    res$`15-19` <- res$`MCPR for married adolescents (15-19)`
    res$`20-24` <- res$`MCPR for married youth (20-24)`
    res$`15-24` <- res$`MCPR for married adolescent and youth (15-24)`
    
    res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24", "15-24")
    res.long
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph16 <- renderPlot({
    source<- ayfp_mod_marr_compare()
    mcp_mw_compare <- (ggplot(ayfp_mod_marr_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity") +
      coord_flip() + theme_classic() + geom_text(aes(label= round(`Percent`,1)), color="black", size=3.5) + 
      labs(subtitle = "MCP(%) Among Married") +  theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") +
      labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$mcp_mw_compare <- mcp_mw_compare
    print(mcp_mw_compare)
    
    
  })
  output$downloadGraph16 <- downloadHandler(
    filename = function() {
      paste("MCP- Married Women Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$mcp_mw_compare)
      dev.off()
    })
  #END PLOT
  
  #Traditional Contraceptive Prevalence UMSA -- COMPARE
  ayfp_trad_unmarr_compare<- reactive({
    res <- ayfp_compare %>% select(2,3,13,14) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    res$`15-19` <- res$`% of unmarried sexually active** older adolescents aged 15-19 using a traditional method`
    res$`20-24` <- res$`% of unmarried sexually active** older youth aged 20-24 using a traditional method`
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph17 <- renderPlot({
    source <- ayfp_trad_unmarr_compare()
    tcp_aw_compare <- (ggplot(ayfp_trad_unmarr_compare(),aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity") +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "TCP(%) Among Unmarried Sexually Active") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp3, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$tcp_aw_compare <- tcp_aw_compare
    print(tcp_aw_compare)
    
    
  })
  output$downloadGraph17 <- downloadHandler(
    filename = function() {
      paste("TCP- UMSA Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$tcp_aw_compare)
      dev.off()
    })
  #END PLOT
  
  #Traditional Contraceptive Prevalence Married -- COMPARE
  ayfp_trad_marr_compare<- reactive({
    res <- ayfp_compare %>% select(2,3,15,16,17) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    res$`15-19` <- res$`% of married older adolescents aged 15-19 using a traditional method`
    res$`20-24` <- res$`% of married older youth aged 20-24 using a traditional method`
    res$`15-24` <- res$`% of married youth aged 15-24 using a traditional method`
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph18 <- renderPlot({
    source<- ayfp_trad_marr_compare()
    tcp_mw_compare <- (ggplot(ayfp_trad_marr_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity") +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "TCP(%) Among Married") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$tcp_mw_compare <- tcp_mw_compare
    print(tcp_mw_compare)
    
    
  })
  output$downloadGraph18 <- downloadHandler(
    filename = function() {
      paste("TCP- Married Women Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$tcp_mw_compare)
      dev.off()
    })
  #END PLOT
  
  #Unmet Need UMSA -- COMPARE
  ayfp_unmet_unmarr_compare<- reactive({
    res <- ayfp_compare %>% select(2,3,19,20,21) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    res$`15-19` <- res$"Unmet need 15-19 UMSA"
    res$`20-24` <- res$"Unmet need 20-24 UMSA"
    res$`15-24` <- res$"Unmet need 15-24 UMSA"
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph19 <- renderPlot({
    source <- ayfp_unmet_unmarr_compare()
    un_aw_compare <- (ggplot(ayfp_unmet_unmarr_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity") +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "% Unmet Need Among Unmarried Sexually Active") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$un_aw_compare <- un_aw_compare
    print(un_aw_compare)
    
    
  })
  output$downloadGraph19 <- downloadHandler(
    filename = function() {
      paste("Unmet Need - UMSA Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$un_aw_compare)
      dev.off()
    })
  #END PLOT
  
  #Unmet Need Married -- COMPARE
  ayfp_unmet_marr_compare<- reactive({
    res <- ayfp_compare %>% select(2,3,22,23,24) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    res$`15-19` <- res$"Unmet need 15-19 Married"
    res$`20-24` <- res$"Unmet need 20-24 Married"
    res$`15-24` <- res$"Unmet need 15-24 Married"
    res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph20 <- renderPlot({
    source <- ayfp_unmet_marr_compare()
    un_mw_compare <- (ggplot(ayfp_unmet_marr_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity") +
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "% Unmet Need Among Married") + theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp5, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$un_mw_compare <- un_mw_compare
    print(un_mw_compare)
    
    
  })
  output$downloadGraph20 <- downloadHandler(
    filename = function() {
      paste("Unmet Need - Married Women Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$un_mw_compare)
      dev.off()
    })
  #END PLOT
  
  #Condom Use -- COMPARE
  ayfp_con_res_compare<- reactive({
    res <- ayfp_compare %>% select(2,3,29) %>% filter(Country %in% input$country_compare)
    req(nrow(res) > 0)
    
    res$`15-24` <- res$`Condom use during last sex: 15-24 year olds`
    res.long <- res %>% gather("Age.Group", "Percent", "15-24")
    res.long %>% filter(res.long$Percent>0)
    
  })
  
  output$graph21 <- renderPlot({
    source <- ayfp_con_res_compare()
    fig_compare <- (ggplot(ayfp_con_res_compare(), aes(x= reorder(`Age.Group`, -`Percent`), y = `Percent`, fill = `Age.Group`))) + 
      geom_bar(stat = "identity")+
      coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
      labs(subtitle = "% Condom Use During Last Sex") +  theme(plot.subtitle=element_text(size=13, color="black")) +
      scale_fill_manual(values = cbp4, name = "Age Group") + facet_grid(vars(Country))+
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_blank(),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "bottom") #+labs(caption=paste0("Source: ",source$Source), size=7)
    
    vals$fig_compare <- fig_compare
    print(fig_compare)
    
    
  })
  output$downloadGraph21 <- downloadHandler(
    filename = function() {
      paste("Condom Use at Last Sex Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$fig_compare)
      dev.off()
    })
  #END PLOT
  
  #timeline of key life events COMPARE
  kle_age_res_compare <- reactive({
    res2 <- kle_age_compare %>% filter(Country %in% input$country_compare)
    req(nrow(res2) > 0)
    res2.long <- res2 %>% gather(Event, Age, `First Marriage`,`First Sex`, `First Birth`)
    res2.long$Event<- factor(res2.long$Event, levels = c("First Marriage", "First Sex", "First Birth"))
    res2.long
    
  })
  
  output$linegraph2 <- renderPlot({
    source<- kle_age_res_compare() 
    #Create Plot
    timeline_plot2<- ggplot(kle_age_res_compare(), aes(x=Age, y=0.25, col=Event, label=""))+
      theme_classic() + 
      #labs(col="Events") + 
      geom_hline(yintercept = 0.25, color = '#000000', size = 0.25) +
      geom_point(aes(y=0.25), size=6) +
      geom_text(aes (x = Age, y = 0.65, label = Age), size = 3.5, color = "#000000",check_overlap = TRUE) +  
      scale_color_manual(values = cbp1) +
      scale_x_continuous(name="Median Age at Event (Among 25-29 Year Olds)", breaks=seq(15, 25, 2), labels=c("15", "17", "19", "21", "23", "25"), limits=c(15, 25)) + 
      scale_y_continuous(limits=c(0,1)) +
      facet_grid(Country~.)+
      theme(plot.background = element_rect(fill = '#f0f1f2'),
            panel.background = element_rect(fill = '#f0f1f2'),
            axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(size = 12, margin = margin(0, 0, 10, 0)),
            axis.line.x = element_line(size = 1, color = '#121212'),
            axis.text.x = element_text(size = 12, margin = margin(1, 0, 15, 0)),
            axis.ticks.x =element_line(),
            axis.ticks.length.x = unit(0.25, "cm"),
            legend.position = "top",
            legend.background = element_rect(fill = '#f0f1f2'),
            legend.text = element_text(size = 12),
            legend.title = element_blank())
    
    #NEED TO ADD SOURCE
    
    timeline_plot2
    
    vals$timeline_plot2<- timeline_plot2
    print(timeline_plot2)
    
    
  })
  output$downloadGraph0acomp <- downloadHandler(
    filename = function() {
      paste("Key Life Events Compare", "png", sep = ".")
    }, 
    content = function(file) {
      png(file, width = 980, height = 400) 
      print(vals$timeline_plot2)
      dev.off()
    })
  #END PLOT
  
  #Key Life Events Table - Compare Page 
  kle_mar_res_compare<- reactive({
    res4 <- kle_marriage_compare %>% filter(Country %in% input$country_compare)
    as.data.frame(res4)
    
    
  })
  
  output$table1 = renderDataTable({
    DT::datatable(kle_mar_res_compare())
  })
  
  ##  
  source_table<- reactive({
    res5 <- kle_marriage_compare%>% select(1,2)  %>% filter(Country %in% input$country_compare)
    as.data.frame(res5)
    
    
  })
  
  output$table2 = renderDataTable({
    DT::datatable(source_table(), caption = "Sources for data under Key Life Events; Prevalence of Sexual Activity;Contraceptive Use; 
                  Unmet Need; and Condom Use at Last Sex")
    
  })
  
  
  ##
  
}


# Run the application 
shinyApp(ui = ui, server = server)



