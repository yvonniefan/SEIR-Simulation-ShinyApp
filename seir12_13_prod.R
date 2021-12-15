
library(shiny)
library(tidyverse)

# this plot has one plotly graph

##########################################################################
# school numbers 
# using fall 2020 student-to-faculty ratios
# all data from Common Data Sets (CDS)
# full-time equivalent students
# full-time instructional faculty (no TAs)

# brown: #https://oir.brown.edu/sites/g/files/dprerj381/files/2020-04/CDS_2020_2021_Final2_0.pdf
brownstudents= 6610 

brownfaculty= 1056 

brown <- brownstudents + brownfaculty

# harvard: https://oir.harvard.edu/files/huoir/files/harvard_cds_2020-2021.pdf
harvardstudents= 5186

harvardfaculty= 973 

harvard <- harvardstudents + harvardfaculty  

# yale: https://oir.yale.edu/sites/default/files/cds_2020-2021_yale_vf_030521.pdf
yalestudents= 4698

yalefaculty= 1102

yale <- yalestudents + yalefaculty

#upenn: https://ira.upenn.edu/sites/default/files/UPenn-Common-Data-Set-2020-21.pdf
upennstudents= 9752

upennfaculty= 1686

upenn <- upennstudents + upennfaculty

#cornell: http://irp.dpb.cornell.edu/wp-content/uploads/2021/06/CDS_2020-2021_FINAL.pdf
corstudents= 14708

corfaculty= 1688

cornell <- corstudents + corfaculty
# ------ simulation ---------
# source('~/Downloads/PHP2560R/simulation_YZ.R')
source("simulation_part.R")

# -------- Shiny App ---------
library(plotly)

ui <- fluidPage(
  
  # Application title
  titlePanel("Simulation-Based What-if Analysis of COVID-19 Spread in Universities"),
  h4("Kara Rofe, Talia Seshaiah, Yifan Zhao"),
  p("This app displays a model study for the transmission of COVID-19 among both students and faculty.
    This shows a single run of the model using default parameters. Users can change 
    the model to better fit university characteristics. Users can also select an Ivy League
    school to set the arrival rate of that specific university."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("seir"),
      sliderInput("cmax", "Constant contact rate:",
                  min=0,
                  max=40,
                  value=13), 
      sliderInput("i_n", "Infection probability:", 
                  min=0, 
                  max=1, 
                  value=0.037
      ), 
      sliderInput("h", "Student Sensitivity to COVID:", 
                  min=0, 
                  max=1000, 
                  value=100
      ), 
      radioButtons("phi", "University Closure Decision", 
                   choices = c(0,1) ,
                   selected = 1
      ),
      # select ivy school
      selectInput("ivy", "Choose a school:",
                  choices= c("Brown University",
                             "Harvard University",
                             "Yale University",
                             "University of Pennsylvania",
                             "Cornell University", 
                             "None of the above"),
                  selected = "None of the above"
      ),
      #description of variables
      actionButton("variables", "Information about Inputs"),
      #new: bulleted list https://stackoverflow.com/questions/22923784/how-to-add-bullet-points-in-r-shinys-rendertext/22930719
      uiOutput("myList")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Daily cases", plotlyOutput("distPlot"), textOutput('text'), textOutput('contact')),
                  tabPanel("Cumulative cases", plotlyOutput("distPlotcum"), textOutput('text2'), textOutput('contact2')),
                  tabPanel("SEIR Curve", plotlyOutput("seir_curve"), textOutput('textseir')),
                  tabPanel("Behaviour and Policy Analysis", plotOutput("analysis"),
                          radioButtons("graph", "Choose Statistic:", choices = c("Daily Average", "Daily Maximum")),
                          textOutput('textanalysis')),
                  
                  tabPanel("Summary", verbatimTextOutput("summary"), 
                           p("C is contact rate. This is related to 
                             student sensitivity to COVID-19 and rolling average of COVID-19 cases."))),
      # turn off error messages while app is loading
      tags$style(type= "text/css",
                 ".shiny-output-error{visibility:hidden;}",
                 ".shiny-output-error:before{visibility:hidden;}")
                           )
  )
)

# Define server logic 
server <- function(input, output) {
  
  record_reac <- reactive({ 
    simfunc(A = input$students,C_max = input$cmax, i_N =input$i_n, phi=as.numeric(input$phi), h=input$h)
  })
  cum_reac <- reactive({ 
    func_getcum(record_reac())
  })
  output$seir <- renderUI({
    if (input$ivy=="None of the above"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = 100)
    }
    else if (input$ivy=="Brown University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(brown/14))
    }
    else if (input$ivy=="Harvard University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(harvard/14))
    }
    else if (input$ivy=="Yale University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(yale/14))
    }
    else if (input$ivy=="University of Pennsylvania"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(upenn/14))
    }
    else if (input$ivy=="Cornell University"){
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = round(cornell/14))
    }
  })
  
  #variable information 
  variableInfo <- observeEvent(input$variables, {
    output$myList <- renderUI(HTML("<ul>
                                   <li> <b> Arrival rate of students</b>: students per day</li>
                                   <li> <b> Constant contact rate </b>: Maximum contact (person/day) possible </li> 
                                   <li> <b> Infection probability </b>: The affect rate of transmission/infection in the absence of a mask (person/day) </li>
                                   <li> <b>Student Sensitivity to COVID</b>: Student's sensitivity to the disease and the number of cases at the university.
                                   This affects the contact rate (e.g 0 = student has no sensitivity, contact rate is kept at the maximum) </li>
                                   <li> <b> University Closure Decision</b>: 0 (university open), 1 (university decided to close) </li>
                                   </ul>"))
  })
  
  # plot
  colors <- c("Susceptible" = "black",
              "Exposed" = "yellow", 
              "Infected" = "red", 
              "Recovered" = "green")
  colors2 <- c("Exposed" = "yellow", 
               "Infected" = "red")
  linetypes <- c('C'=3)
  
  #daily cases plot
  output$distPlot <- renderPlotly({
    ggplot(data=record_reac()) +
      # geom_line(aes(x=days, y=susceptible, color="Susceptible")) +
      geom_line(aes(x=days, y=exposed, color="Exposed")) +
      geom_line(aes(x=days, y=infected, color="Infected")) +
      geom_line(aes(x=days, y=C), linetype=3) +
      # geom_line(aes(x=days, y=C), linetype=3) +
      # geom_line(aes(x=days, y=rollavg), linetype=2) +
      labs(y="Daily cases", x= "Days") +
      scale_color_manual(name='Status', values = colors2) +
      # annotate("text", x = 90, y = 20, label = 'Dotted line: Contact Rate', hjust = 0, size = 2) +
      ggtitle("Daily cases of COVID-19")
  })
  
  # plot that shows cumulative cases
  output$distPlotcum <- renderPlotly({
    ggplot(data=cum_reac()) +
      geom_line(aes(x=days, y=cum_E, color="Exposed"))+
      geom_line(aes(x=days, y=cum_I, color="Infected"))+
      labs(y="Cumulative Cases", x= "Days")+
      scale_color_manual(name='Status', values = c( "Exposed" = "yellow", 
                                                    "Infected" = "red"))+
      ggtitle("Cumulative cases of COVID-19")
  })
  # plot that shows all SEIR curves
  output$seir_curve <- renderPlotly({
    ggplot(data=cum_reac()) +
      geom_line(aes(x=days, y=susceptible, color="Susceptible")) +
      geom_line(aes(x=days, y=exposed, color="Exposed")) +
      geom_line(aes(x=days, y=infected, color="Infected")) +
      geom_line(aes(x=days, y=recovered, color="Recovered"))+
      labs(y="Daily Number", x= "Days") +
      scale_color_manual(values = colors) +
      ggtitle("SEIR Model")
  })
  
  output$summary <- renderPrint({
    summary(simfunc())
  })

  output$contact <- renderText({
    paste0('Minimum contact rate: ', round(min(record_reac()$C),1),
           '  Max contact rate: ', round(max(record_reac()$C),1), 
           '  Average contact rate: ', round(mean(record_reac()$C),1))
  })
  output$contact2 <- renderText({
    paste0('Minimum contact rate: ', round(min(record_reac()$C),1), 
           '  Max contact rate: ', round(max(record_reac()$C),1), 
           '  Average contact rate: ', round(mean(record_reac()$C),1))
  })
  
  # Add description of plots  
  output$text <- renderText({
    "Dotted line shows contact rate change over time, which depends on students sensitivity to daily COVID cases. Solid lines represent daily cases with status exposed and infected"
  })
  output$text2 <- renderText({
    "Dotted line shows contact rate change over time, which depends on students sensitivity to daily COVID cases. Solid lines represent daily cases with status exposed and infected"
  })
  output$textseir <- renderText({
    "SEIR curve under the parameters chosen. Note when sensitivity is set to 0, the graph looks like a typical SEIR curve."
  })
  output$textanalysis <- renderText({
    "Daily cases is negatively related to student sensitivity. In addition, when university conducts closure policy (green line), we would observe lower cases in both infected and exposed"
  })
  
  # Behaviour ana Policy analysis:
  sensi <- seq(1,20)*50 # initilize a list of sensitivities
  policy1 <- analysis_behaviour(1, sensi)
  policy1$policy = 1
  policy0 <- analysis_behaviour(0, sensi)
  policy0$policy = 0
  policy2 <- rbind(policy0,policy1)
  
  plotmean <- reactive({
    policy2 %>% filter(stat=='mean') %>%
      ggplot() +
      geom_line(aes(x=sensi, y=value, color=closure, linetype=status)) +
      labs(x='Student Sensitivity', y='Daily Values') +
      ggtitle('Daily Average by Student Sensitivity and University Closure Decision')
  })
  plotmax <- reactive({
    policy2 %>% filter(stat=='max') %>%
      ggplot() +
      geom_line(aes(x=sensi, y=value, color=closure, linetype=status)) + 
      labs(x='Student Sensitivity', y='Daily Values') +
      ggtitle('Daily Maximum by Student Sensitivity and University Closure Decision')
  })
  
  graphInput <- reactive({
    switch(input$graph,
           "Daily Average" = plotmean(),
           "Daily Maximum" = plotmax()
    )
  })
  output$analysis <- renderPlot({ 
    graphInput()
  })
  
  

  }


# Run the application 
shinyApp(ui = ui, server = server)
