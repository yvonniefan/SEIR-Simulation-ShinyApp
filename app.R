library(shiny)
library(tidyverse)
library(docstring)


#######################################################################
############################## Simulation #############################
#######################################################################
# source("simulation_part.R")

# This is the simulation/parameter code:
# ------ Simulation function --------

simfunc <- function(A = 1786, tao_arrival = 14, tao_1=6, tao_2=10,mu=0.66, y=1, C_max=13, i_N=0.037,
                    E0=10,I0=1,R0=0, len=120, phi = 1, h = 100) {
  #' SEIR Simulation 
  #' 
  #' Generate SEIR simulation according to specified parameters
  #' @param A numeric, rate of incoming students, default 1786
  #' @param tao_arrival numeric, Arrival duration, default 14
  #' @param tao_1 numeric,  exposure period: average time to move from early to late stage of the disease, default 6
  #' @param tao_2 numeric,  infection post-exposure period, default 10
  #' @param mu numeric,  relative infectivity of exposed to undocumented cases, default 0.66
  #' @param  y numeric,  scaler, default 1
  #' @param C_max numeric, constant contact rate absent any exogenous or endogenous changes in social behaviour, default 13
  #' @param i_N numeric, infection probablity in the absense of mask, default 0.037
  #' @param E0 numeric, initial exposure, default 10
  #' @param I0 numeric, initial infected, default 1
  #' @param R0 numeric, initial recovered, default 0
  #' @param len numeric, Number of days simulated, default 120
  #' @param phi numeric, 1 or 0, university closure decision, 0 open, 1 closed, default 1
  #' @param h numeric, coefficient representing the sensitivity of a college to the number of daily cases; h=0 means students don't care at all about COVID transmission and keep at maximum contact rate, default 100
  #' @return A dataframe that stores: daily number of cases of S, E, I, R status, 14-day rolling average of infected, contact rates, multiplier for changing contact rates
  
  # Initializations:
  alpha = 0.2 * y # relative infectivity of documented to undocumented cases
  r_recover=1/tao_2 # recover rate
  S0 = A*tao_arrival # initial Susceptible
  N = A*tao_arrival  # initial population, same as initial susceptible
  T = 14 # days; average time to evacuate the college town after school closure
  
  record = data.frame(matrix(0,nrow=len, ncol=4))
  colnames(record) <- c('susceptible', 'exposed', 'infected','recovered')
  record[1,'C'] = C_max # Initial contact rate is the maximum contact rate 
  
  L_S = phi*S0/T # Leaving rate of students with status susceptible
  L_E = phi*E0/T # Leaving rate of students with status exposed
  L_I = phi*I0/T # Leaving rate of students with status infected
  L_R = phi*R0/T # Leaving rate of students with status recovered
  A_S = 0.97*A # daily arrival rate of students with status susceptible
  A_E = 0.003*A # daily arrival rate of students with status exposed
  
  for (i in 1:len){
    S = S0
    E = E0
    I = I0
    R = R0
    # Transmission dynamics:
    S0 = max(0, S - i_N*record[max(i-1, 1), 'C'] *S/N * (mu*E + alpha*I) + A_S - L_S) # equation 3
    E0 = max(0, E + i_N*record[max(i-1, 1), 'C'] *S/N * (mu*E + alpha*I) + A_E - E/tao_1 - L_E) # equation 4
    I0 = max(0, I + E/tao_1 - I0/tao_2 - L_I) # equation 6
    R0 = max(0, R + I0/tao_2 - L_R) # equation 8
    
    record[i,1] = S0
    record[i,2] = E0
    record[i,3] = I0
    record[i,4] = R0
    # 7 days rolling average:
    record[i,'rollavg'] = mean(record[max((i-14),1) :i,3], na.rm=TRUE)
    # W_N: factor that affects contact rate. Students tend to change behavior according to current covid cases
    record[i, 'W_N'] = exp(-h * ((record[i,'rollavg'])/N))
    record[i, 'C'] = C_max * record[i,'W_N']
  }
  record$days = c(1:len)
  return(record)
}

docstring(simfunc)

# Get cumulative record: you should use simfunc() as data when calling this function:
func_getcum <- function(data){
  #' Generate Cumulative Record
  #' 
  #' Generate a cumulative record table based on daily cases table
  #' @param data numeric, dataframe that records daily cases of S, E, I, R status
  #' @return A dataframe that calculates cumulative number of exposed, infected and recovered cases
  cumulative <- data %>% mutate(cum_E = cumsum(exposed),
                                cum_I = cumsum(infected), 
                                cum_R = cumsum(recovered))
  return(cumulative)
}
docstring(func_getcum)


# Below are just tests for generating graphs. The actual graphs in the ShinyApp are created in the ShinyApp server.
ourrecord <- simfunc(A=100)
cumulative <- func_getcum(ourrecord)

# Explore relationship between student sensitivity, university closure policy and infected:
analysis_behaviour <- function(closure, sensitivity) {
  #' Analyze behaviour and COVID cases
  #' 
  #' Analyze relationship between student sensitivity, university closure policy and COIVD cases. 
  #' The data is based on simulation run in simfunc() with default parameters value, except phi and h
  #' @param closure numeric, university closre policy, 1 means university closed, 0 means university open
  #' @param sensitivity numeric vector, students' sensitivity to daily COIVD cases
  #' @return A dataframe that records statistics (mean, maximum, and standard deviation) of daily cases under different policy and students' sensitivities
  stats = data.frame(matrix(nrow = length(sensi), ncol=6))
  for (i in 1:length(sensi)){
    runsim <- simfunc(phi=closure, h=sensitivity[i])
    stats[i,1] <- mean(runsim$infected)
    stats[i,2] <- max(runsim$infected)
    stats[i,3] <- sd(runsim$infected)
    stats[i,4] <- mean(runsim$exposed)
    stats[i,5] <- max(runsim$exposed)
    stats[i,6] <- sd(runsim$exposed)
  }
  colnames(stats) = c('meaninfect','maxinfect','sdinfect','meanexpose','maxexpose','sdexpose')
  stats$sensi <- sensitivity
  stats$closure <- as.factor(closure)
  stats <- stats %>% 
    pivot_longer(cols=c('meaninfect','maxinfect','sdinfect','meanexpose','maxexpose','sdexpose'), 
                 names_to='statistic',
                 values_to='value') %>%
    mutate(status = substr(statistic,nchar(statistic) - 6+1, nchar(statistic)),
           stat = substr(statistic, 1, nchar(statistic) -6) )
  return(stats)
}

docstring(analysis_behaviour)
sensi <- seq(1,20)*50 # initilize a list of sensitivities



#######################################################################
############################## Shiny App ##############################
#######################################################################
# this plot has one plotly graph


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
                             student sensitivity to COVID-19 and rolling average of COVID-19 cases.
                             W_N is factors affecting contact rate. Students tend to change behavior according to
                             COVID-19 cases."))),
      # turn off error messages while app is loading
      tags$style(type= "text/css",
                 ".shiny-output-error{visibility:hidden;}",
                 ".shiny-output-error:before{visibility:hidden;}")
                           )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # Generate reactive daily case dataframes according to users' input from sidebar
  record_reac <- reactive({ 
    simfunc(A = input$students,C_max = input$cmax, i_N =input$i_n, phi=as.numeric(input$phi), h=input$h)
  })
  # Generate reactive cumulative cases based on daily case table above
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
  # Colors for SEIR graph:
  colors <- c("Susceptible" = "black",
              "Exposed" = "yellow", 
              "Infected" = "red", 
              "Recovered" = "green")
  
  # Colors for daily/cumulative graph:
  colors2 <- c("Exposed" = "yellow", 
               "Infected" = "red")
  linetypes <- c('C'=3)
  
  # Daily cases plot
  output$distPlot <- renderPlotly({
    ggplot(data=record_reac()) +
      geom_line(aes(x=days, y=exposed, color="Exposed")) +
      geom_line(aes(x=days, y=infected, color="Infected")) +
      geom_line(aes(x=days, y=C), linetype=3) +
      labs(y="Daily cases", x= "Days") +
      scale_color_manual(name='Status', values = colors2) +
      ggtitle("Daily cases of COVID-19")
  })
  
  # Cumulative cases plot
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
  
  # Summary tab:
  output$summary <- renderPrint({
    summary(simfunc())
  })
  
  # Text that shows min/max/avg contact rate:
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
  
  # Behaviour and Policy analysis:
  sensi <- seq(1,20)*50 # initilize a list of sensitivities
  # Table policy1: Number of infected/exposed under different student sensitivity, with university closure policy
  policy1 <- analysis_behaviour(1, sensi)
  policy1$policy = 1
  # Table policy0: Number of infected/exposed under different student sensitivity, without university closure policy
  policy0 <- analysis_behaviour(0, sensi)
  policy0$policy = 0
  # Combine the two tables above
  policy2 <- rbind(policy0,policy1)
  
  # Plot that shows daily average under different behaviour
  plotmean <- reactive({
    policy2 %>% filter(stat=='mean') %>%
      ggplot() +
      geom_line(aes(x=sensi, y=value, color=closure, linetype=status)) +
      labs(x='Student Sensitivity', y='Daily Values') +
      ggtitle('Daily Average by Student Sensitivity and University Closure Decision')
  })
  
  # Plot that shows daily maximum under different behaviour
  plotmax <- reactive({
    policy2 %>% filter(stat=='max') %>%
      ggplot() +
      geom_line(aes(x=sensi, y=value, color=closure, linetype=status)) + 
      labs(x='Student Sensitivity', y='Daily Values') +
      ggtitle('Daily Maximum by Student Sensitivity and University Closure Decision')
  })
  
  # User can choose which plot to display in the UI
  # Reference: https://stackoverflow.com/questions/48312392/shiny-allow-users-to-choose-which-plot-outputs-to-display
  graphInput <- reactive({
    switch(input$graph,
           "Daily Average" = plotmean(),
           "Daily Maximum" = plotmax()
    )
  })
  
  # Display either one of the plots from above
  output$analysis <- renderPlot({ 
    graphInput()
  })
  
  }


# Run the application 
shinyApp(ui = ui, server = server)
