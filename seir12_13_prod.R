
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

# ------ Simulation function --------

simfunc <- function(A = 1786, tao_arrival = 14, tao_1=6, tao_2=10,mu=0.66, y=1, C_max=13, i_N=0.037,
                    E0=10,I0=1,R0=0, len=120, phi = 1, h = 100) {
  alpha = 0.2 * y # relative infectivity of documented to undocumented cases
  r_recover=1/tao_2 # recover rate
  S0 = A*tao_arrival # initial Susceptible
  N = A*tao_arrival  # initial population, same as initial susceptible
  # h = 100 # coefficient representing the sensitivity of a college to the number of daily cases
  
  record = data.frame(matrix(0,nrow=len, ncol=4))
  colnames(record) <- c('susceptible', 'exposed', 'infected','recovered')
  record[1,'C'] = C_max
  
  # new:
  # phi = 1 #1 or 0, university closure decision, 0 open, 1 closed
  T = 14 # days; average time to evacuate the college town after school closure
  
  L_S = phi*S0/T
  L_E = phi*E0/T
  L_I = phi*I0/T
  L_R = phi*R0/T
  A_S = 0.97*A #daily arrival rate of students with status susceptible
  A_E = 0.003*A
  
  for (i in 1:len){
    S = S0
    E = E0
    I = I0
    R = R0
    # Transmission dynamics:
    # Code below are based on equation 3,4,6,8 from the article (simplified)
    # S0 = max(0, S - i_N*C_max*S/N * (mu*E + alpha*I))  # equation 3
    # E0 = max(0, E + i_N*C_max*S/N * (mu*E + alpha*I) - E/tao_1)  # equation 4
    # I0 = max(0, I + E/tao_1 - I0/tao_2 )# equation 6
    # R0 = max(0, R + I0/tao_2) # equation 8
    S0 = max(0, S - i_N*record[max(i-1, 1), 'C'] *S/N * (mu*E + alpha*I) + A_S - L_S)  # equation 3
    E0 = max(0, E + i_N*record[max(i-1, 1), 'C'] *S/N * (mu*E + alpha*I) + A_E - E/tao_1 - L_E)  # equation 4
    I0 = max(0, I + E/tao_1 - I0/tao_2 - L_I)# equation 6
    R0 = max(0, R + I0/tao_2 - L_R) # equation 8
    
    record[i,1] = S0
    record[i,2] = E0
    record[i,3] = I0
    record[i,4] = R0
    # 7 days rolling average:
    record[i,'rollavg'] = mean(record[max((i-14),1) :i,3], na.rm=TRUE)
    
    
    # W_N: factor that affect contact rate. Students tend to change behavior according to current covid cases
    record[i, 'W_N'] = exp(-h * ((record[i,'rollavg'])/N))
    record[i, 'C'] = C_max * record[i,'W_N']
    # record[i, 'C'] = C_max 
    
  }
  record$days = c(1:len)
  # record$rollavg1 <- roll_mean(record$infected, n=7, align='right',fill=NA)
  
  return(record)
}


# Get cumulative record: you should use simfunc() as data when calling this function:
func_getcum <- function(data){
  cum <- data %>% mutate(cum_E = cumsum(exposed),
                         cum_I = cumsum(infected),
                         cum_R = cumsum(recovered))
  return(cum)
}

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
                  tabPanel("Daily cases", plotlyOutput("distPlot"), textOutput('contact'), textOutput('text')),
                  tabPanel("Cumulative cases", plotlyOutput("distPlotcum")),
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
  
  #daily cases plot
  output$distPlot <- renderPlotly({
    ggplot(data=record_reac()) +
      geom_line(aes(x=days, y=exposed, color="Exposed"))+
      geom_line(aes(x=days, y=infected, color="Infected"))+
      # New: plot contact rate changes over time:
      geom_line(aes(x=days, y=C), linetype = 3) +
      labs(y="Daily cases", x= "Days")+
      scale_color_manual(values = colors)+
      ggtitle("Daily cases of COVID-19")
  })
  
  # plot that shows cumulative cases
  output$distPlotcum <- renderPlotly({
    ggplot(data=cum_reac()) +
      geom_line(aes(x=days, y=cum_E, color="Exposed"))+
      geom_line(aes(x=days, y=cum_I, color="Infected"))+
      labs(y="Cumulative Cases", x= "Days")+
      scale_color_manual(values = c( "Exposed" = "yellow", 
                                     "Infected" = "red"))+
      ggtitle("Cumulative cases of COVID-19")
  })
  output$summary <- renderPrint({
    summary(simfunc())
  })
  # 
  output$contact <- renderText({
    paste0('Minimum contact rate: ', round(min(record_reac()$C),1), 
           '  Max contact rate: ', max(record_reac()$C), 
           '  Average contact rate: ', round(mean(record_reac()$C),1))
  })
  
  #NEW: add description of plot 
  output$text <- renderText({
    "Yvonne - put description of plot (explain what black dotted line is perhaps?)
    and maybe also explain the statistics above --> Get rid of this text when you 
    put your stuff here!"
  })
  }


# Run the application 
shinyApp(ui = ui, server = server)
