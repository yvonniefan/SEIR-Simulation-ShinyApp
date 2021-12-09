
# ------ Simulation function --------

simfunc <- function(A = 1786, tao_arrival = 14, tao_1=6, tao_2=10,mu=0.66, y=1, C_max=13, i_N=0.037,
                    E0=10,I0=1,R0=0, len=200) {
  alpha=0.2*y
  r_recover=1/tao_2
  S0 = A*tao_arrival
  N = A*tao_arrival
  record = data.frame(matrix(0,nrow=len, ncol=4))
  colnames(record) <- c('susceptible', 'exposed', 'infected','recovered')
  
  for (i in 1:len){
    S = S0
    E = E0
    I = I0
    R = R0
    
    # Transmission dynamics:
    # Code below are based on equation 3,4,6,8 from the article (simplified)
    S0 = max(0, S - i_N*C_max*S/N * (mu*E + alpha*I))  # equation 3
    E0 = max(0, E + i_N*C_max*S/N * (mu*E + alpha*I) - E/tao_1) # equation 4
    I0 = max(0, I + E/tao_1 - I0/tao_2) # equation 6
    R0 = max(0, R + I0/tao_2) # equation 8
    
    record[i,1] = S0
    record[i,2] = E0
    record[i,3] = I0
    record[i,4] = R0
  }
  record$days = c(1:len)

  colors <- c("Susceptible" = "black",
              "Exposed" = "yellow", 
              "Infected" = "red", 
              "Recovered" = "green")

  return(record)
}

# -------- Shiny App ---------
library(plotly)
# 
# tao_1 = 6 # day; average time to move from early to late stage of the disease
# tao_2 = 10 # day; infection post-exposure period
# mu = 0.66 # relative infectivity of exposed to undocumented cases
# y=1 # scaler
# alpha = 0.2 * y # relative infectivity of documented to undocumented cases
# # T = 14
# C_max = 13 # constant contact rate; person/day
# i_N = 0.037 # infection probablity absense of mask; person/day
# 
# # Test related - not used yet
# T_sp = 0.998
# T_sn = 0.8
# T_c = 500 # test/day
# k_s = 0.01
# k_E = 0.6
# 
# # Population
# A = 400 # person/day
# tao_arrival = 14 # arrival duration
# a_s = 0.97 
# A_E = 0.003
# a_IU = 0.0015
# B_3060 = 2500
# B_gt60 = 500

# Policy - not used yet
# M = 0
# theta = 1
# h = 100
# beta_3060 = 1
# beta_gt60 = 1
# 
# # Fatality - not used yet
# f_lt30 = 0.00004
# f_3060 = 0.0005
# f_gt60 = 0.03

# # ---- initialization -----
# S0_inc = A*tao_arrival
# N = A*tao_arrival # initial population
# E0 = 10 # initial exposure
# I0 = 1 # initial infection
# R0 = 0 # initial recovered
len = 200 # days of observation
# record = data.frame(matrix(0,nrow=len, ncol=4))
# 
# # rates:
# alpha = 0.0002 * y # relative infectivity of documented to undocumented cases
# mu = 0.66 # relative infectivity of exposed to undocumented cases
# r_recover = 1/tao_2



ui <- fluidPage(
  
  # Application title
  titlePanel("Simulation-Based What-if Analysis of COVID-19 Spread in Universities"),
  h4("Kara Rofe, Talia Seshaiah, Yifan Zhao"),
  p("insert paragraph here"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("students",
                  "Arrival rate of students:",
                  min = 10,
                  max = 10000,
                  value = 100),
      sliderInput("cmax", "Constant contact rate:",
                  min=0,
                  max=40,
                  value=13), 
      sliderInput("i_n", "Infection probability:", 
                  min=0, 
                  max=1, 
                  value=0.037
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  record_reac <- reactive({ 
    simfunc(A = input$students,C_max = input$cmax, i_N =input$i_n )
    })
  
  # plot
  
  colors <- c("Susceptible" = "black",
              "Exposed" = "yellow", 
              "Infected" = "red", 
              "Recovered" = "green")
  # days <- 1:len
  
  output$distPlot <- renderPlotly({
    ggplot(data=record_reac()) +
      geom_line(aes(x=days, y=susceptible, color="Susceptible")) +
      geom_line(aes(x=days, y=exposed, color="Exposed"))+
      geom_line(aes(x=days, y=infected, color="Infected"))+
      geom_line(aes(x=days, y=recovered, color="Recovered"))+
      labs(y="Cumulative cases", x= "Days")+
      scale_color_manual(values = colors)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
