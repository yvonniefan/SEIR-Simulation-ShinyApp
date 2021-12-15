# This is the simulation/parameter code:

library(tidyverse)
library(docstring)
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
  record[1,'C'] = C_max
  
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
    # W_N: factor that affect contact rate. Students tend to change behavior according to current covid cases
    record[i, 'W_N'] = exp(-h * ((record[i,'rollavg'])/N))
    record[i, 'C'] = C_max * record[i,'W_N']
  }
  record$days = c(1:len)
  return(record)
}

# docstring(simfunc)

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
# docstring(func_getcum)


# Below are just tests for generating graphs. The actual graphs in the ShinyApp are created in the ShinyApp server.
ourrecord <- simfunc(A=100)
cumulative <- func_getcum(ourrecord)
library(plotly)


colors <- c("Susceptible" = "black",
            "Exposed" = "yellow",
            "Infected" = "red",
            "Recovered" = "green")

# daily cases plot:
ggplot(data=ourrecord) +
  geom_line(aes(x=days, y=exposed, color="Exposed")) +
  geom_line(aes(x=days, y=infected, color="Infected")) +
  geom_line(aes(x=days, y=C, linetype='C')) +
  geom_line(aes(x=days, y=rollavg, linetype='roll')) +
  labs(y="Daily cases", x= "Days") +
  scale_color_manual(name='Status',values = c( Exposed = "yellow", Infected = "red")) +
  scale_linetype_manual(name='Contact Rate',values=c(C = 3, roll=2)) + 
  annotate("text", x = 90, y = 20, label = 'Dotted line: Contact Rate', hjust = 0, size = 2) +
  # annotation_custom('text', xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+
  
  ggtitle("Daily cases of COVID-19")

# cumulative cases plot:
ggplot(data=cumulative) +
  geom_line(aes(x=days, y=cum_E, color="Exposed"))+
  geom_line(aes(x=days, y=cum_I, color="Infected"))+
  labs(y="Cumulative Cases", x= "Days")+
  scale_color_manual(values = c( "Exposed" = "yellow",
                                 "Infected" = "red"))
# rolling average:
ggplot(data=ourrecord)+
  geom_line(aes(x=days, y=rollavg), color="orange") +
  ggtitle('14 Days Rolling Average')


# SEIR plot:
ggplot(data=ourrecord) +
  geom_line(aes(x=days, y=susceptible, color="Susceptible")) +
  geom_line(aes(x=days, y=exposed, color="Exposed")) +
  geom_line(aes(x=days, y=infected, color="Infected")) +
  geom_line(aes(x=days, y=recovered, color="Recovered"))+
  labs(y="Daily cases", x= "Days") +
  scale_color_manual(values = colors)

# Explore relationship between student sensitivity, university closure policy and infected:
analysis_behaviour <- function(closure, sensitivity){
  #' Analyze behaviour and COVID cases
  #' 
  #' Analyze relationship between student sensitivity, university closure policy and COIVD cases
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


