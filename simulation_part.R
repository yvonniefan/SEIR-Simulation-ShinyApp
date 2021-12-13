# This is the simulation/parameter code:
library(tidyverse)
require(RcppRoll)
# install.packages('RcppRoll')
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

ourrecord[2:9,3]
sum(ourrecord[2:9,3])

# Get cumulative record: you should use simfunc() as data when calling this function:
func_getcum <- function(data){
  cumulative <- data %>% mutate(cum_E = cumsum(exposed),
                                cum_I = cumsum(infected), 
                                cum_R = cumsum(recovered))
  return(cumulative)
}
# 
# ourrecord <- simfunc(A=100)
# # ourrecord$rollavg <- roll_mean(ourrecord$infected, n=7, align='right',fill=NA)
# # mean(ourrecord[2:8,3])
# 
# cumulative <- func_getcum(ourrecord)
# 
# 
# colors <- c("Susceptible" = "black",
#             "Exposed" = "yellow", 
#             "Infected" = "red", 
#             "Recovered" = "green")
# ggplot(data=ourrecord) +
#   geom_line(aes(x=days, y=susceptible, color="Susceptible")) +
#   geom_line(aes(x=days, y=exposed, color="Exposed"))+
#   geom_line(aes(x=days, y=infected, color="Infected"))+
#   geom_line(aes(x=days, y=recovered, color="Recovered"))+
#   geom_line(aes(x=days, y=rollavg, color="purple"))+
#   labs(y="Daily cases", x= "Days")+
#   scale_color_manual(values = colors)
# 
# 
# ggplot(data=cumulative) +
#   geom_line(aes(x=days, y=cum_E, color="Exposed"))+
#   geom_line(aes(x=days, y=cum_I, color="Infected"))+
#   # geom_line(aes(x=days, y=cum_R, color="Recovered")) +
#   labs(y="Cumulative Cases", x= "Days")+
#   scale_color_manual(values = c( "Exposed" = "yellow", 
#                                  "Infected" = "red"))
