# This is the simulation/parameter code:
library(tidyverse)
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
  
  return(record)
}




# Get daily record: you should use simfunc() as data when calling this function:
func_getdaily <- function(data){
  daily <- data %>% select(c(1:5)) %>% 
    mutate(lags=lag(susceptible), lage=lag(exposed),lagi=lag(infected),lagr=lag(recovered),
           d_sus = pmax(0,lags-susceptible, na.rm=TRUE),
           d_exp = pmax(0,exposed-lage, na.rm=TRUE), 
           d_inf = pmax(0,infected-lagi, na.rm=TRUE), 
           d_rec = pmax(0,recovered-lagr, na.rm=TRUE)) %>%
    select(days, d_sus, d_exp,d_inf,d_rec)
  return(daily)
}


# simfunc()
# ourrecord <- simfunc()
# daily <- func_getdaily(ourrecord)
