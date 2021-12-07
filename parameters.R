library(tidyverse)
# ------ Parameters --------
# Definitions are based on Table 1 from the article.
# The values are based on Table 2 from the article. These are potential parameters that users can play with in our ShinyApp
# Note that for simplicity, not all the parameters will be included in the simulation


# Population Parameters:
A = 1786 # daily arrival rate of students
tao_arrival = 14 # arrival duration

# Disease transmission Parameters: 
tao_1 = 6 # day; exposure period: average time to move from early to late stage of the disease
tao_2 = 10 # day; infection post-exposure period
mu = 0.66 # relative infectivity of exposed to undocumented cases
y=1 # scaler
alpha = 0.2 * y # relative infectivity of documented to undocumented cases
r_recover = 1/tao_2 # recover rate

C_max = 13 # person/day; constant contact rate absent any exogenous or endogenous changes in social behaviour
i_N = 0.037 # person/day; infection probablity in the absense of mask


# ---- Initialization -----
S0 = A*tao_arrival # initial Susceptible
N = A*tao_arrival # initial population, same as initial susceptible
E0 = 10 # initial exposure
I0 = 1 # initial infection
R0 = 0 # initial recovered
len = 200 # Number of days simulated

# Dataframe to record cumulative cases:
# this is the table you can use for creating visuals
recordcum = data.frame(matrix(0,nrow=len, ncol=4))
colnames(recordcum) <- c('susceptible', 'exposed', 'infected','recovered')

# ------ Simulation --------
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
  
  recordcum[i,1] = S0
  recordcum[i,2] = E0
  recordcum[i,3] = I0
  recordcum[i,4] = R0
}
recordcum$day = c(1:len)
# recordcum
colors <- c("Susceptible" = "black",
            "Exposed" = "yellow", 
            "Infected" = "red", 
            "Recovered" = "green")

ggplot(data=recordcum) +
  geom_line(aes(x=day, y=susceptible, color='Susceptible')) +
  geom_line(aes(x=day, y=exposed, color='Exposed')) +
  geom_line(aes(x=day, y=infected, color='Infected')) +
  geom_line(aes(x=day, y=recovered, color='Recovered')) +
  labs(y="Cumulative cases", x= "Days") +
  scale_color_manual(values = colors)


