# ------ Parameters --------
# The values are based on Table 2 from the article. These are potential parameters that users can play with in our ShinyApp
# Note that for simplicity, not all the parameters will be included in the simulation

library(tidyverse)
# Disease transmission
tao_1 = 6 # day; average time to move from early to late stage of the disease
tao_2 = 10 # day; infection post-exposure period
mu = 0.66 # relative infectivity of exposed to undocumented cases
y=1 # scaler
alpha = 0.2 * y # relative infectivity of documented to undocumented cases
# T = 14
C_max = 13 # constant contact rate; person/day
i_N = 0.037 # infection probablity absense of mask; person/day

# Test related - not used yet
T_sp = 0.998
T_sn = 0.8
T_c = 500 # test/day
k_s = 0.01
k_E = 0.6

# Population
A = 1786 # person/day
tao_arrival = 14 # arrival duration
a_s = 0.97 
A_E = 0.003
a_IU = 0.0015
B_3060 = 2500
B_gt60 = 500

# Policy - not used yet
M = 0
theta = 1
h = 100
beta_3060 = 1
beta_gt60 = 1

# Fatality - not used yet
f_lt30 = 0.00004
f_3060 = 0.0005
f_gt60 = 0.03

# ---- initialization -----
S0 = A*tao_arrival
N = A*tao_arrival # initial population
E0 = 10 # initial exposure
I0 = 1 # initial infection
R0 = 0 # initial recovered
len = 200 # days of observation
record = data.frame(matrix(0,nrow=len, ncol=4))

# rates:
alpha = 0.0002 * y # relative infectivity of documented to undocumented cases
mu = 0.66 # relative infectivity of exposed to undocumented cases
r_recover = 1/tao_2


######## ------ Simulation --------
for (i in 1:len){
  S = S0
  E = E0
  I = I0
  R = R0
  
  S0 = max(0, S - i_N*C_max*S/N * (mu*E + alpha*I))
  E0 = max(0, E + i_N*C_max*S/N * (mu*E + alpha*I) - E/tao_1)
  I0 = max(0, I + E/tao_1 - I0/tao_2)
  R0 = max(0, R + I0/tao_2)
  
  record[i,1] = S0
  record[i,2] = E0
  record[i,3] = I0
  record[i,4] = R0
}
# record
colors <- c("Susceptible" = "black",
            "Exposed" = "yellow", 
            "Infected" = "red", 
            "Recovered" = "green")

ggplot(data=record) +
  geom_line(aes(x=1:len, y=X1, color='Susceptible')) +
  geom_line(aes(x=1:len, y=X2, color='Exposed')) +
  geom_line(aes(x=1:len, y=X3, color='Infected')) +
  geom_line(aes(x=1:len, y=X4, color='Recovered')) +
  labs(y="Cumulative cases", x= "Days") +
  scale_color_manual(values = colors)


