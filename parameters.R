# ------ parameters --------
# Disease transmission
tao_1 = 6
tao_2 = 10
mu = 0.66 # infectivity rate
y=1
alpha = 0.2 * y 
T = 14
C_max = 13 # constant contact rate; person/day
i_N = 0.037 # infection probablity absense of mask; person/day

# Test related
T_sp = 0.998
T_sn = 0.8
T_c = 500 # test/day
k_s = 0.01
k_E = 0.6

# Population
A = 1786 # person/day
tao_arrival = 14
a_s = 0.97
A_E = 0.003
a_IU = 0.0015
B_3060 = 2500
B_gt60 = 500

# Policy
M = 0
theta = 1
h = 100
beta_3060 = 1
beta_gt60 = 1

# Fatality
f_lt30 = 0.00004
f_3060 = 0.0005
f_gt60 = 0.03

# ---- initialization -----
S0 = A*tao_arrival
E0 = 10
I0 = 1
R0 = 0
len = 90
record = data.frame(matrix(0,nrow=len, ncol=4))
# rates:
y=1
alpha = 0.0002 * y # relative infectivity of documented to undocumented cases
mu = 0.66 # relative infectivity of exposed to undocumented cases
r_recover = mu/tao_2
# # 
# len = 500
# S0=327200000
# alpha=1/1000000000
# r_recover =1/14
# simulation:
for (i in 1:len){
  S = S0
  E = E0
  I = I0
  R = R0
  # S0 = max(0, S  - i_N*C_max * (mu*S*E + alpha*S*I))
  # E0 = max(0, E + i_N*C_max * (mu*S*E + alpha*S*I))
  # I0 = max(0, I + i_N*C_max * (alpha*S*I) - r_recover*I0)
  
  # S0 = max(0, S  - i_N*C_max * (alpha*S*E))
  # E0 = max(0, E + i_N*C_max * (alpha*S*E) - i_N*C_max * (mu*E))
  # I0 = max(0, I + i_N*C_max * (mu*E) - r_recover*I0)
  # R0 = max(0, R + r_recover*I0)
  
  # seems working: based on video
  S0 = max(0, S  - i_N*C_max * (alpha*S*E))
  E0 = max(0, E + i_N*C_max * (alpha*S*E) - i_N*C_max * (mu*E))
  I0 = max(0, I + i_N*C_max * (mu*E) - r_recover*I0)
  R0 = max(0, R + r_recover*I0)
 
  record[i,1] = S0
  record[i,2] = E0
  record[i,3] = I0
  record[i,4] = R0
}
record
ggplot(data=record) +
  geom_line(aes(x=1:len, y=X1)) +
  geom_line(aes(x=1:len, y=X2), color='yellow')+
  geom_line(aes(x=1:len, y=X3), color='red')+
  geom_line(aes(x=1:len, y=X4), color='green')
