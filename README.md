# Simulation Based What-If Analysis of COVID-19 Spread in Universities

#### Kara Rofe, Talia Seshaiah, Yifan Zhao


## Application 

This project was based off the article *"Simulation-based what if-analysis for controlling the spread of COVID-19 in universities"* (Ghaffarzadegan, 2021). 

Our project uses a SEIR-model framework to look at the transmission of COVID-19 among both students and faculty among Ivy League universities. SEIR (Susceptible, Exposed, Infected, Recovered) models are often used for diseases, and can help with understanding disease dynamics. 

- S: susceptible individuals (those who able to contract the disease)
- E: exposed individuals (those who have been exposed/infected)
- I: infectious individuals (those who can transmit the disease)
- R: recovered individuals (those who have recovered from the disease)


## Variables
<img src="https://github.com/taliajs/php2560final/blob/main/parameter-table.PNG">

## Simulation 


### Main Functions

There is one main function for this simulation - `simfunc`. 


### Equations

The equations used were based on Equations 3,4,6, and 8 from the article (the equations are simplified), and represent the transmission dynamics

- S0 = max(0, S - i_N * C_max * S/N * (mu * E + alpha*I) + A_S - L_S)  (from equation 3)
- E0 = max(0, E + i_N* C_max * S/N * (mu * E + alpha*I) + A_E - E/tao_1 - L_E)  (from equation 4)
- I0 = max(0, I + E/tao_1 - I0/tao_2 - L_I) (equation 6)
- R0  = max(0, R + I0/tao_2 - L_R) (equation 8)
  


## Shiny App 

[Link to app](https://taliajs.shinyapps.io/seir_covid_model/)

When the app is first run, it shows a single run of this model using default parameters. Users can look at the transmission of COVID-19 among students and faculty for different universities, by selecting an Ivy League school and changing the model parameters. When users select an Ivy League, the arrival rate is changed to that specific university. Users can learn more about the inputs by clicking on the `Information about Inputs` button on the App.

**School Data**
- Fall 2020 student to faculty ratios were used. 
- All data came from Common Data Sets(CDS)
- Only using full-time equivalent students and full-time instructional faculty (no TAs)


The tab `Daily cases` shows the daily cases of COVID-19 for exposed and infected individuals, with min, max and average contact rate listed below. 

The tab `Cumulative cases` shows the cumulative cases of COVID-19 over time. 

The `Summary` table shows the statistics for contact rate for specific variables.



### Sources
- Ghaffarzadegan N (2021) Simulation-based what-if analysis for controlling the spread of Covid-19 in universities. PLoS ONE 16(2): e0246323. https://doi.org/10.1371/journal.pone.0246323

- https://sites.me.ucsb.edu/~moehlis/APC514/tutorials/tutorial_seasonal/node4.html
