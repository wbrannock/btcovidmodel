library('truncnorm')

#Assumptions/Model Parameters:

#Percent of infected passengers
cases_percentage <- (2/110)
#Using the prevalence among UVA football players as a guide

#My best guess at average ridership numbers
#Average number of passengers on bus
pass_avg <- 50
#Standard deviation of # of passengers
standard_dev <- 20

#Calculations:

#Monte Carlo Simulation to estimate odds of being riding a bus
#with at least 1 passenger that has COVID-19
nrep = 10000 #number of simulations
averages = c();
for (i in 0:nrep - 1){
    #Generating a random number of passengers for each simulated busride
    #assuming a truncated normal distribution of passengers
    passengers <- round(rtruncnorm(1,1,75,pass_avg,standard_dev))
    
    #Calculating odds of 1 or more person on bus having COVID
    odds <- 1 - pbinom(0, passengers, cases_percentage)
    #saving result of individual simulation
    averages <- c(averages, odds)
}

#Estimating number of rides necessary to be 99% sure you have shared a bus
#with a COVID-19 positive person using negative binomial distribution
busrides <- qnbinom(0.99,1,mean(averages))
  
#Outputs

sprintf("Chance of a busride having at least one person with COVID-19: %f", mean(averages)*100)
sprintf("Number of busrides nessary to be 99 percent sure you have shared a bus with someone with COVID-19: %i", round(busrides, 0))