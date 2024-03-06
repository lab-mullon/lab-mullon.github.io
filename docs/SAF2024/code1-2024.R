# Individual-based simulation

IND_ACT = function(age, fec, psuv){
    # Reproduction
    no_offspring = rpois(n = 1, lambda = fec)
    no_new_n1 = rbinom(1, size = no_offspring, prob = psuv)
    new_n1 = rep(1, no_new_n1)
    
    # Try to survive
    if (runif(1) < psuv){
      # Survives and ages
      age = age + 1
      if (age > 6){ age = NA } # Caps max age at 6
    } else {
      # Dies
      age = NA
    }
    
  # Returns new adults and individual
  return(c(new_n1, age))
} 

DYN = function(n0, FinalYear, fec, psuv){
  ### Initialization
  Population = rep(1, n0)
  PopulationSize = rep(0, FinalYear)
  
  for (year in 1:FinalYear){
    NextPopulation = c()
    
    for(i in 1:length(Population)){
      # Append to NextPopulation all "families"
      NextPopulation = c(NextPopulation, IND_ACT(age = Population[i], fec = fec, psuv = psuv))
    }
    
    Population = NextPopulation[!is.na(NextPopulation)]
    PopulationSize[year] = length(Population)
    
    if( length(Population) == 0 ){ break } # Population went extinct
    if( length(Population) > 100000 ){ break } # Population skyrocketed
  }

  return(PopulationSize)
}

# Runs one simulation with a set of parameters
sim = DYN(n0 = 100, FinalYear = 200, fec = 1.02, psuv = 0.5)

# Plots the output of the simulation
plot(x = 1:length(sim), y = sim,
     type = "b",
     col = "red", pch = 16, 
     ylim = c(0, 2*max(sim)),
     xlab = "Years", ylab = "Pop. size")

