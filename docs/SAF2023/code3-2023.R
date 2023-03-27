################################### Exercise sheet 4 ###################################

rm(list=ls()) # Clear all stored variables.

### Summary of parameters
TF    = 1200   #Final time of observation
Npop  = 500   # Initial population size
Lloci = 50    # Number of loci (max number of deleterious mutations)
u     = 0.005  # Mutation rate
s     = 0.005  # Deleterious fitness effect
f0    = 2.1   # Baseline fitness
GAMMA = 0.001 # Density regulation 
sigma = 0     # Probability of undergoing sexual reproduction


fitness = function(xi){
  ### Fitness effects as a function of the number of deleterious mutations (xi)
  num = sum(xi)
  (1 - s) ^ num  
}

mutate = function(xi){
  ### .....
  new = xi + rbinom(Lloci, 1, u)
  1.0*(new>=1)
}

recombine = function(xi, xj){
  ### ......
  joint = c(xi, xj)
  joint[seq(1, Lloci) + Lloci*rbinom(Lloci, 1, 0.5) ]
}

count = function(a){
  ### Counts the number of total mutations in each individual
  asum = rep(0, length(a))
  for(idx in seq(1, length(a))) asum[idx] = sum(a[[idx]])
  asum
}

# Initialize population
pop.xi = rep(list(rep(0, Lloci)), Npop)

# Vectors to save statistics
gens     = c()
pop.size = c()
pop.mean = c()
pop.min  = c()

for (time in seq(1,TF)) {
  # Initialize vectors at each time step
  new_pop.xi = c()
  popOff.xi = c()
  
  # Calculates the fitnesses of each adult
  fitnesses = sapply(pop.xi, fitness)
  
  for (ind in seq(1, length(pop.xi))) { # For each adult
    xi = pop.xi[[ind]]  # Get its genome
    Noff = rpois(1, f0) # Sample its number of offspring from a poisson distribution
    
    if(Noff > 0) { # If the number of offspring is greater than 0
      
      if (rbinom(1, 1, sigma)){ # Individuals are produced sexually
        other.xi = sample(pop.xi, size = Noff, replace = T) # Sample mate
        
        for (off in seq(1, Noff) ){ # Create the genome of offspring
          popOff.xi = append(popOff.xi, list(mutate(recombine(xi, other.xi[[off]]))))
        } 
        
      } else { # Individuals are produced asexually
        for (off in seq(1, Noff)){
          popOff.xi = append(popOff.xi, list(mutate(xi)))
        } 
      }
      
    }
    
  }# End of loop over adults
  
  # Now comes the survival probability of these offspring
  reg = 1.0 / (1.0 + GAMMA * (length(pop.xi) ) )
  
  for(index in seq(1, length(popOff.xi)) )
    if(runif(1) < reg*fitness(popOff.xi[[index]])){ # Probability has two factors: density-dependency and mutations
      # If individual survives, then add it to the list of new_pop
      new_pop.xi = append(new_pop.xi, list(popOff.xi[[index]]))
    } 
  
  # new_pop becomes the population
  pop.xi = new_pop.xi

  # If you want to print at each time step
  print(c(time, length(pop.xi), mean(count(pop.xi))))
  
  # Ends simulation if extinction occurs
  if ( length(pop.xi) == 0 ) break
  
  # Measures at each time step
  gens     = append(gens, time )
  pop.size = append(pop.size, length(pop.xi) )
  pop.mean = append(pop.mean, mean(count(pop.xi)))
  pop.min  = append(pop.min , min(count(pop.xi)) )
}

# Plots
par(mfrow=c(2,2), cex=0.7, mai=c(0.7,0.7,0.1,0.1))
plot(gens, pop.size, 'l', ylim = c(0, max(pop.size)), xlab="Time", ylab = "Population size")
plot(gens, pop.min , 'l', ylim = c(0, Lloci) , xlab="Time", ylab = "Minimum number of mutations")
plot(gens, pop.mean, 'l', ylim = c(0, Lloci) , xlab="Time", ylab = "Mean number of mutations")
hist(count(pop.xi), ylab="Distribution", xlab = "Mean number of mutations", main="Distribution of mutations at final time")


### Plot fitness
#plot(sapply(seq(1, Lloci), fitness), xlab="Number of deleterious mutations", ylab="Fitness")

### Calculate mean time at each minimum
#mean(table(pop.min))
