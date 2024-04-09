################################### Exercise sheet 5 ###################################

rm(list=ls()) # Clear all stored variables.

### Summary of parameters
TF    = 400   #Final time of observation
Npop  = 1000   # Initial population size
Lloci = 200    # Number of loci (max number of deleterious mutations)
kbar  = 50     # Number of deleterious mutations which epistasis is centered
u     = 0.007  # Mutation rate
s     = 0.02  # Deleterious fitness effect
f0    = 2.5   # Baseline fitness
GAMMA = 0.0003 # Density regulation 
sex.r = 0.5   # Sex ratio at birth
epsilon = 75  # Level of epistasis 

fitness = function(xi, exp = epsilon, kbar = 50){
  ### Fitness effects as a function of the number of deleterious mutations (xi)
  num = sum(xi)
  (1 - s) ^ ( (num/kbar) ^ exp )
}

mutate = function(xi){
  ### .....
  new = xi + rbinom(Lloci, 1, u)
  1.0*(new>=1)
}

recombine = function(xi, xj){
  ### .....
  joint = c(xi, xj)
  joint[seq(1, Lloci) + Lloci*rbinom(Lloci, 1, 0.5) ]
}

count = function(a){
  ### Counts the number of total mutations in each individual
  asum = rep(0, length(a))
  for(idx in seq(1, length(a))) asum[idx] = sum(a[[idx]])
  asum
}

# Initialize population // Population starts completely sexual
popF.xi = rep(list(rep(0, Lloci)), (1-sex.r)*Npop)
popF.sigma = rep(1, (1-sex.r)*Npop)
popM.xi = rep(list(rep(0, Lloci)), sex.r*Npop)
popM.sigma = rep(1, sex.r*Npop)

# Vectors to save statistics
gens     = c()
pop.size = c()
popA.size = c()
popS.mean = c()
popA.mean = c()
pop.min  = c()
sigma.mean  = c()
popA.size  = c()

for (time in seq(1,TF)) {
  # Initialize vectors at each time step
  new_popF.xi = c()
  new_popF.sigma = c()
  new_popM.xi = c()
  new_popM.sigma = c()
  
  popOff.xi = c()
  popOff.sigma = c()
  popOffA.xi = c()
  popOffA.sigma = c()

  for (ind in seq(1, length(popF.xi))) { # For each adult
    xi = popF.xi[[ind]]  # Get its genome
    ps = popF.sigma[ind] # Gets its probability of sex
    Noff = rpois(1, f0) # Sample its number of offspring from a poisson distribution
    
    if(Noff > 0) { # If the number of offspring is greater than 0
      if (rbinom(1, 1, ps)){ # Individuals are produced sexually
        other = sample(length(popM.xi), size = Noff, replace = T) # Sample mate
        
        for (off in seq(1, Noff) ){ # Create the genome and sigma of offspring
          popOff.xi = append(popOff.xi, list(mutate(recombine(xi, popM.xi[[other[off]]]))))
          popOff.sigma = append(popOff.sigma, ifelse(rbinom(1,1,0.5), ps, popM.sigma[other[off]]) )
        } 
        
      } else { # Individuals are produced asexually
        for (off in seq(1, Noff)){
          popOffA.xi = append(popOffA.xi, list(mutate(xi)))
          popOffA.sigma = append(popOffA.sigma,  ps)
        } 
      }
      
    }
    
  }# End of loop over adults
  
  
  # Now comes the survival probability of these offspring
  reg = 1.0 / (1.0 + GAMMA * (length(popF.xi)+length(popM.xi)) )
  
  if(length(popOff.sigma)) 
  for(index in seq(1, length(popOff.xi)) )
    if(runif(1) < reg*fitness(popOff.xi[[index]])){ # Probability has two factors: density-dependency and mutations
      # If individual survives, then add it to the list of new_pop
      if(rbinom(1,1,sex.r)) {
        new_popM.xi = append(new_popM.xi, list(popOff.xi[[index]]))
        new_popM.sigma = append(new_popM.sigma, popOff.sigma[index])
      } else {
        new_popF.xi = append(new_popF.xi, list(popOff.xi[[index]]))
        new_popF.sigma = append(new_popF.sigma, popOff.sigma[index])
      }
    } 
  
  if(length(popOffA.sigma)) 
  for(index in seq(1, length(popOffA.xi)) )
    if(runif(1) < reg*fitness(popOffA.xi[[index]])){ # Probability has two factors: density-dependency and mutations
      # If individual survives, then add it to the list of new_pop
        new_popF.xi = append(new_popF.xi, list(popOffA.xi[[index]]))
        new_popF.sigma = append(new_popF.sigma, popOffA.sigma[index])
    } 
  
  # new_pop becomes the population
  popF.xi = new_popF.xi
  popM.xi = new_popM.xi
  popF.sigma = new_popF.sigma
  popM.sigma = new_popM.sigma
  
  # If you want to print at each time step
  print(paste0("Generation:", time, " - No. females: ", length(popF.xi), ", No. males: ", length(popM.xi), ", Mean no. of mutations: ",  mean(count(popF.xi))))
  
  # Ends simulation if extinction occurs
  if ( length(popF.xi) == 0  ) break
  
  # Ends simulation if population gets too big
  if ( length(popF.xi)+length(popM.xi) > 4000  ) break
  
  ### ......
  if ( time == 200) popF.sigma[sample(length(popF.sigma), 1)] = 0
  
  # Measures at each time step
  gens = append(gens, time )
  pop.size =  append(pop.size, sum(popF.sigma==1)+length(popM.xi) )
  popA.size =  append(popA.size, sum(popF.sigma==0) )
  popS.mean = append(popS.mean, sum(count(popF.xi)*popF.sigma)/sum(popF.sigma))
  popA.mean = append(popA.mean, sum(count(popF.xi)*(1-popF.sigma)/sum(1-popF.sigma)))
  pop.min  =  append(pop.min , min(min(count(popF.xi))))
  sigma.mean  =  append(sigma.mean , mean(popF.sigma) ) 
}

# Plots
par(mfrow=c(2,2), cex=0.7, mai=c(0.7,0.7,0.1,0.1))
plot(gens, pop.size, 'l', ylim = c(0, max(max(pop.size), max(popA.size))), xlab="Generations", ylab = "Population size")
points(gens, popA.size, 'l', col="red")
plot(gens, pop.min , 'l', ylim = c(0, Lloci) , xlab="Generations", ylab = "Minimum number of mutations")
plot(gens, popS.mean, 'l', ylim = c(0, Lloci) , xlab="Generations", ylab = "Mean number of mutations")
points(gens, popA.mean, 'l', col="red")
plot(gens, sigma.mean, 'l', ylim = c(0, 1) , xlab="Generations", ylab = "Mean prob. of sexual rep.")


### Plot fitness
#par(mfrow=c(2,2), cex=0.7, mai=c(0.7,0.7,0.1,0.1))
#plot(seq(1, Lloci), sapply(seq(1, Lloci), fitness, exp=75, kbar=150 ), 
#     'l', xlab="No. of del. mutations", ylab="Survival probability,", ylim=c(-0.01, 1.01))

