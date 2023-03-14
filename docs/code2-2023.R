rm(list=ls()) # Clear all stored variables

# Parameters of the simulation
# n = population size
# nt = number of time steps
# gamma = intensity of the trade-off between survival and reproduction
# c = strength of extrinsic mortality
# b0 = maximal fecundity (fecundity when reproductive effort is 1)
# sig = size of mutation steps
# u = mutation rate

RE=function(n, nt, gamma, c, b0, sig, u)
{
  pop=sample(x=seq(0,1,0.01), size=n, replace=T) # Create a population of size 'n' with reproductive efforts ranging from 0 to 1.
  res=rep(0,nt) # Create a vector for the results
  
  for(t in 1:nt) # For each time step...
  {
    fec=b0*pop/sum(b0*pop) # Compute the relative fecundity of individuals in the population and store it in the 'fec' vector. 
    par=pop # Save the parental population in the 'par' vector
    
    for(i in 1:n) # For each individual in the population
    {
      if( runif(1) > c*(1 - pop[i]^gamma) ) # ???
      {
        p = sample(x=c(1:n), size=1, prob= fec ) # ???
        
        if(runif(1) < u) # If the individual mutates
        {
          off = par[p] + sig*rnorm(n=1, mean=0, sd=1) # sample its new value in a Gaussian N(0,sig) around the parental value
          
          # Keep reproductive effort values within bounds:
          
          if(off > 1) # If the new value is over one, set it to 1.
          {
            off=1
          }
          
          if(off < 0) # If the new value is below zero, set it to 0.
          {
            off=0
          }
        }
        else
        {
          off = par[p] # If the individual does not mutate, it simply inherits the parental value.
        }
        
        pop[i] = off # Incorporate the new individual in the population.
      }
    }
    
    res[t] = mean(pop) # Measure the mean reproductive effort in the population and store it in the 'res' vector
    
  }
  
  return(res) # Return results.
}

# Parameters of the simulation (copy-pasted here for convenience)
# n = population size
# nt = number of time steps
# gamma = intensity of the trade-off between survival and reproduction
# c = strength of extrinsic mortality
# b0 = maximal fecundity (fecundity when reproductive effort is 1)
# sig = size of mutation steps
# u = mutation rate

# The results of the simulation are stored in the object 'sim'
sim=RE(n= , nt= , gamma= , c= , b0= , sig= , u= )