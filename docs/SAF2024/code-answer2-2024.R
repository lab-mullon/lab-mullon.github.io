rm(list=ls()) # Clear all stored variables

# Parameters of the simulation
# n = population size
# tmax = number of time steps
# gamma = intensity of the trade-off between survival and reproduction
# c = strength of extrinsic mortality
# b0 = maximal fecundity (fecundity when reproductive effort is 1)
# sig = size of mutation steps
# u = mutation rate

RE=function(n, tmax, gamma, c, b0, sig, u)
{
  Pop=sample(x=seq(0,1,0.01), size=n, replace=T) # Create a population of size 'n' with reproductive efforts ranging from 0 to 1.
  Res=rep(0,tmax) # Create a vector for the results (mean reproductive effort in the population)
  
  for(t in 1:tmax) # For each time step...
  {
    Fec=b0*Pop/sum(b0*Pop) # Compute the relative fecundity of individuals in the population and store it in the 'fec' vector. 
    Par = Pop # Store the parental population
    
    for(i in 1:n) # For each individual in the population
    {
      if( runif(1) > c*(1 - Pop[i]^gamma) ) # If individual pop[i] dies, 
      {
        parent = sample(x=c(1:n), size=1, prob= Fec ) # sample the parent producing the offspring that will replace the dead
        
        if(runif(1) < u) # If the individual mutates
        {
          Pop[i] = Par[parent] + sig*rnorm(n=1, mean=0, sd=1) # sample its new value in a Gaussian N(0,sig) around the parental value
          
          # Keep reproductive effort values within bounds:
          
          if(Pop[i] > 1) # If the new value is over one, set it to 1.
          {
            Pop[i]=1
          }
          
          if(Pop[i] < 0) # If the new value is below zero, set it to 0.
          {
            Pop[i]=0
          }
        }
        else
        {
          Pop[i]= Par[parent] # If the individual does not mutate, it simply inherits the parental value.
        }
      } # End of 'if' over survival
    } # End of 'for' loop over individuals
    
    Res[t] = mean(Pop) # Measure the mean reproductive effort in the population and store it in the 'res' vector
    
  } # End of 'for' loop over time
  
  return(Res) # Return results.
}