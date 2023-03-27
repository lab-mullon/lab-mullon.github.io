# Summary of parameters
# n = population size
# p = survival probability between age 1 and age 2
# alpha = fecundity difference between age 1 and age 2
# b = fecundity scaling constant
# ps = survival probability from one age-class to the next
# u = mutation rate
# sig = size of mutations (standard deviation of a Gaussian)
# nt = number of timesteps

rm( list=ls() )

AP=function(n, nt, p, alpha, b, u, sig)
{
  P=runif(n) # Create a population of size 'n' with trait values sampled uniformly on [0,1]
  A=rep(1,n) # Initialise the population with only individuals of age 1
  R=rep(0,nt) # Prepare a vector to store the results
  
  for(t in 1:nt)
  {
    
    Fec= (A-1)*b*alpha*( 1- exp(-(1-P)) ) + (2-A)*b*( 1- exp(-P) ) ### ???
    
    Par=P # Store the population in the previous timestep in a vector 'Par'
    
    for(i in 1:n) # For each individual...
    {
      if( runif(1) > p || A[i] > 1 ) # If it dies
      {
        off = sample(1:n, size=1, prob=Fec) # Sample a parent in the population with a probability proportional to fecundity
        
        A[i] = 1 # Set age of the new individual to 1.
        
        if(runif(1) < u)
        {
          P[i] = Par[off] + sig*rnorm(1) # If it mutates, inherit parental value + mutation
        }
        else
        {
          P[i] = Par[off] # Otherwise, inherit the parental value.
        }
        
        # Keep trait values within bounds:
        if(P[i] < 0)
        {
          P[i]=0
        }
        
        if(P[i] > 1)
        {
          P[i]=1
        }
      }
      else
      {
        A[i] = A[i] + 1 # If the individual survives, increase age by 1.
      }
    }
    
    R[t] = mean(P) # Record the mean phenotypic value in 'R'
  }
  
  return(R) # Return the results vector.
}

# Summary of parameters
# n = population size
# p = survival probability between age 1 and age 2
# alpha = fecundity difference between age 1 and age 2
# b = fecundity scaling constant
# ps = survival probability from one age-class to the next
# u = mutation rate
# sig = size of mutations (standard deviation of a Gaussian)
# nt = number of timesteps

sim=AP(n=, nt=, p=, alpha=, b=, u=, sig=)