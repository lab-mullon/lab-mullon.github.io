######################################
####### Mutation accumulation ########
######################################

### Summary of parameters

# n = population size
# b0 = fecundity constant
# p = survival probability from age 1 to age 2
# u = mutation rate
# omega = intensity of selection
# sigma = size of the mutation step
# Tf = number of timesteps


### Simulation program

MA=function(n, b0, p, u, omega, sigma, Tf, n_a)
{
  A=rep(1,n) # vector of ages
  Z=matrix(0, nrow=n, ncol=n_a) # matrix of phenotype, each line is an individual, one phenotype per age-class.
  R=matrix(0, nrow=Tf, ncol=n_a) # Results matrix 
  
  # At each timestep,
  for(t in 1:Tf) 
  {
    fec=rep(0,n) # Create a vector of size n containing zeroes.
    for(i in 1:n)
    {
      fec[i] = b0*exp(-(Z[ i, A[i] ]^2)/omega) ################# ?
    }

    Zp = Z # Copy the parental population into the Zp vector.
    
    for(i in 1:n) # For each individual,
    {
      if( (runif(1) > p) || (A[i] == n_a) ) ########### ?
      {
        par=sample(x = c(1:n), size = 1, prob=fec) ######### ?
        
        A[i] = 1 # Offspring age is set to 1.
        Z[i,] = Zp[par,] # Offspring inherits the parental trait values.
        
        for(j in 1:n_a) # For each trait,
        {
          if(runif(1) < u) # Offspring mutates with probability u
          {
            Z[i,j] = Z[i,j] + sigma*rnorm(n = 1, mean = 0, sd = 1) # When it mutates, the new value is sampled in a Gaussian centered on the parental value.
          }
        }
      }
      else
      {
        A[i] = A[i] + 1 # Increase individual age by 1.
      }
      
    } # End of loop over individuals
    
    for(j in 1:n_a) # For each trait,
    {
      R[t,j] = mean( b0*exp(-(Z[,j]^2)/omega)  ) # Record mean fecundity and store it in the appropriate column and row in R.
    }
    
  } # End of loop over time.
  
  return(R) # Return the result matrix R.
}

### Run simulations

sim = MA(n=, b0=, p=, u=, omega=, sigma=, Tf=, n_a=)