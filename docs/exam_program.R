######################################
####### Mutation accumulation ########
######################################

### Summary of parameters

# n = population size
# b0 = fecundity constant
# p = survival probability from age 1 to age 2
# u = mutation rate
# omega = intensity of selection
# sig_u = size of the mutation step
# Tf = number of timesteps

### Simulation program

MA=function(n, b0, p, u, omega, sigma, Tf, mes)
{
  A=rep(1,n) # vector of ages
  Z=matrix(0, nrow=n, ncol=2) # matrix of phenotype, each line is an individual, one phenotype per age-class.
  R=matrix(0, nrow=Tf, ncol=2) # Results matrix 
  
  # At each timestep,
  for(t in 1:Tf) 
  {
    fec=rep(0,n)
    for(i in 1:n)
    {
      fec[i] = b0*exp(-(Z[i, A[i]]^2)/omega) 
    }
    
    Zp = Z 
    
    for(i in 1:n)
    {
      if( (runif(1) > p) || (A[i] == 2) ) 
      {
        par=sample(x = c(1:n), size = 1, prob=fec) 
        
        A[i] = 1 
        Z[i,] = Zp[par,] 
        
        for(j in 1:2)
        {
          if(runif(1) < u)
          {
            Z[i,j] = Z[i,j] + sigma*rnorm(n = 1, mean = 0, sd = 1)
          }
        }
      }
      else
      {
        A[i] = A[i] + 1
      }
      
    }
    
    for(j in 1:2)
    {
      R[t,j] = mean(Z[,j])
    }
    
  } # End of loop over time.
  
  return(R) # Return the result matrix R.
}

### Run simulations

sim = MA(n=, b0=, p=, u=, omega=, sigma=, Tf=)
